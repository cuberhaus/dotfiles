#!/usr/bin/env python3
# Description: Check whether a GIF will read well on the ASUS AniMe Vision lid (ROG Strix SCAR 16) and write a lid-ready copy.
"""anime-gif-check: will this GIF read on the 810-LED lid of a ROG Strix SCAR 16 (G635L)?

The lid is a diagonal strip of 810 white LEDs with no colour and about 15 LEDs of width, so most GIFs
turn into mush. This tool simulates what ``asusctl anime gif`` would show and applies the filter that
was built for finding animations that survive it:

  * the lattice and the 16-sample diamond filter mirror ``rog-anime/src/image.rs`` of asusctl 6.3.8
    (the simulation matched its GIF decoder exactly), and the 810-LED layout reproduces the one in
    the Vision data pack's ``mask.csv``;
  * each GIF is cropped to its content, fitted into a 702x160 "strip space" canvas whose long axis
    runs along the strip, then simulated with the flags ``--scale 1.225 --angle 0.607 --x-pos -2.43
    --y-pos 1.49`` (the lid-ready copy is saved that way);
  * the thresholds were calibrated on ASUS's own gallery GIFs (18-29% of the LEDs lit, peak at full
    brightness, 58-79% of the lit LEDs bold) and on animations that were judged unreadable on the lid
    (5-12% lit, fine-detail loss 0.25-0.50). Of 7,578 GIFs harvested from GifCities and Wikimedia
    Commons, 6,487 could be scored and the first seven stages kept 250 of them (3.9%). Text banners
    and solid blocks pass those stages, hence the last two; they left 191.

The checks measure legibility, not subject: a pass says the shapes are big, bright, bold and moving,
not that the GIF depicts what its name claims. The physical orientation of the strip on the lid is not
verified. The tool only reads the GIFs it is given and writes into the folder passed to ``--out``.
"""

from __future__ import annotations

import argparse
from collections import deque
from dataclasses import asdict, dataclass, field
import json
import math
import os
from pathlib import Path
import shlex
import sys
from typing import Callable, List, Optional, Tuple

try:
    import numpy as np
    from PIL import Image, ImageDraw, ImageFilter, ImageSequence
except ImportError as exc:  # numpy and Pillow are not installed by the bootstraps on purpose
    if __name__ != "__main__":
        raise
    sys.stderr.write(
        f"anime-gif-check: {exc}\n"
        "It needs NumPy and Pillow (nothing is installed for you):\n"
        "  Ubuntu/Debian: sudo apt install python3-numpy python3-pil\n"
        "  Arch/Manjaro:  sudo pacman -S python-numpy python-pillow\n"
        "  macOS:         brew install numpy pillow\n"
    )
    raise SystemExit(2)

# --------------------------------------------------------------------------------------------------
# Lid geometry and the asusctl image pipeline (validated against the real decoder, max difference 0)
# --------------------------------------------------------------------------------------------------

SCALE_X = 0.77  # cm between LEDs in a row (asusctl's provisional upstream value)
SCALE_Y = 0.28  # cm between LED rows
PHYS_W = (33.0 + 0.5) * SCALE_X
PHYS_H = 68.0 * SCALE_Y
GROUP = (0.0, 0.5, 1.0, 1.5)  # the 4x4 sample grid of the diamond filter

CANVAS_W, CANVAS_H = 702, 160  # strip space: about 4.4:1, 10.2 canvas px per LED
STRIP_FLAGS = {"scale": 1.225, "angle": 0.607, "tx": -2.43, "ty": 1.49}  # maps the canvas onto the diagonal band
PLAY_FLAGS = (f"--scale {STRIP_FLAGS['scale']} --angle {STRIP_FLAGS['angle']} "
              f"--x-pos {STRIP_FLAGS['tx']} --y-pos {STRIP_FLAGS['ty']}")  # what `asusctl anime gif` needs for it
PX_PER_LED = 10.2


def led_positions() -> "np.ndarray":
    """(x, y) of every LED in LED units, in the order asusd packs them (210-LED triangle + 600-LED band)."""
    points: List[Tuple[float, float]] = []
    for y in range(68):
        width = y // 2 + 1 if y < 28 else 15
        first = 0 if y < 28 else (y - 28) // 2
        for k in range(width):
            points.append((first + k - 0.5 * (y % 2), float(y)))
    return np.array(points, dtype=np.float64)


LEDS = led_positions()
LED_CENTER = (LEDS.min(axis=0) + LEDS.max(axis=0)) * 0.5 + np.array([0.0, 1.0])


def _translate(x: float, y: float) -> "np.ndarray":
    return np.array([[1, 0, x], [0, 1, y], [0, 0, 1]], dtype=np.float64)


def _scale(x: float, y: float) -> "np.ndarray":
    return np.array([[x, 0, 0], [0, y, 0], [0, 0, 1]], dtype=np.float64)


def _rotate(a: float) -> "np.ndarray":
    c, s = math.cos(a), math.sin(a)
    return np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]], dtype=np.float64)


def px_from_led(width: int, height: int, scale: float, angle: float, tx: float, ty: float) -> "np.ndarray":
    base = min(PHYS_W / width, PHYS_H / height)
    led_from_px = (
        _translate(*LED_CENTER)
        @ _scale(1 / SCALE_X, 1 / SCALE_Y)
        @ (_translate(tx, ty) @ _rotate(angle) @ _scale(scale, scale))
        @ _scale(base, base)
        @ _translate(-0.5 * width, -0.5 * height)
    )
    return np.linalg.inv(led_from_px)


def led_values(gray: "np.ndarray", scale: float = 1.0, angle: float = 0.0, tx: float = 0.0,
               ty: float = 0.0, bright: float = 1.0) -> "np.ndarray":
    """Brightness (0-255) of the 810 LEDs for one opaque grayscale frame of shape (height, width)."""
    height, width = gray.shape
    m = px_from_led(width, height, scale, angle, tx, ty)
    du = m[:2, :2] @ np.array([-0.5, 0.5])
    dv = m[:2, :2] @ np.array([0.5, 0.5])
    pos = np.column_stack([LEDS[:, 0], LEDS[:, 1] - 0.5, np.ones(len(LEDS))])
    x0 = (m @ pos.T).T[:, :2]
    total = np.zeros(len(LEDS))
    count = np.zeros(len(LEDS))
    for u in GROUP:
        for v in GROUP:
            s = x0 + u * du + v * dv
            xi = np.trunc(s[:, 0]).astype(np.int64)
            yi = np.trunc(s[:, 1]).astype(np.int64)
            ok = (xi <= width - 1) & (yi <= height - 1) & (xi >= 0) & (yi >= 0)
            vals = np.zeros(len(LEDS))
            vals[ok] = gray[yi[ok], xi[ok]]
            total += vals
            count += ok
    avg = np.divide(total, count, out=np.zeros_like(total), where=count > 0)
    return np.clip(avg * bright, 0, 255).astype(np.uint8)


def render(values: "np.ndarray", px_per_cm: float = 13.0) -> "Image.Image":
    """Draw the 810 LEDs (matrix orientation, LED row 0 on top) the way the lid would show them."""
    margin = 0.6
    xs, ys = LEDS[:, 0] * SCALE_X, LEDS[:, 1] * SCALE_Y
    x_off = -xs.min() + margin
    img = Image.new("RGB", (int((xs.max() - xs.min() + 2 * margin) * px_per_cm), int((ys.max() + 2 * margin) * px_per_cm)),
                    (8, 8, 10))
    draw = ImageDraw.Draw(img)
    radius = 0.19 * px_per_cm
    for x, y, v in zip(xs, ys, values):
        cx, cy = (x + x_off) * px_per_cm, (y + margin) * px_per_cm
        level = int(round(255 * (v / 255.0) ** 0.9))
        colour = (max(level, 22), max(level, 22), max(level, 24)) if v else (24, 24, 28)
        draw.ellipse((cx - radius, cy - radius, cx + radius, cy + radius), fill=colour)
    return img


# --------------------------------------------------------------------------------------------------
# Decoding and fitting a GIF into strip space
# --------------------------------------------------------------------------------------------------

MIN_FRAMES = 3
MAX_FRAMES = 160
MIN_DELAY_MS = 40
SIGMA = 4.5  # canvas px: about half of one LED footprint
MAX_UPSCALE = 14.0
COLOUR_BLEND = 0.5  # 1.0 = Rec.709 luma, 0.0 = brightest channel. Half and half keeps blue/red sprites visible.

LANCZOS = getattr(Image, "Resampling", Image).LANCZOS
BICUBIC = getattr(Image, "Resampling", Image).BICUBIC


class Skip(Exception):
    """The GIF cannot be scored (too few frames, empty, not strip space...)."""


def decode(path: Path, max_frames: int) -> Tuple["np.ndarray", List[int]]:
    """(n, h, w) brightness 0..1 composited on black, plus frame delays in ms."""
    frames: List["np.ndarray"] = []
    delays: List[int] = []
    with Image.open(path) as im:
        n = getattr(im, "n_frames", 1)
        if n < MIN_FRAMES:
            raise Skip(f"only {n} frame(s); an animation needs at least {MIN_FRAMES}")
        if n > max_frames:
            raise Skip(f"{n} frames; the limit is {max_frames} (see --max-frames)")
        size0 = None
        for frame in ImageSequence.Iterator(im):
            rgba = frame.convert("RGBA")
            if size0 is None:
                size0 = rgba.size
            elif rgba.size != size0:
                rgba = rgba.resize(size0)
            canvas = Image.new("RGBA", rgba.size, (0, 0, 0, 255))  # asusctl lights opaque pixels only
            canvas.alpha_composite(rgba)
            a = np.asarray(canvas.convert("RGB"), dtype=np.float32)
            luma = 0.2126 * a[..., 0] + 0.7152 * a[..., 1] + 0.0722 * a[..., 2]
            frames.append((COLOUR_BLEND * luma + (1.0 - COLOUR_BLEND) * a.max(axis=2)) / 255.0)
            d = int(frame.info.get("duration", 100))
            delays.append(100 if d <= 0 else max(d, MIN_DELAY_MS))
    return np.stack(frames), delays


def fix_background(a: "np.ndarray") -> Tuple["np.ndarray", str]:
    """Light backgrounds are inverted and coloured ones subtracted, so the subject is what lights up."""
    ring = np.concatenate([a[:, 0, :].ravel(), a[:, -1, :].ravel(), a[:, :, 0].ravel(), a[:, :, -1].ravel()])
    bg = float(np.median(ring))
    if bg > 0.55:
        return 1.0 - a, "inverted"
    if bg > 0.10:
        return np.clip((a - bg) / (1.0 - bg), 0, 1), "background subtracted"
    return a, "kept"


def fit_to_strip(a: "np.ndarray") -> Tuple["np.ndarray", float]:
    """Crop to the content of the whole animation and fit it into the canvas. Returns (uint8, fg_box)."""
    ys, xs = np.where((a > 0.2).any(axis=0))
    if len(xs) < 12:
        raise Skip("no visible content")
    x0, x1 = max(xs.min() - 1, 0), min(xs.max() + 2, a.shape[2])
    y0, y1 = max(ys.min() - 1, 0), min(ys.max() + 2, a.shape[1])
    crop = a[:, y0:y1, x0:x1]
    bh, bw = crop.shape[1:]
    if bw < 6 or bh < 6:
        raise Skip("content is smaller than 6 px")
    scale = min(CANVAS_W / bw, CANVAS_H / bh, MAX_UPSCALE)
    nw, nh = max(int(round(bw * scale)), 1), max(int(round(bh * scale)), 1)
    method = LANCZOS if scale < 1 else BICUBIC
    out = np.zeros((a.shape[0], CANVAS_H, CANVAS_W), dtype=np.uint8)
    ox, oy = (CANVAS_W - nw) // 2, (CANVAS_H - nh) // 2
    for i, frame in enumerate(crop):
        img = Image.fromarray((np.clip(frame, 0, 1) * 255).astype(np.uint8)).resize((nw, nh), method)
        out[i, oy:oy + nh, ox:ox + nw] = np.asarray(img)
    return out, float((crop > 0.2).mean())


def auto_levels(u8: "np.ndarray") -> "np.ndarray":
    """Lift dim animations (gain up to 3x) so the 99th percentile of the lit pixels reaches 95%."""
    f = u8.astype(np.float32) / 255.0
    sample = f[:, ::4, ::4]
    fg = sample[sample > 0.05]
    if fg.size < 20:
        raise Skip("nothing visible after fitting")
    p = float(np.percentile(fg, 99))
    f = np.clip(f * float(np.clip(0.95 / max(p, 1e-3), 1.0, 3.0)), 0, 1)
    f[f < 0.06] = 0
    return (f * 255).astype(np.uint8)


def prepare(path: Path, strip_space: bool, max_frames: int) -> Tuple["np.ndarray", List[int], float, str]:
    """The canvas frames the lid would receive: (uint8 n x 160 x 702, delays, fg_box, background mode)."""
    a, delays = decode(path, max_frames)
    if strip_space:  # already authored for the lid: only the size is checked
        if a.shape[1:] != (CANVAS_H, CANVAS_W):
            raise Skip(f"--strip-space needs {CANVAS_W}x{CANVAS_H} frames, got {a.shape[2]}x{a.shape[1]}")
        ys, xs = np.where((a > 0.2).any(axis=0))
        if len(xs) < 12:
            raise Skip("no visible content")
        crop = a[:, ys.min():ys.max() + 1, xs.min():xs.max() + 1]
        return (a * 255).astype(np.uint8), delays, float((crop > 0.2).mean()), "as authored"
    a, background = fix_background(a)
    u8, fg_box = fit_to_strip(a)
    return auto_levels(u8), delays, fg_box, background


# --------------------------------------------------------------------------------------------------
# What the animation looks like once it is on the LEDs
# --------------------------------------------------------------------------------------------------

def detail_loss(u8: "np.ndarray") -> float:
    """Share of the picture's light that lives below the LED resolution (flat shapes ~0, faces high)."""
    low = np.asarray(Image.fromarray(u8).filter(ImageFilter.GaussianBlur(SIGMA)), dtype=np.float32)
    f = u8.astype(np.float32)
    return float(np.abs(f - low).sum() / (f.sum() + 1e-6))


def simulate(u8: "np.ndarray") -> "np.ndarray":
    """(n, 810) LED brightness 0-255 for every frame, with the strip-space flags."""
    return np.stack([led_values(f.astype(np.float64), **STRIP_FLAGS) for f in u8])


def flicker_swings(led: "np.ndarray", loop_s: float) -> float:
    """Large whole-lid brightness swings per second (frame to frame change of the total light above 30%)."""
    total = led.astype(np.float64).sum(axis=1)
    step = np.abs(np.diff(np.r_[total, total[:1]])) / max(float(total.max()), 1.0)
    return float((step > 0.30).sum() / loop_s) if loop_s else 0.0


# --- text banners and solid blocks ----------------------------------------------------------------
# Short bold words ("NEW", "NEXT", "WIPE") and flashing squares are perfectly legible on 810 LEDs, so
# the stages above cannot reject them. Both checks look at what is drawn, on the canvas the lid receives.

TEXT_DS = 4          # canvas px -> analysis px
TEXT_REL_THR = 0.35  # foreground = brighter than this share of the frame's peak
TEXT_MIN_H = 5       # the tallest blob must be at least this tall (analysis px)
TEXT_NEED_ONE = 4    # letters in a single frame that settle it
TEXT_NEED_FEW = 3    # letters needed in at least three of the sampled frames
SAME_SIZE = 0.15     # blobs within +-15% of the median width are one icon repeated, not letters
SOLID_FILL = 0.88    # share of the bounding box that is bright in the fullest frame; a disc is ~0.79


def _components(mask: "np.ndarray") -> List[Tuple[int, int, int, int, int]]:
    """8-connected components as (x0, y0, x1, y1, area)."""
    h, w = mask.shape
    seen = np.zeros_like(mask, dtype=bool)
    out = []
    for y in range(h):
        for x in range(w):
            if not mask[y, x] or seen[y, x]:
                continue
            queue = deque([(y, x)])
            seen[y, x] = True
            x0 = x1 = x
            y0 = y1 = y
            area = 0
            while queue:
                cy, cx = queue.popleft()
                area += 1
                x0, x1, y0, y1 = min(x0, cx), max(x1, cx), min(y0, cy), max(y1, cy)
                for dy in (-1, 0, 1):
                    for dx in (-1, 0, 1):
                        ny, nx = cy + dy, cx + dx
                        if 0 <= ny < h and 0 <= nx < w and mask[ny, nx] and not seen[ny, nx]:
                            seen[ny, nx] = True
                            queue.append((ny, nx))
            out.append((x0, y0, x1, y1, area))
    return out


def letters_in_frame(frame_u8: "np.ndarray") -> int:
    """Length of the longest contiguous chain of similar-height blobs of varied width on one line."""
    f = frame_u8[: frame_u8.shape[0] // TEXT_DS * TEXT_DS, : frame_u8.shape[1] // TEXT_DS * TEXT_DS].astype(np.float32)
    ds = f.reshape(f.shape[0] // TEXT_DS, TEXT_DS, f.shape[1] // TEXT_DS, TEXT_DS).max(axis=(1, 3))
    peak = ds.max()
    if peak < 40:
        return 0
    comps = [c for c in _components(ds >= TEXT_REL_THR * peak) if c[4] >= 4]
    if not comps:
        return 0
    heights = np.array([c[3] - c[1] + 1 for c in comps], dtype=np.float64)
    hmax = heights.max()
    if hmax < TEXT_MIN_H:
        return 0
    cy = np.array([(c[1] + c[3]) / 2 for c in comps])
    tall = heights >= 0.45 * hmax
    if not tall.any():
        return 0
    med = np.median(cy[tall])
    line = sorted((c for c, t, y in zip(comps, tall, cy) if t and abs(y - med) <= 0.30 * hmax), key=lambda c: c[0])
    chains: List[list] = []
    cur: list = [line[0]] if line else []
    for c in line[1:]:
        if c[0] - cur[-1][2] <= 1.2 * hmax:  # the next blob starts within about one letter height
            cur.append(c)
        else:
            chains.append(cur)
            cur = [c]
    if cur:
        chains.append(cur)
    best = 0
    for chain in chains:
        if len(chain) < 3:
            continue
        widths = np.array([c[2] - c[0] + 1 for c in chain], dtype=np.float64)
        if np.all(np.abs(widths - np.median(widths)) <= SAME_SIZE * np.median(widths)):
            continue  # identical icons in a row (three owls), not letters
        best = max(best, len(chain))
    return best


def is_text_like(u8: "np.ndarray") -> bool:
    idx = np.linspace(0, len(u8) - 1, min(8, len(u8))).astype(int)
    counts = [letters_in_frame(u8[i]) for i in idx]
    return max(counts) >= TEXT_NEED_ONE or sum(1 for c in counts if c >= TEXT_NEED_FEW) >= 3


def solid_fill(u8: "np.ndarray") -> float:
    """Share of the content's bounding box that is bright (>50% of peak) in the fullest frame."""
    on = u8 > 0.5 * float(u8.max())
    fullest = int(on.reshape(len(u8), -1).sum(axis=1).argmax())
    ink = on.any(axis=0)
    xs = np.where(ink.any(axis=0))[0]
    ys = np.where(ink.any(axis=1))[0]
    return float(on[fullest][ys.min():ys.max() + 1, xs.min():xs.max() + 1].mean())


# --------------------------------------------------------------------------------------------------
# The filter
# --------------------------------------------------------------------------------------------------

@dataclass
class Report:
    path: str
    frames: int = 0
    loop_s: float = 0.0
    lit: float = 0.0
    peak: float = 0.0
    bold: float = 0.0
    motion_ps: float = 0.0
    static_ratio: float = 0.0
    detail_loss: float = 0.0
    fg_box: float = 0.0
    fill: float = 0.0
    text_like: bool = False
    flicker_swings_per_s: float = 0.0
    background: str = ""
    skip: str = ""  # why it could not be scored; empty when it was
    stages: List[dict] = field(default_factory=list)
    passed: bool = False


@dataclass
class Stage:
    key: str
    label: str
    ok: Callable[[Report], bool]
    show: Callable[[Report], str]


# Thresholds, calibrated on measurements (docs/ASUSCTL.md, "Lid animations"). The stage labels print
# these same values, so a label can never disagree with its check. SOLID_FILL is defined with the
# text and solid checks above.
LOOP_S = (0.3, 24.0)       # seconds for one loop
LIT = (0.14, 0.50)         # share of the 810 LEDs that are lit (above 25%) on average
PEAK_MIN = 0.85            # brightness reached by the 99.5th percentile LED
BOLD_MIN = 0.40            # share of the lit LEDs that are above 60%
MOTION_PS = (0.15, 3.0)    # mean change of the LEDs per second
STILL_MAX = 0.50           # share of frame steps that may be (nearly) unchanged
DETAIL_MAX = 0.22          # fine-detail loss
FLOOD_MAX = 0.80           # share of the content's box that is lit

STAGES: Tuple[Stage, ...] = (
    Stage("loop", f"loops in {LOOP_S[0]:g}-{LOOP_S[1]:g} s", lambda r: LOOP_S[0] <= r.loop_s <= LOOP_S[1],
          lambda r: f"{r.loop_s:.1f} s"),
    Stage("lit", f"lights {LIT[0] * 100:.0f}-{LIT[1] * 100:.0f}% of the LEDs", lambda r: LIT[0] <= r.lit <= LIT[1],
          lambda r: f"{r.lit:.0%}"),
    Stage("peak", f"reaches at least {PEAK_MIN:.0%} brightness", lambda r: r.peak >= PEAK_MIN,
          lambda r: f"{r.peak:.0%}"),
    Stage("bold", f"at least {BOLD_MIN:.0%} of the lit LEDs are bold", lambda r: r.bold >= BOLD_MIN,
          lambda r: f"{r.bold:.0%}"),
    Stage("motion", f"moves ({MOTION_PS[0]:g}-{MOTION_PS[1]:.1f} per s) and is not frozen",
          lambda r: MOTION_PS[0] <= r.motion_ps <= MOTION_PS[1] and r.static_ratio <= STILL_MAX,
          lambda r: f"{r.motion_ps:.2f} per s, {r.static_ratio:.0%} of steps still"),
    Stage("detail", f"fine-detail loss at most {DETAIL_MAX:.2f}", lambda r: r.detail_loss <= DETAIL_MAX,
          lambda r: f"{r.detail_loss:.2f}"),
    Stage("flood", "not a full-frame flood", lambda r: r.fg_box <= FLOOD_MAX,
          lambda r: f"{r.fg_box:.0%} of its box"),
    Stage("text", "not a text banner", lambda r: not r.text_like,
          lambda r: "looks like text" if r.text_like else "no text"),
    Stage("solid", f"has a silhouette (fill at most {SOLID_FILL:.0%})", lambda r: r.fill <= SOLID_FILL,
          lambda r: f"{r.fill:.0%} filled"),
)


def analyse(path: Path, strip_space: bool = False, max_frames: int = MAX_FRAMES
            ) -> Tuple[Report, Optional["np.ndarray"], Optional[List[int]], Optional["np.ndarray"]]:
    """Run one GIF through the pipeline. Returns (report, canvas frames, delays, LED values) or Nones on skip."""
    report = Report(path=str(path))
    try:
        u8, delays, fg_box, background = prepare(path, strip_space, max_frames)
    except Skip as exc:
        report.skip = str(exc)
        return report, None, None, None
    except Exception as exc:  # corrupt GIFs are normal on the old web
        report.skip = f"unreadable: {type(exc).__name__}: {str(exc)[:80]}"
        return report, None, None, None

    led = simulate(u8)
    led_f = led.astype(np.float32) / 255.0
    lit_mask = led_f > 0.25
    loop_s = sum(delays) / 1000.0
    diffs = np.abs(np.diff(np.vstack([led_f, led_f[:1]]), axis=0)).mean(axis=1)
    idx = np.linspace(0, len(u8) - 1, min(len(u8), 10)).astype(int)

    report.frames = len(u8)
    report.loop_s = round(loop_s, 2)
    report.lit = round(float(lit_mask.mean()), 3)
    report.peak = round(float(np.percentile(led_f, 99.5)), 3)
    report.bold = round(float((led_f[lit_mask] > 0.6).mean()) if lit_mask.any() else 0.0, 3)
    report.motion_ps = round(float(diffs.sum() / loop_s) if loop_s else 0.0, 3)
    report.static_ratio = round(float((diffs < 0.002).mean()), 3)
    report.detail_loss = round(float(np.mean([detail_loss(u8[i]) for i in idx])), 3)
    report.fg_box = round(fg_box, 3)
    report.fill = round(solid_fill(u8), 3)
    report.text_like = bool(is_text_like(u8))
    report.flicker_swings_per_s = round(flicker_swings(led, loop_s), 1)
    report.background = background
    report.stages = [{"key": s.key, "label": s.label, "ok": bool(s.ok(report)), "value": s.show(report)} for s in STAGES]
    report.passed = all(s["ok"] for s in report.stages)
    return report, u8, delays, led


# --------------------------------------------------------------------------------------------------
# Output
# --------------------------------------------------------------------------------------------------

def save_lid_gif(u8: "np.ndarray", delays: List[int], out: Path) -> None:
    """An opaque grayscale GIF (R=G=B) that asusctl reads as brightness."""
    images = [Image.fromarray(f, "L") for f in u8]
    images[0].save(out, save_all=True, append_images=images[1:], duration=delays, loop=0, optimize=False, disposal=1)


def save_preview_gif(led: "np.ndarray", delays: List[int], out: Path) -> None:
    frames = [render(v) for v in led]
    frames[0].save(out, save_all=True, append_images=frames[1:], duration=delays, loop=0, optimize=False)


def write_outputs(out_dir: Path, name: str, u8: "np.ndarray", delays: List[int], led: "np.ndarray",
                  force: bool) -> Tuple[List[Path], List[Path]]:
    """Write NAME.lid.gif and NAME.preview.gif without replacing existing files unless forced."""
    written: List[Path] = []
    skipped: List[Path] = []
    out_dir.mkdir(parents=True, exist_ok=True)
    for suffix, writer in ((".lid.gif", lambda p: save_lid_gif(u8, delays, p)),
                           (".preview.gif", lambda p: save_preview_gif(led, delays, p))):
        target = out_dir / f"{name}{suffix}"
        if target.exists() and not force:
            skipped.append(target)
            continue
        writer(target)
        written.append(target)
    return written, skipped


class Style:
    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled

    def _wrap(self, code: str, text: str) -> str:
        return f"\033[{code}m{text}\033[0m" if self.enabled else text

    def ok(self, text: str) -> str:
        return self._wrap("32", text)

    def warn(self, text: str) -> str:
        return self._wrap("33", text)

    def fail(self, text: str) -> str:
        return self._wrap("31", text)

    def info(self, text: str) -> str:
        return self._wrap("34", text)


def print_report(report: Report, style: Style) -> None:
    name = Path(report.path).name
    if report.skip:
        print(f"{name}\n  {style.fail('SKIP')}  {report.skip}")
        return
    verdict = style.ok("PASS") if report.passed else style.fail("FAIL")
    print(f"{name}  {report.frames} frames, {report.loop_s:.1f} s loop, background {report.background}  {verdict}")
    for s in report.stages:
        mark = style.ok("ok  ") if s["ok"] else style.fail("FAIL")
        print(f"  {mark}  {s['label']:<44s} {s['value']}")
    if report.flicker_swings_per_s > 3.0:
        print("  " + style.warn(f"note  flickers: {report.flicker_swings_per_s:.1f} big brightness swings per second"))


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="anime-gif-check",
        description="Check whether GIFs will read well on the ROG Strix SCAR 16 AniMe Vision lid, "
                    "and optionally write lid-ready copies. Never plays anything on the lid.",
        epilog=f"Play a lid-ready copy with: asusctl anime gif --path FILE.lid.gif {PLAY_FLAGS}\n"
               "The command stays attached while it plays. The daemon turns its built-in animations off on the "
               "first write; restore them with: asusctl anime --enable-powersave-anim true",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("files", nargs="+", metavar="FILE", help="GIF files to check")
    parser.add_argument("-o", "--out", metavar="DIR", help="write NAME.lid.gif and NAME.preview.gif here for GIFs that pass")
    parser.add_argument("-a", "--all", action="store_true", help="with --out, also write the files that fail")
    parser.add_argument("-f", "--force", action="store_true", help="with --out, replace files that already exist")
    parser.add_argument("--strip-space", action="store_true",
                        help=f"the GIFs are already {CANVAS_W}x{CANVAS_H} lid-ready art: skip cropping and fitting")
    parser.add_argument("--max-frames", type=int, default=MAX_FRAMES, metavar="N",
                        help=f"skip GIFs with more frames (default {MAX_FRAMES}; the checks run per frame)")
    parser.add_argument("-j", "--json", action="store_true", help="print the reports as JSON instead of text")
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    style = Style(sys.stdout.isatty() and "NO_COLOR" not in os.environ and not args.json)
    out_dir = Path(args.out).expanduser() if args.out else None
    reports: List[Report] = []
    written_any: List[Path] = []
    status = 0
    for raw in args.files:
        path = Path(raw).expanduser()
        if not path.is_file():
            report = Report(path=str(path), skip="not a file")
            art = (None, None, None)
        else:
            report, *rest = analyse(path, strip_space=args.strip_space, max_frames=args.max_frames)
            art = tuple(rest)
        reports.append(report)
        if not report.passed:
            status = 1
        if not args.json:
            print_report(report, style)
        if out_dir is not None and art[0] is not None and (report.passed or args.all):
            written, skipped = write_outputs(out_dir, path.stem, art[0], art[1], art[2], args.force)
            written_any += [p for p in written if p.name.endswith(".lid.gif") and report.passed]
            if not args.json:
                for p in written:
                    print(f"  {style.info('wrote')}  {p}")
                for p in skipped:
                    print(f"  {style.warn('exists')} {p} (use --force to replace)")
    if args.json:
        print(json.dumps([asdict(r) for r in reports], indent=2))
    elif written_any:
        print(f"\nPlay one (not run): asusctl anime gif --path {shlex.quote(str(written_any[0]))} {PLAY_FLAGS}")
        print("It stays attached while it plays and turns the built-in animations off; restore them with "
              "`asusctl anime --enable-powersave-anim true`.")
    return status


if __name__ == "__main__":
    sys.exit(main())
