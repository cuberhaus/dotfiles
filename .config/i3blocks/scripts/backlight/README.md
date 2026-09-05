# backlight

Show and adjust the laptop backlight using `brightnessctl`.
Clicking uses `xset` to turn off the backlight, scrolling increases or decreases
the brightness.

![](example.png)

## Setup / Usage

Example i3blocks configuration:

```
[backlight]
command=$SCRIPT_DIR/backlight
label=☀
interval=20
#STEP_SIZE=5
```

- right click: turn off backlight
- scroll: increase/decrease the brightness in percentage steps according to `STEP_SIZE`

## Dependencies

These tools are needed:

- `brightnessctl`
- `xorg-xset`

The Ubuntu and Arch bootstrap adds the user to the `video` group so
`brightnessctl` can write the kernel backlight device after the next login.
