#!/usr/bin/env python3
"""
Estimate Claude Code token usage and approximate cost across every session
recorded under ~/.claude/projects/.

Reads the JSONL session transcripts Claude Code writes per project, sums up
token usage by model, and prints a human-readable cost breakdown using the
public Anthropic API list prices (per million tokens).

If you're routing Claude Code through Bedrock or Vertex (CLAUDE_CODE_USE_BEDROCK
or CLAUDE_CODE_USE_VERTEX), the per-token rates differ — pass --rates
bedrock|vertex to use those numbers instead. Bedrock and Vertex rates are
ballparked from public pricing as of late 2025; check the AWS / GCP pages
for authoritative spend.

Usage:
  python claude-cost-stats.py                     # everything, Anthropic rates
  python claude-cost-stats.py --since 30d         # last 30 days only
  python claude-cost-stats.py --since 2026-01-01  # since a specific date
  python claude-cost-stats.py --by project        # break down per project (cwd)
  python claude-cost-stats.py --rates bedrock     # Bedrock pricing
  python claude-cost-stats.py --json              # machine-readable output
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

# ─── Pricing (USD per 1M tokens, as of late 2025) ──────────────────────────
#
# Anthropic API list prices. Cache writes are 1.25× input price for 5-minute
# cache, 2× for 1-hour cache. Cache reads are 0.1× input price.
# https://www.anthropic.com/pricing
#
# Bedrock and Vertex match Anthropic API rates closely for input/output but
# may bill cache differently — these numbers are best-effort estimates;
# trust the cloud provider's billing console for hard numbers.

PRICING = {
    'anthropic': {
        # Opus 4.x
        'claude-opus-4-7':              {'input': 15.0, 'cache_write_5m': 18.75, 'cache_write_1h': 30.0, 'cache_read': 1.50, 'output': 75.0},
        'claude-opus-4-6':              {'input': 15.0, 'cache_write_5m': 18.75, 'cache_write_1h': 30.0, 'cache_read': 1.50, 'output': 75.0},
        'claude-opus-4-5':              {'input': 15.0, 'cache_write_5m': 18.75, 'cache_write_1h': 30.0, 'cache_read': 1.50, 'output': 75.0},
        # Sonnet 4.x
        'claude-sonnet-4-6':            {'input':  3.0, 'cache_write_5m':  3.75, 'cache_write_1h':  6.0, 'cache_read': 0.30, 'output': 15.0},
        'claude-sonnet-4-5':            {'input':  3.0, 'cache_write_5m':  3.75, 'cache_write_1h':  6.0, 'cache_read': 0.30, 'output': 15.0},
        'claude-sonnet-4-5-20250929':   {'input':  3.0, 'cache_write_5m':  3.75, 'cache_write_1h':  6.0, 'cache_read': 0.30, 'output': 15.0},
        'claude-sonnet-4-0':            {'input':  3.0, 'cache_write_5m':  3.75, 'cache_write_1h':  6.0, 'cache_read': 0.30, 'output': 15.0},
        # Haiku 4.x
        'claude-haiku-4-5':             {'input':  1.0, 'cache_write_5m':  1.25, 'cache_write_1h':  2.0, 'cache_read': 0.10, 'output':  5.0},
        'claude-haiku-4-5-20251001':    {'input':  1.0, 'cache_write_5m':  1.25, 'cache_write_1h':  2.0, 'cache_read': 0.10, 'output':  5.0},
    },
}
# Bedrock and Vertex use the same per-token rates as Anthropic for the
# Claude family at time of writing; the differences are in cache billing
# (Bedrock charges full input rate for cache reads on some plans). Treat as
# Anthropic-equivalent unless the user asks otherwise.
PRICING['bedrock'] = PRICING['anthropic']
PRICING['vertex'] = PRICING['anthropic']

DEFAULT_FALLBACK_PRICE = PRICING['anthropic']['claude-sonnet-4-5']


def find_transcripts(root: Path) -> list[Path]:
    """Every *.jsonl under ~/.claude/projects/."""
    if not root.is_dir():
        return []
    return sorted(root.glob('**/*.jsonl'))


def parse_since(spec: str | None) -> datetime | None:
    """Parse 'NNd', 'NNh', or YYYY-MM-DD into a tz-aware datetime."""
    if not spec:
        return None
    now = datetime.now(timezone.utc)
    m = re.fullmatch(r'(\d+)([dhwm])', spec)
    if m:
        n, unit = int(m.group(1)), m.group(2)
        delta = {
            'h': timedelta(hours=n),
            'd': timedelta(days=n),
            'w': timedelta(weeks=n),
            'm': timedelta(days=n * 30),
        }[unit]
        return now - delta
    try:
        return datetime.fromisoformat(spec).replace(tzinfo=timezone.utc)
    except ValueError:
        sys.exit(f'error: --since must be NNd / NNh / NNw / NNm or YYYY-MM-DD, got {spec!r}')


def event_timestamp(e: dict) -> datetime | None:
    ts = e.get('timestamp')
    if not ts:
        return None
    try:
        return datetime.fromisoformat(ts.replace('Z', '+00:00'))
    except (ValueError, AttributeError):
        return None


def cost_for(usage: dict, rates: dict) -> dict:
    """Compute USD cost given a usage dict and a per-million-token rate table."""
    fresh_input = usage.get('input_tokens', 0)
    cache_write_total = usage.get('cache_creation_input_tokens', 0)
    # Distinguish 5m vs 1h cache writes if the runtime broke them out;
    # otherwise assume 5m (the default ttl).
    cache_breakdown = usage.get('cache_creation') or {}
    cache_5m = cache_breakdown.get('ephemeral_5m_input_tokens', cache_write_total)
    cache_1h = cache_breakdown.get('ephemeral_1h_input_tokens', 0)
    if cache_5m + cache_1h == 0 and cache_write_total > 0:
        cache_5m = cache_write_total

    cache_read = usage.get('cache_read_input_tokens', 0)
    output = usage.get('output_tokens', 0)

    return {
        'fresh_input':  fresh_input * rates['input']          / 1_000_000,
        'cache_write_5m': cache_5m  * rates['cache_write_5m'] / 1_000_000,
        'cache_write_1h': cache_1h  * rates['cache_write_1h'] / 1_000_000,
        'cache_read':   cache_read  * rates['cache_read']     / 1_000_000,
        'output':       output      * rates['output']         / 1_000_000,
    }


def humanize(n: int) -> str:
    if n >= 1_000_000_000: return f'{n/1_000_000_000:.2f}B'
    if n >= 1_000_000:     return f'{n/1_000_000:.2f}M'
    if n >= 1_000:         return f'{n/1_000:.1f}K'
    return str(n)


def main() -> int:
    parser = argparse.ArgumentParser(
        description='Estimate Claude Code token usage and approximate cost.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='Cost figures are estimates based on public list prices. For '
               'authoritative billing, use the cloud provider console.',
    )
    parser.add_argument('--since', help='time window (e.g. 7d, 30d, 24h, 2026-01-01)')
    parser.add_argument('--rates', choices=list(PRICING), default='anthropic',
                        help='pricing table to use (default: anthropic)')
    parser.add_argument('--by', choices=['model', 'project', 'session'], default='model',
                        help='grouping (default: model)')
    parser.add_argument('--root', type=Path, default=Path.home() / '.claude' / 'projects',
                        help='Claude Code projects dir (default: ~/.claude/projects)')
    parser.add_argument('--json', action='store_true', help='emit machine-readable JSON')
    args = parser.parse_args()

    since = parse_since(args.since)
    rates_table = PRICING[args.rates]

    # Aggregate by model and by chosen grouping in parallel.
    by_model: dict[str, dict] = defaultdict(lambda: dict(turns=0, tokens=defaultdict(int), cost=defaultdict(float)))
    by_group: dict[str, dict] = defaultdict(lambda: dict(turns=0, tokens=defaultdict(int), cost=defaultdict(float)))

    for path in find_transcripts(args.root):
        # Project key is the encoded-cwd directory name above the session file.
        project_key = path.parent.name
        session_key = path.stem
        try:
            with open(path, encoding='utf-8') as f:
                for line in f:
                    try:
                        e = json.loads(line)
                    except json.JSONDecodeError:
                        continue
                    if e.get('type') != 'assistant':
                        continue
                    if since:
                        ts = event_timestamp(e)
                        if ts and ts < since:
                            continue
                    msg = e.get('message') or {}
                    usage = msg.get('usage') or {}
                    if not usage:
                        continue
                    model = msg.get('model') or 'unknown'
                    rates = rates_table.get(model, DEFAULT_FALLBACK_PRICE)
                    cost = cost_for(usage, rates)

                    group_key = {
                        'model':   model,
                        'project': project_key,
                        'session': f'{project_key}/{session_key}',
                    }[args.by]

                    for bucket in (by_model[model], by_group[group_key]):
                        bucket['turns'] += 1
                        bucket['tokens']['input']        += usage.get('input_tokens', 0)
                        bucket['tokens']['cache_create'] += usage.get('cache_creation_input_tokens', 0)
                        bucket['tokens']['cache_read']   += usage.get('cache_read_input_tokens', 0)
                        bucket['tokens']['output']       += usage.get('output_tokens', 0)
                        for k, v in cost.items():
                            bucket['cost'][k] += v
        except OSError:
            continue

    if not by_model:
        print('No usage found. Did Claude Code run on this machine?')
        return 0

    # Grand total
    grand = dict(turns=0, tokens=defaultdict(int), cost=defaultdict(float))
    for b in by_model.values():
        grand['turns'] += b['turns']
        for k, v in b['tokens'].items(): grand['tokens'][k] += v
        for k, v in b['cost'].items():   grand['cost'][k] += v

    if args.json:
        out = {
            'rates_source': args.rates,
            'since': args.since,
            'grand_total': {
                'turns': grand['turns'],
                'tokens': dict(grand['tokens']),
                'cost_usd': dict(grand['cost']),
                'cost_usd_total': sum(grand['cost'].values()),
            },
            'by_' + args.by: {
                k: {
                    'turns': v['turns'],
                    'tokens': dict(v['tokens']),
                    'cost_usd': dict(v['cost']),
                    'cost_usd_total': sum(v['cost'].values()),
                }
                for k, v in by_group.items()
            },
        }
        print(json.dumps(out, indent=2, default=str))
        return 0

    # Human-readable report
    print(f'Claude Code usage report - {args.rates} rates' + (f' since {args.since}' if args.since else ''))
    print('=' * 90)
    # Show the per-1M rates we used (so totals aren't a black box). Print
    # only the models we actually saw usage for, including any unknowns
    # that fell back to the default Sonnet rate.
    print('Rates applied (USD per 1M tokens):')
    print(f'  {"model":<30s} {"input":>8s} {"cache_w5m":>10s} {"cache_w1h":>10s} {"cache_r":>8s} {"output":>8s}')
    for model in sorted(by_model):
        rates = rates_table.get(model, DEFAULT_FALLBACK_PRICE)
        marker = '' if model in rates_table else '   <- fallback (Sonnet rate)'
        print(f'  {model:<30s} {rates["input"]:>8.2f} {rates["cache_write_5m"]:>10.2f} '
              f'{rates["cache_write_1h"]:>10.2f} {rates["cache_read"]:>8.2f} {rates["output"]:>8.2f}{marker}')
    print()
    header = f'{args.by:<35s} {"turns":>7s} {"input":>10s} {"cache+":>10s} {"cache_r":>10s} {"output":>10s}'
    print(header)
    print('-' * 90)
    rows = sorted(by_group.items(), key=lambda kv: -sum(kv[1]['cost'].values()))
    for k, b in rows:
        t = b['tokens']
        print(f'{k[:35]:<35s} {b["turns"]:>7,d} '
              f'{humanize(t["input"]):>10s} {humanize(t["cache_create"]):>10s} '
              f'{humanize(t["cache_read"]):>10s} {humanize(t["output"]):>10s}')
    print('-' * 90)
    print(f'{"TOTAL":<35s} {grand["turns"]:>7,d} '
          f'{humanize(grand["tokens"]["input"]):>10s} {humanize(grand["tokens"]["cache_create"]):>10s} '
          f'{humanize(grand["tokens"]["cache_read"]):>10s} {humanize(grand["tokens"]["output"]):>10s}')
    print()
    print('Estimated cost (USD)')
    print('-' * 90)
    for k, b in rows:
        total = sum(b['cost'].values())
        print(f'  {k[:55]:<55s}  ${total:>10.2f}')
    print('-' * 90)
    grand_total = sum(grand['cost'].values())
    print(f'  {"GRAND TOTAL":<55s}  ${grand_total:>10.2f}')
    print()
    print(f'  fresh input:    ${grand["cost"]["fresh_input"]:>10.2f}')
    print(f'  cache write 5m: ${grand["cost"]["cache_write_5m"]:>10.2f}')
    print(f'  cache write 1h: ${grand["cost"]["cache_write_1h"]:>10.2f}')
    print(f'  cache read:     ${grand["cost"]["cache_read"]:>10.2f}')
    print(f'  output:         ${grand["cost"]["output"]:>10.2f}')
    print()
    print(f'  cache hit ratio: {grand["tokens"]["cache_read"] / max(1, grand["tokens"]["cache_read"] + grand["tokens"]["input"] + grand["tokens"]["cache_create"]):.1%}')
    print()
    print('Note: prices reflect public list rates. Bedrock/Vertex actual spend')
    print('may differ - check the cloud provider billing console.')
    return 0


if __name__ == '__main__':
    sys.exit(main())
