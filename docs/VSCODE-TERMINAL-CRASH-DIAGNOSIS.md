# VS Code Terminal Renderer Crash Diagnosis

## Summary

On 2026-09-10, Visual Studio Code repeatedly terminated windows with reason
`crashed` and exit code `132`. The host still had ample free memory, but the
kernel journal identified V8 heap exhaustion in the VS Code renderer process.
Renderer logs repeatedly sampled xterm marker registration, disposal, and reset
work immediately before the failures.

The confirmed mitigation is to use xterm's software renderer and prevent old
integrated-terminal state from being restored into a new window. VS Code's user
settings are not managed by this repository, so apply the documented settings
to `~/.config/Code/User/settings.json` on affected Linux machines.

## Environment

- VS Code `1.137.0`, commit
  `645f29cc3176500b4b5762ba887cf2a7f0ffdf2c`, x86-64.
- Microsoft APT package `code` at version `1.137.0-1788902055`; the installed
  version was also the repository candidate.
- Zsh in the VS Code integrated terminal.
- Eleven completed Crashpad dumps existed before the final restart.

## Confirmed failure path

The evidence established this sequence:

1. Integrated terminal output exercised xterm marker cleanup and reset paths.
2. The renderer became unresponsive while processing those paths.
3. Renderer memory grew until V8 reported heap exhaustion.
4. The renderer terminated with code `132`; extension hosts then exited cleanly
   because their window had disappeared.
5. Persistent terminal restoration reconnected progressively more orphaned
   shells after each restart, replaying old state and amplifying recurrence.

This was a renderer-local memory failure, not system-wide memory pressure. The
host had approximately 52 GiB free and the kernel showed no GPU reset or NVIDIA
Xid event associated with the crashes.

## Hypotheses tested

Disabling shell integration and persistent terminal restoration stopped stale
shell reconnection, but did not stop active terminal rendering from retaining
memory. A bounded workload of 500 in-place terminal redraws increased renderer
RSS from 401,024 KiB to 886,076 KiB, a gain of 485,052 KiB.

After disabling terminal GPU acceleration and creating a fresh renderer, the
same 500-redraw workload changed renderer RSS from 289,956 KiB to 289,516 KiB,
a decrease of 440 KiB. The renderer PID remained alive, no Crashpad dump was
created, and the kernel journal contained no new OOM or crash event. This A/B
result identifies the accelerated xterm renderer as the active leak in this
environment; restored terminal sessions were an additional amplifier.

## Workaround

Add these keys to the VS Code user settings:

```json
{
  "terminal.integrated.shellIntegration.enabled": false,
  "terminal.integrated.enablePersistentSessions": false,
  "terminal.integrated.persistentSessionReviveProcess": "never",
  "terminal.integrated.gpuAcceleration": "off"
}
```

Close every VS Code window, wait for the application processes to exit, and
reopen VS Code. A window reload alone may retain the existing renderer or pty
host and is not a clean verification boundary.

These settings trade terminal command decorations, terminal restoration, and
GPU rendering for stability. Ordinary terminal commands and Zsh continue to
work, but terminal processes no longer survive an editor restart.

## Verification

After a full restart:

1. Confirm the four settings remain effective.
2. Confirm the newest Crashpad dump predates the restarted VS Code process.
3. Check the new VS Code logs for terminal reconnection, renderer OOM, xterm
   unresponsiveness, and renderer exit code `132`.
4. Run the bounded redraw workload in a newly created integrated terminal:

   ```bash
   for i in {1..500}; do
       printf '\rterminal-render-smoke %04d' "$i"
   done
   printf '\n'
   ```

5. Compare renderer RSS before and after the workload and confirm that no new
   Crashpad dump or kernel OOM event appears.

The accepted 2026-09-10 verification kept the same renderer PID, changed RSS by
`-440 KiB`, created no dump, logged no kernel OOM, and created no orphaned
terminal generation.

## Rollback

When an upstream VS Code release fixes the renderer leak, remove the four user
settings and repeat the same A/B workload after a full restart. Re-enable one
behavior at a time so any regression can be attributed to terminal rendering,
shell integration, or persistence rather than changing all variables together.

Do not treat deleting Crashpad dumps as a fix. The dumps are evidence of the
failure and do not cause renderer memory growth.
