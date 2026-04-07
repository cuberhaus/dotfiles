# Security Policy — dotfiles

## Reporting a Vulnerability
If you discover a security vulnerability, please email polcg10@gmail.com. Do not open a public issue.

## Security Considerations

### Review Before Running
- Bootstrap scripts execute with elevated privileges (`sudo`). **Always read the scripts before running them** — especially after pulling updates.
- Piping scripts to `sh` from a remote source (`curl | sh`) is inherently risky. Clone the repo and review locally first.
- Verify the repository URL and branch before executing any setup scripts.

### Privilege Escalation Awareness
- Scripts that use `sudo` can modify system-level configurations, install packages, and alter file permissions.
- Understand what each script does before granting `sudo` access. A compromised script with `sudo` has full system control.
- Do not run dotfile scripts as root directly — use `sudo` only where necessary within the scripts.

### GPG & Commit Verification
- If the repo uses GPG-signed commits, verify signatures before trusting updates: `git log --show-signature`.
- Import only trusted GPG keys. Verify key fingerprints through a separate trusted channel.

### Sensitive Data
- Do not store passwords, API keys, tokens, or private SSH keys in dotfiles.
- Use credential managers, keyrings, or encrypted vaults for secrets.
- If any secrets are accidentally committed, rotate them immediately — git history retains deleted content.
- Review `.gitignore` to ensure sensitive files (`.ssh/`, `.gnupg/`, `.env`) are excluded.

### Recommendations
- Fork the repo and maintain your own branch to control updates.
- Use `git diff` to review changes before pulling updates.
- Test bootstrap scripts in a VM or container before running on your primary machine.
- Pin package versions where possible to avoid unintended upgrades.
