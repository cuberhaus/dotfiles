# AGENTS.md template

Use this as the structure to fill in for a per-repo AGENTS.md. **OMIT any section you cannot fill concretely** — a shorter file is better than padding.

```markdown
# {Repo Name}

{1–3 sentences of context: what this repo is, primary stack, primary purpose. Link to README. If frozen coursework or hackathon code, say so here.}

## Architecture

{Non-obvious module boundaries. Where the entry point lives. Map of top-level folders if there are several with different roles. Link to `docs/architecture/*` if it exists rather than duplicating.}

## Build and Test

{Concrete commands an agent will actually run: `make test`, `npm run dev`, `cargo build`, `mvn -pl <module> test`, `docker compose up -d`, etc. Note language versions or external services required (Docker, GPU, ElasticSearch, MATLAB, …).}

## Conventions

{Project-specific patterns that differ from common practice or that an agent wouldn't infer. Examples: i18n JSON parity rules, hash format for IDs, naming prefixes, file-organization invariants. Skip generic conventions a linter would catch.}

## Pitfalls

{Gotchas that will burn an agent:
- Files that must NOT be deleted (legacy reference code, graded deliverables).
- Stale or conflicting manifest files (e.g. wrong `requirements.txt` at root).
- Hardcoded secrets that need auditing/rotating.
- Build/run requirements not in the manifest (GPU, MATLAB, system fonts).
- Frozen-coursework boundaries.
- OS-only constraints (macOS-only build, Windows-only GUI).
- Pre-commit hook quirks (interactive prompts, /dev/tty reads).
}

See [README.md](README.md) for full setup.
```

## Sizing guidance

| Repo type                             | Target lines |
| ------------------------------------- | ------------ |
| Tiny / placeholder (e.g. 5 files)     | 5–12         |
| Single-purpose frozen coursework      | 15–25        |
| Multi-component or active project     | 25–45        |
| Information-dense active repo         | 40–60        |

Stop at 60 lines. If you need more, link out to `docs/`.

## Section-by-section examples

### Architecture — what works

> Three-layer Java: `FONTS/{domini,persistencia,presentacio}` with `Main.java` as entry. The `web/` Spring Boot add-on is consumed live by [PersonalPortfolio](../../PersonalPortfolio/).

### Architecture — what doesn't

> The repo has multiple folders. Code is organized into logical components following standard patterns.

### Pitfalls — what works

> - Root `requirements.txt` is stale and conflicts with the active one in `web/backend/requirements.txt` — use the latter.
> - `Helloworld2/` is the lingering original module name; rename when refactoring the project file.
> - Pre-commit hook reads from `/dev/tty` — non-interactive shells hang.

### Pitfalls — what doesn't

> - Be careful when modifying files.
> - Test your changes.
> - Follow best practices.
