# Domain Docs

Before exploring the codebase, read `CONTEXT.md` at the repo root when it exists and any relevant ADRs under `docs/adr/`.

If those files do not exist, proceed without creating them up front.

## File structure

This is a single-context repository:

/
|-- CONTEXT.md
`-- docs/adr/
    `-- 0001-example-decision.md

## Use the glossary's vocabulary

When naming a domain concept in an issue title, refactor proposal, or test, use the terms defined in `CONTEXT.md`. If the needed concept is not defined there, note the terminology gap for `/domain-modeling`.

## Flag ADR conflicts

If proposed work contradicts an existing ADR, surface the conflict explicitly instead of silently overriding it.