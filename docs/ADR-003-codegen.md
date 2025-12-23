# ADR-003: Stub Codegen + `vibe run`

- **Status**: Accepted
- **Date**: 2025-12-20

## Context

The CLI currently stops at semantic analysis. To validate end-to-end wiring and surface analyzer diagnostics when executing code, we need a downstream stage that ingests the semantic graph and produces an artifact the CLI can "run". A full compiler will eventually emit optimized JavaScript plus inline source maps, but for now we just need a deterministic place where IR/codegen responsibilities can grow without blocking other work.

## Decision

1. Introduce `@vibe/codegen`, a package that accepts the parser AST and semantic graph and returns a stub IR plus emitted JavaScript text. The stub IR mirrors scope/symbol summaries so downstream tooling can inspect hygiene data without mutating the AST.
2. Expose `generateModule(program, graph, options)` which returns `{ ok, diagnostics, ir, moduleText }`. The initial implementation is pure data plumbing but reserves hooks for future diagnostics (e.g., unsupported forms during lowering).
3. Extend `vibe` CLI with a `run` command that resolves source, parses, analyzes, feeds the result into `@vibe/codegen`, and dynamically imports the emitted module. If parsing or analysis fails, diagnostics prevent execution, keeping scope-aware feedback visible during `vibe run`.
4. Reuse Bun's ability to import `data:` URLs so we can evaluate the generated JS without touching disk. The CLI prints the stub IR and whatever the generated module exports until a real runtime exists.
5. Index the semantic graph inside codegen so each AST node can be paired with its `SemanticNodeRecord`. Assigned JavaScript identifiers now reuse the analyzer's alias metadata (keyed by `SymbolId`), and the IR includes these identifiers via `IrSymbolSummary.identifier`.

### Update – Module Imports (2025-12-21)

- `generateModule` now recognizes standalone `(require alias ...)` and `(external alias ...)` statements. The emitter hoists them to `import * as alias from '...';` statements (rewriting `.lang` inputs to `.js`) and re-exports the alias so CLI consumers still observe the binding in `__env`.
- Namespace-qualified symbols (`alias/member`) and explicit `(get alias member)` forms compile down to property access. Dotted access is used for identifier-safe members while bracket notation preserves punctuation-heavy names.

## Consequences

- The compiler pipeline now has an explicit seam for IR/codegen experiments, making it easier to add lowering passes or source maps incrementally.
- `vibe run` demonstrates the full front-end stack (tokenize → parse → analyze → codegen → execute) even before evaluation semantics are defined, and already benefits from semantic diagnostics.
- Future work (macro expansion, real runtime, optimizer) can evolve inside `@vibe/codegen` without changing CLI wiring.
- Because the generated IR contains per-symbol identifiers sourced from the semantic graph's alias metadata, downstream tooling (e.g., IR viewers, `--show-ir`) can correlate JS bindings with their originating `SymbolId` without reverse-engineering the emitted module.
