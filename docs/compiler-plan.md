# Compiler Completion Plan

## 1. Semantic Analysis

- [x] Build the `@vibe/semantics` resolver + macro expander: construct symbol tables, assign hygiene tags, and run expansion passes.
  - Implemented `defmacro` registration, syntax-quote templating, `~`/`~@` substitution, simple `gensym` support, and expansion-at-callsite in `packages/semantics/src/analyzer.ts`.
  - Added tests in `packages/semantics/tests/analyze.test.ts` covering expansion, introduced bindings, and recursion diagnostics.
- [x] Define the IR contract (scope graph, symbols, nodes) in an ADR and expose `analyze(program, options)` that returns `{ graph, diagnostics }`. **(Completed)** — ADR-002 now documents the semantic graph schema, invariants, and sample payload shape.
- [x] Expand tests in `packages/semantics/tests` to cover shadowing, gensyms, macro recursion, unresolved identifiers, and span-accurate failures. **(Completed)** — new cases in `packages/semantics/tests/analyze.test.ts` exercise shadowed bindings, gensym-generated symbols, and diagnostic span accuracy.

## 2. Macro Authoring API

- [x] Lock down macro declaration syntax in `docs/ADR-002-semantics.md` and document lifecycle hooks (expansion, validation). **(Completed)** — ADR updated to describe registration, expansion behavior, recursion detection, and gensym.
- [x] Update `packages/syntax/CHANGELOG.md` with any AST metadata changes and add parser golden samples showing macro forms. **(Completed)** — syntax changelog now calls out macro AST metadata, and `packages/parser/tests/snapshots.test.ts` includes a macro declaration + usage snapshot.

## 3. Identifier Resolution

- [x] Teach the parser to emit scope identifiers and thread them through the AST. **(Completed)** — `ScopeAnnotator` now tags every node with deterministic `scopeId` metadata and parser tests lock down `let`/`fn` scope threading.
- [x] Update semantics resolver to allocate `ScopeId`, `SymbolId`, alias metadata, and hygiene tags for every binding. **(Completed)** — the analyzer now reuses parser-supplied scope IDs, keeps counters in sync, and scrubs stale metadata from macro expansions before analysis.
- [x] Strengthen `packages/semantics/tests/analyze.test.ts` with regression cases for nested scopes, shadowing diagnostics, and macro hygiene leaks. **(Completed)** — added deep-nesting, duplicate-shadowing, macro hygiene, and alias metadata regressions so symbol identifiers remain deterministic.

## 4. IR → Codegen Integration

- [x] Document IR expectations in a new ADR and ensure `packages/codegen/src/generator.ts` consumes the real `SemanticGraph` output. **(Completed)** — ADR-003 now captures the IR contract (including `IrSymbolSummary.identifier`), and `ModuleEmitter` indexes the semantic graph to assign identifiers per `SymbolId` during emission.
- [x] Remove placeholder scope/symbol data inside codegen once semantics provides the authoritative graph. **(Completed)** — the generated IR mirrors analyzer scopes and symbols plus the concrete JS identifier assigned to each binding, removing the prior stub clone.
- [x] Extend `packages/codegen/tests/generate.test.ts` to run parser → semantics → codegen end-to-end with snapshot checks for generated JS + source maps. **(Completed)** — snapshot coverage now includes a macro-style pipeline fixture plus IR dumps to lock the emitter + identifier metadata.

## 5. CLI Wiring & Flags

- Update `@vibe/cli` so `vibe run`/`vibe compile` execute lexer → parser → semantics → codegen sequentially. **(Completed)** — front-end pipeline consolidated in `packages/cli/index.ts`.
- [x] Surface diagnostics (with spans) from every stage and add `--show-ir`, `--show-ast`, and `--debug-macros` flags. **(Completed)** — `vibe run/compile` now accept `--show-ast` and `--show-ir` to stream AST + IR JSON to stderr alongside the existing diagnostics/pretty/debug-macro plumbing.
- `--debug-macros` now streams semantic graphs + diagnostics to stderr for `analyze`, `run`, and `compile`. **(Completed)**
- Capture the new behavior and flags in `packages/cli/CHANGELOG.md` and README usage examples. **(Completed)**
- [x] Teach the CLI to read `package.json` `vibe.modules` metadata so `(require ...)` can target workspace packages (e.g., `@vibe/prelude`) while the emitted JavaScript relies on the package's published `exports` for runtime resolution.
- [x] Introduce `vibe build [package|path]` which walks the workspace dependency graph, honors each package's `vibe.sources`/`vibe.outDir` config, and emits `.js` artifacts (in dependency order) so downstream packages import the published JavaScript instead of the `.lang` sources.

## 6. Docs, Tooling, and Automation

- [x] Log every package-level change in its `CHANGELOG.md` and add a macro authoring guide under `docs/`. **(Completed)** — documented macro declaration, hygiene, and debugging workflows in `docs/macro-authoring.md` so contributors can follow consistent patterns.
- Expand CI to run `bun test` and `bun run build` for all packages plus lint/format checks before release.
- Ensure each package exposes the required scripts so `bun test` succeeds locally and in automation.
