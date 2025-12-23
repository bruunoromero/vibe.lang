# Improvements TODO

This document lists prioritized, concrete improvements to evolve the `lang` compiler into a fuller programming language. Each item contains: goal, rationale, acceptance criteria, files to change, an ordered set of small actionable subtasks, estimated effort, and notes.

---

## Immediate notes

- Work is organized into focused tasks so reviewers can accept incremental PRs.
- For each change, add or update an ADR in `docs/` describing design decisions.
- Keep tests updated — every new package must include `bun test` and `bun run build` scripts.

---

## 1) Create `packages/runtime` (runtime / standard library) [COMPLETED]

Goal

- Provide a centralized runtime/stdlib package with runtime helpers and small stdlib functions (e.g. `println`, `env`, `isTruthy`, collection creators). Stop embedding helpers inline inside generated modules.

Rationale

- Current codegen inlines small runtime helpers inside emitted modules, which duplicates code, complicates distribution, and prevents a single point for runtime bugfixes and feature additions.

Acceptance criteria

- `packages/runtime` exists with `package.json` and `src/index.ts` exporting a minimal API used by codegen.
- `packages/codegen` emits references/imports to runtime helpers (or a well-defined runtime accessor) rather than full inlined implementations.
- Tests in `packages/codegen/tests` updated to expect runtime import usage.

Files to create / change

- Create: `packages/runtime/package.json`
- Create: `packages/runtime/src/index.ts`
- Change: `packages/codegen/src/generator.ts`
- Change: `packages/codegen/tests/generate.test.ts` (snapshots)

Actionable subtasks

1. Create `packages/runtime/package.json` with name `@vibe/runtime` (or `packages/runtime` workspace name), `main` pointing to `dist/index.js` or `src/index.ts` depending on build approach, and basic scripts.
2. Implement `packages/runtime/src/index.ts` with exports:
   - `println(...args)` (delegates to console by default)
   - `__env` (minimal runtime environment accessor)
   - `isTruthy(value)` helper
   - small collection helpers if codegen uses them (`makeVector`, `makeList`)
3. Add tests for runtime functions in `packages/runtime/tests` if needed.
4. Update `packages/codegen/src/generator.ts` to import from `@vibe/runtime` and emit `import { println, __env } from '@vibe/runtime'` (or emit a runtime accessor import depending on packaging decision).
5. Keep temporary shims in codegen to preserve emitted names for backwards compatibility while tests are updated.
6. Update codegen snapshots in `packages/codegen/tests/__snapshots__` to reflect import usage.
7. Run `bun test` for changed packages and fix issues.

Estimated effort: Medium

Notes

- Decide ADR: should `@vibe/runtime` be published separately or bundled per-project? Document in an ADR and `packages/runtime/README.md`.

---

## 2) Refactor codegen to use runtime imports (follow-on to 1) [COMPLETED]

Goal

- Remove inlined runtime helpers from emitted modules; emit imports or references to the `packages/runtime` API.

Rationale

- Reduce duplication, simplify emitted output, and enable deterministic runtime behavior across modules.

Acceptance criteria

- `packages/codegen/src/generator.ts` no longer inlines full helper implementations.
- Generated modules import runtime helpers when necessary.

Files to change

- `packages/codegen/src/generator.ts`
- `packages/codegen/tests/generate.test.ts` (update snapshots)

Actionable subtasks

1. Identify all helper functions currently inlined by `packages/codegen/src/generator.ts` (search for `__println`, `__env`, or other `__` helpers).
2. Replace inlined implementations with import stubs: at the top of emitted modules emit `import { println as __println } from '@vibe/runtime'` (name-mapping strategy is up to codegen; keep names stable to minimize breakage).
3. Add toggles/options in the generator to either inline (compat) or import (new behavior) for a transitional period.
4. Update tests and snapshots.
5. Run tests and iterate.

Estimated effort: Medium

Notes

- Keep backward-compatible shims until consumers are migrated.

---

## 3) Module system & import semantics [COMPLETED]

Goal

- Introduce a language-level module/import form and map it to ES module imports/exports. Provide semantics for module-level scopes and name resolution across files.

Rationale

- Needed for multi-file programs, package management, and good developer ergonomics.

Status / Completion Notes

- Completed (2025-12-22): Implemented using the **file-level implicit module** approach — every top-level `def` is auto-exported as a named ES export; no explicit `module`/`export` forms were added to the surface syntax.
- Changes applied across packages: `@vibe/parser` (namespace import AST node), `@vibe/semantics` (module export recording & namespace member validation), `@vibe/codegen` (emit named exports only; removed default `__env` export), `@vibe/runtime` (runtime helpers exported from package), `@vibe/cli` (`compile-all` support and example app build).
- Tests updated in `packages/semantics` and `packages/codegen` and the full test suite passes.

Acceptance criteria

- Parser accepts namespace import forms (`require`/`external`) and promotes them to `NamespaceImport` nodes for downstream stages.
- Semantics records module-level exports and resolves imports across files (file-local resolution); namespace member validation and diagnostics are emitted when resolution fails.
- Codegen emits ES `export` statements for top-level defs (named exports only); no default runtime env export is emitted.
- Tests include a two-file example (example-app) compiled with `compile-all` and producing `dist/` JS files.

Files changed (high level)

- `packages/parser/src/parser.ts` — `NamespaceImport` handling
- `packages/semantics/src/analyzer.ts` — module exports & namespace member validation
- `packages/codegen/src/generator.ts` — emit named exports, remove default env wrapper
- `packages/runtime/src/index.ts` — runtime helpers
- `packages/cli/index.ts` — `compile-all` wiring
- Updated tests and docs: `packages/semantics/tests`, `packages/codegen/tests`, `docs/ADR-001-parser.md`, `docs/syntax-spec.md`

Actionable follow-ups (optional)

- Add explicit `ns` or `(import ...)` forms if you want an explicit module declaration syntax (currently not required since implicit modules cover use cases).
- Add package-level resolution (registry/semver) if needed in the future.

Estimated effort: Medium

Notes

- Implementation and design decisions are recorded in `docs/ADR-001-parser.md` (see "Update – Module Imports" section).
- If you'd like, I can add a short CHANGELOG entry and tag relevant commits/PRs for release notes.

---

## 4) Add `vibe repl` to the CLI [COMPLETED]

Goal

- Provide an interactive read-eval-print loop that reads forms, runs them through parse→analyze→codegen→eval and prints results and diagnostics.

Rationale

- REPL significantly improves language discoverability and developer feedback loop.

Acceptance criteria

- `packages/cli/index.ts` exposes a `repl` command.
- REPL supports multi-line entry (balanced forms), prints results, and prints analyzer diagnostics.
- Basic in-memory history (arrow up/down) works.

Files changed (high level)

- `packages/cli/src/repl.ts` — REPL implementation and input loop
- `packages/cli/index.ts` — CLI `repl` command registration and options
- `packages/cli/tests/repl.test.ts` — unit tests for REPL behavior (multi-line, history, diagnostics, pretty-print)
- `packages/syntax/index.ts` — added and exported `BUILTIN_SYMBOLS` used by REPL/analyzer
- `packages/semantics/src/analyzer.ts` — allowed same-scope redefinition (shadowing) and adjusted module export recording
- `packages/codegen/src/generator.ts` — minor generator expectations/guards to support REPL emission

Status / Completion Notes

- Completed (2025-12-22): Implemented `lang repl` with multi-line support, a candidate-history strategy that only commits successful statements (prevents repeated diagnostics), improved pretty-printing for runtime values (functions/Errors), and friendly handling of bare builtin symbol inputs. The analyzer was adjusted to allow re-definitions during interactive sessions (shadowing). Unit tests added and updated (`packages/cli/tests/repl.test.ts`, `packages/semantics/tests`) and the affected test suites pass.
- Follow-ups: Move debug traces to an explicit CLI flag (e.g., `--debug`), consider expanding the pretty-printer to additional runtime types, and add an ADR documenting the analyzer's shadowing semantics if you'd like this behavior recorded.

Actionable subtasks

1. Design REPL loop (document behavior in `packages/cli/README.md`): read lines until parser reports a complete form (use parser utilities to check balanced forms).
2. On complete form: call `parseSource`, `analyzeProgram`, `generateModule` (or a small-eval emitter) and evaluate the resulting JS using existing `run` pathway.
3. Print analyzer diagnostics along with any runtime exceptions (map to spans when possible).
4. Implement a small history buffer using Node/Bun `readline` or an alternative. Keep dependencies minimal.
5. Add tests that simulate REPL input and assert expected results and diagnostics.

Estimated effort: Low

Notes

- Reuse existing `run` pipeline internals to evaluate modules to avoid duplication.

---

## 5) Harden macros & expand macro capabilities [COMPLETED]

Goal

- Remove current macro limitations (notably nested `~` beyond `(gensym ...)`), implement robust handling of nested unquotes and splicing, improve diagnostics, and add comprehensive tests.

Rationale

- Macros are core to Lisp-like languages; current limits are documented and will block users.

Status / Completion Notes

- Completed (2025-12-22): Implemented variadic macro parameters (`&` rest), recursive macro expansion with depth limiting, compile-time evaluator for nested unquotes supporting sequence operations (`first`, `next`, `rest`, `seq?`, `list`, `cons`) and conditionals (`if`), comprehensive test coverage in `packages/semantics/tests/advanced-macros.test.ts`.
- Changes applied: Updated `MacroDefinition` interface to include optional rest parameter, modified parameter parsing in analyzer to handle `&` symbol, implemented recursive expansion with `fullyExpandAndVisit` method, built compile-time evaluator in `evaluateCompileTimeCall`, updated documentation in `docs/macro-authoring.md`.
- All 107 tests pass including 13 new advanced macro tests covering variadic parameters, recursive expansion, compile-time evaluation, and error diagnostics.

Acceptance criteria

- Analyzer supports variadic macros with `&` rest parameters (including arbitrary remaining args), correctly hygienizes introduced identifiers, and enforces splicing constraints. ✅
- Recursive macro expansion works with depth limiting (max 100) and cycle detection. ✅
- Compile-time evaluator supports `first`, `next`, `rest`, `seq?`, `list`, `cons`, `if` for use in unquote expressions. ✅
- Tests in `packages/semantics/tests/advanced-macros.test.ts` cover variadic macros, recursive expansion, compile-time evaluation, nested unquote, splicing, invalid uses, and recursion detection. ✅

Files changed

- `packages/semantics/src/analyzer.ts` — variadic parameter support, recursive expansion, compile-time evaluator
- `packages/semantics/tests/advanced-macros.test.ts` — comprehensive test coverage (NEW)
- `packages/codegen/src/generator.ts` — variadic function emission with JS rest params
- `packages/runtime/src/index.ts` — runtime sequence helpers
- `packages/cli/package.json` — added missing @vibe/syntax dependency
- `docs/macro-authoring.md` — updated with variadic syntax, recursive expansion, compile-time evaluation examples

Actionable subtasks

All completed:

1. ✅ Implemented variadic parameter parsing for both macros and functions with `&` symbol support
2. ✅ Implemented recursive macro expansion with `fullyExpandAndVisit` and depth tracking
3. ✅ Built compile-time evaluator supporting first, next, rest, seq?, list, cons, if operations
4. ✅ Added 13 comprehensive tests covering all new capabilities and edge cases
5. ✅ Updated documentation with examples and new diagnostic codes

Estimated effort: Medium (actual: completed in one session)

Notes

- The implementation enables complex macros like threading macros (`->`) and control-flow macros (`and`, `or`).
- Compile-time evaluation is limited to safe, deterministic operations on AST nodes (no arbitrary code execution).
- Hygiene is automatically maintained across all expansions and recursion levels.

---

## 6) Extract & improve sourcemap generation and runtime mapping

Goal

- Separate sourcemap logic into a dedicated module and add runtime helpers to map runtime errors back to source spans (optionally externalize source maps).

Rationale

- Externalizable and consumable source maps are crucial for debugging in editors and production. Mapping runtime errors to original spans improves developer experience.

Acceptance criteria

- New `packages/codegen/src/sourcemap.ts` exports a `SourceMapBuilder` used by `generator.ts`.
- Codegen supports an option to emit external `.map` files.
- Runtime helper (in `packages/runtime`) can map a stack trace to source spans using source map data and augment Error objects.
- Tests validate stack-to-source mapping for a thrown error.

Files to change / add

- Extract: `packages/codegen/src/sourcemap.ts` (new)
- Change: `packages/codegen/src/generator.ts` to use the new module and support external maps
- Add to runtime: `packages/runtime/src/error-map.ts` (or similar)
- Add tests: `packages/codegen/tests` for mapping behavior

Actionable subtasks

1. Extract and clean the sourcemap builder into `packages/codegen/src/sourcemap.ts` with an API for external file emission.
2. Add generator options to emit external maps vs inline maps.
3. Implement a small runtime helper in `packages/runtime` that, given a stack trace and source map, resolves frames to original source spans.
4. In codegen, optionally wrap top-level module code with a try/catch that augments thrown errors with original span info (opt-in for debug builds).
5. Add tests that intentionally throw an error from generated code and assert that the runtime error object contains original span metadata.

Estimated effort: Medium

Notes

- Consider existing source map format compatibility and test with a small consumer to verify mapping correctness.

---

## 7) Formatter (`vibe fmt`) and LSP scaffolding

Goal

- Provide a code formatter (idempotent) and scaffold a minimal LSP server to surface diagnostics and hover information in editors.

Rationale

- Formatting yields consistent code style across contributors. LSP dramatically improves UX and adoption.

Acceptance criteria (formatter)

- `packages/formatter` provides a canonical `formatSource(source: string)` function.
- CLI gains `vibe fmt` command that formats files and has tests for idempotency.

Acceptance criteria (LSP)

- `packages/lsp` contains a basic LSP server that returns diagnostics from parse+analyze and hover info for AST nodes.

Files to change / add

- Create: `packages/formatter/package.json`, `packages/formatter/src/index.ts`, tests
- Change: `packages/cli/index.ts` to add `fmt` command
- Create: `packages/lsp/package.json`, `packages/lsp/src/server.ts`, README/config snippets

Actionable subtasks (formatter)

1. Implement AST-to-source pretty-printer using `@vibe/syntax` AST.
2. Implement `formatFile(path)` API and wire it to `lang fmt`.
3. Add tests that assert formatting is deterministic and idempotent.

Actionable subtasks (LSP)

1. Scaffold a small LSP server that responds to `textDocument/didOpen` and `didChange` by calling parser+analyzer and sending diagnostics.
2. Implement `textDocument/hover` to return AST node kind and span at the cursor position.
3. Add a README with VS Code launch/client hints.

Estimated effort: Formatter Medium, LSP High

Notes

- Start with the formatter (lower effort, quick UX win), then add LSP.

---

## 8) ADRs: Typing model & runtime targets

Goal

- Record clear decisions for the typing strategy (dynamic vs static vs gradual) and runtime targets (Bun-first vs Node vs browser/WASM).

Rationale

- These fundamental design choices affect many subsequent implementation details.

Acceptance criteria

- Two ADRs added to `docs/` describing chosen typing model and runtime priority with pros/cons and migration plan.

Files to add

- `docs/ADR-00x-typing.md`
- `docs/ADR-00x-runtime-target.md`

Actionable subtasks

1. Draft ADR options with trade-offs and a recommended default (suggestion: start dynamic + optional gradual typing later).
2. Decide runtime target (suggestion: Bun-first with Node-friendly output) and record implications.
3. Share ADR drafts for review.

Estimated effort: Low (writing) / High (implementation if static typing chosen)

---

## 9) Tests, CI, and documentation updates

Goal

- Ensure every new package has tests and scripts, CI runs the workspace tests, and docs/ADRs are updated for each change.

Actionable subtasks

1. For each created package, add `package.json` scripts: `test` (bun test) and `build` (bun run build or tsc compile step).
2. Update root-level CI/workflow to run `bun test` across workspaces.
3. Add integration tests under `packages/cli/tests` for pipeline end-to-end.

Estimated effort: Medium

---

## 10) Allow recursive macros [COMPLETED]

Goal

- Enable macros to expand recursively (macros that expand to forms which themselves may invoke macros), and detect/report recursion cycles to avoid infinite expansion.

Rationale

- Recursive macros are required to express common control-flow forms (e.g., `and`, `or`) and powerful DSL constructs. Current expansion may be single-pass and cannot handle macros that expand into other macros safely.

Status / Completion Notes

- Completed (2025-12-22): Implemented as part of step 5 (harden macros). The analyzer now expands macros recursively using `fullyExpandAndVisit` with depth tracking and cycle detection via the `macroExpansionStack`.

Acceptance criteria

- The analyzer expands macros recursively until no macro forms remain or until a recursion cycle is detected. ✅
- The analyzer emits `SEM_MACRO_RECURSION` with a helpful span when a recursion cycle or configurable max depth is reached. ✅
- Tests demonstrate expansion of recursive macros and detect indirect recursion. ✅

Files changed

- `packages/semantics/src/analyzer.ts` — recursive expansion implementation
- `packages/semantics/tests/analyze.test.ts` — existing recursion detection test passes
- `packages/semantics/tests/advanced-macros.test.ts` — additional recursive expansion tests
- `docs/macro-authoring.md` — documented recursive macro support

Actionable subtasks

All completed:

1. ✅ Refactored macro-expander into recursive `fullyExpandAndVisit` routine with expansion stack and depth tracking
2. ✅ Recursion detection via `macroExpansionStack.includes(binding.id)` check
3. ✅ Max depth limit (100) with `SEM_MACRO_MAX_DEPTH` diagnostic
4. ✅ Tests verify recursive expansion and recursion diagnostics
5. ✅ Documentation updated with examples and diagnostic codes

Estimated effort: Medium (actual: completed as part of macro hardening)

Notes

- Hygiene metadata is correctly maintained across recursive expansions.
- The existing `SEM_MACRO_RECURSION` test continues to pass with the new implementation.

---

## 11) Support rest/spread args (`&` rest) in functions and macros [COMPLETED]

Goal

- Support variadic parameter lists using `&` in parameter vectors for function definitions and `defmacro`, enabling forms like `(defmacro and [& expr] ...)` and `(fn [& xs] ...)`.

Rationale

- Variadic parameters are essential for macros like `and` that accept an arbitrary number of sub-expressions. The parser, semantic analyzer, macro expander, and codegen must all agree on how rest args are represented and lowered.

Status / Completion Notes

- Completed (2025-12-22): Implemented as part of step 5 (harden macros). Full support for variadic parameters in both macros and functions with proper validation, binding, and JS code generation.

Acceptance criteria

- Parser recognizes `&` in parameter vectors (no AST changes needed - handled semantically). ✅
- Analyzer validates rest usage (single `&`, symbol following `&`, no duplicate params) and annotates function/macro nodes accordingly. ✅
- Macro expansion binds rest args correctly when expanding variadic macros (collects remaining args into vector). ✅
- Codegen emits appropriate JS code (rest parameter syntax `...param`). ✅
- Tests cover valid and invalid usages and variadic examples. ✅

Files changed

- `packages/semantics/src/analyzer.ts` — variadic parameter parsing and validation for both defmacro and fn
- `packages/codegen/src/generator.ts` — emit JS rest parameters in generated functions
- `packages/semantics/tests/advanced-macros.test.ts` — comprehensive variadic tests (NEW)
- `docs/macro-authoring.md` — documented variadic macro syntax and diagnostics

Actionable subtasks

All completed:

1. ✅ Modified defmacro and fn parameter parsing to detect `&` and bind rest parameter
2. ✅ Added validation: `SEM_MACRO_DUPLICATE_REST`, `SEM_MACRO_REST_REQUIRES_SYMBOL`, `SEM_MACRO_PARAMS_AFTER_REST` (and FN equivalents)
3. ✅ Updated macro expansion to collect remaining args into a vector for rest parameter
4. ✅ Modified codegen to emit `...restParam` syntax in generated JS functions
5. ✅ Added 13 tests covering variadic macros, variadic functions, and all validation cases
6. ✅ Updated documentation with examples and new diagnostic codes

Estimated effort: Low–Medium (actual: completed as part of macro hardening)

Notes

- Native JS rest parameters are used for modern runtime compatibility.
- Both fixed + variadic parameters (`[x y & rest]`) and pure variadic (`[& all]`) patterns are supported.
- Empty variadic invocations work correctly (rest bound to empty vector).

---

## 12) Add `lang compile-all` and sample package scaffolding [COMPLETED]

Goal

- Provide an ergonomic way to compile every `.lang` file in a project tree and document a reference package that demonstrates namespace imports (`require`) and externals.

Rationale

- Prior to this change, users had to call `lang compile` per file, which duplicated work and made multi-file builds error-prone. The sample package now shows a realistic workflow (including `(external ...)`) so documentation and tooling stay aligned.

Acceptance criteria

- `packages/cli/index.ts` exposes `lang compile-all <dir>` with `--out-dir`, `--pretty`, and `--debug-macros` flags.
- `packages/example-app` includes a `build` script that runs `compile-all` once and demonstrates both `(require math "./math.lang")` and `(external path "node:path")` forms.
- Root `README.md` and package docs mention the new command and how to run the example project end-to-end.

Files changed

- Change: `packages/cli/index.ts` (new helpers, command registration).
- Change: `packages/cli/CHANGELOG.md` (documented feature).
- Change: `README.md`, `packages/example-app/README.md`, `packages/example-app/package.json`, `packages/example-app/src/main.lang`.

Actionable subtasks

1. Implement directory discovery + mirroring inside the CLI, emitting deterministic `.js` paths for every `.lang` file.
2. Add the `compile-all` command wiring (options, help text, examples) and update CLI docs/changelog.
3. Update the example app to rely on `compile-all`, add a `math.lang` helper plus `(external path "node:path")`, and document the workflow.
4. Run `bun run build` + `node dist/main.js` inside `packages/example-app` to verify both namespace imports and externals work.

Estimated effort: Low

Notes

- Long term, the compiler packages can expose a reusable batch API, but for now the CLI orchestrates file walking while still reusing parser/analyzer/codegen per file.

---

## Suggested ordering (first 6 weeks of work)

1. `packages/runtime` creation + codegen import refactor (items 1–2) [COMPLETED]
2. `lang repl` in CLI (item 4)
3. Module/import system (item 3)
4. Harden macros + tests (item 5)
5. Extract sourcemap + runtime mapping (item 6)
6. Formatter and LSP scaffold (item 7)

---

## How I can help next

- I can open PRs for any of the above items. Pick one and I will draft a detailed implementation checklist and apply an initial patch to the repo.

Created checklist reference (tracked): see the repo TODO list items created via the workspace task manager.

---

End of file.
