# @vibe/codegen Changelog

## 2025-12-30

- Top-level emission now distinguishes between `def` and `defp`. Public defs still emit `export const` declarations while private defs emit unexported `const` bindings, keeping generated modules aligned with the new visibility semantics.
- Macro filtering inspects both `def` and `defp` heads so private macros remain compile-time only. A new regression test in `packages/codegen/tests/generate.test.ts` verifies that `defp` bindings never appear in the compiled module exports.
- Dropped the gensym-specific alias sanitizer path. Identifier mapping no longer reserves `_HASH` or rewrites `foo#` placeholders since the syntax was removed upstream.

## 2025-12-29

- **Destructuring emission** — `let` bindings and function parameters now run through the shared binding-pattern parser before code generation. Vector patterns emit deterministic destructuring helpers (including nested patterns, `& rest`, and `:as` aliases) while map patterns expand explicit keys plus the `:keys`/`:strs`/`:syms` shorthands with inline default expressions. Generated modules now mirror the interpreter’s runtime binding semantics, and new tests in `packages/codegen/tests/generate.test.ts` exercise end-to-end destructuring.

## 2025-12-29 (maps removed)

- Removed emission support for map literals and map-pattern lowering. The codegen no longer emits code for `{ ... }` map literals; callers should use vectors, explicit accessors, or prelude helpers for associative data access.

## 2025-12-28

- **Code generation for try/catch/finally** — Added `emitTry` and `emitThrow` methods that lower try forms to JavaScript try/catch/finally blocks wrapped in IIFEs to preserve value semantics. The catch binding is resolved from the semantic graph and sanitized to a valid JavaScript identifier.
- **Throw error coercion** — Emitted throw statements validate that the error value is an instance of `Error`, falling back to `new Error(String(...))` for non-error values. This matches the runtime coercion behavior.
- **Try/catch/finally partitioning** — Added `partitionTryForm()` helper (matching interpreter and semantics logic) to consistently extract body, catch, and finally clause nodes from the try list structure across all compilation stages.

## 2025-12-26

- Top-level macro skipping now keys off `(def name (macro ...))` instead of the removed `defmacro` head. The emitter inspects `def` forms for macro literals and omits them from generated JavaScript so compile-time declarations never leak into runtime output regardless of the surface syntax.
- `let` emission now consults semantic symbol metadata and drops any bindings whose identifiers are macros, ensuring local macro helpers remain compile-time only and no longer cause the emitter to choke on `(macro ...)` literals or output dead runtime code.

## 2025-12-25

- Alias sanitizer now preserves the new `foo#` gensym placeholders by mapping trailing `#` characters to `_HASH` in emitted identifiers. This keeps analyzer and emitter serialization in lockstep once auto gensyms expand to sanitized JavaScript bindings.
- `fn` emission now mirrors the interpreter's multi-arity semantics. Single-clause functions continue to emit concise arrows, while multi-clause forms generate a dispatcher that selects the appropriate clause (or variadic fallback) based on argument count. This keeps compiled JavaScript in lockstep with runtime arity validation.

## 2025-12-24

- Flattened `(import ...)` namespaces now skip macro exports entirely, so the emitter only generates destructured bindings for runtime values. This pairs with the analyzer's new macro-metadata pipeline to keep runtime output free of compile-time-only helpers like `and`/`or`.
- Removed the `(get alias member)` lowering branch. Namespace access now relies solely on the existing `alias/member` syntax, and any `get` usage is treated as a normal function call (e.g., `prelude/get`). Tests now cover both slash access and the library helper to ensure bracket-notation regressions stay caught.

- Removed the temporary `__result` sink variable from generated modules. Top-level forms now emit as regular statements/exports, trimming runtime noise and matching the interpreter-based REPL pipeline.

- Top-level `defmacro` forms are now treated as compile-time declarations only, so the emitter skips them and relies on the analyzer's expanded AST when producing runtime JavaScript. This prevents crashes when a program defines macros alongside regular definitions.

- Deleted the arithmetic fast-path (`emitArithmetic`) so `+`, `-`, `*`, and `/` lower exactly like user-defined functions. Generated modules now call the prelude/runtime helpers instead of inlining JS operators, which keeps semantics consistent with the interpreter and lets arithmetic symbols be referenced as ordinary values.

- Removed the legacy `println` builtin shortcut from `emitSymbol()`. Programs now reference `println` through the prelude/runtime just like any other binding, which keeps analyzer, interpreter, and emitter behavior aligned.

## 2025-12-23

- Emitter now understands alias-less `(import "./module.lang")` forms by generating a temporary namespace import and re-exporting each flattened binding individually (`export const foo = __import__0.foo;`). This mirrors the analyzer's new `ModuleImportRecord.flatten` metadata so imported symbols behave like native `def`s in generated JavaScript.
- Added regression coverage to ensure flattened bindings produce stable identifiers and continue to compile even when no explicit alias is present in the source program.

## 2025-12-21

- Hoisted the new `(require alias ...)` and `(external alias ...)` forms into explicit `import * as alias from '...';` statements, rewriting `.lang` paths to `.js` and re-exporting the alias so `__env` snapshots keep observing the binding.
- Stopped re-exporting namespace import aliases; `require`/`external` bindings remain private to the module while still available for evaluation side effects.
- Lowered namespace sugar (`alias/member` and `(get alias member)`) into JS property access with automatic fallback to bracket notation for non-identifier members.
- Added regression tests covering namespace imports plus property access to lock down the new emission strategy.
- Updated the emitter to consume the parser's new `NamespaceImport` nodes so codegen no longer depends on raw list matching when generating import statements.
- External namespace members now share the same identifier serialization as the analyzer/runtime (e.g., `runtime/symbol?` maps to `symbol_QMARK`), fixing property lookups for helpers that include suffix punctuation.

## 2025-12-20

- Introduced the stub code generation stage that turns semantic graphs into IR summaries and executable JS modules.
- Upgraded codegen to lower `def`, `let`, and function application into runnable JavaScript with inline source maps for `lang run`.
- Integrated semantic metadata into the emitter: symbol identifiers now come from the analyzer's `SymbolId`s, IR exports each binding's final JS name, and new tests cover the enriched IR surface.
- Extended snapshot coverage with a macro-style pipeline fixture plus IR dumps to guard identifier hygiene and source map stability.
- Now consumes the semantic alias metadata for every symbol so emitted identifiers and IR reports stay perfectly aligned with analyzer output.
