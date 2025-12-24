# @vibe/codegen Changelog

## 2025-12-24

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
