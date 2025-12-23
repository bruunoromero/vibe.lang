# @vibe/codegen Changelog

## 2025-12-21

- Hoisted the new `(require alias ...)` and `(external alias ...)` forms into explicit `import * as alias from '...';` statements, rewriting `.lang` paths to `.js` and re-exporting the alias so `__env` snapshots keep observing the binding.
- Stopped re-exporting namespace import aliases; `require`/`external` bindings remain private to the module while still available for evaluation side effects.
- Lowered namespace sugar (`alias/member` and `(get alias member)`) into JS property access with automatic fallback to bracket notation for non-identifier members.
- Added regression tests covering namespace imports plus property access to lock down the new emission strategy.
- Updated the emitter to consume the parser's new `NamespaceImport` nodes so codegen no longer depends on raw list matching when generating import statements.

## 2025-12-20

- Introduced the stub code generation stage that turns semantic graphs into IR summaries and executable JS modules.
- Upgraded codegen to lower `def`, `let`, and function application into runnable JavaScript with inline source maps for `lang run`.
- Integrated semantic metadata into the emitter: symbol identifiers now come from the analyzer's `SymbolId`s, IR exports each binding's final JS name, and new tests cover the enriched IR surface.
- Extended snapshot coverage with a macro-style pipeline fixture plus IR dumps to guard identifier hygiene and source map stability.
- Now consumes the semantic alias metadata for every symbol so emitted identifiers and IR reports stay perfectly aligned with analyzer output.
