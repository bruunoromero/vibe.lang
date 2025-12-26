# @vibe/prelude

## Unreleased

- Declared `vibe.sources` (plus `vibe.entry`) in `package.json` so workspace builds can resolve `@vibe/prelude` imports without hard-coded file paths.
- Reimplemented `+`, `-`, `*`, and `/` as variadic reducers backed by runtime primitives so zero-arg identities, unary forms, and long folds all behave like the codegen fast-path (with Bun tests covering the behavior).
- Added `get`, a thin wrapper over `runtime/get`, so user code can call `(prelude/get alias "member" default)` instead of relying on the removed builtin special form. The helper preserves optional default arguments and passes tests covering namespace-like objects.
- Introduced `and`/`or` macros that mirror Clojure's short-circuit semantics while keeping macro-generated temporaries hygienic via `gensym`.
- Upgraded `and`/`or` to multi-clause macros so zero-arg, unary, and variadic cases get explicit clauses that dispatch deterministically and align with the language-wide multi-arity semantics.

## 0.1.0 (2025-12-23)

### Features

- Initial prelude package with standard library written in vibe
- Import primitives from @vibe/runtime via `(external runtime "@vibe/runtime")`
- Arithmetic operators: `+`, `-`, `*`, `/`, `mod`
- Comparison operators: `=`, `<`, `>`, `<=`, `>=`
- Sequence operations: `first`, `rest`, `cons`, `count`, `nth`, `map`, `filter`, `reduce`, `concat`, `take`, `drop`, `reverse`
- Map operations: `get`, `assoc`, `dissoc`, `keys`, `vals`
- Collection constructors: `list`, `vector`
- Utility functions: `type`, `str`, `gensym`, `println`
