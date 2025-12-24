# @vibe/prelude

## Unreleased

- Declared `vibe.modules` in `package.json` so workspace builds can resolve `@vibe/prelude` imports without hard-coded file paths.

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
