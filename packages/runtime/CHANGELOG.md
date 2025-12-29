# @vibe/runtime Changelog

## 2025-12-28

- No runtime changes for try/catch/finally support — error handling is implemented entirely in the interpreter and code generator. Error objects are passed directly through the value system without requiring new runtime helpers.

## Unreleased

- Added shared numeric guards for `add*`, `sub*`, `mul*`, `div*`, and `mod*` so prelude reducers get consistent error messages while they replace the codegen fast-path semantics.
- `get` now accepts namespace objects emitted by codegen as well as plain `Map` instances, returning `null` (or an explicit default) when a member is missing. This makes `(prelude/get alias member)` work the same way for runtime data structures and imported modules.
- Keywords now mirror symbols at the JS boundary: `keyword`/`keyword?` create `{ __vibeType: "keyword", name }` objects, `type` returns tagged keywords, and equality/string helpers understand them. Keywords are interned so runtime map destructuring can rely on object identity.

## 2025-12-29

- Removed specialized `MapValue` runtime shapes and map-literal helpers that assumed `{ ... }` syntax. Runtime helpers now operate over plain JS objects or `Map` instances where appropriate, but the language no longer emits map literals; prefer vectors and explicit accessors or prelude helpers for associative data.

## 2025-12-23

- Added tagged runtime symbols (`{ __vibeType: "symbol", name }`) plus `symbol`/`symbol?` helpers so user code can distinguish symbols from raw strings without new special forms.
- Updated `eq*`, `str`, `type`, and map helpers to understand tagged symbols.
- Documented and tested the new behavior to keep interpreter/module pipelines in sync.
