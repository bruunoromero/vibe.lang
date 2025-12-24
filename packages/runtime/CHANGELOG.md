# @vibe/runtime Changelog

## Unreleased

- Added shared numeric guards for `add*`, `sub*`, `mul*`, `div*`, and `mod*` so prelude reducers get consistent error messages while they replace the codegen fast-path semantics.
- `get` now accepts namespace objects emitted by codegen as well as plain `Map` instances, returning `null` (or an explicit default) when a member is missing. This makes `(prelude/get alias member)` work the same way for runtime data structures and imported modules.

## 2025-12-23

- Added tagged runtime symbols (`{ __vibeType: "symbol", name }`) plus `symbol`/`symbol?` helpers so user code can distinguish symbols from raw strings without new special forms.
- Updated `eq*`, `str`, `type`, and map helpers to understand tagged symbols.
- Documented and tested the new behavior to keep interpreter/module pipelines in sync.
