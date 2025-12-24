# @vibe/runtime Changelog

## 2025-12-23

- Added tagged runtime symbols (`{ __vibeType: "symbol", name }`) plus `symbol`/`symbol?` helpers so user code can distinguish symbols from raw strings without new special forms.
- Updated `eq*`, `str`, `type`, and map helpers to understand tagged symbols.
- Documented and tested the new behavior to keep interpreter/module pipelines in sync.
