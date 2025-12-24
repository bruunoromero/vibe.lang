# Changelog

## [0.1.0] - 2025-12-23

### Added

- Initial interpreter implementation with full evaluation engine
- Value type system with closures and all collection types
- Environment management with lexical scope chain
- Comprehensive builtin operations library
- Integration with macro expansion system
- Support for function closures with captured environments
- Runtime bridge now understands tagged symbols from `@vibe/runtime`, so `valueToJS` emits `{ __vibeType: "symbol", name }` objects and `jsToValue` converts them back into `SymbolValue`s. Tests cover `runtime/symbol`, `runtime/symbol?`, and `runtime/type` to keep semantic analysis and codegen in sync.
- Added an alias-less `(import "./module.lang")` special form that reuses the existing `require` loader, but flattens every exported binding into the calling environment. Conflicts raise interpreter diagnostics and the new tests verify that imported bindings behave like native `def`s.
