# Changelog

## 2025-12-25

- Syntax-quote evaluation now mirrors Clojure's auto gensym reader sugar. Symbols ending with `#` (e.g., `temp#`) are replaced with shared gensymmed names per syntax-quote scope, reuse the interpreter's gensym counter, and emit `INTERP_SYNTAX_GENSYM_NAMESPACE` when namespace-qualified placeholders appear. Tests cover both placeholder reuse and the new diagnostics.
- `fn` now accepts multiple clauses at runtime. The interpreter evaluates clause lists, records every clause (including variadic ones) in the new `FunctionClauseValue` shape, dispatches calls based on argument count, and surfaces `INTERP_FN_*` diagnostics for duplicate arities, misplaced variadic clauses, and unmatched argument counts. REPL-friendly pretty printing keeps working via the updated value metadata.

## 2025-12-24

- Added syntax-quote evaluation to the interpreter so `` `(...) `` forms, along with `~`/`~@`, produce the same data structures macros expect at analysis time. This enables `defmacro` bodies to run arbitrary control flow before returning either a template or a manually assembled form.

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
