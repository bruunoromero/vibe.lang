# Changelog

## 2025-12-30

- Recognized the new `defp` special form inside the evaluator so private definitions behave identically to `def` at runtime. The interpreter still mutates the current environment for `defp`, ensuring private helpers are available within a module even though they remain hidden from import tables.
- Extended the special-form regression suite to cover `defp` so REPL and interpreter flows stay in sync with the analyzer.

## 2025-12-29

- **Destructuring in let/fn** — The evaluator now parses binding targets through the shared `parseBindingPattern` helper, caches patterns per AST node, and binds destructured values for both `let` bindings and function parameters. Vector patterns support nested bindings, `& rest`, and `:as` aliases, while map patterns understand explicit keys plus the `:keys`/`:strs`/`:syms` shorthands with lazy `:or` defaults.
- **Runtime diagnostics for malformed patterns** — Pattern parse failures surface `INTERP_PATTERN_*` diagnostics that mirror the analyzer’s feedback, keeping REPL and interpreter error messages aligned with compile-time checks.
- Added coverage in `packages/interpreter/tests/evaluate.test.ts` for destructured let expressions and function parameters, including aliasing, rest collection, and default expressions that depend on previously bound symbols.

## 2025-12-28

- **Error handling with try/catch/finally** — Full runtime support for try forms with optional catch and finally clauses. The new `evaluateTry` method handles error catching with proper scope isolation, and `evaluateThrow` allows explicit error raising via the `throw` special form.
- **RuntimeThrow signal class** — Exceptions now flow through the async evaluator using an internal `RuntimeThrow` class that wraps the error and source span, preserving context while avoiding JS exception pollution. The top-level `evaluate` wrapper catches these signals and converts them to semantic diagnostics.
- **ErrorValue type and coercion** — Extended the `Value` type system with `ErrorValue` to represent caught errors (wrapping raw JS `Error` objects). Helper functions `makeError()`, `isErrorValue()`, and `coerceValueToError()` handle conversion between the Vibe value system and JavaScript error semantics.
- **Sequence evaluation helpers** — Added `runSequenceSafely()` and `evaluateSequenceNodes()` to safely evaluate node lists while capturing thrown errors via `SequenceOutcome` objects, enabling proper control flow in finally clauses.
- Tests in `packages/interpreter/tests/evaluate.test.ts` cover normal flow, error catching, catch binding, and unhandled throw diagnostics.

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
