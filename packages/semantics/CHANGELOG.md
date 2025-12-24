# @vibe/semantics Changelog

## 2025-12-24

- Macro bodies defined via `defmacro` are now analyzed as general expressions instead of requiring a top-level syntax quote. During expansion the analyzer evaluates the body with the interpreter when necessary, allowing macros to compute templates in `let`/`if` blocks or return raw data structures without wrapping everything in `` `(...) ``.
- Removed the `get` builtin special form. Namespace-qualified symbols (`alias/member`) now provide the entire surface for compile-time namespace validation, while ordinary `get` calls flow through user-defined functions (e.g., the prelude helper) without bespoke analyzer plumbing.

- Removed the unused `__result` alias reservation now that codegen no longer emits the sink variable, freeing the name for user-defined bindings.

- Analyzer fixtures now inject stub definitions for `+`, `-`, `*`, and `/` so tests exercise the same prelude-supplied operators users rely on instead of extending the builtin list beyond the core special forms.

- Preserved scope metadata for unquoted arguments and spliced sequences so macro-expanded references keep their original bindings, preventing generated JavaScript from falling back to raw symbol names.
- Compile-time unquote expressions now execute through the full interpreter instead of the previous `gensym`-only restriction, so macros can run arbitrary helpers (`if`, arithmetic, sequence inspection, etc.) when constructing new syntax.
- Removed the `SEM_MACRO_COMPILE_TIME_UNSUPPORTED` diagnostic; compile-time evaluation now only reports interpreter failures (`SEM_MACRO_EVAL_ERROR`) or missing macro parameters (`SEM_MACRO_UNKNOWN_PARAM`).
- Macro expansions now scrub inherited scope metadata and overwrite the original AST node, ensuring analyzer + downstream stages treat macro-generated usages as part of the caller's scope and allowing codegen to consume the expanded forms instead of the raw `(defmacro ...)` call sites.

## 2025-12-23

- Default builtin seeding now matches the core special forms only, so names like `println`, `map`, or runtime helpers must come from user code or explicit `(require)` statements. Analyzer consumers (REPL, tests) can still pass additional `builtins` via `AnalyzeOptions` when they execute code that loads the prelude out-of-band.
- Alias allocation now mirrors the runtime `_STAR` / `_QMARK` serialization so operators like `eq*` export as `eq_STAR`, hyphenated names stay readable (e.g., `foo-bar` → `foo_bar`), and diagnostics/tests no longer rely on the deprecated `operatorNames` table or mutable `used` reservations.
- Added `(import "./module.lang")` handling: top-level import forms now flatten the target module's exported bindings into the current scope, emit `SEM_IMPORT_*` diagnostics for invalid usage (non-string specifiers, missing exports, duplicate bindings, non-top-level placement), and record the imported symbols in `ModuleImportRecord.flatten` so codegen can destructure namespaces deterministically.

## 2025-12-22

- **Improved operator identifier sanitization** — Operators like `<=`, `>=`, `+`, `-`, `*`, `/` now map to readable aliases (`lte`, `gte`, `plus`, `minus`, `mul`, `div`) instead of producing underscore-heavy identifiers. This ensures functions with operator-only names are distinguishable and readable in generated JavaScript.
- Sanitization now applies before symbol ID suffix, preventing collisions between operators like `<=` and `>=`.

## 2025-12-21

- Added namespace-aware symbol resolution so `alias/member` sugar binds through aliases introduced via `(require alias ...)` or `(external alias ...)`, emitting `SEM_UNRESOLVED_NAMESPACE_ALIAS` when the alias is missing.
- Validated the new top-level import forms, ensuring they accept exactly one string literal argument and surfacing `SEM_REQUIRE_EXPECTS_STRING` / `SEM_EXTERNAL_EXPECTS_STRING` diagnostics (plus alias/top-level errors) otherwise.
- Registered `get` as a builtin head so `(get alias member)` can act as the canonical namespace accessor without producing unresolved symbol diagnostics.
- The analyzer now records every `(require)/(external)` form as a `ModuleImportRecord`, consults an optional workspace `ModuleResolver`, and emits `SEM_REQUIRE_RESOLVE_FAILED` without aborting other files when a module cannot be resolved.

## 2025-12-20

- Introduced the semantic analyzer skeleton that records scopes, symbols, and hygiene metadata for parsed programs.
- Added macro expansion support, including `defmacro` definitions, syntax-quote templating with unquote/unquote-splicing, gensym generation, and diagnostics for recursion/arity issues.
- Expanded analyzer test coverage to lock down diagnostics for special forms and macro expansion failures.
- Taught the analyzer to reuse parser-emitted `scopeId` hints when creating scopes, ensured macro expansions scrub stale metadata, and added regression tests that assert semantic scopes align with parser annotations.
- Landed regression suites covering deeply nested scopes, duplicate shadowing diagnostics, and macro hygiene to prevent expansion-time leakage.
- Exposed alias metadata on every `SymbolRecord`, ensuring deterministic sanitized identifiers are available to codegen and tests.
