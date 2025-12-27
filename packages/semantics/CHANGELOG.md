# @vibe/semantics Changelog

## 2025-12-30

- Added full analyzer support for the new `defp` special form. Private definitions now bind symbols, macros, and diagnostics exactly like `def` while skipping module-export registration so imports only see public APIs.
- Analyzer fixtures now cover private exports to confirm local usages resolve correctly and the `graph.exports` payload remains unchanged.

## 2025-12-29

- **Destructuring patterns for bindings** — `let` bindings and `fn` parameters now flow through the shared `parseBindingPattern` helper so vector/map patterns (including nested forms, `& rest`, `:keys`/`:strs`/`:syms`, `:or`, and `:as`) introduce scoped symbols deterministically. The analyzer reports the new `SEM_PATTERN_*` diagnostics when a pattern is malformed and visits `:or` defaults eagerly so their expressions participate in dependency analysis.
- **Alias + default traversal** — Map defaults (`:or`) and alias expressions are now revisited during analysis so macro-generated defaults and nested destructuring emit the same diagnostics as ordinary bindings.
- Tests in `packages/semantics/tests/analyze.test.ts` cover both let-bound and function-parameter destructuring, ensuring every introduced symbol appears in the semantic graph.

## 2025-12-28

- **Feature: try/catch/finally semantic analysis** — Added `handleTry` and `handleThrow` to the analyzer so try forms are validated and catch bindings are defined within scoped environments. Diagnostics now catch clause ordering violations (`SEM_TRY_CLAUSE_ORDER`), duplicate catch/finally clauses, and malformed bindings.
- **Error binding hygiene** — Catch bindings are automatically registered as `var` symbols in their child scope, enabling proper resolution of caught error identifiers downstream.
- **Throw validation** — Throw expressions are validated for arity (`SEM_THROW_REQUIRES_VALUE`, `SEM_THROW_TOO_MANY_ARGS`) and visited to ensure arguments are well-formed.
- Tests in `packages/semantics/tests/analyze.test.ts` cover try/catch scoping and throw argument checking.

## 2025-12-27

- Macro expansion now shares a persistent interpreter environment with previously evaluated `def` forms, so compile-time code can call helper functions defined earlier in the same module. Top-level definitions are executed once during analysis, cached for later macro runs, and exposed to tests covering imported runtime aliases.
- Removed support for the `@` deref reader macro during semantic analysis. Macro expansion, hygiene tracking, and scope scrubbing no longer special-case deref nodes, and analyzers now rely on explicit `(deref ...)` function calls if future runtimes reintroduce dereferencing behavior.

## 2025-12-26

- Macro expansion now clears parser-provided `scopeId` metadata from unquoted arguments (including spliced sequences) so inserted expressions adopt the caller's lexical scope. This fixes unresolved parameter diagnostics for macros like `defn` that expand multi-arity functions before other macros such as `or` rewrite their bodies.
- Replaced the `defmacro` special form with `macro` literals that bind via ordinary forms like `def` and `let`. The analyzer now detects `(macro ...)` initializers, registers macro metadata (clauses, dependencies) for those bindings, and skips runtime analysis of macro literals while still allowing macros to be scoped lexically. Non-top-level macros remain private because only top-level `def` exports are recorded, and standalone `(macro ...)` expressions emit `SEM_MACRO_LITERAL_CONTEXT` so macros stay restricted to binding initializers.

## 2025-12-25

- Syntax-quoted templates now understand Clojure-style auto gensym placeholders (`foo#`). During macro expansion the analyzer replaces each placeholder with a deterministic hygiene-safe symbol, shares counters with `(gensym)`, and surfaces targeted diagnostics when placeholders escape syntax quotes (`SEM_GENSYM_PLACEHOLDER_CONTEXT`) or include namespace qualifiers (`SEM_GENSYM_PLACEHOLDER_NAMESPACE`). Nested syntax quotes now allocate independent placeholder scopes.
- `fn` now supports multiple parameter clauses. The analyzer allocates a clause scope for every vector, enforces unique fixed arities, restricts each function to a single trailing variadic clause, and reports targeted diagnostics (`SEM_FN_DUPLICATE_ARITY`, `SEM_FN_MULTIPLE_REST_CLAUSES`, `SEM_FN_REST_POSITION`, `SEM_FN_CLAUSE_REQUIRES_BODY`). The resulting semantic graph keeps per-clause parameter bindings and hygiene metadata aligned with the parser's scope hints.
- `defmacro` now mirrors the same multi-clause semantics. Macro definitions can provide multiple parameter vectors, and the analyzer validates duplicate arities, multiple variadic clauses, and clause ordering (`SEM_MACRO_DUPLICATE_ARITY`, `SEM_MACRO_MULTIPLE_REST_CLAUSES`, `SEM_MACRO_REST_POSITION`). Macro metadata (including flattened imports) now preserves every clause so imported macros retain their dispatch tables.

## 2025-12-24

- Namespaced `(require alias ...)` statements now register exported macros as `alias/name` bindings so imported macros retain their metadata (parameters, dependencies, hygiene) just like flattened `(import ...)` forms.
- Macro dependency seeding resolves both `external` and `require` aliases, so compile-time evaluation autoloads any JS module a macro depends on (including relative `require` paths) instead of only handling externals.
- Flattened imports now carry full macro metadata, so macros exported from other modules (`@vibe/prelude`'s `and`, `or`, etc.) expand during analysis instead of leaking raw macro calls into generated JavaScript or failing at runtime.
- Macro expansion automatically loads any `external` dependencies recorded in imported macro metadata (e.g., `runtime` for prelude macros), so compile-time evaluation no longer surfaces `INTERP_UNDEFINED_NAMESPACE` when macros rely on runtime helpers.
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
