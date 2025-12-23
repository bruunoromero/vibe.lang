# @vibe/semantics Changelog

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
