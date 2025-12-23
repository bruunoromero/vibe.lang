# Macro Authoring Guide

Building macros in `vibe` lets you extend the surface syntax without patching the compiler. This guide documents the supported `defmacro` surface, how syntax quoting works, and the diagnostics you can expect from `@vibe/semantics`.

## Declaring Macros

Use `defmacro` at the top level:

```
(defmacro with-temp [expr]
  `(let [tmp ~expr]
     (println tmp)
     tmp))
```

Rules enforced by the analyzer:

- The macro name must be a symbol.
- Parameters must be provided via a vector of symbols. Duplicate parameters trigger `SEM_MACRO_DUPLICATE_PARAM`.
- Exactly one body expression is supported, and it must be wrapped in a syntax quote `` `(...) ``.
- A body is required (`SEM_MACRO_REQUIRES_BODY`).

## Syntax Quote, Unquote, and Splicing

Inside the syntax-quoted template you can embed caller arguments with unquote (`~`) and unquote-splicing (`~@`).

```
(defmacro vector-of [value]
  `(vector ~value))
```

```
(defmacro spread [items]
  `(list ~@items))
```

Constraints:

- `~` targets must reference known parameters; otherwise `SEM_MACRO_UNKNOWN_PARAM` is reported.
- `~@` is only valid inside list/vector/set literals, and it must receive a sequence (`SEM_MACRO_SPLICE_SEQUENCE`).
- Nested expressions inside `~` are not yet supported beyond `(gensym ...)`.

## Gensym and Hygiene

Use `gensym` inside `~` to generate hygienic temporaries:

```
(defmacro with-unique [expr]
  `(let [~(gensym "tmp") ~expr]
     42))
```

The analyzer assigns hygiene tags and alias metadata to every symbol so macro-introduced identifiers cannot leak into caller scopes. Each invocation receives a unique alias (`tmp__symbol_42`, etc.).

## Diagnostics and Debugging

Macro expansion happens during semantic analysis. Common diagnostics include:

- `SEM_MACRO_ARITY_MISMATCH` / `SEM_MACRO_ARG_MISSING` for argument count issues.
- `SEM_MACRO_RECURSION` when a macro expands to itself directly or indirectly.
- `SEM_MACRO_EXPECTS_SYNTAX_QUOTE` when the body is not syntax-quoted.

To inspect macro expansions:

- `vibe analyze --debug-macros` prints macro symbols, scopes, and diagnostics.
- `vibe run --show-ast --show-ir` streams the parsed AST and generated IR to stderr, making it easy to confirm aliases and hygiene tags.

## Best Practices

1. Prefer descriptive gensym hints (`(gensym "with-temp-value")`) for readable IR dumps.
2. Keep macro bodies small and delegate complex logic to regular functions invoked by the expanded form.
3. Treat user-provided symbols as opaque; avoid string munging or evaluation inside macro definitions.
4. Add regression tests in `packages/semantics/tests/analyze.test.ts` whenever you introduce new macro forms or diagnostics.
