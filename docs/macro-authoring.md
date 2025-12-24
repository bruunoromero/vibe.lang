# Macro Authoring Guide

Building macros in `vibe` lets you extend the surface syntax without patching the compiler. This guide documents the supported `defmacro` surface, how syntax quoting works, variadic parameters, compile-time evaluation, and the diagnostics you can expect from `@vibe/semantics`.

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
- Exactly one body expression is supported. The body is evaluated with the interpreter at analysis time and must produce a form.
- Returning a syntax-quoted template `` `(...) `` is still the most ergonomic way to use `~`/`~@`, but it is optional—macros can also construct lists, vectors, or maps manually and return them directly.
- A body is required (`SEM_MACRO_REQUIRES_BODY`).

### Variadic Macros

Macros support variadic parameters using `&` followed by a rest parameter name:

```
(defmacro my-list [& items]
  `[~@items])

(my-list 1 2 3)  ; Expands to [1 2 3]
```

Variadic rules:

- Use `&` followed by a symbol to collect remaining arguments.
- Only one `&` rest parameter is allowed (`SEM_MACRO_DUPLICATE_REST`).
- No parameters can appear after the `&` rest parameter (`SEM_MACRO_PARAMS_AFTER_REST`).
- The rest parameter is bound to a vector containing all remaining arguments.
- Empty argument lists are allowed (rest will be an empty vector).

Combined fixed and variadic parameters:

```
(defmacro thread-first [x & forms]
  `(if ~(first forms)
     (cons ~(first (first forms)) (cons ~x (rest (first forms))))
     ~x))
```

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

### Compile-Time Evaluation in Unquotes

Nested expressions inside `~` run through the **full interpreter** while the macro expands. Any construct that works at runtime—`if`, `let`, arithmetic, sequence helpers, even helper functions defined earlier in the same module—can execute during expansion. Macro parameters that represent literal data are converted into interpreter values, letting you inspect or transform them before producing new syntax.

```
(defmacro pick-first [forms]
  `(vector ~(if (eq* 0 (count forms))
                nil
                (first forms))))

(pick-first [1 2 3]) ;=> (vector 1)
```

- `gensym` continues to share the analyzer's hygiene counter, so compile-time symbol generation stays deterministic.
- If the interpreter raises an error (unknown symbol, invalid arity, etc.), it is surfaced as `SEM_MACRO_EVAL_ERROR` or a more specific diagnostic and aborts the macro expansion.
- Referencing undeclared macro parameters (`~missing`) is still rejected with `SEM_MACRO_UNKNOWN_PARAM`.

Use this power sparingly: running large computations during analysis slows the build, but small helpers drastically simplify macro authoring.

## Returning Plain Forms Without Syntax Quote

Because the macro body itself runs through the interpreter, you can return any data structure that mirrors the surface syntax. For example, `(list 'let bindings body)` or `(vector 'foo)` are both valid macro results even though they never use `` `(...) ``. The analyzer will convert the returned value back into an AST node via `valueToNode`, so choose whichever style (raw data or syntax quote) keeps the macro simplest. Syntax quotes remain ideal when you want `~`/`~@` splicing, while raw data works well for generated forms that are easier to assemble with regular list helpers.

## Symbols vs. Strings at Runtime

When code reaches the runtime, symbols are represented as tagged JavaScript objects with `__vibeType: "symbol"`. Use the helpers in `@vibe/runtime` to work with them:

```
(external runtime "@vibe/runtime")

(def quoted (quote foo))
(def runtime-sym (runtime/symbol "foo"))
(runtime/symbol? runtime-sym)  ;=> true
(runtime/eq* runtime-sym quoted) ;=> true
```

Because `runtime/symbol` lives in the runtime package, you can build or compare symbols inside macros without adding a new builtin special form. Map helpers such as `runtime/assoc` and `runtime/get` also understand tagged symbols, so you can safely use them as keys without stringifying manually.

Example using core primitives:

```
(defmacro and [& forms]
  `(if (eq* 0 (count forms))
     true
     (if ~(first forms)
       (and ~@(rest forms))
       false)))
       false)
     false))
```

## Recursive Macro Expansion

Macros that expand to other macro calls are automatically expanded recursively until no macro forms remain:

```
(defmacro inner [x] `(list ~x))
(defmacro outer [x] `(inner ~x))

(outer 42)  ; Fully expands to (list 42)
```

The analyzer detects direct recursion and limits expansion depth:

- Direct recursion (`SEM_MACRO_RECURSION`) is detected when a macro expands to itself.
- Maximum expansion depth is 100 (`SEM_MACRO_MAX_DEPTH`).

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

- `SEM_MACRO_ARITY_MISMATCH` / `SEM_MACRO_ARG_MISSING` for argument count issues (variadic macros check minimum arity).
- `SEM_MACRO_RECURSION` when a macro expands to itself directly or indirectly.
- `SEM_MACRO_MAX_DEPTH` when expansion exceeds the depth limit (100 levels).
- `SEM_MACRO_REST_REQUIRES_SYMBOL` when `&` is not followed by a symbol.
- `SEM_MACRO_DUPLICATE_REST` when multiple `&` parameters are declared.
- `SEM_MACRO_PARAMS_AFTER_REST` when parameters appear after the rest parameter.
- `SEM_MACRO_UNQUOTE_UNSUPPORTED` when an unsupported compile-time function is used in unquote.
- `SEM_MACRO_EVAL_ERROR` when compile-time evaluation fails.

To inspect macro expansions:

- `vibe analyze --debug-macros` prints macro symbols, scopes, and diagnostics.
- `vibe run --show-ast --show-ir` streams the parsed AST and generated IR to stderr, making it easy to confirm aliases and hygiene tags.

## Best Practices

1. Prefer descriptive gensym hints (`(gensym "with-temp-value")`) for readable IR dumps.
2. Keep macro bodies small and delegate complex logic to regular functions invoked by the expanded form.
3. Treat user-provided symbols as opaque; avoid string munging or evaluation inside macro definitions.
4. Use variadic parameters (`&`) when your macro needs to accept an arbitrary number of forms.
5. Leverage compile-time evaluation (`first`, `next`, `if`) to build sophisticated control-flow macros.
6. Add regression tests in `packages/semantics/tests/analyze.test.ts` or `advanced-macros.test.ts` whenever you introduce new macro forms or diagnostics.
