# Macro Authoring Guide

Building macros in `vibe` lets you extend the surface syntax without patching the compiler. This guide documents the supported `macro` literal surface (used via `(def name (macro ...))`), how syntax quoting works, variadic parameters, compile-time evaluation, and the diagnostics you can expect from `@vibe/semantics`.

## Declaring Macros

Define macros by binding a `macro` literal (typically via `def`):

```
(def with-temp
  (macro [expr]
    `(let [tmp ~expr]
       (println tmp)
       tmp)))
```

Rules enforced by the analyzer:

- The binding target must be a symbol (the enclosing `def` enforces this at the top level).
- Parameters must be provided via a vector of symbols. Duplicate parameters trigger `SEM_MACRO_DUPLICATE_PARAM`.
- Exactly one body expression is supported. The body is evaluated with the interpreter at analysis time and must produce a form.
- Returning a syntax-quoted template `` `(...) `` is still the most ergonomic way to use `~`/`~@`, but it is optional—macros can also construct lists, vectors, or maps manually and return them directly.
- A body is required (`SEM_MACRO_REQUIRES_BODY`).
- Macro literals are only valid inside binding forms (e.g., `def`, `let`). Macros defined via `def` are exported; macros introduced inside other bindings remain scoped to that binding.

### Variadic Macros

Macros support variadic parameters using `&` followed by a rest parameter name:

```
(def my-list (macro [& items] `[~@items]))

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
(def thread-first
  (macro [x & forms]
    `(if ~(first forms)
       (cons ~(first (first forms)) (cons ~x (rest (first forms))))
       ~x)))
```

## Multi-Clause Macros

Just like `fn`, macros can provide multiple clauses that dispatch on arity:

```
(def build
  (macro
    ([x] `(vector ~x))
    ([x y] `(vector ~x ~y))
    ([x y & rest] `(vector ~x ~y ~@rest))))
```

Rules mirror `fn`:

- Clauses are attempted in the order they are written.
- Each fixed-arity clause must use a unique arity (`SEM_MACRO_DUPLICATE_ARITY`).
- Only one variadic clause is permitted, it must appear last, and it still enforces its fixed prefix (`SEM_MACRO_REST_POSITION`, `SEM_MACRO_MULTIPLE_REST_CLAUSES`).
- Clause bodies remain single expressions evaluated at analysis time, so wrap multiple steps in `let`/`do` if needed.

## Syntax Quote, Unquote, and Splicing

Inside the syntax-quoted template you can embed caller arguments with unquote (`~`) and unquote-splicing (`~@`).

```
(def vector-of (macro [value] `(vector ~value)))
```

```
(def spread (macro [items] `(list ~@items)))
```

Constraints:

- `~` targets must reference known parameters; otherwise `SEM_MACRO_UNKNOWN_PARAM` is reported.
- `~@` is only valid inside list/vector/set literals, and it must receive a sequence (`SEM_MACRO_SPLICE_SEQUENCE`).
- Inside syntax quotes you can mark hygienic placeholders by appending `#` to a symbol name (`foo#`, `temp-value#`). Each distinct placeholder within the same syntax quote evaluates to a single gensymmed symbol, so repeated occurrences refer to the same binding without explicit `(gensym)` calls.
- Auto gensym placeholders are only valid inside syntax quotes; using `foo#` elsewhere triggers `SEM_GENSYM_PLACEHOLDER_CONTEXT`. Placeholders must be simple symbols (no `alias/foo#`) or `SEM_GENSYM_PLACEHOLDER_NAMESPACE` is emitted.

### Compile-Time Evaluation in Unquotes

Nested expressions inside `~` run through the **full interpreter** while the macro expands. Any construct that works at runtime—`if`, `let`, arithmetic, sequence helpers, even helper functions defined earlier in the same module—can execute during expansion. Macro parameters that represent literal data are converted into interpreter values, letting you inspect or transform them before producing new syntax.

```
(def pick-first
  (macro [forms]
    `(vector ~(if (eq* 0 (count forms))
                  nil
                  (first forms)))))

(pick-first [1 2 3]) ;=> (vector 1)
```

- `gensym` continues to share the analyzer's hygiene counter, so compile-time symbol generation stays deterministic.
- If the interpreter raises an error (unknown symbol, invalid arity, etc.), it is surfaced as `SEM_MACRO_EVAL_ERROR` or a more specific diagnostic and aborts the macro expansion.
- Referencing undeclared macro parameters (`~missing`) is still rejected with `SEM_MACRO_UNKNOWN_PARAM`.

Use this power sparingly: running large computations during analysis slows the build, but small helpers drastically simplify macro authoring.

## Returning Plain Forms Without Syntax Quote

Because the macro body itself runs through the interpreter, you can return any data structure that mirrors the surface syntax. For example, `(list 'let bindings body)` or `(vector 'foo)` are both valid macro results even though they never use `` `(...) ``. The analyzer will convert the returned value back into an AST node via `valueToNode`, so choose whichever style (raw data or syntax quote) keeps the macro simplest. Syntax quotes remain ideal when you want `~`/`~@` splicing, while raw data works well for generated forms that are easier to assemble with regular list helpers.

## Symbols and Keywords at Runtime

When code reaches the runtime, symbols and keywords are represented as tagged JavaScript objects (`__vibeType: "symbol"` or `"keyword"`). Use the helpers in `@vibe/runtime` to work with them:

```
(external runtime "@vibe/runtime")

(def quoted (quote foo))
(def runtime-sym (runtime/symbol "foo"))
(def runtime-kw (runtime/keyword "foo"))
(runtime/symbol? runtime-sym)  ;=> true
(runtime/keyword? runtime-kw)   ;=> true
(runtime/eq* runtime-sym quoted) ;=> true
```

Because these helpers live in the runtime package, you can build or compare identifiers inside macros without adding new builtin special forms. Map helpers such as `runtime/assoc` and `runtime/get` also understand tagged symbols/keywords, so you can safely use them as keys without stringifying manually.

Example using core primitives:

```
(def and
  (macro
    ([] true)
    ([x] x)
    ([x & rest]
      (let [temp (gensym "and")]
        `(let [~temp ~x]
           (if ~temp
             (and ~@rest)
             ~temp))))))
```

## Recursive Macro Expansion

Macros that expand to other macro calls are automatically expanded recursively until no macro forms remain:

```
(def inner (macro [x] `(list ~x)))
(def outer (macro [x] `(inner ~x)))

(outer 42)  ; Fully expands to (list 42)
```

The analyzer detects direct recursion and limits expansion depth:

- Direct recursion (`SEM_MACRO_RECURSION`) is detected when a macro expands to itself.
- Maximum expansion depth is 100 (`SEM_MACRO_MAX_DEPTH`).

## Gensym and Hygiene

Use `gensym` inside `~` to generate hygienic temporaries:

```
(def with-unique
  (macro [expr]
    `(let [~(gensym "tmp") ~expr]
       42)))
```

The analyzer assigns hygiene tags and alias metadata to every symbol so macro-introduced identifiers cannot leak into caller scopes. Each invocation receives a unique alias (`tmp__symbol_42`, etc.).

For terser macros, use the auto gensym shorthand: append `#` to a symbol inside the syntax quote and the analyzer/interpreter will replace it with a shared gensym. For example, `` `(let [foo# ~expr] foo#) `` expands as if you had manually called `(gensym "foo")` once and reused the result throughout the template.

## Diagnostics and Debugging

Macro expansion happens during semantic analysis. Common diagnostics include:

- `SEM_MACRO_ARITY_MISMATCH` / `SEM_MACRO_ARG_MISSING` for argument count issues (variadic macros check minimum arity).
- `SEM_MACRO_DUPLICATE_ARITY`, `SEM_MACRO_MULTIPLE_REST_CLAUSES`, and `SEM_MACRO_REST_POSITION` enforce well-formed multi-clause definitions.
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
