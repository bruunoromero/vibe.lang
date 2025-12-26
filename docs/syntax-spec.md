# Syntax Specification

This document is the canonical description of the Lisp-inspired surface syntax that `@vibe` accepts. Every syntax-affecting change **must** update this file alongside relevant ADRs and CHANGELOG entries.

## Guiding Principles

1. Source files are UTF-8 text processed by Bun. A UTF-8 BOM and a leading shebang (`#!/usr/bin/env vibe`) are ignored when present.
2. The syntax favors homoiconicity: the primary data structures (lists, vectors, maps, sets) double as the core program representation.
3. Reader macros stay close to Clojure semantics so upstream Lisp users can predict behavior.
4. AST nodes defined in `@vibe/syntax` map one-to-one with the forms described below and remain immutable.

## Lexical Conventions

### Separators & Whitespace

- Whitespace characters: space, tab, vertical tab, formfeed, carriage return, newline.

## Scope Metadata

- Every AST node now carries an optional `scopeId` string (`"scope_0"`, `"scope_1"`, …) assigned by the parser.
- `scopeId` identifies the lexical scope that owns the node. The parser seeds `scope_0` for the program root and allocates new IDs for `let` and `fn` bodies, binding vectors, and any nested expressions that execute in child scopes.
- Reader and collection nodes inherit their surrounding scope ID unless the syntax itself introduces a new lexical region.
- Macro-expanded nodes drop any `scopeId` hints before analysis so the semantic stage can remap them to the caller’s scope.
- Downstream stages (semantics, codegen, tooling) treat `scopeId` as deterministic metadata for correlating nodes with scope tables.
- Commas are treated as whitespace for readability (`(def foo [1, 2, 3])`).
- Line comments start with `;` and continue to the next line terminator.
- The lexer removes insignificant whitespace before emitting tokens.

| `(` `)` | `(` `)` | Begin/end list |
| `[` `]` | `[` `]` | Begin/end vector |
| `{` `}` | `{` `}` | Begin/end map or (with dispatch) set |
| `'` | `'form` | Quote reader macro |
| `` ` `` | `` `form`` | Syntax-quote reader macro |
| `~` | `~form` | Unquote |
| `~@` | `~@form` | Unquote splicing |
| `@` | `@form` | Deref |
| `#` | `#dispatch` | Dispatch macro prefix (currently `#{` for sets) |
| Numbers | `42` `-3.14` `6.022e23` | Optional sign, decimal part, exponent |
| Strings | `"hello"` | Double-quoted with escapes |
| Characters | `\a` `\newline` `\u03bb` | Backslash-prefixed literals |
| Symbols | `foo` `user/name` `*main*` `foo?` `bar!` `baz*` `foo#` | Cannot start with digits or delimiters; may end with `?`, `!`, `*`, `+`, `-`, `=`, `<`, `>`, `/`. A trailing `#` is reserved for auto-gensym placeholders inside syntax-quoted forms. |
| Keywords | `:ok` `::auto` | Optional double-colon auto-namespace |
| Boolean/Nil | `true` `false` `nil` | Case-insensitive |

### Strings

- Begin and end with `"`.
- Supports escapes: `\"`, `\'`, `\\`, `\n`, `\r`, `\t`, `\b`, `\f`, and `\uXXXX` (four hex digits).
- Unterminated strings emit `LEX_STRING_UNTERMINATED` diagnostics but still produce a token to keep parsing moving.

### Characters

### Numbers

- Invalid sequences emit `LEX_NUMBER_INVALID`.

### Symbols & Keywords

- The identifiers `nil`, `true`, and `false` always produce dedicated node kinds rather than generic symbols.

## Literal Forms

| List | `(...)` | `ListNode` | Function call or special form; empty list evaluates to `null` in codegen. |
| Vector | `[...]` | `VectorNode` | Ordered collection; used for binding forms (`let`, `fn`). |
| Map | `{key value ...}` | `MapNode` | Must contain an even number of forms; otherwise emits `PARSE_MAP_ODD_ENTRIES` and pairs dangling key with `null`. |
| Set | `#{...}` | `SetNode` | Created via dispatch macro `#` immediately followed by `{`. |

All delimiters must balance. Unmatched closers report `PARSE_UNEXPECTED_CLOSING`; missing closers report `PARSE_*_UNTERMINATED` diagnostics.

## Reader Macros & Dispatch

| Reader macro | Expands to             | Description                                                                                                                                           |
| ------------ | ---------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| `'form`      | `Quote` node           | Prevents evaluation.                                                                                                                                  |
| `` `form``   | `SyntaxQuote` node     | Like quote but supports hygiene-aware unquoting.                                                                                                      |
| `~form`      | `Unquote` node         | Allowed inside syntax-quoted forms only.                                                                                                              |
| `~@form`     | `UnquoteSplicing` node | Inserts a sequence into the surrounding form.                                                                                                         |
| `@form`      | `Deref` node           | Signals runtime dereferencing (`(deref form)`).                                                                                                       |
| `#form`      | `Dispatch` node        | Generic dispatch marker; currently only `#{...}` is recognized as a set literal. Additional dispatch targets must be documented here when introduced. |

If a reader or dispatch macro lacks a following form, the parser emits `PARSE_MACRO_MISSING_TARGET` or `PARSE_DISPATCH_MISSING_TARGET` but still constructs a node with `target = null`.

## Special Forms (Recognized Heads)

These symbols are intercepted by downstream stages for non-generic evaluation:

- `def` — Top-level definition only. Emits exported bindings in codegen. When the value is a macro literal (see below), the analyzer registers the binding as a macro so downstream compilation stages can expand it while still exporting the symbol through the module metadata.
- `macro` — Macro literal that mirrors `fn` clause syntax. Must appear as the initializer of a binding form such as `def` or `let` (standalone `(macro ...)` expressions are rejected). Single-clause macros look like `(def name (macro [params*] body))`. Multi-clause macros reuse `fn` semantics: `(def name (macro ([params*] body) ([params2*] body2) ...))`. Clauses are matched by arity from top to bottom; only one variadic clause (one that uses `& rest`) is allowed and it must appear last. Each clause body is a single expression that the analyzer evaluates at analysis time, and it must return a form. Returning a syntax-quoted template (e.g., `` `(template ...) ``) still enables `~`/`~@` splicing, but macros can also build data manually and return it directly. Macros bound via `def` are exported; macros introduced inside other bindings remain scoped to that binding.
- `let` — Introduces a lexical scope: `(let [name expr ...] body...)`.
- `fn` — Lambda literal. The single-clause form `(fn [params...] body...)` remains valid, and multi-clause forms follow Clojure's syntax: `(fn ([params...] body...) ([params2...] body2...) ...)`. Clauses are evaluated in order; the first clause whose fixed arity matches the call is selected, falling back to a single variadic clause (one that uses `& rest`) when present. Only one variadic clause is allowed per function and it must appear last. Each clause requires at least one body expression.
  Example:
  ```
  (fn
  	([x] x)
  	([x y] (+ x y))
  	([x y & rest] x))
  ```
- `if` — Branching form: `(if condition then [else])`. Missing else defaults to `nil`.
- `quote` — Prevents evaluation, equivalent to `'form`.
- `do` — Sequential evaluation in a single expression position: `(do form* expr)`.
- `require` — Module import (see below). Restricted to the top level.
- `external` — JavaScript module import (see below). Restricted to the top level.
- `import` — Module import that flattens the target module's exports into the current scope (see below). Restricted to the top level.

Arithmetic helpers (`+`, `-`, `*`, `/`, comparisons, sequence utilities, etc.) are implemented in `@vibe/prelude`. User code should `(require prelude "@vibe/prelude")` (or whichever module provides the desired helpers) rather than relying on implicit global definitions.

## Module Imports & Namespaces

Module-level dependencies opt into explicit `def` bindings that wrap `require` or `external` forms:

| Form                           | Description                                                                                                                                            |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `(require math "./math.lang")` | Imports another Vibe source file and binds the alias `math` to the generated module (`./math.lang` automatically maps to `./math.js` in emitted code). |
| `(external fs "node:fs")`      | Imports a JavaScript/TypeScript module verbatim (Node/Bun specifiers, package names, or relative `.js` paths) and binds the alias `fs`.                |
| `(import "@vibe/prelude")`     | Imports a Vibe module without an alias and flattens every exported binding into the current module as if they were defined locally.                    |

- Imports are namespace-only; no selective/destructured imports are supported yet. Every alias receives whatever the referenced module exports. The `import` form is the exception: it loads a module and auto-defines each exported binding in the current scope without requiring an alias.
- Import aliases remain module-private; `require`/`external` do not implicitly re-export those bindings.
- Relative `require` paths accept either `.lang` extensions (rewritten to `.js`) or bare paths (a `.js` suffix is appended during codegen).
- `require`/`external` statements must supply a symbol alias followed by a single string literal argument.
- These forms are only valid at the top level; using them inside nested scopes is rejected during analysis.
- `require` specifiers must be relative paths that map to Lang sources discoverable within the current workspace registry. The analyzer records each import and consults a resolver (provided by the CLI) to verify the referenced module exists. Missing modules emit `SEM_REQUIRE_RESOLVE_FAILED` diagnostics but do not prevent `lang compile-all` from compiling unrelated files.
- `external` specifiers continue to target JavaScript modules directly; they bypass the Lang module resolver and are emitted verbatim in generated code.
- The parser now emits dedicated namespace-import AST nodes so downstream stages can reason about imports without re-inspecting generic list shapes.
- Every top-level `def` is exported automatically as a named ES export in generated code; there is no explicit export syntax in the source language. The code generator no longer emits a default export or runtime environment wrapper.
- `import` depends on module export metadata surfaced by the CLI; analysis reports diagnostics if exports cannot be enumerated or if flattening would overwrite existing bindings. Generated JavaScript destructures the imported namespace into individual bindings so downstream modules can reference them without alias prefixes.

### Namespace Access

- `alias/member` stays a single symbol; the analyzer resolves the alias binding while preserving the member lexeme for codegen so property access can be emitted without rewriting the AST.
- When `member` is a valid JavaScript identifier the emitted code uses dot notation (`math/add` → `math.add`). Otherwise bracket notation preserves the original lexeme (`path/path-separator` → `path["path-separator"]`).
- For dynamic property lookups or fallback values use the library helper exposed as `prelude/get` (wrapping `runtime/get`), which accepts regular arguments such as `(prelude/get alias "member" default)`.

```
program        ::= form*
form           ::= atom | list | vector | map | set | reader_macro | dispatch
list           ::= '(' form* ')'
vector         ::= '[' form* ']'
map            ::= '{' (form form)* '}'
set            ::= '#{' form* '}'
reader_macro   ::= quote | syntax_quote | unquote | unquote_splicing | deref
quote          ::= "'" form
syntax_quote   ::= "`" form
unquote        ::= "~" form
unquote_splicing ::= "~@" form
deref          ::= "@" form
dispatch       ::= '#' form
atom           ::= number | string | character | keyword | symbol | boolean | nil
```

### Auto Gensym Placeholders

- Within syntax-quoted forms, simple symbols that end with `#` (e.g., `temp#`) act as auto gensym placeholders. Each placeholder is replaced with a deterministic, hygiene-preserving symbol (`temp__0`, `temp__1`, …) during macro expansion or runtime syntax-quote evaluation.
- Every syntax quote resets its placeholder scope, so nested quotes receive fresh gensym counters rather than leaking `temp#` bindings from surrounding templates.
- Placeholders are rejected outside syntax quotes (`SEM_GENSYM_PLACEHOLDER_CONTEXT`) and must be simple symbols without namespace qualifiers (`SEM_GENSYM_PLACEHOLDER_NAMESPACE`). When in doubt, keep the hint portion alphanumeric and let the compiler handle alias sanitization.
- Manual `(gensym "hint")` calls and placeholder sugar share the same hygiene counter, ensuring predictable codegen aliases.

## Diagnostics Overview

- `LEX_*` codes originate from the lexer (invalid numbers, strings, characters, etc.).
- `PARSE_LIST_UNTERMINATED`, `PARSE_VECTOR_UNTERMINATED`, `PARSE_MAP_UNTERMINATED`, `PARSE_SET_UNTERMINATED` guard missing closers.
- `PARSE_UNEXPECTED_CLOSING` fires for stray `)`/`]`/`}`.
- `PARSE_MAP_ODD_ENTRIES` indicates uneven map literals.
- `PARSE_MACRO_MISSING_TARGET` / `PARSE_DISPATCH_MISSING_TARGET` highlight reader/dispatch macros with no operand.
- Semantic-only metadata (scope IDs, hygiene tags) remains outside this spec; refer to `docs/ADR-002-semantics.md` for details.
- Macro helpers: `gensym` may appear only inside syntax-quoted macro bodies (e.g., `~(gensym "hint")`) to produce hygiene-friendly synthetic symbols. Document any new helpers alongside their ADR.
