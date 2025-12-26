# Clojure-Style Function Naming Support

**Date:** December 22, 2025  
**Status:** ✅ Implemented and tested

## Summary

The Vibe language now supports Clojure-style function naming conventions, allowing symbols to end with special characters like `?`, `!`, `*`, `+`, `-`, `=`, `<`, `>`, and `/`.

## Changes Made

### 1. Lexer Updates (`packages/lexer/src/lexer.ts`)

- Added `isSymbolSuffix()` method to recognize special characters: `?!*+\-=<>/`
- Updated `isSymbolChar()` to accept these characters anywhere in a symbol (including suffix positions)
- All delimiters properly exclude these characters while allowing them in identifiers

### 2. Syntax Specification (`docs/syntax-spec.md`)

Updated the symbols row in the lexical conventions table to document new naming patterns:

```
| Symbols | foo user/name *main* foo? bar! baz* | Cannot start with digits or delimiters; may end with ?, !, *, +, -, =, <, >, / (Clojure-style naming) |
```

### 3. Tests Added

#### Lexer Tests (`packages/lexer/tests/tokenize.test.ts`)

- ✅ `supports Clojure-style function names with special suffixes` — Validates single suffixes
- ✅ `supports symbols with multiple special characters` — Validates multiple consecutive suffixes

#### Parser Tests (`packages/parser/tests/parse.test.ts`)

- ✅ `parses function definitions with Clojure-style names` — Validates parsing of `def` forms with special-character function names

### 4. Documentation

Updated CHANGELOG files:

- `packages/syntax/CHANGELOG.md` — Documented the feature addition
- `packages/parser/CHANGELOG.md` — Noted test coverage for new naming patterns

## Test Results

All 111 tests pass:

```
✓ tokenize > supports Clojure-style function names with special suffixes [0.17ms]
✓ tokenize > supports symbols with multiple special characters [0.22ms]
✓ parseSource > parses function definitions with Clojure-style names [1.38ms]
```

## Examples

Valid Clojure-style function names are now supported:

```vibe
(def is-valid? (fn [x] true))       ; predicate (returns boolean)
(def set-value! (fn [v] v))         ; mutating operation
(def splat* (fn [args] args))       ; special variant
(def foo-bar? (fn [] false))        ; compound with suffix
(def baz! (fn [x] x))               ; single suffix
(def qux+ (fn [a b] (+ a b)))      ; arithmetic variant
(def get-or-else= (fn [x y] x))     ; equality check
(def foo< (fn [a b] a))             ; comparison variant
(def foo> (fn [a b] b))             ; reverse comparison
(def path/resolver (fn [] nil))     ; namespace with slash
```

Generated JavaScript now reuses the runtime fragments for punctuation:

- `is-valid?` → `is_valid_QMARK`
- `set-value!` → `set_value_BANG`
- `splat*` → `splat_STAR`

To build or inspect symbols that carry these names at runtime, import the helpers from `@vibe/runtime`:

```
(external runtime "@vibe/runtime")
(runtime/symbol? (runtime/symbol "set-value!"))  ;=> true
```

## Auto Gensym Placeholders (`foo#`)

- Inside syntax-quoted forms (`` `(…) ``), any simple symbol that ends with `#` becomes an **auto gensym placeholder**. Each placeholder is replaced with a deterministic, hygiene-safe symbol such as `foo__0` during macro expansion or runtime syntax-quote evaluation.
- Placeholders may only appear inside syntax-quoted templates. Using `foo#` in regular code now triggers the `SEM_GENSYM_PLACEHOLDER_CONTEXT` diagnostic to mirror Clojure's reader semantics.
- The placeholder name cannot be namespace-qualified (`alias/foo#`)—`SEM_GENSYM_PLACEHOLDER_NAMESPACE` guides authors to pick simple hints and rely on hygiene tags instead.
- Analyzer and interpreter share the same gensym counter, so `(gensym "tmp")` and `tmp#` produce consistent aliases across compile-time and runtime quoting.
- Nested syntax quotes each receive their own placeholder scope, matching Clojure's behavior:

```
(def capture
	(macro [expr]
		`(let [foo# ~expr]
			 foo#)))

; Expands to something like (let [foo__0 expr] foo__0)
```

Macro authors can freely mix explicit `(gensym)` calls with placeholder sugar; both feed the same hygiene metadata and downstream aliasing rules (`foo#` → `foo__symbol_7`, sanitized to `foo__symbol_7` / `foo__symbol_7__symbol_12` in codegen as needed).

## Impact

✅ **Backward compatible** — No breaking changes; existing code continues to work  
✅ **End-to-end tested** — Lexer, parser, semantic analyzer, and codegen all verify  
✅ **Well-documented** — Syntax spec and changelogs updated  
✅ **Clojure-aligned** — Follows Lisp naming conventions users expect

## Future Considerations

1. IDE tooling may benefit from name-to-safe-identifier mappings during hover/completion
2. Documentation should include style guide recommendations (e.g., when to use `?` vs `!`)
3. Runtime error messages could reference canonical names alongside sanitized JavaScript identifiers
