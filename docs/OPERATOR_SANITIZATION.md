# Operator Identifier Sanitization

**Date:** December 23, 2025  
**Status:** ✅ Updated

## Summary

- `AliasAllocator` now serializes special characters using the same runtime-friendly fragments that `@vibe/runtime` exports (`eq*` → `eq_STAR`, `seq?` → `seq_QMARK`).
- The old `operatorNames` table and manual alias reservations were removed in favor of deterministic, character-level serialization.
- Analyzer and doc tests cover the new naming scheme to ensure user-defined operators line up with runtime helpers and namespace imports.

## Serialization Rules

Each symbol character is processed left-to-right:

- ASCII letters, digits, and underscores remain unchanged.
- Hyphens turn into underscores inside compound names (e.g., `foo-bar` → `foo_bar`). A standalone `-` becomes `_DASH` so arithmetic forms stay distinguishable.
- Characters in the table below expand to their runtime fragment:

| Character | Fragment | Character | Fragment |
| --------- | -------- | --------- | -------- |
| `*`       | `_STAR`  | `?`       | `_QMARK` |
| `!`       | `_BANG`  | `+`       | `_PLUS`  |
| `=`       | `_EQ`    | `<`       | `_LT`    |
| `>`       | `_GT`    | `/`       | `_SLASH` |

Any other punctuation falls back to `_`. Identifiers that would start with a digit gain a leading underscore, and JavaScript keywords still receive a prefixed underscore via the existing `RESERVED_IDENTIFIERS` guard.

## Examples

- `(def eq* (fn [a b] (= a b)))` → exports `eq_STAR`
- `(def is-valid? (fn [x] true))` → exports `is_valid_QMARK`
- `(def + (fn [a b] a))` → exports `_PLUS`

These names now match namespace imports from runtime modules, so `(require rt "@vibe/runtime")` followed by `rt/eq*` continues to resolve to `rt.eq_STAR` without additional mapping code.

## Tests & Documentation

- `packages/semantics/tests/analyze.test.ts` asserts the new `_STAR`, `_QMARK`, `_LT`, etc. aliases for operators and compound names.
- `docs/CLOJURE_NAMING_FEATURE.md` and `packages/semantics/CHANGELOG.md` describe the runtime-style serialization so downstream tools know what to expect.

## Impact

- ✅ **Consistency** — Analyzer, interpreter, and runtime now agree on how special suffixes are spelled.
- ✅ **Determinism** — Removing the mutable `used` set means aliases are derived solely from the symbol text plus `symbolId` when necessary.
- ✅ **Interop** — Namespace imports referencing runtime helpers (e.g., `runtime/eq*`) continue to work without ad-hoc string replacements.
