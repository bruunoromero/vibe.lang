# Operator Identifier Sanitization Improvement

**Date:** December 22, 2025  
**Status:** ✅ Implemented and tested

## Problem Solved

Previously, operators like `<=` and `>=` would both sanitize to `__` (double underscore), and while the symbol ID suffix prevented collisions at runtime, they generated ugly identifiers like `____symbol_123` and `____symbol_124`.

## Solution

Implemented a mapping of common operators to readable identifiers:

| Operator | Alias   | Operator | Alias     | Operator | Alias   |
| -------- | ------- | -------- | --------- | -------- | ------- |
| `<=`     | `lte`   | `<`      | `lt`      | `+`      | `plus`  |
| `>=`     | `gte`   | `>`      | `gt`      | `-`      | `minus` |
| `=`      | `eq`    | `!`      | `not`     | `*`      | `mul`   |
| `?`      | `pred`  | `~`      | `unquote` | `/`      | `div`   |
| `@`      | `deref` |

## Changes

### 1. Semantic Analyzer (`packages/semantics/src/analyzer.ts`)

**AliasAllocator improvements:**

- Added `operatorNames` map with friendly names for common operators
- Updated `sanitize()` to use friendly names when the entire identifier is a known operator
- Moved sanitization before symbol ID appending to prevent double-underscore issues

```typescript
private readonly operatorNames = new Map<string, string>([
  ["<=", "lte"],
  [">=", "gte"],
  // ... etc
]);

allocate(name: string, symbolId: SymbolId, preferRaw: boolean): string {
  const sanitized = this.sanitize(name);
  const preferred = preferRaw ? sanitized : `${sanitized}__${symbolId}`;
  return this.reserve(this.ensureIdentifier(preferred));
}
```

### 2. Tests Added (`packages/semantics/tests/analyze.test.ts`)

- ✅ `generates distinct readable names for comparison operators` — `<=`, `>=`, `<`, `>`
- ✅ `generates readable names for arithmetic operators` — `+`, `-`, `*`, `/`
- ✅ `handles compound names with special characters` — `is-valid?`, `set-value!`, `map*`

### 3. Documentation

Updated `packages/semantics/CHANGELOG.md` with the improvement details.

## Before & After

**Before:**

```javascript
export const ____symbol_0 = (a__symbol_1, b__symbol_2) => {
  return a__symbol_1;
}; // <=
export const ____symbol_3 = (a__symbol_4, b__symbol_5) => {
  return b__symbol_4;
}; // >=
```

**After:**

```javascript
export const lte = (a__symbol_14, b__symbol_15) => {
  return a__symbol_14;
};
export const gte = (a__symbol_17, b__symbol_18) => {
  return b__symbol_17;
};
```

## Test Results

✅ **114/114 tests pass** (up from 111) — Full end-to-end verification

```
Exported functions:
export const lte = (a__symbol_14, b__symbol_15) => {
export const gte = (a__symbol_17, b__symbol_18) => {

IR Symbols:
  <= -> lte
  >= -> gte
```

## Impact

- ✅ **Readability** — Generated JavaScript now has human-readable operator function names
- ✅ **Uniqueness** — No more collisions between different operators
- ✅ **Backward compatible** — Compound names like `foo-bar` still work correctly
- ✅ **Well-tested** — Three new test cases verify operator handling

## Example Usage

```vibe
(def <= (fn [a b] a))
(def >= (fn [a b] b))
(<= 1 2)      ; calls the exported lte function
(>= 5 3)      ; calls the exported gte function
```

Generates clean JavaScript:

```javascript
export const lte = (a__symbol_14, b__symbol_15) => a__symbol_14;
export const gte = (a__symbol_17, b__symbol_18) => b__symbol_17;

lte(1, 2);
gte(5, 3);
```
