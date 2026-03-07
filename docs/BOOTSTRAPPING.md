# Bootstrapping Roadmap

This document tracks Vibe's path toward self-hosting — replacing the TypeScript compiler implementation with one written in Vibe itself.

## Strategy

Bootstrapping follows an incremental, bottom-up approach. Each compiler phase is ported from TypeScript to Vibe independently, verified against the existing test suite, then swapped in. The key principle: **never break the existing compiler while bootstrapping**.

The approach:

1. Write the Vibe version of a compiler phase
2. Test it against the same inputs as the TypeScript version
3. Verify output equivalence
4. Replace the TypeScript phase with a JavaScript-calling wrapper around the compiled Vibe module
5. Run the full test suite to confirm

## Current Status

### Phase 1: Lexer — IN PROGRESS

**Location**: `packages/vibe-lexer`

The lexer is the ideal first bootstrapping target:

- Simplest phase (character-by-character scanning, no recursion into other phases)
- No regex dependency (pure character classification)
- Small surface area (~430 lines of TypeScript → ~650 lines of Vibe)

**What's done**:

- Prelude extended with String/Char operations needed for lexing (`charAt`, `slice`, `length`, `unsafeCharAt`, `Char.isAlpha/isDigit/isWhitespace`, `Char.toCode/fromCode`, `Ord` instances)
- `VibeLexer.Types` module with `TokenKind` ADT (24 variants), `Position`, `Span`, `Token`, `LexerState`, `LexError` record types
- `VibeLexer` module with full lexer logic using immutable state threading
- Package compiles successfully to JavaScript

**Compiler bugs resolved**:

1. ✅ **Record literal codegen**: `generateRecord` and `generateRecordUpdate` in `packages/codegen/src/index.ts` now emit `({ ... })` so arrow function bodies are always valid JS expressions.
2. ✅ **Pattern literal codegen**: `generateLiteralPatternValue` now strips surrounding source quotes from string/char values before `JSON.stringify`, matching the logic in `generateLiteral`.
3. ✅ **Spurious Eq/Show requirements**: `canTypeImplementProtocol` in `packages/semantics/src/index.ts` now correctly returns `false` for concrete imported types with no existing instance. `validateTypeImplementsEq` was also fixed to check concrete uppercase type names instead of silently skipping them.

**Prelude extensions added for bootstrapping** (in `packages/prelude/src/Vibe/`):

- `Char.vibe`: `Ord` instance, `toCode`, `fromCode`, `isAlpha`, `isDigit`, `isAlphaNum`, `isUpper`, `isLower`, `isWhitespace`
- `Char.ffi.js`: `charToCode`, `charFromCode`, `charOrd`, `charOrdGt`
- `String.vibe`: `length`, `charAt`, `unsafeCharAt`, `slice`, `isEmpty`, `startsWith`, `endsWith`, `contains`, `toList`, `fromList`, `trim`, `split`, `lines`, `Ord` instance
- `String.ffi.js`: All corresponding FFI implementations

### Phase 2: Parser — NOT STARTED

**Estimated complexity**: High  
**Key blockers**:

- Needs `Map`/`Dict` type for keyword lookups, operator precedence tables
- Layout engine requires tracking indentation state (column tracking already works via `LexerState`)
- Parser combinator or recursive descent — both are feasible in Vibe
- AST type definitions are large (many variants, deeply nested)

**Prerequisites**:

- Fix the codegen bugs from Phase 1
- Add `Map`/`Dict` to prelude (or as a separate package)
- Potentially add `Set` type

### Phase 3: Semantics — NOT STARTED

**Estimated complexity**: Very High  
**Key blockers**:

- Large and complex phase (~6000+ lines of TypeScript)
- Needs mutable state or state monad for type inference (unification variables)
- Requires `Map`, `Set`, and efficient graph algorithms (Tarjan's SCC)
- Protocol resolution and dictionary passing are intricate

**Prerequisites**:

- Working parser (Phase 2)
- `Map`, `Set` data structures
- Either mutable references (`Ref`) or a state-passing pattern for unification

### Phase 4: IR Lowering — NOT STARTED

**Estimated complexity**: Medium  
**Key blockers**:

- Transforms analyzed AST to intermediate representation
- Needs access to semantic information (types, constraints, instances)
- Dependency resolution for module ordering

### Phase 5: Codegen — NOT STARTED

**Estimated complexity**: Medium  
**Key blockers**:

- String building/concatenation (already available via `++`)
- Path calculation for JS imports
- Name sanitization (operator → identifier mapping)

### Phase 6: Self-Hosting — NOT STARTED

Once all phases are ported, the compiler can compile itself. This is the final milestone.

## Language Limitations Discovered During Bootstrapping

These limitations were found while writing the lexer and should be addressed to make bootstrapping easier:

### Must Fix (Blocks Bootstrapping Progress)

[X] **Record literal codegen** — Arrow functions returning records produce invalid JS
[X] **Pattern literal codegen** — String/char literals in `case` patterns include source quotes in emitted JS
[X] **Spurious Eq/Show requirements** — Cross-module record types incorrectly required manual protocol implementations; auto-derive now works correctly
[ ] **No tuple destructuring in `let`** — Forces awkward `case` nesting or helper functions for every tuple return value

### Nice to Have

[ ] **No literal arguments in constructor patterns** — `Just '\n'` doesn't work; requires unwrapping then matching
[ ] **`isPatternStart` missing literal tokens** — Parser's `isPatternStart` doesn't include `Number`, `String`, `Char` tokens, preventing literal arguments in constructor patterns
[ ] **Error messages without locations** — Compiler errors often lack file/line information, making debugging harder

## Architecture Principles

- **Immutable state threading**: The Vibe lexer uses explicit state passing (`LexerState -> (result, LexerState)`) rather than mutable state. This is idiomatic for a pure functional language.
- **Records use `type`, not `type alias`**: Record types with named fields are declared using `type Name = { ... }`, not `type alias`. This is by design — `type alias` is for structural aliases only; `type` creates a nominal record type with auto-derived `Eq`/`Show` support.
- **`case` over `if`**: Due to layout strictness, use `case condition of True -> ... False -> ...` instead of `if/then/else` for multi-line conditionals.
- **Helper functions over deep nesting**: Extract small functions (like `skip`, `skip2`, `identKind`) to keep `case` nesting manageable.
- **FFI for primitives**: Character/string operations that need JS runtime access use FFI. Pure logic stays in Vibe.
