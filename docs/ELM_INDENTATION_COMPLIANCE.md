# Elm Indentation Compliance Checklist

This document tracks what aspects of the Vibe parser follow Elm-style indentation rules and what still needs work.

## Summary

Elm uses **whitespace-sensitive** parsing where indentation determines code structure. Key principles:

1. **Top-level declarations** must start at column 1
2. **Continuation lines** must be indented further than the starting line
3. **Block constructs** (`let`, `case`, `where`) use equal indentation for siblings
4. **Expressions** spanning lines must have continuations indented past the base

---

## Currently Implemented ✅

### 1. Layout Tokens for Keywords

**Status: ✅ Implemented in `layout.ts`**

The layout preprocessor inserts virtual `BlockStart`, `BlockSep`, and `BlockEnd` tokens for:

- `let` ... `in` blocks
- `case` ... `of` branches
- `where` blocks (protocols, implement)

```
┌──────────┐     ┌──────────────┐     ┌────────┐
│  Lexer   │────>│ Layout Pass  │────>│ Parser │
│ (tokens  │     │ (inserts     │     │        │
│  + NL)   │     │  Block*)     │     │        │
└──────────┘     └──────────────┘     └────────┘
```

### 2. Top-Level Column 1 Enforcement

**Status: ✅ Implemented in `parseProgram()`**

All top-level declarations are checked via `enforceColumn1()`:

- Module declarations
- Import declarations
- Value/function declarations
- Type declarations
- Protocol/implement declarations

### 3. Decorated Declaration Layout

**Status: ✅ Just implemented**

Decorator arguments can span indented lines, but the declaration name must start at column 1:

```vibe
-- Valid
@external "./module.js" "export"
myValue : Int

-- Valid (multi-line args)
@external "./module.js"
    "export"
myValue : Int

-- Invalid (name not at column 1)
@external "./module.js" "export"  myValue : Int
```

### 4. Function Application Continuation

**Status: ✅ Implemented via `continuesLayout()`**

Arguments on continuation lines must be indented >= base column:

```vibe
result =
    someFunction
        arg1
        arg2
```

### 5. Let/In Bindings

**Status: ✅ Implemented via layout tokens**

All bindings at equal indentation, `in` aligns or closes block:

```vibe
foo =
    let
        a = 1
        b = 2
    in
    a + b
```

### 6. Case Branches

**Status: ✅ Implemented via layout tokens**

All branches at equal indentation:

```vibe
describe x =
    case x of
        0 -> "zero"
        _ -> "other"
```

---

## Needs Attention ⚠️

### 1. Module Exposing Clause Multi-Line

**Status: ⚠️ Works but could be stricter**

Current behavior: Allows exposing list to span multiple lines inside parentheses
Elm behavior: Continuation items should be indented past `exposing`

```vibe
-- Current (allowed)
module Foo exposing
    ( bar
    , baz
    )

-- Should enforce indentation past 'exposing' keyword column
```

**Recommendation**: The exposing list content is inside parentheses, so this is acceptable. Parenthesized content follows bracket rules, not layout rules. **Low priority**.

### 2. Type Declaration Constructors

**Status: ⚠️ Works but not strictly enforced**

ADT constructors can span multiple lines:

```vibe
type Result e a
    = Ok a
    | Err e
```

**Current**: Works because `|` pattern continues expression
**Elm**: Requires `|` at equal indentation

**Recommendation**: Add layout context for `type` ... `=` to enforce equal indentation for `|` variants. **Medium priority**.

### 3. Protocol Method Declarations

**Status: ✅ Works via `where` layout**

Methods must be at equal indentation:

```vibe
protocol Show a where
    show : a -> String
    showList : List a -> String  -- must align with show
```

### 4. Record Literal Multi-Line

**Status: ⚠️ Works but not enforced**

Records use braces, so they follow bracket rules:

```vibe
person =
    { name = "Alice"
    , age = 30
    }
```

**Current**: Works because it's inside braces
**Recommendation**: Bracket-enclosed content doesn't need strict layout. **Low priority**.

### 5. List Literal Multi-Line

**Status: ⚠️ Same as records**

```vibe
numbers =
    [ 1
    , 2
    , 3
    ]
```

**Recommendation**: Bracket-enclosed. **Low priority**.

### 6. Binary Operators on Continuation Lines

**Status: ⚠️ Works but could validate**

```vibe
result =
    longValue
        + anotherValue
        + thirdValue
```

**Current**: Allowed via `continuesLayout()` which checks column >= base
**Elm**: Operator must be indented

**Recommendation**: This already works correctly. **No action needed**.

### 7. Lambda Body Indentation

**Status: ⚠️ Could be stricter**

```vibe
myLambda = \x ->
    x + 1
```

**Current**: Body must continue layout
**Elm**: Body must be indented past `\`

**Recommendation**: Add check that lambda body column > lambda start column when on new line. **Medium priority**.

### 8. If/Then/Else Alignment

**Status: ⚠️ Could be stricter**

```vibe
value =
    if condition then
        trueCase
    else
        falseCase
```

**Current**: Works via continuation rules
**Elm**: `then`, `else` should align or be indented

**Recommendation**: Elm allows flexible if/then/else layout. Current behavior is acceptable. **Low priority**.

### 9. Tuple/Parenthesized Expression Multi-Line

**Status: ✅ Works via brackets**

Parentheses create bracket context that suspends layout rules:

```vibe
pair =
    ( longExpr1
    , longExpr2
    )
```

---

## Implementation Priority

### High Priority (Breaking/Misleading if not done)

1. ~~**Decorated declarations**~~ ✅ Done - name must be at column 1

### Medium Priority (Elm compliance)

1. **Type declaration constructors** - enforce `|` alignment
2. **Lambda body indentation** - body must be indented past `\`

### Low Priority (Already works acceptably)

1. Module exposing multi-line - bracket rules apply
2. Record literals - bracket rules apply
3. List literals - bracket rules apply
4. If/then/else - flexible in Elm too

---

## Technical Notes

### How Layout Works

1. **Lexer** emits `Newline` tokens at line breaks
2. **Layout pass** (`layout.ts`) transforms `Newline` into:
   - `BlockStart` when entering layout keyword block
   - `BlockSep` when at same column as block
   - `BlockEnd` when dedenting below block column
3. **Parser** uses layout tokens + `continuesLayout()` for other constructs

### Key Functions

- `insertLayoutTokens()` - layout preprocessing
- `continuesLayout(baseIndent, lastEnd, next)` - checks if token continues current expression
- `enforceColumn1(token, label)` - validates top-level position
- `expectBlockStart/Sep/End()` - consume layout tokens

### Adding New Layout Rules

To add layout enforcement for a new construct:

1. **If keyword-triggered block**: Add keyword to `isLayoutKeyword()` in `layout.ts`
2. **If column-based continuation**: Use `continuesLayout()` in the parse function
3. **If top-level requirement**: Call `enforceColumn1()` before parsing

---

## References

- Elm Guide: https://elm-lang.org/docs/syntax
- Haskell Layout Rules: https://www.haskell.org/onlinereport/lexemes.html#sect2.7
- Vibe Grammar: `docs/grammar.ebnf`
