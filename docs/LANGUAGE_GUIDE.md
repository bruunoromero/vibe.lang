# Vibe Language Guide

Vibe is a statically-typed, purely functional language inspired by Elm 0.18. It compiles to JavaScript and uses indentation-sensitive (layout) parsing.

## Module System

Every Vibe file must begin with a module declaration. The module name must match the file's path relative to the `src/` directory.

```vibe
module MyApp exposing (..)            -- expose everything
module Data.Utils exposing (parse)    -- expose specific items
module Internal                       -- expose nothing
```

### Imports

```vibe
import Vibe exposing (..)             -- import everything from prelude
import Vibe.String as String          -- qualified import
import Vibe.Maybe exposing (Maybe(..))-- import type with constructors
import Vibe.List exposing ((::))      -- import operator
```

### Import Conventions

- **Prelude** (`packages/prelude`): Standard library types and functions. Global JS interop (not Node-specific) belongs here. The prelude is NOT auto-imported — you must `import Vibe exposing (..)`.
- **Node** (`packages/node`): Node.js-specific interop (file system, process, etc.) goes here.
- When adding FFI, ask: "Does this work in all JS runtimes?" If yes → prelude. If Node-only → node package.

## Types

### Built-in Types

`Bool`, `Int`, `Float`, `String`, `Char`, `Unit`, `List` are always available without imports.

### Type Declarations (Records)

Records are defined with `type`, **not** `type alias`:

```vibe
type Person =
    { name : String
    , age : Int
    }
```

> **Important**: `type alias` cannot define records. Use `type alias` only for non-record type synonyms (e.g., `type alias StringList = List String`).

### Algebraic Data Types

```vibe
type Color = Red | Green | Blue

type Maybe a = Just a | Nothing

type Result err ok = Err err | Ok ok
```

### Type Annotations

```vibe
add : Int -> Int -> Int
add x y = x + y

greet : String -> String
greet name = "Hello, " ++ name
```

## Functions

Functions are defined at the top level or in `let` expressions. All functions are curried.

```vibe
double x = x * 2

add x y = x + y

-- Partial application
increment = add 1
```

### Let Expressions

```vibe
circleArea radius =
  let pi = 3.14159
      rSquared = radius * radius
  in pi * rSquared
```

> **Important**: `let` bindings do not support tuple destructuring. Use `case` for that:
>
> ```vibe
> -- WRONG: let (a, b) = someTuple
> -- RIGHT:
> case someTuple of
>   (a, b) -> a + b
> ```

### Lambda Expressions

```vibe
double = \x -> x * 2
add = \x y -> x + y
```

## Pattern Matching

### Case Expressions

```vibe
describe : Maybe Int -> String
describe m =
  case m of
    Just n -> "Got " ++ toString n
    Nothing -> "Nothing"
```

### Supported Patterns

- **Variable**: `x` — binds value to name
- **Wildcard**: `_` — matches anything, no binding
- **Constructor**: `Just x`, `Nothing`, `Ok value`
- **Tuple**: `(a, b)`, `(x, y, z)`
- **List**: `[]`, `[x]`, `[a, b, c]`
- **Cons**: `x :: xs`, `a :: b :: rest`
- **Literals**: `0`, `"hello"`, `'a'`

### Pattern Limitation: Constructor Arguments

Constructor patterns in `case` branches cannot take literal arguments directly (parser limitation). Instead, unwrap first:

```vibe
-- WRONG: case peek state of Just '\n' -> ...
-- RIGHT:
case peek state of
  Just c ->
    case c of
      '\n' -> handleNewline
      _    -> handleOther
  Nothing -> handleEnd
```

### Boolean Dispatch

Multi-line `if/else` requires the `then` and `else` keywords to be at least as indented as the `if` column when on a separate line:

```vibe
main =
  if someCondition
  then handleTrue
  else handleFalse
```

Alternatively, use `case` on `Bool`:

```vibe
-- Preferred pattern for boolean conditions:
case someCondition of
  True -> handleTrue
  False -> handleFalse
```

## Protocols (Type Classes)

### Defining Protocols

```vibe
protocol Printable a where
  display : a -> String
```

### Implementing Protocols

```vibe
implement Printable Person where
  display p = p.name ++ " (age " ++ toString p.age ++ ")"
```

### Constrained Types

```vibe
showBoth : (Show a, Show b) => a -> b -> String
showBoth x y = toString x ++ " and " ++ toString y
```

### Built-in Protocols

- `Eq` — `==`, `/=`
- `Ord` — `<`, `>`, `<=`, `>=`, `compare`
- `Show` — `toString`
- `Num` — `+`, `-`, `*`, `negate`, `abs`, `signum`, `fromInteger`
- `Fractional` — `/`
- `Integral` — `//`, `%`, `rem`
- `Appendable` — `++`

## Operators

### Built-in (Compiler-Provided)

`&&` and `||` are short-circuit operators provided by the compiler. **Do not** import them from modules.

### Common Operators

| Operator             | Description          | Fixity     |
| -------------------- | -------------------- | ---------- |
| `\|>`                | Forward pipe         | `infixl 0` |
| `<\|`                | Backward pipe        | `infixr 0` |
| `>>`                 | Forward composition  | `infixr 9` |
| `<<`                 | Backward composition | `infixl 9` |
| `::`                 | List cons            | `infixr 5` |
| `++`                 | Append               | `infixr 5` |
| `==`, `/=`           | Equality             | `infix 4`  |
| `<`, `>`, `<=`, `>=` | Comparison           | `infix 4`  |
| `+`, `-`             | Arithmetic           | `infixl 6` |
| `*`                  | Multiply             | `infixl 7` |
| `/`                  | Divide               | `infixl 7` |
| `//`                 | Integer divide       | `infixl 7` |
| `%`                  | Modulo               | `infixl 7` |

### Custom Operators

```vibe
infixl 6 |+|
(|+|) : Int -> Int -> Int
(|+|) a b = a + b + 1
```

## FFI (Foreign Function Interface)

### External Bindings

```vibe
@external "./MyModule.ffi.js" "myFunction"
myFunction : Int -> String
```

The FFI file (`MyModule.ffi.js`) sits alongside the `.vibe` file and exports named functions:

```javascript
// MyModule.ffi.js
export const myFunction = (n) => String(n);
```

### Property Access

```vibe
@get "length"
arrayLength : List a -> Int
```

### Method Calls

```vibe
@call "toUpperCase"
toUpper : String -> String
```

### Global Values

```vibe
@val "console"
console : Console
```

## Record Operations

### Creation

```vibe
origin = { x = 0, y = 0 }
```

### Field Access

```vibe
name = person.name
```

### Record Update

```vibe
moved = { point | x = point.x + 1 }
```

## Indentation Rules

Vibe uses layout-sensitive parsing (like Elm and Haskell):

1. **Top-level declarations** must start at column 1
2. **Function bodies** must be indented past the function name
3. **`let` bindings** must be aligned with each other
4. **`case` branches** must be aligned with each other
5. **`then`/`else`** must be at least as indented as `if` when on a separate line

## Known Language Limitations

These are current limitations of the Vibe compiler that affect what patterns you can write:

1. **No tuple destructuring in `let`** — Use `case` expressions instead
2. **No literal arguments in constructor patterns** — `Just 'a'` doesn't work; match the constructor first, then the literal
3. **`type alias` cannot define records** — Use `type` for records
4. **`&&` and `||` are built-in** — They cannot be imported from modules
