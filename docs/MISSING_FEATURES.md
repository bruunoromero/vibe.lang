# Vibe Language — Missing Features & Improvement Opportunities

Comprehensive audit of the Vibe language as of March 2026. Features, gaps, test opportunities, and ideas for future work.

---

## Tier 1: High-Impact Language Features

### `@as` Decorator — Constructor-to-String Compilation

Allow ADT constructors without arguments to compile to JS strings instead of tagged objects.

```vibe
@as "string"
type Color = Red | Green | Blue
-- Red compiles to "Red" instead of { $tag: 0 }
```

Dramatically improves JS interop (JSON APIs, DOM attributes, CSS values). Scope: parser (new decorator), semantics (validate only zero-arg constructors), IR (metadata on IRConstructor), codegen (emit string + string comparison in pattern matching).

### String Interpolation

```vibe
greeting = "Hello, ${name}!"
```

Currently the only way is `"Hello, " ++ name ++ "!"`. Touches lexer (new token for interpolated strings), parser (parse expression holes), semantics (holes must be `String`), codegen (emit JS template literals).

### Guards in Pattern Matching

Boolean conditions on case branches:

```vibe
case x of
  n | n > 0 -> "positive"
  n | n < 0 -> "negative"
  _ -> "zero"
```

Currently requires nesting `if` inside case branches. Scope: parser, AST (guard field on CaseBranch), exhaustiveness checker (guards make branches partial), IR lowering (desugar to nested ifs).

### Where Clauses on Value Definitions

Local bindings after the body:

```vibe
circleArea r = pi * r * r
  where
    pi = 3.14159
```

More natural reading order. Elm 0.18 had this. Can desugar to `let...in` during parsing/IR.

### Compiler Hooks / Metaprogramming

Scripts that generate `.vibe` files before compilation:

```json
// vibe.json
{ "hooks": { "pre-compile": ["./codegen/generate-api-types.ts"] } }
```

Hooks are Bun scripts that write files to a `generated/` directory, module resolver includes them.

### Debug / Trace Facility

```vibe
import Vibe.Debug exposing (log)
result = log "x is" x  -- prints "x is: 42", returns x
```

No way to inspect values today. `Vibe.Debug` module with FFI to `console.log`. Compiler could warn on Debug usage in production builds.

---

## Tier 2: Interop & Ecosystem

### More `@as` / FFI Decorators

- `@new "ClassName"` — call JS constructors (`new Date(...)`)
- `@as "number"` — constructors as plain numbers (enum interop)
- `@as "json"` — auto JSON encode/decode for a type

### Source Maps

Zero source map support today. Span info exists on every AST/IR node — codegen just needs to emit `.js.map` files.

### JSON Encoding/Decoding

Without this, every type that touches an API needs manual FFI. Either Elm-style `Json.Encode`/`Json.Decode` or auto-derivable.

### Improved Error Messages

Add source code snippets, "did you mean?" suggestions, and colored formatting (Elm-style).

---

## Tier 3: Type System Enhancements

### Extensible Records / Row Polymorphism

```vibe
getName : { r | name : String } -> String
getName record = record.name
```

Functions that work on any record with certain fields. Major type system addition.

### Phantom Types

```vibe
type Id a = MkId String  -- 'a' is phantom, for type-level safety
```

May already work — needs testing and documentation.

### Type Holes

```vibe
foo : _ -> Int  -- compiler tells you what _ should be
```

Turns the compiler into a development partner for type-driven development.

### Type Alias Expansion in Error Messages

Show `Expected Point3D` not `Expected { x: Int, y: Int, z: Int }`.

---

## Tier 4: Tooling & DX

### Formatter

Currently a stub (`console.log("I'm formatter")`). Needs full implementation for `vibe fmt`.

### REPL

No interactive mode. `vibe repl` for quick experimentation.

### Package Manager

No `vibe install/publish`. Could use npm/GitHub packages initially.

### Dead Code / Unused Import Warnings

No usage tracking today. Warn on unused bindings, unused imports, unreachable code.

### Built-in Test Framework

No way to test Vibe code without dropping to JS. An idiomatic test module pattern would help.

### Watch Mode Improvements

Currently re-compiles everything. Incremental compilation would speed up the feedback loop.

---

## Tier 5: Known Bugs & Test Gaps

### Known Bug: Nested Exhaustiveness Checking

- `(True, True)` not detected as non-exhaustive
- `Just (Just True)` not detected
- See `packages/semantics/tests/exhaustiveness.bug.test.ts`

### Test Coverage Gaps

| Gap                             | Notes                                                    |
| ------------------------------- | -------------------------------------------------------- |
| End-to-end integration          | No test compiles multi-module project and runs output JS |
| CLI `build` command             | No test with real file I/O                               |
| Record auto-derive Show         | Documented TODO in auto_derive_show.test.ts              |
| Circular module detection       | No tests for import cycles                               |
| Cross-module instance chains    | Instance in A → used in B → consumed in C                |
| Error message quality           | Tests check errors exist but not message quality         |
| Unicode identifiers             | Untested                                                 |
| FFI arity edge cases            | Zero-arity externals, variadic JS functions              |
| Complex qualified imports       | Multi-level qualified access                             |
| Large module performance        | No stress tests                                          |
| Pattern match with type aliases | Aliases in pattern positions                             |

---

## Tier 6: Language Niceties (Lower Priority)

### Numeric Literal Enhancements

- Hex: `0xFF`
- Binary: `0b1010`
- Underscores: `1_000_000`
- Scientific: `1.5e10`

### Multi-line Strings

```vibe
description = """
  This is a multi-line
  string with automatic
  indentation stripping.
  """
```

### Do-notation / Computation Expressions

```vibe
do
  content <- readFile "data.txt"
  parsed <- parseJson content
  pure parsed.name
```

Requires HKT/Monad support first.

### Redundant Pattern Warnings

Warn when a case branch can never match.
