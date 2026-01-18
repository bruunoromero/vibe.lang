# Vibe Compiler Project Context

## Project Overview

**Vibe** is a programming language project consisting of a compiler, interpreter, and associated tooling. It is built as a TypeScript monorepo using **Bun** as the runtime and package manager. The language compiles to JavaScript and It's heavily Inspired by Elm 0.18.

## Architecture

The project follows a standard compiler pipeline architecture distributed across multiple packages:

1.  **Lexer** (`@vibe/lexer`): Tokenizes the source code.
2.  **Parser** (`@vibe/parser`): Parses tokens into an Abstract Syntax Tree (AST).
3.  **Semantics** (`@vibe/semantics`): Performs semantic analysis, scope resolution, and et.c.
4.  **Codegen** (`@vibe/codegen`): Generates JavaScript code from the analyzed IR.

## Directory Structure

The project is organized as a workspace with packages in the `packages/` directory:

- **`packages/cli`**: The main command-line interface for the compiler.
- **`packages/lexer`**: Tokenizer logic.
- **`packages/parser`**: Parsing logic and AST definitions.
- **`packages/semantics`**: Semantic analysis, symbol tables, and etc.
- **`packages/codegen`**: JavaScript code generator.
- **`packages/runtime`**: Runtime support library for compiled Vibe code.
- **`packages/prelude`**: The standard library for Vibe.
- **`packages/syntax`**: Shared AST node definitions and utilities.
- **`packages/module-resolver`**: Logic for resolving imports and package graphs.
- **`packages/language-server`**: Language Server Protocol (LSP) implementation for IDE support.
- **`packages/vscode-extension`**: VS Code extension providing Vibe language support.
- **`packages/example-app`**: A sample project demonstrating Vibe usage.
- **`docs/`**: Architecture Decision Records (ADRs) and feature specifications.

## Development Workflow

### Prerequisites

- **Bun**: This project uses `bun` (v1.3.5+) for package management, script execution, and testing.

### Installation

Install dependencies for all workspaces:

```bash
bun install
```

### Running Tests

Run tests across all packages:

```bash
bun test
```

### Building

After making changes, rebuild all packages using one of the following commands from the root:

```bash
# Build all packages with Turbo (verifies dependency graph)
bunx turbo run build

# Alternatively: use the root build script
bun run build
```

Always run a rebuild after completing code changes to validate the workspace dependency graph and ensure all packages compile successfully.

### Running the Example App

The example app (`packages/example-app`) demonstrates Vibe language features:

```bash
cd packages/example-app

# Build the app
bun run build

# Run the compiled JavaScript
bun run start
```

The example app showcases:

- Multi-parameter protocols with constrained instances
- Type annotations with return type inference
- Dictionary-passing for protocol constraints

## Configuration

- **`package.json`**: Defines workspaces and common scripts.
- **`tsconfig.base.json`**: Base TypeScript configuration inherited by packages.
- **`vibe` config**: Packages can define a `vibe` key in their `package.json` to configure source directories, output directories, and entry points (e.g., see `packages/example-app/package.json`).

## Standard Library (Prelude)

The Vibe standard library is defined in `packages/prelude/src/Vibe.vibe`. Unlike some languages, **the prelude is NOT automatically imported**. Users must explicitly import it:

```vibe
import Vibe exposing (..)
```

Or import specific items:

```vibe
import Vibe exposing (Maybe(..), Result(..), (|>), (>>))
```

The prelude provides:

- Core types: `Maybe`, `Result`, `Pair`
- Protocols: `Num`, `Fractional`, `Integral`, `Eq`, `Ord`, `Show`
- Operators: arithmetic (`+`, `-`, `*`, `/`, `//`, `%`), comparison (`==`, `/=`, `<`, `>`, `<=`, `>=`), logical (`&&`, `||`), list (`::`, `++`), function composition (`>>`, `<<`, `|>`, `<|`)
- Utility functions: `identity`, `always`, `flip`, `apply`, `not`

Built-in types (`Bool`, `Int`, `Float`, `String`, `Char`, `Unit`, `List`) are always available without imports.

## Documentation

- Refer to `docs/compiler-plan.md` for the current development status and roadmap.
- See `docs/ADR-*.md` for architectural decisions regarding parsing, semantics, and codegen.

## TypeScript Error Checking

**CRITICAL:** Always fix all TypeScript compilation errors before considering a task complete. This is a hard requirement:

1. **After any code changes**, run the typecheck command:

   ```bash
   bun run typecheck
   ```

   This uses Turbo to run `tsc --noEmit` across all TypeScript packages in parallel. You can also use the `get_errors` tool to check for TypeScript errors.

2. **The task is NOT finished** if there are any remaining TypeScript errors, even if:
   - Tests pass
   - The code appears to work
   - Only minor errors remain
   - You believe you've addressed most issues

3. **Use `get_errors` tool frequently** to verify all compilation errors are resolved:

   ```
   get_errors(["/path/to/file.ts"])
   ```

4. **Common error patterns** in this project:
   - Duplicate interface definitions with conflicting property types
   - Variable scope issues (variables used outside their declaration scope)
   - Type mismatches between function parameters and arguments
   - Missing interface definitions

5. **Zero tolerance policy**: A task with TypeScript errors is fundamentally incomplete and may cause issues for subsequent work.

## Regression Testing

**CRITICAL:** Always write tests to prevent regressions after fixing bugs or implementing features:

1. **When to write tests**:
   - After fixing any bug, write tests that would have caught the bug
   - After implementing a new feature, add tests covering the feature
   - When making changes to type checking, constraint resolution, or code generation logic

2. **Test scope and coverage**:
   - Tests should verify both positive cases (correct behavior works) and negative cases (invalid code fails with proper errors)
   - Test error messages to ensure they are clear and accurate
   - Test edge cases and boundary conditions relevant to the fix
   - Include examples that directly relate to the bug being fixed

3. **Test organization**:
   - Group related tests in a describe block
   - Use clear, descriptive test names that explain what is being tested
   - Add comments in tests explaining why they exist (especially referencing the bug they prevent)
   - Place tests near the code they test (e.g., protocol tests in `packages/semantics/tests/protocol.test.ts`)

4. **Verification process**:
   - Run the specific test file to verify new tests pass: `bun test packages/<package>/tests/<file>.test.ts`
   - Run the full test suite to ensure no regressions: `bun test`
   - Verify all tests pass before considering the task complete

5. **Example**: When fixing multi-parameter protocol constraint validation:
   - Test that constraints apply to correct type parameters (not just the first one)
   - Test that invalid implementations with constraint violations are rejected
   - Test that valid implementations with polymorphic types are accepted
   - Test edge cases like nested type parameters

## Grammar File Updates

**CRITICAL:** Whenever you modify the language's parsing or syntax, you **MUST** update the grammar file at [docs/grammar.ebnf](docs/grammar.ebnf):

1. **Every syntax change requires grammar updates**:
   - Adding new expression types
   - Modifying operator precedence
   - Introducing new keywords
   - Changing import/export syntax
   - Adding new declaration types
   - Modifying pattern syntax
   - Updating type expression rules

2. **Grammar file is the source of truth** for the language syntax:
   - It serves as documentation for language users and developers
   - It helps catch ambiguities and inconsistencies in the parser
   - Parser implementation should strictly follow the grammar
   - Keep grammar EBNF notation accurate and clear

3. **Update process**:
   - Make changes to parser/lexer/syntax
   - Update grammar.ebnf to reflect the changes
   - Include clear comments and examples in the grammar
   - Run `bun test` to ensure parser tests pass
   - Review grammar for clarity and correctness

4. **Example**: When adding module export specifications:
   - Grammar shows the full syntax: `ExportSpec = LowerIdentifier | UpperIdentifier | "(", Operator, ")" | ...`
   - Comments explain each variant with examples
   - Parser implementation follows this grammar exactly

## Identifier Sanitizers

- When adding or renaming operator-like identifiers (e.g., `.` → `_DOT`), update the shared map in `packages/syntax/index.ts` (`IDENTIFIER_OPERATOR_MAPPINGS`).
- Both the semantics alias allocator and the code generator consume those exports, so no per-package edits are needed once the shared constants change.
- Rebuild any affected packages (typically `bun run packages/prelude` or `bun run packages/cli/index.ts build <target>`) to regenerate emitted JavaScript and confirm the new alias shows up.

## Pattern Matching

The Vibe language supports comprehensive pattern matching in `case` expressions and function parameters:

- **Variable patterns**: `x` binds a value to a name
- **Wildcard patterns**: `_` matches anything without binding
- **Constructor patterns**: `Just x`, `Nothing`, `Cons h t` match ADT variants
- **Tuple patterns**: `(a, b)` destructures tuples
- **List patterns**: `[]` matches empty list, `[x, y, z]` matches fixed-length lists
- **Cons patterns**: `x :: xs` destructures a list into head and tail

The cons pattern (`::`) is right-associative, allowing `a :: b :: rest` to match the first two elements of a list.

## Mutual Recursion

Mutually recursive functions are detected and handled using Tarjan's SCC algorithm. Functions in the same strongly connected component are type-inferred together before generalization, ensuring proper polymorphic types.

Example:

```
isEven n = if n == 0 then True else isOdd (n - 1)
isOdd n = if n == 0 then False else isEven (n - 1)
```

Both functions will be correctly typed as `Int -> Bool`.

## Dictionary Passing (Type Class Implementation)

The compiler uses dictionary-passing style for protocol (type class) constraints:

1. **Constraint Collection**: When a protocol method (like `+` from `Num`) is used during type inference, the constraint is collected and attached to the function's type scheme.

2. **Dictionary Parameters**: Functions with constraints receive dictionary parameters (e.g., `$dict_Num`) that contain the method implementations.

3. **Method Lookup**: Protocol operators look up their implementations from the dictionary parameter (e.g., `$dict_Num._PLUS`).

4. **Instance Resolution**: At monomorphic call sites (concrete types), the compiler resolves the appropriate instance dictionary (e.g., `$dict_Num_Int`). At polymorphic call sites, dictionaries are passed through.

Example transformation:

```
-- Vibe source
add : Num a => a -> a -> a
add x y = x + y

-- Generated JavaScript (conceptual)
const add = ($dict_Num) => (x) => (y) => $dict_Num._PLUS(x)(y);

-- Call with Int
add($dict_Num_Int)(1)(2)
```

## Qualified Type Constraints

User-annotated qualified type constraints (e.g., `Num a => a -> a -> a`) are now fully supported:

1. **Constraint Extraction**: Constraints from `QualifiedType` annotations are extracted and validated during semantic analysis.

2. **Protocol Validation**: Constraints must reference existing protocols with the correct number of type arguments. Hard errors are thrown for:
   - Unknown protocols in constraints
   - Wrong number of type arguments for a protocol
   - Constraints applied to concrete types instead of type variables

3. **Constraint Merging**: User-annotated constraints are merged with inferred constraints (from actual protocol method usage) during type generalization. The "satisfiable" strategy allows user annotations to include additional constraints not used in the function body.

4. **Deduplication**: When the same constraint is both annotated and inferred, it appears only once in the final type scheme.

Example:

```
-- User annotation declares both Num and Eq constraints
-- Function body only uses Num (via plus)
-- Both constraints are included in the type scheme
compute : (Num a, Eq a) => a -> a -> a
compute x y = plus x y
```

## Known Limitations

Currently no known semantic limitations. See individual package documentation for any package-specific constraints.
