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

### Running the CLI

You can run the CLI directly via `bun`:

```bash
# General usage
bun run packages/cli/index.ts <command> [args]

# Examples
bun run packages/cli/index.ts tokenize "(println :ok)"
bun run packages/cli/index.ts parse src/main.lang
bun run packages/cli/index.ts analyze src/main.lang
bun run packages/cli/index.ts run src/main.lang
bun run packages/cli/index.ts compile src/main.lang
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

## Configuration

- **`package.json`**: Defines workspaces and common scripts.
- **`tsconfig.base.json`**: Base TypeScript configuration inherited by packages.
- **`vibe` config**: Packages can define a `vibe` key in their `package.json` to configure source directories, output directories, and entry points (e.g., see `packages/example-app/package.json`).

## Documentation

- Refer to `docs/compiler-plan.md` for the current development status and roadmap.
- See `docs/ADR-*.md` for architectural decisions regarding parsing, semantics, and codegen.

## TypeScript Error Checking

**CRITICAL:** Always fix all TypeScript compilation errors before considering a task complete. This is a hard requirement:

1. **After any code changes**, run:

   ```bash
   bun run packages/cli/src/index.ts --help  # Or the relevant package
   ```

   and check for TypeScript errors using the `get_errors` tool.

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

## Identifier Sanitizers

- When adding or renaming operator-like identifiers (e.g., `.` → `_DOT`), update the shared map in `packages/syntax/index.ts` (`IDENTIFIER_OPERATOR_MAPPINGS`).
- Both the semantics alias allocator and the code generator consume those exports, so no per-package edits are needed once the shared constants change.
- Rebuild any affected packages (typically `bun run packages/prelude` or `bun run packages/cli/index.ts build <target>`) to regenerate emitted JavaScript and confirm the new alias shows up.
