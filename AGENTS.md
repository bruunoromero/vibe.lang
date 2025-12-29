# Vibe Compiler Project Context

## Project Overview
**Vibe** is a programming language project consisting of a compiler, interpreter, and associated tooling. It is built as a TypeScript monorepo using **Bun** as the runtime and package manager. The language compiles to JavaScript.

## Architecture
The project follows a standard compiler pipeline architecture distributed across multiple packages:
1.  **Lexer** (`@vibe/lexer`): Tokenizes the source code.
2.  **Parser** (`@vibe/parser`): Parses tokens into an Abstract Syntax Tree (AST).
3.  **Semantics** (`@vibe/semantics`): Performs semantic analysis, scope resolution, and macro expansion.
4.  **Codegen** (`@vibe/codegen`): Generates JavaScript code from the analyzed IR.

## Directory Structure
The project is organized as a workspace with packages in the `packages/` directory:

-   **`packages/cli`**: The main command-line interface for the compiler.
-   **`packages/lexer`**: Tokenizer logic.
-   **`packages/parser`**: Parsing logic and AST definitions.
-   **`packages/semantics`**: Semantic analysis, symbol tables, and macro expansion.
-   **`packages/codegen`**: JavaScript code generator.
-   **`packages/interpreter`**: AST evaluator (used for macros and simple execution).
-   **`packages/runtime`**: Runtime support library for compiled Vibe code.
-   **`packages/prelude`**: The standard library for Vibe.
-   **`packages/syntax`**: Shared AST node definitions and utilities.
-   **`packages/module-resolver`**: Logic for resolving imports and package graphs.
-   **`packages/example-app`**: A sample project demonstrating Vibe usage.
-   **`docs/`**: Architecture Decision Records (ADRs) and feature specifications.

## Development Workflow

### Prerequisites
-   **Bun**: This project uses `bun` (v1.3.5+) for package management, script execution, and testing.

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
Individual packages can be built (if applicable) or the example app can be built using the Vibe CLI:
```bash
# In packages/example-app
bun run build
```

## Configuration
-   **`package.json`**: Defines workspaces and common scripts.
-   **`tsconfig.base.json`**: Base TypeScript configuration inherited by packages.
-   **`vibe` config**: Packages can define a `vibe` key in their `package.json` to configure source directories, output directories, and entry points (e.g., see `packages/example-app/package.json`).

## Documentation
-   Refer to `docs/compiler-plan.md` for the current development status and roadmap.
-   See `docs/ADR-*.md` for architectural decisions regarding parsing, semantics, and codegen.
