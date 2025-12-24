# @vibe/cli

Command-line interface for the Lang compiler toolchain.

## REPL (lang repl) ✅

Start an interactive REPL that parses, analyzes, compiles, and executes Lang forms:

- Start REPL with Bun:

  bun packages/cli/index.ts repl

- Options:

  - `--pretty, -p <n>` — Pretty-print JSON results (default: 2)
  - `--debug-macros` — Dump macro-expansion metadata to stderr

- Usage:

  - Primary prompt: `lang> `
  - Continuation prompt: `... ` (used for multi-line forms)
  - Exit the REPL with `exit`, `.exit`, or Ctrl-D

- Example session:

  lang> (+ 1 2)
  {"result":3}

  lang> (def a 10)
  {"result":10}

  lang> a
  {"result":10}

- Notes:
  - The REPL preserves definitions for the session (each input is evaluated in the cumulative session context).
  - Diagnostics (parse/semantic/codegen) are printed to stderr in human-readable form.

## Workspace Builds (`vibe build`) 🚀

`vibe build [package|path]` walks `package.json#dependencies` starting from the selected package, topologically sorts every package that declares Lang sources, and compiles each `.lang` file into the package's configured output directory.

- Declare Lang inputs inside each package manifest using the `vibe` block:

  ```jsonc
  {
    "name": "@vibe/example-app",
    "dependencies": {
      "@vibe/prelude": "workspace:*"
    },
    "vibe": {
      "sources": ["./src"],
      "outDir": "./dist",
      "modules": {
        ".": "./src/main.lang",
        "./math": "./src/math.lang"
      }
    }
  }
  ```

- Run the build:

  ```bash
  bun packages/cli/index.ts build packages/example-app
  # or by package name when it's published/linked
  bun packages/cli/index.ts build @vibe/example-app
  ```

- The build:
  - Compiles dependency packages (e.g., `@vibe/prelude`) before dependents.
  - Mirrors the `sources` directory structure under `outDir`, swapping `.lang` → `.js`.
  - Leaves package-level `require` imports as bare specifiers so Node can honor each package's `exports` map while forcing relative imports to reference their `.js` siblings.
