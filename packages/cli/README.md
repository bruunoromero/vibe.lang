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

- Declare Lang inputs via a `vibe.config.ts` (or `.js`) file at each package root:

  ```ts
  import { defineConfig } from "@vibe/config";

  export default defineConfig({
    package: {
      sources: ["./src"],
      entry: "./src/main.lang",
      outDir: "./dist",
    },
  });
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
  - Resolves package imports by looking inside the `sources` tree and falls back to `vibe.entry` when the specifier is bare (e.g., `(require "@vibe/prelude")`).
  - Pass `--force` to rebuild every package even when the outputs are newer than their `.lang` sources.

## Formatter (`vibe fmt`) ✨

`vibe fmt [path]` rewrites Lang sources in-place using the canonical pretty printer so the codebase stays deterministic.

- Run without arguments to format every `.lang` file under the current working directory:

  ```bash
  bun packages/cli/index.ts fmt
  ```

- Target a directory or a single file:

  ```bash
  bun packages/cli/index.ts fmt src
  bun packages/cli/index.ts fmt src/main.lang
  ```

- Use `--check` during CI to fail when files would change without touching the filesystem:

  ```bash
  bun packages/cli/index.ts fmt --check
  ```

By default the command rewrites files; `--check` switches to read-only verification mode but still prints parser diagnostics for invalid sources.

### Formatter configuration

`vibe fmt` looks for a `vibe.config.ts`/`.js` file in the target directory (or any parent) and merges the declared specs with the built-in formatter defaults. The file must default-export a `formatter` block (optionally wrapped in `defineConfig`):

```ts
import { defineConfig, defineFormatterSpec } from "@vibe/config";

const defhandler = defineFormatterSpec({
  inlineHeadArgCount: 2,
  vectorArgumentIndices: [2],
});

export default defineConfig({
  formatter: {
    defhandler,
    "defhandler+": defhandler,
    cond: "condClausePairs", // Reference built-in presets
  },
});
```

- `formatter` is a map from macro/function names to either:
  - a spec object using fields like `inlineHeadArgCount`, `vectorArgumentIndices`, `forceBodyMultiline`, and `clauseGrouping`
  - `forceBodyMultiline` defaults to `true`; set it to `false` when a form's body can safely remain inline.
  - a factory created via `defineFormatterSpec` for shared, immutable specs
  - or a string referencing a built-in preset such as `definitionWithBindings`, `lambdaWithBindings`, `lexicalBindingForm`, or `condClausePairs`

All formatted files under the same directory tree share the closest `vibe.config.*`, which makes it easy to tailor formatting rules per package or workspace.
