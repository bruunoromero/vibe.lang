# @vibe/example-app

A tiny reference package that shows how to take a `.lang` program, compile it to JavaScript with the Lang CLI, and execute the result with Node.js.

## Layout

```
packages/example-app/
├── package.json      # scripts that wrap the Lang CLI
├── README.md         # this file
└── src/
   ├── main.lang     # sample program using (require prelude "@vibe/prelude"), (require math "./math.lang"), and (external path "node:path")
   └── math.lang     # simple namespace imported via (require math "./math.lang")
```

Feel free to add more `.lang` files under `src/` and adjust the build script to compile each entrypoint you need.

## Usage

1. **Install deps at the repo root (once):**

   ```bash
   bun install
   ```

2. **Compile the Lang source to JavaScript:**

   ```bash
   cd packages/example-app
   bun run build
   ```

   - Runs `lang compile-all src --out-dir dist`, which walks every `.lang` file under `src/` and mirrors the tree inside `dist/` with `.js` outputs.
   - Creates `dist/math.js`, `dist/main.js`, and any other generated siblings so `(require ...)` statements automatically resolve to their compiled modules.

3. **Execute the compiled file with Node:**

   ```bash
   bun run start
   # or directly
   node dist/main.js
   ```

4. **Iterate quickly:**

   ```bash
   bun run dev
   ```

   - Invokes `lang run` so you can execute the program directly without emitting files, useful while editing.

5. **Clean build artifacts:**
   ```bash
   bun run clean
   ```

## Notes

- The build script creates the `dist/` folder on demand; feel free to commit additional compile steps (e.g., compiling multiple `.lang` files or copying static assets).
- Because the Lang CLI already outputs ESM, Node.js can execute the generated files without extra bundling.
- Pass flags like `--show-ast`, `--show-ir`, or `--debug-macros` to the CLI commands in `package.json` if you want more visibility into the pipeline.
- `main.lang` demonstrates importing Node/Bun modules via `(external path "node:path")`, so feel free to swap in other externals like `node:fs`.
- `package.json` now declares `vibe.sources` plus a `vibe.entry` so `(require prelude "@vibe/prelude")` resolves via the shared source tree while the emitted JavaScript keeps importing the package's published JS via Node's `exports` field.
