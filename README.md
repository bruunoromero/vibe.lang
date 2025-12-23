# vibe

To install dependencies:

```bash
bun install
```

To run:

```bash
bun run index.ts
```

This project was created using `bun init` in bun v1.3.5. [Bun](https://bun.com) is a fast all-in-one JavaScript runtime.

## CLI commands

- `bun run packages/cli/index.ts tokenize "(println :ok)"` – emit raw tokens.
- `... parse` – print the AST JSON.
- `... analyze` – include semantic graph metadata (scopes, symbols).
- `... run` – full pipeline (parse → analyze → codegen) that lowers supported forms to JavaScript, executes them, and surfaces diagnostics plus inline source maps.
- `... compile` – full pipeline that writes the generated JavaScript to stdout or `--out` for inspection/bundling.
- `... compile-all <dir>` – recursively compiles every `.lang` file within a directory tree into a mirrored `--out-dir` (defaults to `dist`).
- Pass `--debug-macros` to `analyze`, `run`, `compile`, or `compile-all` to dump macro-expansion metadata (scopes, symbols, diagnostics) to stderr without disrupting primary output. Use `--pretty <n>` to control the indentation of these debug payloads.

## Example package

- See [packages/example-app](packages/example-app/README.md#L1) for a minimal Vibe program plus scripts that run `vibe compile-all` and execute the emitted JavaScript with Node.
- From the repo root: `cd packages/example-app && bun run build && bun run start`.
