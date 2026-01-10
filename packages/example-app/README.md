# @vibe/example-app

Minimal starter project for the Vibe language. It demonstrates a `vibe.json` config and a single `ExampleApp` module.

## Structure

- `vibe.json` — project config (name, src, dist, packages)
- `src/ExampleApp.vibe` — root module used by the CLI

## Try it

From the repo root (Bun required):

```bash
bun packages/cli/src/index.ts tokenize packages/example-app/src/ExampleApp.vibe
bun packages/cli/src/index.ts parse packages/example-app/src/ExampleApp.vibe
```

Or from the package directory using the helper scripts:

```bash
cd packages/example-app
bun run tokenize
bun run parse
```
