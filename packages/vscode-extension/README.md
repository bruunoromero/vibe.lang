# Vibe VS Code Extension

This package bundles VS Code integrations for the Vibe language: syntax highlighting, formatter integration, and a client that spawns the existing `@vibe/lsp` server.

## Features

- TextMate grammar generated from [`docs/syntax-spec.md`](../../docs/syntax-spec.md) covering keywords, literals, and import forms.
- Automatic workspace discovery for any folder that contains a `vibe.config.*` file. Each config directory gets its own language-client instance so multi-root workspaces Just Work.
- Formatter + hover + diagnostics routed through the Bun-based language server.
- Manual `Vibe: Format Document` command that runs the formatter directly in VS Code when you need ad-hoc rewrites.

## Development

```sh
bun install
bun run build
bun run watch  # optional dev loop
```

## Packaging a VSIX

1. Build the extension artifacts: `bun run build`.
2. Run `bun run package` to create `vibe-vscode.vsix` in this folder.
3. Install locally with `code --install-extension vibe-vscode.vsix`.

The VSIX keeps the bundled `@vibe/lsp` workspace dependency and executes it through the Bun runtime specified via the `vibe.languageServer.bunPath` setting (defaults to `bun`).
