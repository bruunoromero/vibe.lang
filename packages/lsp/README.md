# @vibe/lsp

The Vibe language server powers IDE experiences such as diagnostics, hover
information, and source formatting. The server is designed to run under Bun and
communicates using the Language Server Protocol.

## Getting started

```bash
# Run over stdio (default)
bun run packages/lsp/src/server.ts

# Run as a TCP server
bun run packages/lsp/src/server.ts --transport socket --port 7413
```

The server automatically discovers Vibe workspaces, reuses the existing compiler
pipeline, and can be embedded into future editor extensions such as VS Code.
