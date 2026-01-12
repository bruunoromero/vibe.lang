# Vibe VS Code Extension

Language support for the [Vibe programming language](https://github.com/vibe-lang/vibe).

## Features

- **Syntax Highlighting**: TextMate grammar for basic syntax coloring
- **Semantic Tokens**: Rich, context-aware token highlighting powered by the compiler
- **Diagnostics**: Real-time error reporting from the compiler
- **Hover Information**: Type information on hover
- **Go to Definition**: Jump to symbol definitions
- **Code Completion**: Context-aware symbol suggestions
- **Document Symbols**: Outline view of declarations

## Requirements

- VS Code 1.85.0 or higher
- Node.js 18+ or Bun runtime

## Installation

### From VS Code Marketplace

Search for "Vibe Language" in the Extensions view.

### From VSIX

1. Download the `.vsix` file from the releases page
2. In VS Code, go to Extensions view
3. Click the `...` menu and select "Install from VSIX..."
4. Select the downloaded file

### Development Mode

For development, link the extension:

```bash
cd packages/vscode-extension
bun install
bun run build
code --install-extension .
```

## Configuration

| Setting                        | Default | Description                          |
| ------------------------------ | ------- | ------------------------------------ |
| `vibe.languageServer.enabled`  | `true`  | Enable/disable the language server   |
| `vibe.diagnostics.enabled`     | `true`  | Enable/disable diagnostics           |
| `vibe.semanticTokens.enabled`  | `true`  | Enable/disable semantic highlighting |
| `vibe.diagnostics.maxProblems` | `100`   | Maximum diagnostics per file         |

## Commands

- `Vibe: Restart Language Server` - Restart the language server
- `Vibe: Show Output` - Show the language server output channel

## Development

```bash
# Build the extension
bun run build

# Watch mode
bun run watch

# Package as VSIX
bun run package
```

## License

MIT
