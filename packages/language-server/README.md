# Vibe Language Server

Language Server Protocol (LSP) implementation for the Vibe programming language.

## Features

- **Diagnostics**: Real-time error reporting from lexer, parser, and semantic analysis
- **Hover**: Type information on hover for values, functions, constructors, and types
- **Go to Definition**: Jump to symbol definitions
- **Code Completion**: Context-aware symbol suggestions
- **Document Symbols**: Outline view of declarations in a file
- **Semantic Tokens**: Rich, context-aware syntax highlighting powered by compiler analysis

## Architecture

The language server uses a multi-level caching strategy for performance:

1. **Token Cache**: Tokenization results cached per document version
2. **Parse Cache**: AST cached per document version
3. **Semantic Cache**: Type information and semantic analysis cached per document version

Cache invalidation is tied to document version numbers, ensuring consistency with minimal recomputation.

## Usage

### As a standalone server

```bash
# Run with stdio transport (default for LSP)
bun run dev

# Or build and run
bun run build
node dist/server.js --stdio
```

### As a library

```typescript
import { DocumentManager, provideSemanticTokens } from "@vibe/language-server";

const manager = new DocumentManager();

// Update document (triggers analysis)
const cache = manager.updateDocument(textDocument);

// Get diagnostics
console.log(cache.diagnostics);

// Get hover info
const hover = manager.getHoverInfo(uri, line, character);

// Get completions
const symbols = manager.getSymbolsAtPosition(uri);
```

## Development

```bash
# Install dependencies
bun install

# Build
bun run build

# Run tests
bun test

# Run in development mode
bun run dev
```

## Protocol Support

The server implements LSP 3.17 with the following capabilities:

| Feature                          | Status |
| -------------------------------- | ------ |
| textDocument/publishDiagnostics  | ✅     |
| textDocument/hover               | ✅     |
| textDocument/definition          | ✅     |
| textDocument/completion          | ✅     |
| textDocument/documentSymbol      | ✅     |
| textDocument/semanticTokens/full | ✅     |
| workspace/configuration          | ✅     |

## License

MIT
