/**
 * Vibe Language Server
 *
 * LSP 3.17 implementation for the Vibe programming language.
 */
import {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  TextDocumentSyncKind,
  CompletionItem,
  CompletionItemKind,
  Hover,
  MarkupKind,
  Location,
  Range,
  DiagnosticSeverity,
  SemanticTokens,
  DocumentDiagnosticReportKind,
} from "vscode-languageserver/node";
import type {
  InitializeParams,
  InitializeResult,
  Definition,
  SemanticTokensParams,
  SemanticTokensRangeParams,
  DocumentDiagnosticParams,
  DocumentDiagnosticReport,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import { DocumentManager } from "./document-manager";
import {
  SEMANTIC_TOKENS_LEGEND,
  provideSemanticTokens,
} from "./semantic-tokens";
import type { ServerConfig, SymbolInfo } from "./types";
import { SymbolKind, DEFAULT_CONFIG } from "./types";

// Create server connection (supports stdio, IPC, or socket)
const connection = createConnection(ProposedFeatures.all);

// Text document manager (syncs document content)
const documents = new TextDocuments(TextDocument);

// Our document analysis cache
const documentManager = new DocumentManager();

// Server configuration
let config: ServerConfig = { ...DEFAULT_CONFIG };

// Capabilities negotiation
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;
let hasSemanticTokensCapability = false;

/**
 * Initialize handler - negotiate capabilities with client.
 */
connection.onInitialize((params: InitializeParams): InitializeResult => {
  const capabilities = params.capabilities;

  // Check workspace capabilities
  hasWorkspaceFolderCapability = !!capabilities.workspace?.workspaceFolders;

  // Check diagnostic capabilities
  hasDiagnosticRelatedInformationCapability =
    !!capabilities.textDocument?.publishDiagnostics?.relatedInformation;

  // Check semantic tokens capabilities
  hasSemanticTokensCapability = !!capabilities.textDocument?.semanticTokens;

  // Return server capabilities
  const result: InitializeResult = {
    capabilities: {
      // Incremental text sync
      textDocumentSync: TextDocumentSyncKind.Incremental,

      // Code completion
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: [".", ":"],
      },

      // Hover information
      hoverProvider: true,

      // Go to definition
      definitionProvider: true,

      // Document symbols (outline)
      documentSymbolProvider: true,

      // Semantic tokens (full document)
      semanticTokensProvider: hasSemanticTokensCapability
        ? {
            legend: SEMANTIC_TOKENS_LEGEND,
            full: true,
            range: false,
          }
        : undefined,

      // Workspace folders
      workspace: hasWorkspaceFolderCapability
        ? {
            workspaceFolders: {
              supported: true,
              changeNotifications: true,
            },
          }
        : undefined,
    },
    serverInfo: {
      name: "vibe-language-server",
      version: "0.1.0",
    },
  };

  return result;
});

/**
 * Initialized handler - server is ready.
 */
connection.onInitialized(() => {
  connection.console.log("Vibe Language Server initialized");

  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log("Workspace folder change event received");
    });
  }
});

// ============================================================================
// Document Synchronization
// ============================================================================

/**
 * Document opened handler.
 */
documents.onDidOpen((event) => {
  const doc = event.document;
  connection.console.log(`Document opened: ${doc.uri}`);
  const cache = documentManager.updateDocument(doc);
  sendDiagnostics(doc.uri, cache.diagnostics);
});

/**
 * Document changed handler.
 */
documents.onDidChangeContent((event) => {
  const doc = event.document;
  const cache = documentManager.updateDocument(doc);
  sendDiagnostics(doc.uri, cache.diagnostics);
});

/**
 * Document closed handler.
 */
documents.onDidClose((event) => {
  const uri = event.document.uri;
  connection.console.log(`Document closed: ${uri}`);
  documentManager.removeDocument(uri);
  // Clear diagnostics for closed document
  connection.sendDiagnostics({ uri, diagnostics: [] });
});

/**
 * Send diagnostics to client.
 */
function sendDiagnostics(
  uri: string,
  diagnostics: import("vscode-languageserver").Diagnostic[]
): void {
  connection.sendDiagnostics({
    uri,
    diagnostics: diagnostics.slice(0, config.maxDiagnostics),
  });
}

// ============================================================================
// Completion
// ============================================================================

/**
 * Completion handler.
 */
connection.onCompletion((params): CompletionItem[] => {
  const uri = params.textDocument.uri;
  const symbols = documentManager.getSymbolsAtPosition(uri);

  return symbols.map((symbol) => ({
    label: symbol.name,
    kind: symbolKindToCompletionKind(symbol.kind),
    detail: symbol.type
      ? documentManager.formatTypeScheme(symbol.type)
      : undefined,
    documentation: symbol.documentation,
    data: { uri, name: symbol.name },
  }));
});

/**
 * Completion resolve handler - provide additional details.
 */
connection.onCompletionResolve((item): CompletionItem => {
  // Additional resolution can be done here if needed
  return item;
});

/**
 * Map our symbol kind to LSP completion kind.
 */
function symbolKindToCompletionKind(kind: SymbolKind): CompletionItemKind {
  switch (kind) {
    case SymbolKind.Function:
      return CompletionItemKind.Function;
    case SymbolKind.Value:
      return CompletionItemKind.Variable;
    case SymbolKind.Constructor:
      return CompletionItemKind.Constructor;
    case SymbolKind.Type:
      return CompletionItemKind.Class;
    case SymbolKind.TypeAlias:
      return CompletionItemKind.Interface;
    case SymbolKind.OpaqueType:
      return CompletionItemKind.Class;
    case SymbolKind.Protocol:
      return CompletionItemKind.Interface;
    case SymbolKind.TypeVariable:
      return CompletionItemKind.TypeParameter;
    case SymbolKind.Parameter:
      return CompletionItemKind.Variable;
    case SymbolKind.Field:
      return CompletionItemKind.Field;
    case SymbolKind.Operator:
      return CompletionItemKind.Operator;
    case SymbolKind.Module:
      return CompletionItemKind.Module;
    default:
      return CompletionItemKind.Text;
  }
}

// ============================================================================
// Hover
// ============================================================================

/**
 * Hover handler.
 */
connection.onHover((params): Hover | null => {
  const uri = params.textDocument.uri;
  const { line, character } = params.position;

  const hoverInfo = documentManager.getHoverInfo(uri, line, character);

  if (!hoverInfo) {
    return null;
  }

  // Format hover content
  const content: string[] = [];

  // Type signature in code block
  if (hoverInfo.type) {
    content.push("```vibe");
    content.push(`${hoverInfo.name} : ${hoverInfo.type}`);
    content.push("```");
  } else {
    content.push(`**${hoverInfo.name}**`);
  }

  return {
    contents: {
      kind: MarkupKind.Markdown,
      value: content.join("\n"),
    },
    range: {
      start: {
        line: Math.max(0, hoverInfo.span.start.line - 1),
        character: Math.max(0, hoverInfo.span.start.column - 1),
      },
      end: {
        line: Math.max(0, hoverInfo.span.end.line - 1),
        character: Math.max(0, hoverInfo.span.end.column - 1),
      },
    },
  };
});

// ============================================================================
// Go to Definition
// ============================================================================

/**
 * Definition handler.
 */
connection.onDefinition((params): Definition | null => {
  const uri = params.textDocument.uri;
  const { line, character } = params.position;

  const definition = documentManager.findDefinition(uri, line, character);

  if (!definition) {
    return null;
  }

  return Location.create(definition.uri, {
    start: {
      line: Math.max(0, definition.span.start.line - 1),
      character: Math.max(0, definition.span.start.column - 1),
    },
    end: {
      line: Math.max(0, definition.span.end.line - 1),
      character: Math.max(0, definition.span.end.column - 1),
    },
  });
});

// ============================================================================
// Document Symbols
// ============================================================================

/**
 * Document symbol handler.
 */
connection.onDocumentSymbol((params) => {
  const uri = params.textDocument.uri;
  const symbols = documentManager.getSymbolsAtPosition(uri);

  return symbols.map((symbol) => ({
    name: symbol.name,
    kind: symbolKindToDocumentSymbolKind(symbol.kind),
    range: symbol.definitionSpan
      ? {
          start: {
            line: Math.max(0, symbol.definitionSpan.start.line - 1),
            character: Math.max(0, symbol.definitionSpan.start.column - 1),
          },
          end: {
            line: Math.max(0, symbol.definitionSpan.end.line - 1),
            character: Math.max(0, symbol.definitionSpan.end.column - 1),
          },
        }
      : {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 0 },
        },
    selectionRange: symbol.definitionSpan
      ? {
          start: {
            line: Math.max(0, symbol.definitionSpan.start.line - 1),
            character: Math.max(0, symbol.definitionSpan.start.column - 1),
          },
          end: {
            line: Math.max(0, symbol.definitionSpan.end.line - 1),
            character: Math.max(0, symbol.definitionSpan.end.column - 1),
          },
        }
      : {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 0 },
        },
  }));
});

/**
 * Map our symbol kind to LSP document symbol kind.
 */
function symbolKindToDocumentSymbolKind(
  kind: SymbolKind
): import("vscode-languageserver").SymbolKind {
  switch (kind) {
    case SymbolKind.Function:
      return 12; // Function
    case SymbolKind.Value:
      return 13; // Variable
    case SymbolKind.Constructor:
      return 9; // Constructor
    case SymbolKind.Type:
      return 5; // Class
    case SymbolKind.TypeAlias:
      return 11; // Interface
    case SymbolKind.OpaqueType:
      return 5; // Class
    case SymbolKind.Protocol:
      return 11; // Interface
    case SymbolKind.TypeVariable:
      return 26; // TypeParameter
    case SymbolKind.Parameter:
      return 13; // Variable
    case SymbolKind.Field:
      return 8; // Field
    case SymbolKind.Operator:
      return 25; // Operator
    case SymbolKind.Module:
      return 2; // Module
    default:
      return 13; // Variable
  }
}

// ============================================================================
// Semantic Tokens
// ============================================================================

/**
 * Semantic tokens handler (full document).
 */
connection.languages.semanticTokens.on(
  (params: SemanticTokensParams): SemanticTokens => {
    const uri = params.textDocument.uri;
    const cache = documentManager.getDocument(uri);

    if (!cache) {
      return { data: [] };
    }

    const data = provideSemanticTokens(cache);
    return { data };
  }
);

// ============================================================================
// Configuration
// ============================================================================

/**
 * Configuration change handler.
 */
connection.onDidChangeConfiguration((change) => {
  // Update configuration if provided
  if (change.settings?.vibe) {
    config = { ...DEFAULT_CONFIG, ...change.settings.vibe };
  }
});

// ============================================================================
// Start Server
// ============================================================================

// Listen for document events
documents.listen(connection);

// Start listening
connection.listen();
