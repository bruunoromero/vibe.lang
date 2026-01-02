import path from "node:path";
import {
  TextDocuments,
  TextDocumentSyncKind,
  DiagnosticSeverity as LspDiagnosticSeverity,
  Range,
  TextEdit,
  type Connection,
  type Diagnostic as LspDiagnostic,
  type DocumentFormattingParams,
  type Hover,
  type InitializeParams,
  type InitializeResult,
  type Position,
  type TextDocumentPositionParams,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import { URI } from "vscode-uri";
import { parseSource } from "@vibe/parser";
import {
  analyzeProgram,
  type AnalyzeResult,
  type SemanticNodeRecord,
  type SymbolRecord,
  type SymbolId,
} from "@vibe/semantics";
import {
  DiagnosticSeverity,
  type Diagnostic,
  type ProgramNode,
  type SourceSpan,
} from "@vibe/syntax";
import { formatSource } from "@vibe/formatter";
import { extractTopLevelExports } from "@vibe/module-resolver";
import { WorkspaceManager, type WorkspaceContext } from "./workspace";

export type TransportKind = "stdio" | "socket";

export interface LanguageServerOptions {
  readonly transport: TransportKind;
}

interface DocumentState {
  readonly moduleId: string;
  readonly workspace: WorkspaceContext;
  readonly program?: ProgramNode;
  readonly analysis?: AnalyzeResult;
  readonly symbolIndex?: Map<SymbolId, SymbolRecord>;
}

export const registerLanguageServer = (
  connection: Connection,
  options: LanguageServerOptions
): void => {
  const documents = new TextDocuments(TextDocument);
  const workspaceManager = new WorkspaceManager(connection);
  const documentStates = new Map<string, DocumentState>();

  connection.onInitialize((params: InitializeParams): InitializeResult => {
    const capabilities = params.capabilities;
    connection.console.info(
      `[vibe][lsp] transport=${options.transport} clientName=${capabilities?.workspace?.workspaceEdit ? "workspace" : "unknown"}`
    );
    return {
      capabilities: {
        textDocumentSync: TextDocumentSyncKind.Incremental,
        hoverProvider: true,
        documentFormattingProvider: true,
      },
    } satisfies InitializeResult;
  });

  documents.onDidOpen((event) => {
    void validateDocument(event.document);
  });

  documents.onDidChangeContent((change) => {
    void validateDocument(change.document);
  });

  documents.onDidClose((event) => {
    documentStates.delete(event.document.uri);
    connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] });
  });

  connection.onHover(async (params: TextDocumentPositionParams) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) {
      return null;
    }
    return resolveHover(doc, params.position);
  });

  connection.onDocumentFormatting(async (params: DocumentFormattingParams) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) {
      return [];
    }
    const modulePath = uriToFilePath(doc.uri);
    if (!modulePath) {
      return [];
    }
    try {
      const workspace = await workspaceManager.getContextForFile(modulePath);
      const formatterConfig = await workspaceManager.getFormatterConfig(
        path.dirname(modulePath)
      );
      const indent = buildIndent(params);
      const formatResult = await formatSource(doc.getText(), {
        indent,
        formConfig: formatterConfig,
      });
      if (!formatResult.ok) {
        return [];
      }
      if (formatResult.formatted === doc.getText()) {
        return [];
      }
      const fullRange = Range.create(
        { line: 0, character: 0 },
        doc.positionAt(doc.getText().length)
      );
      return [TextEdit.replace(fullRange, formatResult.formatted)];
    } catch (error) {
      connection.console.error(
        `[vibe][lsp] Failed to format ${modulePath}: ${String(error)}`
      );
      return [];
    }
  });

  documents.listen(connection);

  async function validateDocument(document: TextDocument): Promise<void> {
    const modulePath = uriToFilePath(document.uri);
    if (!modulePath) {
      connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
      return;
    }
    try {
      const workspace = await workspaceManager.getContextForFile(modulePath);
      workspace.knownModules.add(modulePath);
      const parseResult = await parseSource(document.getText());
      const diagnostics: Diagnostic[] = [...parseResult.diagnostics];
      let analysis: AnalyzeResult | undefined;
      if (parseResult.ok) {
        analysis = await analyzeProgram(parseResult.program, {
          moduleId: modulePath,
          moduleResolver: workspace.moduleResolver,
          moduleExports: workspace.moduleExports,
        });
        diagnostics.push(...analysis.diagnostics);
        await registerModuleExports(modulePath, parseResult.program, workspace);
      }
      const state: DocumentState = {
        moduleId: modulePath,
        workspace,
        program: parseResult.program,
        analysis,
        symbolIndex: buildSymbolIndex(analysis),
      };
      documentStates.set(document.uri, state);
      connection.sendDiagnostics({
        uri: document.uri,
        diagnostics: diagnostics.map(toLspDiagnostic),
      });
    } catch (error) {
      connection.console.error(
        `[vibe][lsp] Failed to analyze ${modulePath}: ${String(error)}`
      );
    }
  }

  async function resolveHover(
    document: TextDocument,
    position: Position
  ): Promise<Hover | null> {
    const state = documentStates.get(document.uri);
    if (!state?.analysis) {
      return null;
    }
    const node = findBestNodeAtPosition(state.analysis.graph.nodes, position);
    if (!node) {
      return null;
    }
    const contents: string[] = [];
    if (node.symbol) {
      contents.push(`**${node.symbol.name}**`);
      contents.push("");
      contents.push(`role: ${node.symbol.role}`);
      if (node.symbol.symbolId && state.symbolIndex) {
        const symbolRecord = state.symbolIndex.get(node.symbol.symbolId);
        if (symbolRecord) {
          contents.push(`scope: ${symbolRecord.scopeId}`);
          contents.push(`kind: ${symbolRecord.kind}`);
        }
      }
    } else {
      contents.push(`**${node.kind}**`);
    }
    return {
      contents: {
        kind: "markdown",
        value: contents.join("\n"),
      },
      range: spanToRange(node.span),
    } satisfies Hover;
  }

  async function registerModuleExports(
    moduleId: string,
    program: ProgramNode,
    workspace: WorkspaceContext
  ): Promise<void> {
    try {
      const entries = await extractTopLevelExports(program, {
        moduleId,
        moduleResolver: workspace.moduleResolver,
        moduleExports: workspace.moduleExports,
      });
      workspace.moduleExports.register(moduleId, entries);
    } catch (error) {
      connection.console.error(
        `[vibe][lsp] Failed to register exports for ${moduleId}: ${String(error)}`
      );
    }
  }
};

const buildSymbolIndex = (
  analysis: AnalyzeResult | undefined
): Map<SymbolId, SymbolRecord> | undefined => {
  if (!analysis) {
    return undefined;
  }
  const index = new Map<SymbolId, SymbolRecord>();
  for (const symbol of analysis.graph.symbols ?? []) {
    index.set(symbol.id, symbol);
  }
  return index;
};

const findBestNodeAtPosition = (
  nodes: readonly SemanticNodeRecord[],
  position: Position
): SemanticNodeRecord | null => {
  let best: SemanticNodeRecord | null = null;
  for (const node of nodes) {
    if (!positionWithinSpan(position, node.span)) {
      continue;
    }
    if (!best || isNarrowerSpan(node.span, best.span)) {
      best = node;
    }
  }
  return best;
};

const isNarrowerSpan = (current: SourceSpan, previous: SourceSpan): boolean => {
  const currentLength = spanLength(current);
  const previousLength = spanLength(previous);
  if (currentLength === previousLength) {
    return current.start.offset >= previous.start.offset;
  }
  return currentLength < previousLength;
};

const spanLength = (span: SourceSpan): number => {
  return span.end.offset - span.start.offset;
};

const positionWithinSpan = (position: Position, span: SourceSpan): boolean => {
  if (position.line < span.start.line) {
    return false;
  }
  if (position.line > span.end.line) {
    return false;
  }
  if (position.line === span.start.line && position.character < span.start.column) {
    return false;
  }
  if (position.line === span.end.line && position.character > span.end.column) {
    return false;
  }
  return true;
};

const spanToRange = (span: SourceSpan): Range => {
  return Range.create(
    { line: span.start.line, character: span.start.column },
    { line: span.end.line, character: span.end.column }
  );
};

const severityMap: Record<DiagnosticSeverity, LspDiagnosticSeverity> = {
  [DiagnosticSeverity.Error]: LspDiagnosticSeverity.Error,
  [DiagnosticSeverity.Warning]: LspDiagnosticSeverity.Warning,
  [DiagnosticSeverity.Info]: LspDiagnosticSeverity.Information,
};

const toLspDiagnostic = (diagnostic: Diagnostic): LspDiagnostic => ({
  message: diagnostic.message,
  severity: severityMap[diagnostic.severity] ?? LspDiagnosticSeverity.Error,
  range: spanToRange(diagnostic.span),
  code: diagnostic.code,
});

const uriToFilePath = (uri: string): string | null => {
  try {
    const parsed = URI.parse(uri);
    if (parsed.scheme !== "file") {
      return null;
    }
    return parsed.fsPath;
  } catch {
    return null;
  }
};

const buildIndent = (params: DocumentFormattingParams): string => {
  const { insertSpaces, tabSize } = params.options;
  if (insertSpaces === false) {
    return "\t";
  }
  const size = Math.max(1, tabSize ?? 2);
  return " ".repeat(size);
};
