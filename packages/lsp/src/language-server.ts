import path from "node:path";
import {
  TextDocuments,
  TextDocumentSyncKind,
  DiagnosticSeverity as LspDiagnosticSeverity,
  Range,
  TextEdit,
  CompletionItemKind,
  Location,
  type Connection,
  type CompletionItem,
  type CompletionParams,
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
  type ScopeRecord,
  type ScopeId,
  type ModuleImportRecord,
  type SymbolRecord,
  type SymbolKind,
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
  readonly version: number;
  readonly moduleId: string;
  readonly workspace: WorkspaceContext;
  readonly program?: ProgramNode;
  readonly analysis?: AnalyzeResult;
  readonly symbolIndex?: Map<SymbolId, SymbolRecord>;
  readonly scopeIndex?: Map<ScopeId, ScopeRecord>;
  readonly namespaceImports?: Map<string, ModuleImportRecord>;
  readonly definitionIndex?: Map<SymbolId, DefinitionRecord>;
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
      `[vibe][lsp] transport=${options.transport} clientName=${
        capabilities?.workspace?.workspaceEdit ? "workspace" : "unknown"
      }`
    );
    connection.console.info(
      "[vibe][lsp] Initializing with capabilities: hover, format, complete"
    );
    return {
      capabilities: {
        textDocumentSync: TextDocumentSyncKind.Incremental,
        hoverProvider: true,
        documentFormattingProvider: true,
        definitionProvider: true,
        completionProvider: {
          resolveProvider: false,
          triggerCharacters: [],
        },
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

  connection.onCompletion(async (params: CompletionParams) => {
    const doc = documents.get(params.textDocument.uri);
    connection.console.info(
      `[vibe][lsp] Completion request for ${params.textDocument.uri}`
    );
    if (!doc) {
      connection.console.warn(
        `[vibe][lsp] No document found for ${params.textDocument.uri}`
      );
      return [];
    }
    const items = await resolveCompletions(doc, params.position);
    connection.console.info(
      `[vibe][lsp] Returning ${items.length} completion items`
    );
    return items;
  });

  connection.onDefinition(async (params: TextDocumentPositionParams) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) {
      return null;
    }
    return resolveDefinition(doc, params.position);
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
    const version = document.version;
    if (!modulePath) {
      connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });
      return;
    }
    try {
      const workspace = await workspaceManager.getContextForFile(modulePath);
      workspace.knownModules.add(modulePath);
      connection.console.debug(
        `[vibe][lsp] Validating ${modulePath} (v${version})`
      );
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
      if (!isLatestDocumentVersion(document.uri, version)) {
        connection.console.debug(
          `[vibe][lsp] Skipping stale validation for ${modulePath}`
        );
        return;
      }

      const symbolIndex = buildSymbolIndex(analysis);
      const scopeIndex = buildScopeIndex(analysis);
      const namespaceImports = buildNamespaceImportMap(analysis);
      const flattenedImports = buildFlattenedImportIndex(analysis);
      const macroImports = buildMacroImportIndex(analysis);
      const definitionIndex = buildDefinitionIndex(
        analysis,
        modulePath,
        symbolIndex,
        flattenedImports,
        macroImports
      );

      const state: DocumentState = {
        version,
        moduleId: modulePath,
        workspace,
        program: parseResult.program,
        analysis,
        symbolIndex,
        scopeIndex,
        namespaceImports,
        definitionIndex,
      };
      documentStates.set(document.uri, state);
      connection.console.info(
        `[vibe][lsp] Updated ${modulePath}: ${
          diagnostics.length
        } diagnostics, ${analysis?.graph.symbols.length ?? 0} symbols`
      );
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

  const isLatestDocumentVersion = (uri: string, version: number): boolean => {
    const current = documents.get(uri);
    if (!current) {
      return false;
    }
    return current.version === version;
  };

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
        `[vibe][lsp] Failed to register exports for ${moduleId}: ${String(
          error
        )}`
      );
    }
  }

  async function resolveCompletions(
    document: TextDocument,
    position: Position
  ): Promise<CompletionItem[]> {
    const state = documentStates.get(document.uri);
    if (!state?.analysis || !state.symbolIndex || !state.scopeIndex) {
      return [];
    }
    const namespaceContext = extractNamespaceCompletionContext(
      document,
      position
    );
    if (namespaceContext) {
      const namespaceItems = collectNamespaceCompletions(
        namespaceContext,
        state
      );
      if (namespaceItems.length > 0) {
        return namespaceItems;
      }
    }
    const node = findBestNodeAtPosition(state.analysis.graph.nodes, position);
    const scopeId = node?.scopeId ?? state.analysis.graph.scopes[0]?.id ?? null;
    if (!scopeId) {
      return [];
    }
    return collectScopeCompletions(
      scopeId,
      state.scopeIndex,
      state.symbolIndex
    );
  }

  async function resolveDefinition(
    document: TextDocument,
    position: Position
  ): Promise<Location | null> {
    const state = documentStates.get(document.uri);
    if (!state?.analysis) {
      return null;
    }
    const importPathLocation = resolveImportPathDefinition(position, state);
    if (importPathLocation) {
      return importPathLocation;
    }
    const namespaceReference = extractNamespaceReference(document, position);
    if (namespaceReference) {
      const namespaceLocation = resolveNamespaceDefinition(
        namespaceReference,
        state
      );
      if (namespaceLocation) {
        return namespaceLocation;
      }
    }
    let node = findBestNodeAtPosition(state.analysis.graph.nodes, position);
    if ((!node || !node.symbol?.symbolId) && state.analysis) {
      node = findSymbolNodeByText(
        state.analysis.graph.nodes,
        document,
        position
      );
    }
    const symbolId = node?.symbol?.symbolId;
    if (!symbolId) {
      return null;
    }
    if (node && node.symbol?.role === "definition") {
      return Location.create(document.uri, spanToRange(node.span));
    }
    const definition = state.definitionIndex?.get(symbolId);
    if (!definition) {
      return null;
    }
    if (definition.imported) {
      const importedLocation = resolveImportedDefinition(
        definition.imported,
        state
      );
      if (importedLocation) {
        return importedLocation;
      }
    }
    const targetUri =
      definition.moduleId === state.moduleId
        ? document.uri
        : URI.file(definition.moduleId).toString();
    return Location.create(targetUri, spanToRange(definition.span));
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

const buildScopeIndex = (
  analysis: AnalyzeResult | undefined
): Map<ScopeId, ScopeRecord> | undefined => {
  if (!analysis) {
    return undefined;
  }
  const index = new Map<ScopeId, ScopeRecord>();
  for (const scope of analysis.graph.scopes ?? []) {
    index.set(scope.id, scope);
  }
  return index;
};

const buildNamespaceImportMap = (
  analysis: AnalyzeResult | undefined
): Map<string, ModuleImportRecord> | undefined => {
  if (!analysis) {
    return undefined;
  }
  const map = new Map<string, ModuleImportRecord>();
  for (const record of analysis.graph.imports ?? []) {
    if (!record?.alias) {
      continue;
    }
    if (
      record.kind !== "require" &&
      record.kind !== "external" &&
      record.kind !== "import"
    ) {
      continue;
    }
    map.set(record.alias, record);
  }
  return map.size > 0 ? map : undefined;
};

type NamespaceCompletionContext = {
  readonly alias: string;
  readonly partial: string;
  readonly range: Range;
};

type NamespaceReference = {
  readonly alias: string;
  readonly member: string;
};

const namespaceSymbolCharRegex = /[A-Za-z0-9_\-\*\+!\?=<>\/]/;
const symbolCharRegex = /[A-Za-z0-9_\-\*\+!\?=<>]/;

const extractNamespaceCompletionContext = (
  document: TextDocument,
  position: Position
): NamespaceCompletionContext | null => {
  const lineText = document.getText(
    Range.create({ line: position.line, character: 0 }, position)
  );
  const match = lineText.match(
    /([A-Za-z_][A-Za-z0-9_\-\*\+!\?=<>\/]*)\/([A-Za-z0-9_\-\*\+!\?=<>\/]*)$/
  );
  if (!match) {
    return null;
  }
  const alias = match[1] ?? "";
  const partial = match[2] ?? "";
  const startChar = position.character - partial.length;
  const range = Range.create(
    { line: position.line, character: startChar },
    position
  );
  return { alias, partial, range };
};

const collectNamespaceCompletions = (
  context: NamespaceCompletionContext,
  state: DocumentState
): CompletionItem[] => {
  if (!state.namespaceImports?.has(context.alias)) {
    return [];
  }
  const record = state.namespaceImports.get(context.alias);
  if (!record) {
    return [];
  }
  const moduleId = resolveModuleIdForImport(record, state);
  if (!moduleId) {
    return [];
  }
  const exports = state.workspace.moduleExports.getExports(moduleId);
  if (!exports) {
    return [];
  }
  const items: CompletionItem[] = [];
  for (const entry of exports) {
    const target = entry.identifier ?? entry.name;
    if (
      context.partial &&
      !target.startsWith(context.partial) &&
      !entry.name.startsWith(context.partial)
    ) {
      continue;
    }
    items.push({
      label: `${context.alias}/${entry.name}`,
      kind:
        entry.kind === "macro"
          ? CompletionItemKind.Function
          : CompletionItemKind.Variable,
      detail: `${entry.kind} from ${path.basename(moduleId)}`,
      textEdit: TextEdit.replace(context.range, target),
    });
  }
  return items;
};

const extractNamespaceReference = (
  document: TextDocument,
  position: Position
): NamespaceReference | null => {
  const fullText = document.getText();
  const offset = document.offsetAt(position);
  let start = offset;
  while (start > 0 && isNamespaceSymbolChar(fullText[start - 1] ?? "")) {
    start -= 1;
  }
  let end = offset;
  while (end < fullText.length && isNamespaceSymbolChar(fullText[end] ?? "")) {
    end += 1;
  }
  if (start >= end) {
    return null;
  }
  const candidate = fullText.slice(start, end);
  const slashIndex = candidate.indexOf("/");
  if (slashIndex <= 0 || slashIndex >= candidate.length - 1) {
    return null;
  }
  const alias = candidate.slice(0, slashIndex);
  const member = candidate.slice(slashIndex + 1);
  if (!alias || !member) {
    return null;
  }
  return { alias, member };
};

const resolveNamespaceDefinition = (
  reference: NamespaceReference,
  state: DocumentState
): Location | null => {
  if (!state.namespaceImports?.has(reference.alias)) {
    return null;
  }
  const record = state.namespaceImports.get(reference.alias);
  if (!record) {
    return null;
  }
  const moduleId = resolveModuleIdForImport(record, state);
  if (!moduleId) {
    return null;
  }
  const exports = state.workspace.moduleExports.getExports(moduleId);
  if (!exports) {
    return null;
  }
  const entry = exports.find((item) => item.name === reference.member);
  if (!entry) {
    return null;
  }
  const targetUri = URI.file(moduleId).toString();
  if (entry.span) {
    return Location.create(targetUri, spanToRange(entry.span));
  }
  return Location.create(
    targetUri,
    Range.create({ line: 0, character: 0 }, { line: 0, character: 0 })
  );
};

const resolveModuleIdForImport = (
  record: ModuleImportRecord,
  state: DocumentState
): string | undefined => {
  if (record.moduleId) {
    return record.moduleId;
  }
  const result = state.workspace.moduleResolver.resolve({
    kind: record.kind,
    specifier: record.specifier,
    fromModuleId: state.moduleId,
  });
  if (result.ok && result.moduleId) {
    state.workspace.knownModules.add(result.moduleId);
    return result.moduleId;
  }
  return undefined;
};

const findSymbolNodeByText = (
  nodes: readonly SemanticNodeRecord[],
  document: TextDocument,
  position: Position
): SemanticNodeRecord | null => {
  const symbolName = extractSymbolAtPosition(document, position);
  if (!symbolName) {
    return null;
  }
  let best: SemanticNodeRecord | null = null;
  for (const node of nodes) {
    if (node.symbol?.name !== symbolName) {
      continue;
    }
    if (!positionWithinSpan(position, node.span)) {
      continue;
    }
    if (!best || isNarrowerSpan(node.span, best.span)) {
      best = node;
    }
  }
  return best;
};

const extractSymbolAtPosition = (
  document: TextDocument,
  position: Position
): string | null => {
  const text = document.getText();
  const offset = document.offsetAt(position);
  let start = offset;
  while (start > 0 && symbolCharRegex.test(text[start - 1] ?? "")) {
    start -= 1;
  }
  let end = offset;
  while (end < text.length && symbolCharRegex.test(text[end] ?? "")) {
    end += 1;
  }
  if (start === end) {
    return null;
  }
  return text.slice(start, end);
};

const resolveImportPathDefinition = (
  position: Position,
  state: DocumentState
): Location | null => {
  const importRecord = findImportRecordAtPosition(
    state.analysis?.graph.imports,
    position
  );
  if (!importRecord) {
    return null;
  }
  const moduleId =
    importRecord.moduleId ?? resolveModuleIdForImport(importRecord, state);
  if (!moduleId) {
    return null;
  }
  state.workspace.knownModules.add(moduleId);
  const targetUri = URI.file(moduleId).toString();
  const zeroRange = Range.create(
    { line: 0, character: 0 },
    { line: 0, character: 0 }
  );
  return Location.create(targetUri, zeroRange);
};

const findImportRecordAtPosition = (
  records: readonly ModuleImportRecord[] | undefined,
  position: Position
): ModuleImportRecord | null => {
  if (!records) {
    return null;
  }
  for (const record of records) {
    if (positionWithinSpan(position, record.span)) {
      return record;
    }
  }
  return null;
};

const resolveImportedDefinition = (
  target: ImportedSymbolTarget,
  state: DocumentState
): Location | null => {
  const exports = state.workspace.moduleExports.getExports(target.moduleId);
  const targetUri = URI.file(target.moduleId).toString();
  const zeroRange = Range.create(
    { line: 0, character: 0 },
    { line: 0, character: 0 }
  );
  if (!exports || exports.length === 0) {
    return Location.create(targetUri, zeroRange);
  }
  const match =
    exports.find((entry) => entry.name === target.exportedName) ??
    (target.exportedIdentifier
      ? exports.find((entry) => entry.identifier === target.exportedIdentifier)
      : undefined);
  if (match?.span) {
    return Location.create(targetUri, spanToRange(match.span));
  }
  if (match) {
    return Location.create(targetUri, zeroRange);
  }
  return Location.create(targetUri, zeroRange);
};

type ImportedSymbolTarget = {
  readonly moduleId: string;
  readonly exportedName: string;
  readonly exportedIdentifier?: string;
};

type DefinitionRecord = {
  readonly moduleId: string;
  readonly span: SourceSpan;
  readonly imported?: ImportedSymbolTarget;
};

const buildDefinitionIndex = (
  analysis: AnalyzeResult | undefined,
  moduleId: string,
  symbolIndex: Map<SymbolId, SymbolRecord> | undefined,
  flattenedImports: Map<string, ImportedSymbolTarget> | undefined,
  macroImports: Map<SymbolId, ImportedSymbolTarget> | undefined
): Map<SymbolId, DefinitionRecord> | undefined => {
  if (!analysis) {
    return undefined;
  }
  const map = new Map<SymbolId, DefinitionRecord>();
  for (const node of analysis.graph.nodes ?? []) {
    if (
      node.symbol?.symbolId &&
      (node.symbol.role === "definition" ||
        node.symbol.role === "macro" ||
        node.symbol.role === "parameter")
    ) {
      const symbolRecord = symbolIndex?.get(node.symbol.symbolId);
      let importedMeta: ImportedSymbolTarget | undefined;
      if (symbolRecord) {
        if (node.symbol.role === "macro") {
          importedMeta =
            macroImports?.get(node.symbol.symbolId) ??
            flattenedImports?.get(symbolRecord.alias);
        } else if (node.symbol.role === "definition") {
          importedMeta = flattenedImports?.get(symbolRecord.alias);
        }
      }
      map.set(node.symbol.symbolId, {
        moduleId,
        span: node.span,
        ...(importedMeta ? { imported: importedMeta } : {}),
      });
    }
  }
  return map.size > 0 ? map : undefined;
};

const buildFlattenedImportIndex = (
  analysis: AnalyzeResult | undefined
): Map<string, ImportedSymbolTarget> | undefined => {
  if (!analysis) {
    return undefined;
  }
  const map = new Map<string, ImportedSymbolTarget>();
  for (const record of analysis.graph.imports ?? []) {
    if (!record?.flatten || !record.moduleId) {
      continue;
    }
    for (const binding of record.flatten) {
      if (!binding?.identifier) {
        continue;
      }
      map.set(binding.identifier, {
        moduleId: record.moduleId,
        exportedName: binding.exportedName,
        exportedIdentifier: binding.exportedIdentifier,
      });
    }
  }
  return map.size > 0 ? map : undefined;
};

const buildMacroImportIndex = (
  analysis: AnalyzeResult | undefined
): Map<SymbolId, ImportedSymbolTarget> | undefined => {
  if (!analysis) {
    return undefined;
  }
  const map = new Map<SymbolId, ImportedSymbolTarget>();
  for (const record of analysis.graph.imports ?? []) {
    if (!record?.macros || !record.moduleId) {
      continue;
    }
    for (const binding of record.macros) {
      map.set(binding.symbolId, {
        moduleId: record.moduleId,
        exportedName: binding.exportedName,
        exportedIdentifier: binding.exportedIdentifier,
      });
    }
  }
  return map.size > 0 ? map : undefined;
};

const collectScopeCompletions = (
  startScopeId: ScopeId,
  scopeIndex: Map<ScopeId, ScopeRecord>,
  symbolIndex: Map<SymbolId, SymbolRecord>
): CompletionItem[] => {
  const items: CompletionItem[] = [];
  const seen = new Set<string>();
  let currentScope: ScopeId | null | undefined = startScopeId;
  while (currentScope) {
    const scope = scopeIndex.get(currentScope);
    if (!scope) {
      break;
    }
    for (const symbolId of scope.symbols) {
      const record = symbolIndex.get(symbolId);
      if (!record) {
        continue;
      }
      if (seen.has(record.name)) {
        continue;
      }
      seen.add(record.name);
      items.push(symbolRecordToCompletion(record));
    }
    currentScope = scope.parentId;
  }
  return items;
};

const symbolKindToCompletionKind: Record<SymbolKind, CompletionItemKind> = {
  var: CompletionItemKind.Variable,
  parameter: CompletionItemKind.Variable,
  macro: CompletionItemKind.Function,
  builtin: CompletionItemKind.Function,
};

const symbolRecordToCompletion = (record: SymbolRecord): CompletionItem => {
  const detail = record.kind === "var" ? undefined : record.kind;
  return {
    label: record.name,
    kind:
      symbolKindToCompletionKind[record.kind] ?? CompletionItemKind.Variable,
    detail,
  } satisfies CompletionItem;
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

const isNamespaceSymbolChar = (value: string): boolean => {
  if (!value) {
    return false;
  }
  return namespaceSymbolCharRegex.test(value);
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
  if (
    position.line === span.start.line &&
    position.character < span.start.column
  ) {
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
