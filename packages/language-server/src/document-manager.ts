/**
 * Document caching and analysis management.
 *
 * Implements module-level caching with dependency tracking.
 */
import type { TextDocument } from "vscode-languageserver-textdocument";
import {
  Diagnostic,
  DiagnosticSeverity,
  Position,
} from "vscode-languageserver";
import type { Span, Token, Program } from "@vibe/syntax";
import { lex, LexError } from "@vibe/lexer";
import { parseWithInfix, ParseError } from "@vibe/parser";
import {
  analyze,
  SemanticError,
  type SemanticModule,
  type TypeScheme,
  type Type,
  type AnalyzeOptions,
} from "@vibe/semantics";
import { resolveModule, type ResolvedVibeConfig } from "@vibe/module-resolver";
import { loadConfig } from "@vibe/config";
import type {
  DocumentCache,
  ParseResult,
  SemanticResult,
  ParseErrorInfo,
  SemanticErrorInfo,
  SymbolInfo,
  ModuleInfo,
  ProjectContext,
} from "./types.js";
import { SymbolKind } from "./types.js";
import * as fs from "node:fs";
import * as path from "node:path";

/**
 * Document manager with multi-level caching and dependency tracking.
 */
export class DocumentManager {
  /** Per-document caches */
  private documents = new Map<string, DocumentCache>();

  /** Project-wide context */
  private projectContext: ProjectContext | null = null;

  /** Pending analysis debounce timers */
  private pendingAnalysis = new Map<string, NodeJS.Timeout>();

  /** Debounce delay in ms */
  private debounceDelay = 150;

  /** Cached prelude semantic module */
  private preludeModule: SemanticModule | null = null;

  /** Flag to indicate prelude loading has been attempted */
  private preludeLoadAttempted = false;

  /**
   * Update a document and trigger reanalysis.
   */
  updateDocument(document: TextDocument): DocumentCache {
    const uri = document.uri;
    const version = document.version;
    const content = document.getText();

    // Check if we already have this version cached
    const existing = this.documents.get(uri);
    if (existing && existing.version === version) {
      return existing;
    }

    // Create new cache entry
    const cache: DocumentCache = {
      uri,
      version,
      content,
      diagnostics: [],
      lastAnalyzed: Date.now(),
    };

    this.documents.set(uri, cache);

    // Run analysis (synchronously for now, can be debounced)
    this.analyzeDocument(cache);

    return cache;
  }

  /**
   * Get cached document data.
   */
  getDocument(uri: string): DocumentCache | undefined {
    return this.documents.get(uri);
  }

  /**
   * Remove a document from the cache.
   */
  removeDocument(uri: string): void {
    this.documents.delete(uri);
    // Clear any pending analysis
    const timer = this.pendingAnalysis.get(uri);
    if (timer) {
      clearTimeout(timer);
      this.pendingAnalysis.delete(uri);
    }
  }

  /**
   * Get all cached documents.
   */
  getAllDocuments(): DocumentCache[] {
    return Array.from(this.documents.values());
  }

  /**
   * Try to load and analyze the Vibe prelude module.
   * This is called lazily on the first document analysis.
   */
  private loadPrelude(documentUri: string): void {
    if (this.preludeLoadAttempted) {
      return;
    }
    this.preludeLoadAttempted = true;

    try {
      // Extract file path from URI
      const filePath = documentUri.startsWith("file://")
        ? decodeURIComponent(documentUri.slice(7))
        : documentUri;

      // Find the workspace root by looking for vibe.json or package.json
      let dir = path.dirname(filePath);
      let config: ResolvedVibeConfig | null = null;

      while (dir && dir !== path.dirname(dir)) {
        const vibeConfigPath = path.join(dir, "vibe.json");
        const packageJsonPath = path.join(dir, "package.json");

        if (fs.existsSync(vibeConfigPath)) {
          config = loadConfig({ path: vibeConfigPath });
          break;
        }

        // Check if package.json has vibe config
        if (fs.existsSync(packageJsonPath)) {
          try {
            const pkgJson = JSON.parse(
              fs.readFileSync(packageJsonPath, "utf8")
            );
            if (pkgJson.vibe) {
              config = loadConfig({ path: packageJsonPath });
              break;
            }
          } catch {
            // Ignore parse errors
          }
        }

        dir = path.dirname(dir);
      }

      if (!config) {
        // No config found, try to find prelude relative to the workspace
        return;
      }

      // Try to resolve the Vibe module
      const resolved = resolveModule({
        config,
        moduleName: "Vibe",
        preferDist: false,
      });

      const preludeSource = fs.readFileSync(resolved.filePath, "utf8");

      // Parse the prelude
      const { program: preludeAst } = parseWithInfix(preludeSource);

      // Analyze the prelude (without prelude injection since it IS the prelude)
      this.preludeModule = analyze(preludeAst, {
        injectPrelude: false,
      });
    } catch {
      // Silently fail - prelude is optional for LSP
      // Errors will show up as "symbol not found" diagnostics
    }
  }

  /**
   * Analyze a document through all compilation phases.
   */
  private analyzeDocument(cache: DocumentCache): void {
    // Try to load prelude on first analysis
    this.loadPrelude(cache.uri);

    const diagnostics: Diagnostic[] = [];

    // Phase 1: Tokenization
    try {
      cache.tokens = lex(cache.content);
    } catch (error) {
      if (error instanceof LexError) {
        diagnostics.push(
          this.createDiagnostic(
            error.message,
            error.span,
            DiagnosticSeverity.Error
          )
        );
      } else {
        diagnostics.push(
          this.createDiagnostic(
            `Lexer error: ${String(error)}`,
            this.defaultSpan(),
            DiagnosticSeverity.Error
          )
        );
      }
      cache.diagnostics = diagnostics;
      return;
    }

    // Phase 2: Parsing
    try {
      const { program } = parseWithInfix(cache.content);
      cache.parseResult = { ast: program, errors: [] };
    } catch (error) {
      const parseError = this.extractParseError(error);
      cache.parseResult = { errors: [parseError] };
      diagnostics.push(
        this.createDiagnostic(
          parseError.message,
          parseError.span,
          DiagnosticSeverity.Error
        )
      );
      cache.diagnostics = diagnostics;
      return;
    }

    // Phase 3: Semantic Analysis
    if (cache.parseResult?.ast) {
      try {
        // Check if this is the Vibe prelude itself
        const isPreludeModule = cache.parseResult.ast.module?.name === "Vibe";

        // Build dependencies map with prelude if available
        const dependencies = new Map<string, SemanticModule>();
        if (this.preludeModule && !isPreludeModule) {
          dependencies.set("Vibe", this.preludeModule);
        }

        // Build analysis options
        const analyzeOptions: AnalyzeOptions = {
          // Enable prelude injection if we have the prelude loaded
          injectPrelude: this.preludeModule !== null && !isPreludeModule,
          dependencies,
        };

        const semanticModule = analyze(cache.parseResult.ast, analyzeOptions);
        cache.semanticResult = { module: semanticModule, errors: [] };
      } catch (error) {
        if (error instanceof SemanticError) {
          cache.semanticResult = {
            errors: [{ message: error.message, span: error.span }],
          };
          diagnostics.push(
            this.createDiagnostic(
              error.message,
              error.span,
              DiagnosticSeverity.Error
            )
          );
        } else {
          const msg = error instanceof Error ? error.message : String(error);
          cache.semanticResult = {
            errors: [{ message: msg, span: this.defaultSpan() }],
          };
          diagnostics.push(
            this.createDiagnostic(
              `Semantic error: ${msg}`,
              this.defaultSpan(),
              DiagnosticSeverity.Error
            )
          );
        }
      }
    }

    cache.diagnostics = diagnostics;
    cache.lastAnalyzed = Date.now();
  }

  /**
   * Create an LSP diagnostic from our error info.
   */
  private createDiagnostic(
    message: string,
    span: Span,
    severity: DiagnosticSeverity
  ): Diagnostic {
    return {
      severity,
      range: {
        start: this.spanToPosition(span.start),
        end: this.spanToPosition(span.end),
      },
      message,
      source: "vibe",
    };
  }

  /**
   * Convert our position to LSP position (0-based).
   */
  private spanToPosition(pos: { line: number; column: number }): Position {
    return {
      line: Math.max(0, pos.line - 1), // Our positions are 1-based
      character: Math.max(0, pos.column - 1),
    };
  }

  /**
   * Extract parse error information.
   */
  private extractParseError(error: unknown): ParseErrorInfo {
    if (error instanceof Error) {
      // Try to extract span from error message (common pattern: "at line X, column Y")
      const lineMatch = error.message.match(/line (\d+)/i);
      const colMatch = error.message.match(/column (\d+)/i);
      const line = lineMatch && lineMatch[1] ? parseInt(lineMatch[1], 10) : 1;
      const column = colMatch && colMatch[1] ? parseInt(colMatch[1], 10) : 1;

      return {
        message: error.message,
        span: {
          start: { offset: 0, line, column },
          end: { offset: 0, line, column: column + 1 },
        },
      };
    }
    return {
      message: String(error),
      span: this.defaultSpan(),
    };
  }

  /**
   * Default span for errors without location info.
   */
  private defaultSpan(): Span {
    return {
      start: { offset: 0, line: 1, column: 1 },
      end: { offset: 0, line: 1, column: 1 },
    };
  }

  /**
   * Get symbols available at a position in a document.
   */
  getSymbolsAtPosition(uri: string): SymbolInfo[] {
    const cache = this.documents.get(uri);
    if (!cache?.semanticResult?.module) {
      return [];
    }

    const symbols: SymbolInfo[] = [];
    const module = cache.semanticResult.module;

    // Add values
    for (const [name, valueInfo] of Object.entries(module.values)) {
      const typeScheme = module.typeSchemes[name];
      symbols.push({
        name,
        kind: this.isFunction(typeScheme)
          ? SymbolKind.Function
          : SymbolKind.Value,
        type: typeScheme,
        definitionSpan: valueInfo.declaration.span,
      });
    }

    // Add types (ADTs)
    for (const [name, adtInfo] of Object.entries(module.adts)) {
      symbols.push({
        name,
        kind: SymbolKind.Type,
        documentation: `type ${name}${
          adtInfo.params.length > 0 ? " " + adtInfo.params.join(" ") : ""
        }`,
        definitionSpan: adtInfo.span,
        moduleName: adtInfo.moduleName,
      });

      // Add constructors
      for (const ctorName of adtInfo.constructors) {
        const ctorInfo = module.constructors[ctorName];
        const ctorType = module.constructorTypes[ctorName];
        if (ctorInfo) {
          symbols.push({
            name: ctorName,
            kind: SymbolKind.Constructor,
            type: ctorType,
            definitionSpan: ctorInfo.span,
            moduleName: ctorInfo.moduleName,
          });
        }
      }
    }

    // Add type aliases
    for (const [name, aliasInfo] of Object.entries(module.typeAliases)) {
      symbols.push({
        name,
        kind: SymbolKind.TypeAlias,
        definitionSpan: aliasInfo.span,
        moduleName: aliasInfo.moduleName,
      });
    }

    // Add protocols
    for (const [name, protocolInfo] of Object.entries(module.protocols)) {
      symbols.push({
        name,
        kind: SymbolKind.Protocol,
        documentation: `protocol ${name} ${protocolInfo.params.join(" ")}`,
        definitionSpan: protocolInfo.span,
        moduleName: protocolInfo.moduleName,
      });
    }

    return symbols;
  }

  /**
   * Check if a type scheme represents a function.
   */
  private isFunction(scheme?: TypeScheme): boolean {
    if (!scheme) return false;
    return scheme.type.kind === "fun";
  }

  /**
   * Find definition of symbol at position.
   */
  findDefinition(
    uri: string,
    line: number,
    character: number
  ): { uri: string; span: Span } | undefined {
    const cache = this.documents.get(uri);
    if (!cache?.parseResult?.ast) {
      return undefined;
    }

    // Convert LSP position (0-based) to our position (1-based)
    const targetLine = line + 1;
    const targetColumn = character + 1;

    // Find the token/node at this position
    const name = this.findIdentifierAtPosition(
      cache.content,
      targetLine,
      targetColumn
    );
    if (!name) {
      return undefined;
    }

    // Look up the symbol
    const symbols = this.getSymbolsAtPosition(uri);
    const symbol = symbols.find((s) => s.name === name);

    if (symbol?.definitionSpan) {
      return { uri, span: symbol.definitionSpan };
    }

    return undefined;
  }

  /**
   * Find identifier at a position in source.
   */
  private findIdentifierAtPosition(
    content: string,
    line: number,
    column: number
  ): string | undefined {
    const lines = content.split("\n");
    if (line < 1 || line > lines.length) {
      return undefined;
    }

    const lineText = lines[line - 1];
    if (!lineText || column < 1 || column > lineText.length + 1) {
      return undefined;
    }

    // Find identifier boundaries
    const idx = column - 1;
    let start = idx;
    let end = idx;

    // Scan backwards
    while (start > 0) {
      const char = lineText[start - 1];
      if (!char || !/[a-zA-Z0-9_']/.test(char)) break;
      start--;
    }

    // Scan forwards
    while (end < lineText.length) {
      const char = lineText[end];
      if (!char || !/[a-zA-Z0-9_']/.test(char)) break;
      end++;
    }

    if (start === end) {
      return undefined;
    }

    return lineText.slice(start, end);
  }

  /**
   * Get hover information at position.
   */
  getHoverInfo(
    uri: string,
    line: number,
    character: number
  ): { type: string; name: string; span: Span } | undefined {
    const cache = this.documents.get(uri);
    if (!cache?.semanticResult?.module) {
      return undefined;
    }

    const targetLine = line + 1;
    const targetColumn = character + 1;

    const name = this.findIdentifierAtPosition(
      cache.content,
      targetLine,
      targetColumn
    );
    if (!name) {
      return undefined;
    }

    const module = cache.semanticResult.module;

    // Check values
    if (module.typeSchemes[name]) {
      const typeStr = this.formatTypeScheme(module.typeSchemes[name]);
      return {
        name,
        type: typeStr,
        span: module.values[name]?.declaration.span || this.defaultSpan(),
      };
    }

    // Check constructors
    if (module.constructorTypes[name]) {
      const typeStr = this.formatTypeScheme(module.constructorTypes[name]);
      return {
        name,
        type: typeStr,
        span: module.constructors[name]?.span || this.defaultSpan(),
      };
    }

    // Check types
    if (module.adts[name]) {
      const adt = module.adts[name];
      return {
        name,
        type: `type ${name}${
          adt.params.length > 0 ? " " + adt.params.join(" ") : ""
        }`,
        span: adt.span,
      };
    }

    // Check type aliases
    if (module.typeAliases[name]) {
      const alias = module.typeAliases[name];
      return {
        name,
        type: `type alias ${name}${
          alias.params.length > 0 ? " " + alias.params.join(" ") : ""
        }`,
        span: alias.span,
      };
    }

    // Check protocols
    if (module.protocols[name]) {
      const protocol = module.protocols[name];
      return {
        name,
        type: `protocol ${name} ${protocol.params.join(" ")}`,
        span: protocol.span,
      };
    }

    return undefined;
  }

  /**
   * Format a type scheme as a readable string.
   */
  formatTypeScheme(scheme: TypeScheme): string {
    const constraints = this.formatConstraints(scheme.constraints);
    const typeStr = this.formatType(scheme.type);

    if (constraints) {
      return `${constraints} => ${typeStr}`;
    }
    return typeStr;
  }

  /**
   * Format constraints.
   */
  private formatConstraints(
    constraints: Array<{ protocolName: string; typeArgs: Type[] }>
  ): string {
    if (constraints.length === 0) return "";
    const parts = constraints.map((c) => {
      const args = c.typeArgs.map((t) => this.formatType(t)).join(" ");
      return `${c.protocolName} ${args}`;
    });
    if (parts.length === 1) return parts[0] ?? "";
    return `(${parts.join(", ")})`;
  }

  /**
   * Format a type as a readable string.
   */
  formatType(type: Type): string {
    switch (type.kind) {
      case "var":
        return this.typeVarName(type.id);
      case "con":
        if (type.args.length === 0) {
          return type.name;
        }
        return `${type.name} ${type.args
          .map((a) => this.formatTypeArg(a))
          .join(" ")}`;
      case "fun": {
        const from = this.formatTypeArg(type.from);
        const to = this.formatType(type.to);
        return `${from} -> ${to}`;
      }
      case "tuple":
        return `(${type.elements.map((e) => this.formatType(e)).join(", ")})`;
      case "record": {
        const fields = Object.entries(type.fields)
          .map(([k, v]) => `${k} : ${this.formatType(v)}`)
          .join(", ");
        return `{ ${fields} }`;
      }
    }
  }

  /**
   * Format a type argument (with parens if needed).
   */
  private formatTypeArg(type: Type): string {
    if (type.kind === "fun" || (type.kind === "con" && type.args.length > 0)) {
      return `(${this.formatType(type)})`;
    }
    return this.formatType(type);
  }

  /**
   * Generate type variable name from ID.
   */
  private typeVarName(id: number): string {
    const letters = "abcdefghijklmnopqrstuvwxyz";
    if (id < letters.length) {
      return letters[id] ?? `t${id}`;
    }
    return `t${id}`;
  }
}
