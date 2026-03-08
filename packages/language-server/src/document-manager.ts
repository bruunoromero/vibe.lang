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
  MultipleSemanticErrors,
  formatTypeSchemeForDisplay,
  formatTypeForDisplay,
  buildNormalizedNames,
  type SemanticModule,
  type TypeScheme,
  type Type,
  type AnalyzeOptions,
} from "@vibe/semantics";
import { resolveModule, discoverSourceModules } from "@vibe/module-resolver";
import { loadConfig, type ResolvedVibeConfig } from "@vibe/config";
import type {
  DocumentCache,
  ParseResult,
  SemanticResult,
  ParseErrorInfo,
  SemanticErrorInfo,
  SymbolInfo,
  ModuleInfo,
  ProjectContext,
} from "./types";
import { SymbolKind } from "./types";
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

  /** Cached loaded modules by name */
  private loadedModules = new Map<string, SemanticModule>();

  /** Cached project configs keyed by the absolute path of the config file */
  private configCache = new Map<string, ResolvedVibeConfig>();

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
   * Resolve the project config for a given document URI.
   * Walks up from the document's directory looking for vibe.json or package.json with a vibe key.
   * Results are cached by the config file path so repeated lookups are cheap.
   */
  private resolveConfigForDocument(
    documentUri: string,
  ): ResolvedVibeConfig | null {
    try {
      const filePath = documentUri.startsWith("file://")
        ? decodeURIComponent(documentUri.slice(7))
        : documentUri;

      let dir = path.dirname(filePath);

      while (dir && dir !== path.dirname(dir)) {
        const vibeConfigPath = path.join(dir, "vibe.json");
        const packageJsonPath = path.join(dir, "package.json");

        if (fs.existsSync(vibeConfigPath)) {
          const cached = this.configCache.get(vibeConfigPath);
          if (cached) return cached;
          const config = loadConfig({ path: vibeConfigPath });
          this.configCache.set(vibeConfigPath, config);
          return config;
        }

        if (fs.existsSync(packageJsonPath)) {
          try {
            const pkgJson = JSON.parse(
              fs.readFileSync(packageJsonPath, "utf8"),
            );
            if (pkgJson.vibe) {
              const cached = this.configCache.get(packageJsonPath);
              if (cached) return cached;
              const config = loadConfig({ path: packageJsonPath });
              this.configCache.set(packageJsonPath, config);
              return config;
            }
          } catch {
            // Ignore parse errors
          }
        }

        dir = path.dirname(dir);
      }
    } catch {
      // Config not found, module loading will be limited
    }
    return null;
  }

  /**
   * Load a module from disk and analyze it.
   * Recursively loads the module's own imports first.
   * Returns null if the module cannot be loaded.
   */
  private loadModule(
    moduleName: string,
    config: ResolvedVibeConfig,
  ): SemanticModule | null {
    // Check if already cached
    if (this.loadedModules.has(moduleName)) {
      return this.loadedModules.get(moduleName) || null;
    }

    try {
      // Resolve the module
      const resolved = resolveModule({
        config,
        moduleName,
        preferDist: false,
      });

      const source = fs.readFileSync(resolved.filePath, "utf8");
      const { program } = parseWithInfix(source);

      // Recursively load this module's own imports first
      const moduleDeps = new Map<string, SemanticModule>();
      if (program.imports) {
        for (const imp of program.imports) {
          if (!this.loadedModules.has(imp.moduleName)) {
            const depModule = this.loadModule(imp.moduleName, config);
            if (depModule) {
              moduleDeps.set(imp.moduleName, depModule);
            }
          } else {
            const cachedModule = this.loadedModules.get(imp.moduleName);
            if (cachedModule) {
              moduleDeps.set(imp.moduleName, cachedModule);
            }
          }
        }
      }

      // Include all previously loaded modules so the semantics engine's
      // global instance visibility can find instances (e.g. Eq Int) from
      // modules that aren't in the direct import chain.  This mirrors the
      // CLI where the full analyzedModules map is passed to every module.
      for (const [name, mod] of this.loadedModules) {
        if (!moduleDeps.has(name)) {
          moduleDeps.set(name, mod);
        }
      }

      // Analyze with the loaded dependencies
      const analyzed = analyze(program, {
        dependencies: moduleDeps,
        fileContext: {
          filePath: resolved.filePath,
          srcDir: resolved.srcDir,
        },
      });

      // Cache it
      this.loadedModules.set(moduleName, analyzed);
      return analyzed;
    } catch {
      // Module could not be loaded, return null
      return null;
    }
  }

  /**
   * Recursively load all dependencies from an AST.
   */
  private loadDependencies(
    ast: Program,
    config: ResolvedVibeConfig,
  ): Map<string, SemanticModule> {
    const dependencies = new Map<string, SemanticModule>();

    if (ast.imports) {
      for (const imp of ast.imports) {
        if (dependencies.has(imp.moduleName)) continue;

        // Prefer already-loaded (cached) modules to avoid redundant disk I/O
        const cached = this.loadedModules.get(imp.moduleName);
        if (cached) {
          dependencies.set(imp.moduleName, cached);
        } else {
          const loaded = this.loadModule(imp.moduleName, config);
          if (loaded) {
            dependencies.set(imp.moduleName, loaded);
          }
        }
      }
    }

    // Discover ALL source modules from the current package and every
    // dependency package so that protocol instances (e.g. Eq Int from
    // Vibe.Int) are visible even when not transitively imported.
    // Semantics already has "global instance visibility" logic that
    // pulls instances from every entry in the dependencies map.
    const currentModule = ast.module?.name;
    const allSourceModules = this.discoverAllPackageModules(config);

    // Multiple passes: modules may depend on sibling instances that
    // aren't loaded yet (e.g. Dict needs Eq Int from Int).  On the
    // first pass some may fail; on the second pass their dependencies
    // are cached and loadModule will find them.
    let pending = allSourceModules.filter(
      (m) => m !== currentModule && !dependencies.has(m),
    );
    for (let pass = 0; pass < 2 && pending.length > 0; pass++) {
      const stillPending: string[] = [];
      for (const moduleName of pending) {
        if (dependencies.has(moduleName)) continue;
        const cached = this.loadedModules.get(moduleName);
        if (cached) {
          dependencies.set(moduleName, cached);
        } else {
          const loaded = this.loadModule(moduleName, config);
          if (loaded) {
            dependencies.set(moduleName, loaded);
          } else {
            stillPending.push(moduleName);
          }
        }
      }
      pending = stillPending;
    }

    return dependencies;
  }

  /**
   * Discover all source module names from the current package and its
   * dependency packages listed in the vibe.json `packages` array.
   */
  private discoverAllPackageModules(config: ResolvedVibeConfig): string[] {
    const seen = new Set<string>();
    const result: string[] = [];

    const addModules = (srcDir: string) => {
      for (const name of discoverSourceModules(srcDir)) {
        if (!seen.has(name)) {
          seen.add(name);
          result.push(name);
        }
      }
    };

    // Current package
    addModules(config.srcDir);

    // Dependency packages
    for (const pkgName of config.packages) {
      try {
        const pkgRoot = this.resolvePackageRoot(pkgName, config.rootDir);
        if (!pkgRoot) continue;
        const pkgConfigPath = path.join(pkgRoot, "vibe.json");
        if (!fs.existsSync(pkgConfigPath)) continue;
        const pkgConfig = loadConfig({ path: pkgConfigPath });
        addModules(pkgConfig.srcDir);
      } catch {
        // Skip unresolvable packages
      }
    }

    return result;
  }

  /**
   * Resolve the root directory of an npm/workspace package by walking up
   * from startDir looking in node_modules or the workspace packages folder.
   */
  private resolvePackageRoot(pkgName: string, startDir: string): string | null {
    let dir = path.resolve(startDir);
    while (true) {
      const candidate = path.join(dir, "node_modules", pkgName);
      if (fs.existsSync(path.join(candidate, "package.json"))) {
        return fs.realpathSync(candidate);
      }
      const parent = path.dirname(dir);
      if (parent === dir) break;
      dir = parent;
    }
    return null;
  }

  /**
   * Analyze a document through all compilation phases.
   */
  private analyzeDocument(cache: DocumentCache): void {
    const config = this.resolveConfigForDocument(cache.uri);

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
            DiagnosticSeverity.Error,
          ),
        );
      } else {
        diagnostics.push(
          this.createDiagnostic(
            `Lexer error: ${String(error)}`,
            this.defaultSpan(),
            DiagnosticSeverity.Error,
          ),
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
          DiagnosticSeverity.Error,
        ),
      );
      cache.diagnostics = diagnostics;
      return;
    }

    // Phase 3: Semantic Analysis
    if (cache.parseResult?.ast) {
      try {
        // Load all dependencies for this module (requires config for module resolution)
        const dependencies = config
          ? this.loadDependencies(cache.parseResult.ast, config)
          : new Map<string, SemanticModule>();

        // Convert URI to file path for semantic analysis
        const docFilePath = cache.uri.startsWith("file://")
          ? decodeURIComponent(cache.uri.slice(7))
          : cache.uri;

        // Build analysis options
        const analyzeOptions: AnalyzeOptions = {
          dependencies,
          fileContext: {
            filePath: docFilePath,
            srcDir: config?.srcDir || "",
          },
        };

        const semanticModule = analyze(cache.parseResult.ast, analyzeOptions);
        cache.semanticResult = { module: semanticModule, errors: [] };
      } catch (error) {
        if (error instanceof MultipleSemanticErrors) {
          // Handle accumulated semantic errors - create a diagnostic for each
          const errorInfos = error.errors.map((e) => ({
            message: e.message,
            span: e.span,
          }));
          cache.semanticResult = { errors: errorInfos };
          for (const e of error.errors) {
            diagnostics.push(
              this.createDiagnostic(
                e.message,
                e.span,
                DiagnosticSeverity.Error,
              ),
            );
          }
        } else if (error instanceof SemanticError) {
          cache.semanticResult = {
            errors: [{ message: error.message, span: error.span }],
          };
          diagnostics.push(
            this.createDiagnostic(
              error.message,
              error.span,
              DiagnosticSeverity.Error,
            ),
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
              DiagnosticSeverity.Error,
            ),
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
    severity: DiagnosticSeverity,
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
    character: number,
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
      targetColumn,
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
    column: number,
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
    character: number,
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
      targetColumn,
    );
    if (!name) {
      return undefined;
    }

    const module = cache.semanticResult.module;

    // Check values
    if (module.typeSchemes[name]) {
      const typeStr = this.formatTypeScheme(module.typeSchemes[name]);
      const valueEntry = module.values[name];

      // If not a regular value, might be a protocol method - try to infer concrete type
      if (!valueEntry && module.instances.length > 0) {
        const concreteType = this.inferProtocolMethodType(name, module);
        if (concreteType) {
          return {
            name,
            type: concreteType,
            span: this.defaultSpan(),
          };
        }
      }

      return {
        name,
        type: typeStr,
        span: valueEntry?.declaration?.span || this.defaultSpan(),
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
   * Uses annotation param names when available, otherwise normalizes to a, b, c...
   */
  formatTypeScheme(scheme: TypeScheme): string {
    return formatTypeSchemeForDisplay(scheme);
  }

  /**
   * Infer the concrete type of a protocol method by checking instances.
   * For a method like toString, look for instances that define it
   * and return the first matching concrete type.
   */
  private inferProtocolMethodType(
    methodName: string,
    module: SemanticModule,
  ): string | undefined {
    // Look through all instances to find one that defines this method
    for (const instance of module.instances) {
      const protocol = module.protocols[instance.protocolName];
      if (!protocol) continue;

      // Check if this protocol has this method
      const methodInfo = protocol.methods.get(methodName);
      if (methodInfo) {
        // Build a temporary scheme to get normalized names
        const tempScheme: TypeScheme = {
          vars: new Set<number>(),
          constraints: [],
          type: methodInfo.type,
        };
        const names = buildNormalizedNames(tempScheme);
        return formatTypeForDisplay(methodInfo.type, names);
      }
    }

    return undefined;
  }

  /**
   * Format a bare type with normalized variable names.
   */
  formatType(type: Type): string {
    if (!type) {
      return "<unknown>";
    }
    const tempScheme: TypeScheme = {
      vars: new Set<number>(),
      constraints: [],
      type,
    };
    const names = buildNormalizedNames(tempScheme);
    return formatTypeForDisplay(type, names);
  }
}
