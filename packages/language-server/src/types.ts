/**
 * Core types for the Vibe Language Server.
 */
import type { Program, Span, Token } from "@vibe/syntax";
import type { SemanticModule, Type, TypeScheme } from "@vibe/semantics";
import type { Diagnostic } from "vscode-languageserver";

/**
 * Cached analysis result for a single document.
 */
export interface DocumentCache {
  /** Document URI */
  uri: string;
  /** Document version for cache invalidation */
  version: number;
  /** Document content */
  content: string;
  /** Tokenization result (fast, always available) */
  tokens?: Token[];
  /** Parse result (may have errors) */
  parseResult?: ParseResult;
  /** Semantic analysis result (may have errors) */
  semanticResult?: SemanticResult;
  /** Combined diagnostics from all phases */
  diagnostics: Diagnostic[];
  /** Timestamp of last analysis */
  lastAnalyzed: number;
}

/**
 * Result of parsing a document.
 */
export interface ParseResult {
  /** The parsed AST (may be partial if there are errors) */
  ast?: Program;
  /** Parse errors */
  errors: ParseErrorInfo[];
}

/**
 * Structured parse error information.
 */
export interface ParseErrorInfo {
  message: string;
  span: Span;
}

/**
 * Result of semantic analysis.
 */
export interface SemanticResult {
  /** Semantic module with type information */
  module?: SemanticModule;
  /** Semantic errors */
  errors: SemanticErrorInfo[];
}

/**
 * Structured semantic error information.
 */
export interface SemanticErrorInfo {
  message: string;
  span: Span;
}

/**
 * Symbol information for completion and go-to-definition.
 */
export interface SymbolInfo {
  /** Symbol name */
  name: string;
  /** Symbol kind (value, type, constructor, etc.) */
  kind: SymbolKind;
  /** Type information (if available) */
  type?: TypeScheme;
  /** Documentation/description */
  documentation?: string;
  /** Definition location */
  definitionSpan?: Span;
  /** Module where symbol is defined */
  moduleName?: string;
}

/**
 * Symbol kinds for categorization.
 */
export enum SymbolKind {
  Value = "value",
  Function = "function",
  Constructor = "constructor",
  Type = "type",
  TypeAlias = "typeAlias",
  OpaqueType = "opaqueType",
  Protocol = "protocol",
  TypeVariable = "typeVariable",
  Parameter = "parameter",
  Field = "field",
  Operator = "operator",
  Module = "module",
}

/**
 * Hover information at a position.
 */
export interface HoverInfo {
  /** The symbol or expression at the position */
  name: string;
  /** Type information formatted as string */
  type?: string;
  /** Additional documentation */
  documentation?: string;
  /** Span of the hovered element */
  span: Span;
}

/**
 * Location in the source code.
 */
export interface SourceLocation {
  uri: string;
  span: Span;
}

/**
 * Project-wide analysis context.
 */
export interface ProjectContext {
  /** Root URI of the project */
  rootUri: string;
  /** All known modules */
  modules: Map<string, ModuleInfo>;
  /** Cross-module symbol index */
  symbolIndex: Map<string, SymbolInfo[]>;
  /** Module dependency graph */
  dependencies: Map<string, Set<string>>;
}

/**
 * Information about a module in the project.
 */
export interface ModuleInfo {
  /** Module name (e.g., "MyModule.Submodule") */
  name: string;
  /** File URI */
  uri: string;
  /** Document cache */
  cache?: DocumentCache;
  /** Exported symbols */
  exports: Map<string, SymbolInfo>;
  /** Imported modules */
  imports: string[];
}

/**
 * Configuration for the language server.
 */
export interface ServerConfig {
  /** Enable semantic tokens */
  semanticTokens: boolean;
  /** Enable code completion */
  completion: boolean;
  /** Enable hover information */
  hover: boolean;
  /** Enable go-to-definition */
  goToDefinition: boolean;
  /** Enable diagnostics */
  diagnostics: boolean;
  /** Maximum number of diagnostics per file */
  maxDiagnostics: number;
  /** Debounce delay for analysis (ms) */
  debounceDelay: number;
}

/**
 * Default server configuration.
 */
export const DEFAULT_CONFIG: ServerConfig = {
  semanticTokens: true,
  completion: true,
  hover: true,
  goToDefinition: true,
  diagnostics: true,
  maxDiagnostics: 100,
  debounceDelay: 150,
};
