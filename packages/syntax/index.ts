export enum TokenType {
  LeftParen = "left_paren",
  RightParen = "right_paren",
  LeftBracket = "left_bracket",
  RightBracket = "right_bracket",
  LeftBrace = "left_brace",
  RightBrace = "right_brace",
  Quote = "quote",
  SyntaxQuote = "syntax_quote",
  Unquote = "unquote",
  UnquoteSplicing = "unquote_splicing",
  Dispatch = "dispatch",
  Number = "number",
  String = "string",
  Character = "character",
  Symbol = "symbol",
  Keyword = "keyword",
  Boolean = "boolean",
  Nil = "nil",
}

export type TokenValue = string | number | boolean | null | undefined;

export interface SourcePosition {
  offset: number;
  line: number;
  column: number;
}

export interface SourceSpan {
  start: SourcePosition;
  end: SourcePosition;
}

export interface Token {
  kind: TokenType;
  lexeme: string;
  span: SourceSpan;
  value?: TokenValue;
}

export type ScopeId = `scope_${number}`;

export enum DiagnosticSeverity {
  Info = "info",
  Warning = "warning",
  Error = "error",
}

export interface Diagnostic {
  message: string;
  span: SourceSpan;
  severity: DiagnosticSeverity;
  code?: string;
}

export type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

export const ok = <T>(value: T): Result<T, never> => ({ ok: true, value });
export const err = <E>(error: E): Result<never, E> => ({ ok: false, error });

export const createPosition = (
  offset: number,
  line: number,
  column: number
): SourcePosition => ({
  offset,
  line,
  column,
});

export const clonePosition = (position: SourcePosition): SourcePosition => ({
  offset: position.offset,
  line: position.line,
  column: position.column,
});

export const createSpan = (
  start: SourcePosition,
  end: SourcePosition
): SourceSpan => ({
  start,
  end,
});

export enum NodeKind {
  Program = "program",
  List = "list",
  Vector = "vector",
  Map = "map",
  MapEntry = "map_entry",
  Set = "set",
  Quote = "quote",
  SyntaxQuote = "syntax_quote",
  Unquote = "unquote",
  UnquoteSplicing = "unquote_splicing",
  Dispatch = "dispatch",
  Symbol = "symbol",
  Keyword = "keyword",
  Number = "number",
  String = "string",
  Character = "character",
  Boolean = "boolean",
  Nil = "nil",
  NamespaceImport = "namespace_import",
}

export const BUILTIN_SYMBOLS = [
  // Special forms (required even without user-provided definitions)
  "def",
  "defp",
  "macro",
  "let",
  "fn",
  "if",
  "quote",
  "do",
  "try",
  "throw",
  "require",
  "external",
  "import",
  "gensym",
  "spread",
] as const;

export interface AstNode<K extends NodeKind = NodeKind> {
  readonly kind: K;
  readonly span: SourceSpan;
  readonly scopeId?: ScopeId;
}

export interface ProgramNode extends AstNode<NodeKind.Program> {
  readonly body: readonly ExpressionNode[];
}

export type SequenceNodeKind = NodeKind.List | NodeKind.Vector | NodeKind.Set;

export interface SequenceNode<K extends SequenceNodeKind> extends AstNode<K> {
  readonly elements: readonly ExpressionNode[];
}

export type ListNode = SequenceNode<NodeKind.List>;
export type VectorNode = SequenceNode<NodeKind.Vector>;
export type SetNode = SequenceNode<NodeKind.Set>;

export interface MapEntryNode extends AstNode<NodeKind.MapEntry> {
  readonly key: ExpressionNode | null;
  readonly value: ExpressionNode | null;
}

export interface MapNode extends AstNode<NodeKind.Map> {
  readonly entries: readonly MapEntryNode[];
}

export type ReaderMacroKind =
  | NodeKind.Quote
  | NodeKind.SyntaxQuote
  | NodeKind.Unquote
  | NodeKind.UnquoteSplicing;

export interface ReaderMacroNode<K extends ReaderMacroKind = ReaderMacroKind>
  extends AstNode<K> {
  readonly target: ExpressionNode | null;
}

export type AtomKind =
  | NodeKind.Symbol
  | NodeKind.Keyword
  | NodeKind.Number
  | NodeKind.String
  | NodeKind.Character
  | NodeKind.Boolean
  | NodeKind.Nil;

interface AtomNodeBase<K extends AtomKind, V> extends AstNode<K> {
  readonly lexeme: string;
  readonly value: V;
}

export type SymbolNode = AtomNodeBase<NodeKind.Symbol, string>;
export type KeywordNode = AtomNodeBase<NodeKind.Keyword, string>;
export type NumberNode = AtomNodeBase<NodeKind.Number, number>;
export type StringNode = AtomNodeBase<NodeKind.String, string>;
export type CharacterNode = AtomNodeBase<NodeKind.Character, string>;
export type BooleanNode = AtomNodeBase<NodeKind.Boolean, boolean>;
export type NilNode = AtomNodeBase<NodeKind.Nil, null>;

export interface DispatchNode extends AstNode<NodeKind.Dispatch> {
  readonly target: ExpressionNode | null;
}

export type NamespaceImportKind = "require" | "external" | "import";

export interface NamespaceImportNode extends AstNode<NodeKind.NamespaceImport> {
  readonly importKind: NamespaceImportKind;
  readonly head: SymbolNode | null;
  readonly alias: ExpressionNode | null;
  readonly source: ExpressionNode | null;
  readonly elements: readonly ExpressionNode[];
}

export type AtomNode =
  | SymbolNode
  | KeywordNode
  | NumberNode
  | StringNode
  | CharacterNode
  | BooleanNode
  | NilNode;

export type ExpressionNode =
  | ListNode
  | VectorNode
  | MapNode
  | SetNode
  | ReaderMacroNode
  | DispatchNode
  | NamespaceImportNode
  | AtomNode;

export * from "./destructuring";
