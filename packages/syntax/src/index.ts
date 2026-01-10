export type Position = {
  /** Byte offset from the start of the source. */
  offset: number;
  /** 1-based line number. */
  line: number;
  /** 1-based column number. */
  column: number;
};

export type Span = {
  start: Position;
  end: Position;
};

export enum TokenKind {
  Keyword = "Keyword",
  LowerIdentifier = "LowerIdentifier",
  UpperIdentifier = "UpperIdentifier",
  Number = "Number",
  String = "String",
  Char = "Char",
  Operator = "Operator",
  LParen = "LParen",
  RParen = "RParen",
  LBrace = "LBrace",
  RBrace = "RBrace",
  LBracket = "LBracket",
  RBracket = "RBracket",
  Comma = "Comma",
  Dot = "Dot",
  Range = "Range",
  Colon = "Colon",
  Equals = "Equals",
  Pipe = "Pipe",
  Backslash = "Backslash",
  Eof = "Eof",
}

export type Token = {
  kind: TokenKind;
  lexeme: string;
  span: Span;
};

export const KEYWORDS = [
  "if",
  "then",
  "else",
  "let",
  "in",
  "case",
  "of",
  "type",
  "alias",
  "module",
  "import",
  "exposing",
  "as",
  "port",
  "infix",
  "infixl",
  "infixr",
] as const;

export type Keyword = (typeof KEYWORDS)[number];

const KEYWORD_SET = new Set<Keyword>(KEYWORDS as readonly Keyword[]);

export function isKeyword(value: string): value is Keyword {
  return KEYWORD_SET.has(value as Keyword);
}

export const IDENTIFIER_OPERATOR_MAPPINGS = {
  ".": "_DOT",
  "..": "_RANGE",
  "::": "_CONS",
  "|>": "_PIPE_FORWARD",
  "<|": "_PIPE_BACKWARD",
  "++": "_CONCAT",
  "==": "_EQ",
  "/=": "_NE",
  "<=": "_LTE",
  ">=": "_GTE",
  "->": "_ARROW",
  "<-": "_LARROW",
} as const;

export type OperatorLexeme = keyof typeof IDENTIFIER_OPERATOR_MAPPINGS;

export function sanitizeOperator(lexeme: string): string {
  return (
    (IDENTIFIER_OPERATOR_MAPPINGS as Record<string, string>)[lexeme] ?? lexeme
  );
}

export type Exposing =
  | { kind: "All"; span: Span }
  | { kind: "Explicit"; names: string[]; span: Span };

export type Pattern =
  | { kind: "VarPattern"; name: string; span: Span }
  | { kind: "WildcardPattern"; span: Span }
  | { kind: "ConstructorPattern"; name: string; args: Pattern[]; span: Span }
  | { kind: "TuplePattern"; elements: Pattern[]; span: Span };

export type RecordField = { name: string; value: Expr; span: Span };

export type TypeExpr =
  | { kind: "TypeRef"; name: string; args: TypeExpr[]; span: Span }
  | { kind: "FunctionType"; from: TypeExpr; to: TypeExpr; span: Span }
  | { kind: "TupleType"; elements: TypeExpr[]; span: Span }
  | {
      kind: "RecordType";
      fields: Array<{ name: string; type: TypeExpr }>;
      span: Span;
    };

// ===== Algebraic Data Type (ADT) Declarations =====
// ADTs allow defining custom sum types like: type Maybe a = Just a | Nothing
// Each ADT has a name, type parameters, and one or more constructors (variants).

/**
 * A single constructor variant in an ADT definition.
 *
 * Example: In `type Maybe a = Just a | Nothing`:
 * - `Just a` is a variant with name "Just" and args [TypeRef "a"]
 * - `Nothing` is a variant with name "Nothing" and args []
 */
export type ConstructorVariant = {
  /** The constructor name (must be uppercase, e.g., "Just", "Nothing") */
  name: string;
  /** Type expressions for constructor arguments (e.g., [a] for Just a) */
  args: TypeExpr[];
  /** Source location span for error reporting */
  span: Span;
};

/**
 * An Algebraic Data Type declaration.
 *
 * Syntax: type Name param1 param2 ... = Constructor1 args | Constructor2 args | ...
 *
 * Examples:
 * - type Bool = True | False
 * - type Maybe a = Just a | Nothing
 * - type Result error value = Ok value | Err error
 * - type List a = Cons a (List a) | Nil  (recursive type)
 *
 * Note: Constructor names must be unique within a module (Elm 0.18 style).
 */
export type TypeDeclaration = {
  kind: "TypeDeclaration";
  /** The type name (must be uppercase, e.g., "Maybe", "Result") */
  name: string;
  /** Type parameters (lowercase identifiers, e.g., ["a"] for Maybe a) */
  params: string[];
  /** Constructor variants (at least one required) */
  constructors: ConstructorVariant[];
  /** Source location span for error reporting */
  span: Span;
};

/**
 * A type alias declaration.
 *
 * Syntax: type alias Name param1 param2 ... = TypeExpr
 *
 * Type aliases create a new name for an existing type expression.
 * Unlike ADTs, they don't introduce new constructors.
 *
 * Examples:
 * - type alias UserId = number
 * - type alias Pair a b = (a, b)
 * - type alias Handler msg = msg -> Model -> Model
 * - type alias Point = { x : number, y : number }
 */
export type TypeAliasDeclaration = {
  kind: "TypeAliasDeclaration";
  /** The alias name (must be uppercase, e.g., "UserId", "Pair") */
  name: string;
  /** Type parameters (lowercase identifiers, e.g., ["a", "b"] for Pair a b) */
  params: string[];
  /** The type expression this alias refers to */
  value: TypeExpr;
  /** Source location span for error reporting */
  span: Span;
};

export type Expr =
  | { kind: "Var"; name: string; namespace: "lower" | "upper"; span: Span }
  | { kind: "Number"; value: string; span: Span }
  | { kind: "String"; value: string; span: Span }
  | { kind: "Char"; value: string; span: Span }
  | { kind: "Lambda"; args: Pattern[]; body: Expr; span: Span }
  | { kind: "Apply"; callee: Expr; args: Expr[]; span: Span }
  | {
      kind: "If";
      condition: Expr;
      thenBranch: Expr;
      elseBranch: Expr;
      span: Span;
    }
  | { kind: "LetIn"; bindings: ValueDeclaration[]; body: Expr; span: Span }
  | {
      kind: "Case";
      discriminant: Expr;
      branches: Array<{ pattern: Pattern; body: Expr; span: Span }>;
      span: Span;
    }
  | { kind: "Infix"; left: Expr; operator: string; right: Expr; span: Span }
  | { kind: "Paren"; expression: Expr; span: Span }
  | { kind: "Tuple"; elements: Expr[]; span: Span }
  | { kind: "Unit"; span: Span }
  | { kind: "List"; elements: Expr[]; span: Span }
  | { kind: "ListRange"; start: Expr; end: Expr; span: Span }
  | { kind: "Record"; fields: RecordField[]; span: Span }
  | { kind: "RecordUpdate"; base: string; fields: RecordField[]; span: Span }
  | { kind: "FieldAccess"; target: Expr; field: string; span: Span };

export type ValueDeclaration = {
  kind: "ValueDeclaration";
  name: string;
  args: Pattern[];
  body: Expr;
  span: Span;
};

export type TypeAnnotationDeclaration = {
  kind: "TypeAnnotationDeclaration";
  name: string;
  annotation: TypeExpr;
  span: Span;
};

export type ExternalTarget = {
  modulePath: string;
  exportName: string;
  span: Span;
};

export type ExternalDeclaration = {
  kind: "ExternalDeclaration";
  name: string;
  target: ExternalTarget;
  annotation: TypeExpr;
  span: Span;
};

export type Declaration =
  | ValueDeclaration
  | TypeAnnotationDeclaration
  | ExternalDeclaration
  | TypeDeclaration
  | TypeAliasDeclaration;

export type ImportDeclaration = {
  moduleName: string;
  alias?: string;
  exposing?: Exposing;
  span: Span;
};

export type ModuleDeclaration = {
  name: string;
  exposing: Exposing | null;
  span: Span;
};

export type Program = {
  module?: ModuleDeclaration;
  imports: ImportDeclaration[];
  declarations: Declaration[];
};
