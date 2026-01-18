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
  "protocol",
  "implement",
  "implementing",
  "where",
] as const;

export type Keyword = (typeof KEYWORDS)[number];

const KEYWORD_SET = new Set<Keyword>(KEYWORDS as readonly Keyword[]);

export function isKeyword(value: string): value is Keyword {
  return KEYWORD_SET.has(value as Keyword);
}

// Re-export operator utilities from dedicated module
export {
  // Character utilities
  OPERATOR_CHARS,
  CHAR_TO_IDENTIFIER,
  sanitizeOperator,
  isOperatorChar,
  // Builtin operator definitions
  BUILTIN_OPERATORS,
  BUILTIN_OPERATOR_FIXITY,
  SHORT_CIRCUIT_OPERATORS,
  SHORT_CIRCUIT_HELPERS,
  // Types
  type OperatorFixity,
  type ShortCircuitHelper,
  type BuiltinOperator,
} from "./operators";

/**
 * Export specification for a single item in an exposing clause.
 *
 * Supports several forms:
 * - `name` - Simple export of a value, function, type (opaque), or protocol (without methods)
 * - `TypeName(..)` - Export type with all constructors, or protocol with all methods
 * - `TypeName(Con1, Con2)` - Export type with specific constructors
 * - `ProtocolName(method1, method2)` - Export protocol with specific methods
 * - `(++)` - Export an operator
 */
export type ExportSpec =
  | {
      /** Export a simple name (value, function, opaque type, or protocol without methods) */
      kind: "ExportValue";
      name: string;
      span: Span;
    }
  | {
      /** Export an operator (e.g., `(++)`, `(<$>)`) */
      kind: "ExportOperator";
      operator: string;
      span: Span;
    }
  | {
      /** Export a type/protocol with all constructors/methods (`TypeName(..)`) */
      kind: "ExportTypeAll";
      name: string;
      span: Span;
    }
  | {
      /** Export a type/protocol with specific constructors/methods (`TypeName(Con1, Con2)`) */
      kind: "ExportTypeSome";
      name: string;
      /** Names of constructors or methods to export */
      members: string[];
      span: Span;
    };

export type Exposing =
  | { kind: "All"; span: Span }
  | { kind: "Explicit"; exports: ExportSpec[]; span: Span };

export type Pattern =
  | { kind: "VarPattern"; name: string; span: Span }
  | { kind: "WildcardPattern"; span: Span }
  | { kind: "ConstructorPattern"; name: string; args: Pattern[]; span: Span }
  | { kind: "TuplePattern"; elements: Pattern[]; span: Span }
  | { kind: "ListPattern"; elements: Pattern[]; span: Span }
  | { kind: "ConsPattern"; head: Pattern; tail: Pattern; span: Span }
  | {
      kind: "RecordPattern";
      fields: { name: string; pattern?: Pattern }[];
      span: Span;
    };

export type RecordField = { name: string; value: Expr; span: Span };

/**
 * A constraint in a qualified type.
 *
 * Example: In `Num a => a -> a -> a`, the constraint is `Num a`
 * - protocolName: "Num"
 * - typeArgs: [TypeRef "a"]
 */
export type Constraint = {
  /** The protocol name (e.g., "Num", "Show") */
  protocolName: string;
  /** Type arguments to the protocol (typically type variables) */
  typeArgs: TypeExpr[];
  /** Source location span for error reporting */
  span: Span;
};

/**
 * A qualified type with constraints.
 *
 * Example: `(Num a, Show a) => a -> a -> String`
 * - constraints: [Constraint(Num a), Constraint(Show a)]
 * - type: FunctionType(a -> FunctionType(a -> String))
 */
export type QualifiedType = {
  kind: "QualifiedType";
  /** List of constraints on type variables */
  constraints: Constraint[];
  /** The underlying type */
  type: TypeExpr;
  /** Source location span for error reporting */
  span: Span;
};

export type TypeExpr =
  | { kind: "TypeRef"; name: string; args: TypeExpr[]; span: Span }
  | { kind: "FunctionType"; from: TypeExpr; to: TypeExpr; span: Span }
  | { kind: "TupleType"; elements: TypeExpr[]; span: Span }
  | {
      kind: "RecordType";
      fields: Array<{ name: string; type: TypeExpr }>;
      span: Span;
    }
  | QualifiedType;

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
 *         [implementing Protocol1, Protocol2, ...]
 *
 * Examples:
 * - type Bool = True | False
 * - type Maybe a = Just a | Nothing
 * - type Result error value = Ok value | Err error
 * - type List a = Cons a (List a) | Nil  (recursive type)
 * - type Person = Person String Int implementing Show, Eq
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
  /** Protocols to automatically implement (requires all methods have defaults) */
  implementing?: string[];
  /** Source location span for error reporting */
  span: Span;
};

/**
 * A type alias declaration.
 *
 * Syntax: type alias Name param1 param2 ... = TypeExpr
 *
 * Type aliases create a new name for an existing type expression, including records.
 * Unlike ADTs, they don't introduce new constructors.
 *
 * Examples:
 * - type alias UserId = number
 * - type alias Pair a b = (a, b)
 * - type alias Handler msg = msg -> Model -> Model
 * - type alias Point = { x : Int, y : Int }
 */
export type TypeAliasDeclaration = {
  kind: "TypeAliasDeclaration";
  /** The alias name (must be uppercase, e.g., "UserId", "Pair") */
  name: string;
  /** Type parameters (lowercase identifiers, e.g., ["a", "b"] for Pair a b) */
  params: string[];
  /** The type expression this alias refers to (cannot be a record type) */
  value: TypeExpr;
  /** Source location span for error reporting */
  span: Span;
};

/**
 * An opaque type declaration.
 *
 * Syntax: type Name param1 param2 ...
 *
 * Opaque types are abstract types that hide their implementation.
 * They are useful for JS interop where the actual type is unknown
 * or for creating abstract data types.
 *
 * No pattern matching or record updates are allowed on opaque types.
 *
 * Examples:
 * - type Promise a           (opaque type for JS Promise)
 * - type Element             (opaque type for DOM elements)
 * - type Map k v             (opaque type for JS Map)
 */
export type OpaqueTypeDeclaration = {
  kind: "OpaqueTypeDeclaration";
  /** The type name (must be uppercase, e.g., "Promise", "Element") */
  name: string;
  /** Type parameters (lowercase identifiers, e.g., ["a"] for Promise a) */
  params: string[];
  /** Source location span for error reporting */
  span: Span;
};

export type Expr =
  | {
      kind: "Var";
      name: string;
      namespace: "lower" | "upper" | "operator";
      span: Span;
    }
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
  | { kind: "Unary"; operator: string; operand: Expr; span: Span }
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

/**
 * A method signature in a protocol declaration, optionally with a default implementation.
 *
 * Example (required method with explicit type):
 *   show : a -> String
 *
 * Example (method with default and explicit type):
 *   neq : a -> a -> Bool
 *   neq x y = not (eq x y)
 *
 * Example (method with default and inferred type):
 *   neq x y = not (eq x y)
 */
export type ProtocolMethod = {
  /** Method name (lowercase identifier or operator) */
  name: string;
  /** Method type signature (optional if defaultImpl is provided) */
  type?: TypeExpr;
  /** Optional default implementation (if provided, method can be omitted in implement blocks) */
  defaultImpl?: {
    /** Parameters for the default implementation */
    args: Pattern[];
    /** Body expression for the default implementation */
    body: Expr;
  };
  /** Source location span for error reporting */
  span: Span;
};

/**
 * A protocol declaration defining a type class interface.
 *
 * Syntax: protocol [Constraints =>] Name param1 param2 ... where
 *           method1 : Type1
 *           method2 : Type2
 *           method3 : Type3
 *           method3 x y = defaultExpr
 *
 * Example:
 * protocol Num a where
 *   plus : a -> a -> a
 *   minus : a -> a -> a
 *   times : a -> a -> a
 *
 * Example with constraints (superclass):
 * protocol Eq a => Ord a where
 *   compare : a -> a -> Ordering
 *
 * Example with defaults:
 * protocol Eq a where
 *   eq : a -> a -> Bool
 *   neq : a -> a -> Bool
 *   neq x y = not (eq x y)
 */
export type ProtocolDeclaration = {
  kind: "ProtocolDeclaration";
  /** The protocol name (must be uppercase, e.g., "Num", "Show") */
  name: string;
  /** Type parameters (lowercase identifiers, typically just one) */
  params: string[];
  /** Superclass constraints (e.g., "Eq a" in "Eq a => Ord a") */
  constraints: Constraint[];
  /** Method signatures with optional default implementations */
  methods: ProtocolMethod[];
  /** Source location span for error reporting */
  span: Span;
};

/**
 * Method implementation in an implementation declaration.
 */
export type MethodImplementation = {
  /** Method name */
  name: string;
  /** Optional pattern parameters for inline function definitions (e.g., `plus x y = ...`) */
  args?: Pattern[];
  /** Implementation expression (usually a variable referencing an implementation function) */
  implementation: Expr;
  /** Source location span for error reporting */
  span: Span;
};

/**
 * An implementation declaration providing a protocol implementation for a specific type.
 *
 * Syntax: implement (Constraint1, Constraint2, ...) => ProtocolName Type1 Type2 ... where
 *           method1 = impl1
 *           method2 = impl2
 *
 * Example:
 * implement Num Int where
 *   plus = intPlusImpl
 *   minus = intMinusImpl
 *   times = intTimesImpl
 *
 * Or with constraints:
 * implement Show a => Show (List a) where
 *   show = showListImpl
 */
export type ImplementationDeclaration = {
  kind: "ImplementationDeclaration";
  /** Context constraints (e.g., "Show a" in "Show a => Show (List a)") */
  constraints: Constraint[];
  /** The protocol name being implemented */
  protocolName: string;
  /** Type arguments (the concrete types satisfying this implementation) */
  typeArgs: TypeExpr[];
  /** Method implementations */
  methods: MethodImplementation[];
  /** Source location span for error reporting */
  span: Span;
};

/**
 * Fixity declaration for operators.
 *
 * Elm-style syntax:
 *   infixl 6 +    -- left-associative, precedence 6
 *   infixr 5 ++   -- right-associative, precedence 5
 *   infix 4 ==    -- non-associative, precedence 4
 *
 * Fixity:
 *   infixl = left-associative
 *   infixr = right-associative
 *   infix  = non-associative
 *
 * Precedence: Higher numbers bind tighter (e.g., * at 7 binds tighter than + at 6)
 */
export type InfixDeclaration = {
  kind: "InfixDeclaration";
  /** The fixity: "infix" (non-assoc), "infixl" (left), "infixr" (right) */
  fixity: "infix" | "infixl" | "infixr";
  /** Precedence level (1-9, higher binds tighter) */
  precedence: number;
  /** The operator being declared (e.g., "+", "==", "|>") */
  operator: string;
  /** Source location span for error reporting */
  span: Span;
};

/**
 * Operator precedence and associativity info.
 * Used by parser for expression parsing.
 */
export type OperatorInfo = {
  precedence: number;
  associativity: "left" | "right" | "none";
};

/**
 * Registry of operator fixity declarations.
 * Built during pre-processing pass before main parsing.
 */
export type OperatorRegistry = Map<string, OperatorInfo>;

export type Declaration =
  | ValueDeclaration
  | TypeAnnotationDeclaration
  | ExternalDeclaration
  | TypeDeclaration
  | TypeAliasDeclaration
  | OpaqueTypeDeclaration
  | ProtocolDeclaration
  | ImplementationDeclaration
  | InfixDeclaration;

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
  /** Every Vibe file must have a module declaration as the first statement */
  module: ModuleDeclaration;
  imports: ImportDeclaration[];
  declarations: Declaration[];
};

// ============================================================
// Built-in Module and Type Constants
// ============================================================

/**
 * The module name used for built-in types (Bool, Unit, Int, etc.)
 * that are always available without imports.
 */
export const BUILTIN_MODULE_NAME = "__builtin__";

/**
 * The name of the Bool type (compiles to native JavaScript booleans).
 */
export const BOOL_TYPE_NAME = "Bool";

/**
 * The name of the Unit type (compiles to undefined).
 */
export const UNIT_TYPE_NAME = "Unit";
