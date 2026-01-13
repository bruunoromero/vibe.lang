/**
 * @vibe/ir - Intermediate Representation for Vibe Compiler
 *
 * This package provides an intermediate representation (IR) that sits between
 * semantic analysis and code generation. The IR performs several transformations:
 *
 * 1. Let-binding lifting: Flattens nested lets into sequential bindings
 * 2. Case expression lowering: Simplifies case expressions on built-in types
 * 3. Record update desugaring: Transforms record updates into record construction
 * 4. Dependency analysis: Topologically sorts declarations, identifying SCCs
 *
 * The IR preserves type information and protocol constraints as metadata,
 * enabling dictionary-passing transformations during code generation.
 */

import type {
  Span,
  TypeExpr,
  Pattern,
  Program,
  ValueDeclaration,
  RecordField,
} from "@vibe/syntax";
import type {
  SemanticModule,
  ConstructorInfo,
  ADTInfo,
  ExportInfo,
} from "@vibe/semantics";

// ============================================================================
// IR Type Definitions
// ============================================================================

/**
 * Internal type representation (mirrors semantics for consistency).
 * These are carried through from semantic analysis.
 */
export type IRTypeVar = { kind: "var"; id: number };
export type IRTypeCon = { kind: "con"; name: string; args: IRType[] };
export type IRTypeFun = { kind: "fun"; from: IRType; to: IRType };
export type IRTypeTuple = { kind: "tuple"; elements: IRType[] };
export type IRTypeRecord = { kind: "record"; fields: Record<string, IRType> };
export type IRTypeList = { kind: "list"; element: IRType };
export type IRType =
  | IRTypeVar
  | IRTypeCon
  | IRTypeFun
  | IRTypeTuple
  | IRTypeRecord
  | IRTypeList;

/**
 * A constraint on a type variable requiring it to implement a protocol.
 * Carried through from semantic analysis for dictionary-passing.
 */
export type IRConstraint = {
  protocolName: string;
  typeArgs: IRType[];
};

// ============================================================================
// IR Expression Types
// ============================================================================

/**
 * IR expressions are a simplified form of AST expressions.
 *
 * Differences from AST Expr:
 * - No LetIn: lifted to top-level sequential bindings
 * - RecordUpdate: either desugared to IRRecord (if type known) or kept as IRRecordUpdate
 * - No Infix: resolved to function application
 * - No Paren: flattened away
 * - Case on primitives: lowered to If chains
 */
export type IRExpr =
  | IRVar
  | IRModuleAccess
  | IRLiteral
  | IRLambda
  | IRApply
  | IRIf
  | IRCase
  | IRTuple
  | IRUnit
  | IRList
  | IRRecord
  | IRRecordUpdate
  | IRFieldAccess
  | IRConstructor;

/** Variable reference */
export type IRVar = {
  kind: "IRVar";
  name: string;
  /** "value" for lowercase identifiers, "constructor" for uppercase */
  namespace: "value" | "constructor";
  /** The inferred type of this variable (if available from semantic analysis) */
  type?: IRType;
  /**
   * For protocol method references, the constraint this method belongs to.
   * Used by codegen to resolve the correct instance dictionary.
   * E.g., for `+` from `Num a`, this would be { protocolName: "Num", typeArgs: [a] }
   */
  constraint?: IRConstraint;
  span: Span;
};

/**
 * Module-qualified access (e.g., JS.null or Vibe.JS.null).
 * This is resolved during IR lowering from FieldAccess chains on module references.
 */
export type IRModuleAccess = {
  kind: "IRModuleAccess";
  /**
   * The import alias to use in generated code (e.g., "JS").
   * For "import Vibe.JS as JS", this is "JS".
   * For "import Vibe.JS", this is also "JS" (last segment of module name).
   */
  importAlias: string;
  /** The full module name (e.g., "Vibe.JS") */
  moduleName: string;
  /** The value name being accessed (e.g., "null") */
  valueName: string;
  /**
   * For external bindings, the actual export name (e.g., "null_").
   * If undefined, valueName is used directly.
   */
  externalName?: string;
  span: Span;
};

/** Literal values */
export type IRLiteral = {
  kind: "IRLiteral";
  value: string | number | boolean;
  literalType: "int" | "float" | "string" | "char" | "bool";
  span: Span;
};

/** Lambda (function) expression */
export type IRLambda = {
  kind: "IRLambda";
  params: IRPattern[];
  body: IRExpr;
  span: Span;
};

/** Function application */
export type IRApply = {
  kind: "IRApply";
  callee: IRExpr;
  args: IRExpr[];
  span: Span;
};

/** Conditional expression */
export type IRIf = {
  kind: "IRIf";
  condition: IRExpr;
  thenBranch: IRExpr;
  elseBranch: IRExpr;
  span: Span;
};

/**
 * Case expression (pattern matching).
 * Case expressions on Bool and other primitives are lowered to IRIf chains.
 * This type is used for ADT pattern matching which remains.
 */
export type IRCase = {
  kind: "IRCase";
  discriminant: IRExpr;
  branches: IRCaseBranch[];
  span: Span;
};

export type IRCaseBranch = {
  pattern: IRPattern;
  body: IRExpr;
  span: Span;
};

/** Tuple expression */
export type IRTuple = {
  kind: "IRTuple";
  elements: IRExpr[];
  span: Span;
};

/** Unit value */
export type IRUnit = {
  kind: "IRUnit";
  span: Span;
};

/** List literal */
export type IRList = {
  kind: "IRList";
  elements: IRExpr[];
  span: Span;
};

/** Record construction (also used for desugared record updates when type is known) */
export type IRRecord = {
  kind: "IRRecord";
  fields: IRRecordField[];
  span: Span;
};

export type IRRecordField = {
  name: string;
  value: IRExpr;
  span: Span;
};

/**
 * Record update expression (spread semantics).
 * Used when the base record type isn't fully known at IR lowering time.
 * Semantics: { ...base, field1: val1, field2: val2 }
 */
export type IRRecordUpdate = {
  kind: "IRRecordUpdate";
  base: IRExpr;
  updates: IRRecordField[];
  span: Span;
};

/** Field access on a record */
export type IRFieldAccess = {
  kind: "IRFieldAccess";
  target: IRExpr;
  field: string;
  span: Span;
};

/** ADT constructor application */
export type IRConstructor = {
  kind: "IRConstructor";
  name: string;
  args: IRExpr[];
  /** Runtime tag for pattern matching dispatch */
  tag: number;
  span: Span;
};

// ============================================================================
// IR Pattern Types
// ============================================================================

/**
 * IR patterns mirror AST patterns but with additional metadata for codegen.
 */
export type IRPattern =
  | IRVarPattern
  | IRWildcardPattern
  | IRConstructorPattern
  | IRTuplePattern
  | IRLiteralPattern
  | IRListPattern
  | IRConsPattern;

export type IRVarPattern = {
  kind: "IRVarPattern";
  name: string;
  span: Span;
};

export type IRWildcardPattern = {
  kind: "IRWildcardPattern";
  span: Span;
};

export type IRConstructorPattern = {
  kind: "IRConstructorPattern";
  name: string;
  args: IRPattern[];
  /** Runtime tag for dispatch */
  tag: number;
  span: Span;
};

export type IRTuplePattern = {
  kind: "IRTuplePattern";
  elements: IRPattern[];
  span: Span;
};

/** Literal pattern for primitive matching */
export type IRLiteralPattern = {
  kind: "IRLiteralPattern";
  value: string | number | boolean;
  literalType: "int" | "float" | "string" | "char" | "bool";
  span: Span;
};

/** List pattern: [] or [x, y, z] */
export type IRListPattern = {
  kind: "IRListPattern";
  elements: IRPattern[];
  span: Span;
};

/** Cons pattern: head :: tail */
export type IRConsPattern = {
  kind: "IRConsPattern";
  head: IRPattern;
  tail: IRPattern;
  span: Span;
};

// ============================================================================
// IR Value and Program Types
// ============================================================================

/**
 * A single value binding in IR form.
 */
export type IRValue = {
  /** Binding name */
  name: string;
  /** Parameters (for function definitions) */
  params: IRPattern[];
  /** The lowered expression body */
  body: IRExpr;
  /** Inferred type from semantic analysis */
  type: IRType;
  /** Protocol constraints on this value */
  constraints: IRConstraint[];
  /** Whether this is an external (FFI) binding */
  isExternal: boolean;
  /** For external bindings: the import target */
  externalTarget?: {
    modulePath: string;
    exportName: string;
  };
  /** Source span for error messages */
  span: Span;
};

/**
 * A strongly connected component in the dependency graph.
 * Values within an SCC are mutually recursive and must be emitted together.
 */
export type SCC = {
  /** The values in this SCC (sorted to preserve declaration order when possible) */
  values: string[];
  /** Whether this SCC contains mutual recursion (size > 1) */
  isMutuallyRecursive: boolean;
};

/**
 * Protocol method dictionary metadata.
 * Used for dictionary-passing transformation in codegen.
 */
export type MethodDictionary = {
  protocolName: string;
  typeArgs: IRType[];
  /** Map from method name to implementation reference */
  methods: Record<string, string>;
};

/**
 * Protocol metadata for code generation.
 */
export type IRProtocol = {
  name: string;
  params: string[];
  methods: Array<{
    name: string;
    type: IRType;
    hasDefault: boolean;
  }>;
};

/**
 * Instance metadata for code generation.
 */
export type IRInstance = {
  protocolName: string;
  typeArgs: IRType[];
  constraints: IRConstraint[];
  methods: Record<string, string>;
};

/**
 * Constructor metadata for runtime dispatch.
 */
export type IRConstructorInfo = {
  name: string;
  parentType: string;
  arity: number;
  tag: number;
  /** Module that defines this constructor */
  moduleName?: string;
};

/**
 * Import alias information for code generation.
 * Maps import aliases to their full module paths.
 */
export type IRImportAlias = {
  /** The alias name (e.g., "JS" from "import Vibe.JS as JS") */
  alias: string;
  /** The full module name (e.g., "Vibe.JS") */
  moduleName: string;
};

/**
 * The complete IR program representation.
 */
export type IRProgram = {
  /** Module name (if declared) */
  moduleName?: string;

  /** Package name this module belongs to */
  packageName?: string;

  /** All lowered value bindings, indexed by name */
  values: Record<string, IRValue>;

  /**
   * Topologically sorted SCCs.
   * Values should be emitted in this order.
   * First SCC has no dependencies, last may depend on all others.
   */
  dependencyOrder: SCC[];

  /**
   * Lifted let-bindings that need to be emitted at module scope.
   * These are named with unique prefixes to avoid collision.
   */
  liftedBindings: IRValue[];

  /** ADT information for pattern matching */
  adts: Record<string, ADTInfo>;

  /** Constructor info with assigned tags */
  constructors: Record<string, IRConstructorInfo>;

  /** Protocol definitions for dictionary generation */
  protocols: Record<string, IRProtocol>;

  /** Instance implementations for dictionary lookup */
  instances: IRInstance[];

  /** Constraint requirements for each constrained value */
  constraintMetadata: Map<string, IRConstraint[]>;

  /** External imports needed by the generated code */
  externalImports: Set<string>;

  /**
   * Import alias mappings for code generation.
   * Maps alias names to their full module names.
   * e.g., "JS" -> "Vibe.JS" from "import Vibe.JS as JS"
   */
  importAliases: IRImportAlias[];

  /** Original source module for reference */
  sourceModule: SemanticModule;

  /** Original AST program for additional source info */
  sourceProgram: Program;

  /** Export information from the module's exposing clause */
  exports: ExportInfo;
};

// ============================================================================
// IR Error Types
// ============================================================================

/**
 * Error thrown during IR lowering.
 */
export class IRError extends Error {
  constructor(message: string, public readonly span: Span) {
    super(message);
    this.name = "IRError";
  }
}

// ============================================================================
// Re-exports for convenient access
// ============================================================================

export type {
  Span,
  Program,
  Pattern,
  TypeExpr,
  RecordField,
} from "@vibe/syntax";
export type {
  SemanticModule,
  ConstructorInfo,
  ADTInfo,
  ExportInfo,
} from "@vibe/semantics";
