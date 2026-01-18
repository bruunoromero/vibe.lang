import type {
  TypeExpr,
  Span,
  Expr,
  Pattern,
  ValueDeclaration,
  ExternalDeclaration,
  ModuleDeclaration,
  ImportDeclaration,
  InfixDeclaration,
  OperatorRegistry,
} from "@vibe/syntax";

// Re-export Span for use by other modules
export type { Span } from "@vibe/syntax";

// ===== Internal Type Representation =====
// Simple HM-style types for type inference using Hindley-Milner algorithm.

export type TypeVar = { kind: "var"; id: number };
export type TypeCon = { kind: "con"; name: string; args: Type[] };
export type TypeFun = { kind: "fun"; from: Type; to: Type };
export type TypeTuple = { kind: "tuple"; elements: Type[] };
export type TypeRecord = { kind: "record"; fields: Record<string, Type> };
export type Type = TypeVar | TypeCon | TypeFun | TypeTuple | TypeRecord;

/**
 * Constraint represents a protocol requirement on a type.
 * Example: Constraint("Num", [TypeVar(0)]) means "type variable 0 must implement Num protocol"
 */
export type Constraint = {
  protocolName: string;
  typeArgs: Type[];
};

/**
 * TypeScheme represents a polymorphic type with universally quantified type variables
 * and optional constraints.
 *
 * Example: `forall a. Num a => a -> a` is represented as:
 * {
 *   vars: Set([0]),
 *   constraints: [{ protocolName: "Num", typeArgs: [TypeVar(0)] }],
 *   type: { kind: "fun", from: TypeVar(0), to: TypeVar(0) }
 * }
 *
 * In Elm/ML terminology:
 * - A monomorphic type has no quantified variables (empty Set)
 * - A polymorphic type has one or more quantified variables
 * - A constrained type has protocol requirements on type variables
 */
export type TypeScheme = {
  vars: Set<number>; // Set of type variable IDs that are quantified (polymorphic)
  constraints: Constraint[]; // Protocol constraints on type variables
  type: Type; // The underlying type structure
};

export type Substitution = Map<number, Type>;

/**
 * Scope maintains a symbol table mapping names to their type schemes.
 * Type schemes enable let-polymorphism: bindings can be polymorphic,
 * and each use site gets a fresh instantiation of the type.
 */
export type Scope = {
  parent?: Scope;
  symbols: Map<string, TypeScheme>; // Maps names to their polymorphic type schemes
};

/**
 * Information about a single constructor in an ADT.
 *
 * For example, in `type Maybe a = Just a | Nothing`:
 * - Just: { arity: 1, argTypes: [TypeVar(a)], parentType: "Maybe", parentParams: ["a"] }
 * - Nothing: { arity: 0, argTypes: [], parentType: "Maybe", parentParams: ["a"] }
 */
export type ConstructorInfo = {
  /** Number of arguments the constructor takes */
  arity: number;
  /** Type expressions for each argument (from source) */
  argTypes: TypeExpr[];
  /** Name of the parent ADT (e.g., "Maybe") */
  parentType: string;
  /** Type parameters of the parent ADT (e.g., ["a"]) */
  parentParams: string[];
  /** The module that defines this constructor (e.g., "Vibe", "MyModule") */
  moduleName?: string;
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about an Algebraic Data Type.
 *
 * For example, `type Maybe a = Just a | Nothing` becomes:
 * {
 *   name: "Maybe",
 *   params: ["a"],
 *   constructors: ["Just", "Nothing"],
 *   span: <source span>
 * }
 */
export type ADTInfo = {
  /** The type name (e.g., "Maybe", "Result") */
  name: string;
  /** The module that defines this type (e.g., "Vibe", "MyModule") */
  moduleName?: string;
  /** Type parameters (e.g., ["a"] for Maybe a) */
  params: string[];
  /** Names of all constructors in this type */
  constructors: string[];
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about a field in a record type.
 */
export type RecordFieldInfo = {
  /** Field name */
  name: string;
  /** Field type expression (AST node for proper type parameter resolution) */
  typeExpr: TypeExpr;
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about a named record type.
 *
 * For example, `type Person = { name : String, age : Int }` becomes:
 * {
 *   name: "Person",
 *   params: [],
 *   fields: [{ name: "name", type: ... }, { name: "age", type: ... }]
 * }
 */
export type RecordInfo = {
  /** The record type name (e.g., "Person", "Point") */
  name: string;
  /** The module that defines this record type */
  moduleName?: string;
  /** Type parameters (e.g., ["a"] for Container a) */
  params: string[];
  /** Record field definitions */
  fields: RecordFieldInfo[];
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about a type alias.
 *
 * For example, `type alias UserId = number` becomes:
 * {
 *   name: "UserId",
 *   params: [],
 *   value: TypeRef "number"
 * }
 */
export type TypeAliasInfo = {
  /** The alias name (e.g., "UserId") */
  name: string;
  /** The module that defines this alias (e.g., "MyModule") */
  moduleName?: string;
  /** Type parameters (e.g., ["a", "b"] for Pair a b) */
  params: string[];
  /** The type expression this alias expands to */
  value: TypeExpr;
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about an opaque type.
 *
 * Opaque types are abstract types that hide their implementation.
 * They are useful for JS interop or creating abstract data types.
 * Pattern matching and record updates are not allowed on opaque types.
 *
 * For example, `type Promise a` becomes:
 * {
 *   name: "Promise",
 *   params: ["a"]
 * }
 */
export type OpaqueTypeInfo = {
  /** The opaque type name (e.g., "Promise", "Element") */
  name: string;
  /** The module that defines this opaque type */
  moduleName?: string;
  /** Type parameters (e.g., ["a"] for Promise a) */
  params: string[];
  /** Source span for error messages */
  span: Span;
};

/**
 * Default implementation for a protocol method.
 * Contains the type-checked parameters and body expression.
 */
export type DefaultMethodImpl = {
  /** Parameters for the default implementation (patterns) */
  args: Pattern[];
  /** Body expression of the default implementation */
  body: Expr;
};

/**
 * Information about a method in a protocol, including optional default implementation.
 */
export type ProtocolMethodInfo = {
  /** The method's type signature */
  type: Type;
  /** Source span for error messages */
  span: Span;
  /** Optional default implementation (if present, method can be omitted in implement blocks) */
  defaultImpl?: DefaultMethodImpl;
};

/**
 * Information about a protocol (type class).
 *
 * For example, `protocol Num a where plus : a -> a -> a` becomes:
 * {
 *   name: "Num",
 *   params: ["a"],
 *   methods: Map { "plus" => { type: ..., span: ..., defaultImpl: undefined } }
 * }
 */
export type ProtocolInfo = {
  /** The protocol name (e.g., "Num", "Show") */
  name: string;
  /** The module that defines this protocol (e.g., "Vibe") */
  moduleName?: string;
  /** Type parameters (typically just one, e.g., ["a"]) */
  params: string[];
  /** Superclass constraints (e.g., [Eq a] in "protocol Eq a => Ord a where") */
  superclassConstraints: Constraint[];
  /** Method signatures with optional default implementations */
  methods: Map<string, ProtocolMethodInfo>;
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about an instance implementation.
 *
 * For example, `instance Num Int where plus = intPlusImpl` becomes:
 * {
 *   protocolName: "Num",
 *   typeArgs: [TypeCon("Int")],
 *   constraints: [],
 *   methods: Map { "plus" => <implementation expr> },
 *   span: ...
 * }
 */
export type InstanceInfo = {
  /** The protocol name being implemented */
  protocolName: string;
  /** The module that defines this instance */
  moduleName?: string;
  /** Concrete type(s) for this instance */
  typeArgs: Type[];
  /** Context constraints (e.g., "Show a" in "Show a => Show (List a)") */
  constraints: Constraint[];
  /** Method implementations: name -> implementation expression */
  methods: Map<string, Expr>;
  /** Method names that were explicitly provided (vs. inherited from defaults) */
  explicitMethods: Set<string>;
  /** Source span for error messages */
  span: Span;
};

export type ValueInfo = {
  declaration: ValueDeclaration | ExternalDeclaration;
  annotation?: TypeExpr;
  externalTarget?: ExternalDeclaration["target"];
  type?: Type;
  /** User-annotated protocol constraints from qualified type annotations */
  annotatedConstraints?: Constraint[];
  /** All constraints collected during type inference (before filtering to polymorphic) */
  collectedConstraints?: Constraint[];
  /** Source location for error reporting */
  span?: Span;
};

/**
 * Information about what a module exports.
 * This is computed from the module's exposing clause and what's defined in the module.
 */
export type ExportInfo = {
  /** Exported values and functions (by name) */
  values: Set<string>;
  /** Exported operators */
  operators: Set<string>;
  /** Exported types (by name) */
  types: Map<
    string,
    {
      /** If true, all constructors are exported; if false, only listed ones */
      allConstructors: boolean;
      /** If allConstructors is false, the specific constructors exported */
      constructors?: Set<string>;
    }
  >;
  /** Exported protocols (by name) */
  protocols: Map<
    string,
    {
      /** If true, all methods are exported; if false, only listed ones */
      allMethods: boolean;
      /** If allMethods is false, the specific methods exported */
      methods?: Set<string>;
    }
  >;
  /** Whether this module exports everything (exposing (..)) */
  exportsAll: boolean;

  /**
   * Re-exported values from other modules.
   * Map from value name -> source module name.
   * Used by codegen to generate proper re-export statements.
   */
  reExportedValues: Map<string, string>;
};

/**
 * The result of semantic analysis for a module.
 *
 * Contains all the analyzed information needed for code generation:
 * - values: All value declarations with their inferred types
 * - annotations: Standalone type annotations
 * - types: Inferred types for all values
 * - adts: User-defined algebraic data types
 * - constructors: Map from constructor names to their info
 * - typeAliases: Type alias definitions
 * - opaqueTypes: Opaque type declarations (for JS interop)
 * - protocols: Protocol (type class) definitions
 * - instances: Instance implementations
 * - module: Module declaration (always present)
 * - imports: Import declarations
 * - exports: Computed export information
 */
export type SemanticModule = {
  values: Record<string, ValueInfo>;
  annotations: Record<string, import("@vibe/syntax").TypeAnnotationDeclaration>;
  module: ModuleDeclaration;
  imports: ImportDeclaration[];
  types: Record<string, Type>;
  /** Type schemes with constraints for each value (for dictionary-passing) */
  typeSchemes: Record<string, TypeScheme>;
  /** Registry of user-defined algebraic data types */
  adts: Record<string, ADTInfo>;
  /** Map from constructor names to their type information */
  constructors: Record<string, ConstructorInfo>;
  /** Map from constructor names to their type schemes (for importing) */
  constructorTypes: Record<string, TypeScheme>;
  /** Registry of type aliases */
  typeAliases: Record<string, TypeAliasInfo>;
  /** Registry of named record types */
  records: Record<string, RecordInfo>;
  /** Registry of opaque types (for JS interop) */
  opaqueTypes: Record<string, OpaqueTypeInfo>;
  /** Registry of protocols (type classes) */
  protocols: Record<string, ProtocolInfo>;
  /** List of instance implementations */
  instances: InstanceInfo[];
  /** Registry of custom operator precedence/associativity */
  operators: OperatorRegistry;
  /** Infix declarations for operators */
  infixDeclarations: InfixDeclaration[];
  /** Computed export information for this module */
  exports: ExportInfo;
};

export interface AnalyzeOptions {
  /** Pre-analyzed dependency modules to merge into scope */
  dependencies?: Map<string, SemanticModule>;

  /**
   * File path context for module declaration validation.
   * When provided, the semantic analyzer validates that:
   * 1. The file has a module declaration
   * 2. The declared module name matches the expected name derived from the file path
   */
  fileContext?: {
    /** Absolute path to the source file being analyzed */
    filePath: string;
    /** Absolute path to the src directory (from vibe.json) */
    srcDir: string;
  };
}

/**
 * ConstraintContext collects protocol constraints during type inference.
 * When a protocol method is accessed (like `+` from `Num`), we record
 * the constraint with the actual type variables involved.
 *
 * This enables dictionary-passing style compilation:
 * - Functions using protocol methods get dictionary parameters
 * - Call sites pass appropriate implementation dictionaries
 */
export type ConstraintContext = {
  /** Collected constraints during inference */
  constraints: Constraint[];
};

/**
 * Result of looking up a symbol, including its type and any constraints.
 */
export type LookupResult = {
  type: Type;
  constraints: Constraint[];
};

/**
 * Result of looking up an instance for a given protocol and concrete type.
 */
export type InstanceLookupResult =
  | { found: true }
  | { found: false; reason: "no-instance" }
  | {
      found: false;
      reason: "unsatisfied-constraint";
      constraint: string;
      forType: string;
    };

/**
 * Result of instantiation including both type and instantiated constraints.
 */
export type InstantiationResult = {
  type: Type;
  constraints: Constraint[];
};

/**
 * TypeVarContext tracks type variable names to their corresponding TypeVar IDs
 * during annotation parsing. This enables polymorphic type annotations like:
 * - id : a -> a
 * - map : (a -> b) -> List a -> List b
 *
 * Type variables are lowercase identifiers (typically single letters: a, b, c)
 * that should be treated as universally quantified variables.
 */
export type TypeVarContext = Map<string, TypeVar>;

/**
 * Result of converting a type annotation to an internal type.
 * Includes both the type and any protocol constraints from qualified types.
 */
export type AnnotationResult = {
  /** The converted internal type */
  type: Type;
  /** Protocol constraints extracted from qualified types (e.g., Num a => ...) */
  constraints: Constraint[];
};

/**
 * Error thrown when a type reference cannot be resolved.
 * Contains helpful information for generating suggestions.
 */
export type TypeValidationError = {
  message: string;
  span: Span;
  suggestion?: string;
};
