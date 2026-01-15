import type {
  Type,
  TypeScheme,
  ADTInfo,
  ConstructorInfo,
  OpaqueTypeInfo,
  Span,
} from "./types";

/**
 * Built-in constructors registry.
 *
 * These are the primitive types that are built into the compiler.
 * They are automatically available in all modules.
 *
 * - Bool: True | False (used in if-then-else expressions)
 * - Unit: Unit (used for functions with no meaningful return value)
 * - Int: Integer numbers
 * - Float: Floating-point numbers
 * - String: Text values
 * - Char: Single characters
 */
export const BUILTIN_CONSTRUCTORS: Record<string, number> = {
  // Bool constructors
  True: 0,
  False: 0,
  // Unit constructor
  Unit: 0,
  // Primitive type constructors (zero-arity, but exist for type system)
  Int: 0,
  Float: 0,
  String: 0,
  Char: 0,
};

/**
 * Built-in operator type signatures are now EMPTY.
 *
 * All operators must be defined via protocols and implementations
 * in user code or the prelude. This enables a clean separation where
 * the compiler handles syntax/semantics while the prelude provides
 * standard operators.
 *
 * Example prelude definitions:
 *   protocol Num a where
 *     (+) : a -> a -> a
 *     (-) : a -> a -> a
 *     (*) : a -> a -> a
 *
 *   implement Num Int where
 *     (+) = intAdd
 *     (-) = intSub
 *     (*) = intMul
 */
export const INFIX_TYPES: Record<string, Type> = {};

/**
 * Built-in span for synthetic nodes (builtins have no source location).
 */
export const BUILTIN_SPAN: Span = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 },
};

/**
 * Initialize built-in ADTs in the registry.
 */
export function initializeBuiltinADTs(
  adts: Record<string, ADTInfo>,
  constructors: Record<string, ConstructorInfo>,
  constructorTypes: Record<string, TypeScheme>
): void {
  // Bool ADT: True | False (still an ADT for pattern matching)
  adts["Bool"] = {
    name: "Bool",
    params: [],
    constructors: ["True", "False"],
    span: BUILTIN_SPAN,
  };
  constructors["True"] = {
    arity: 0,
    argTypes: [],
    parentType: "Bool",
    parentParams: [],
    moduleName: "__builtin__",
    span: BUILTIN_SPAN,
  };
  constructorTypes["True"] = {
    vars: new Set(),
    constraints: [],
    type: { kind: "con", name: "Bool", args: [] },
  };
  constructors["False"] = {
    arity: 0,
    argTypes: [],
    parentType: "Bool",
    parentParams: [],
    moduleName: "__builtin__",
    span: BUILTIN_SPAN,
  };
  constructorTypes["False"] = {
    vars: new Set(),
    constraints: [],
    type: { kind: "con", name: "Bool", args: [] },
  };

  // List ADT: List a (builtin parameterized type for lists)
  // Note: List uses the internal "list" type representation but is exposed as "List" ADT
  adts["List"] = {
    name: "List",
    params: ["a"],
    constructors: [], // List constructors are handled specially (via list literals)
    span: BUILTIN_SPAN,
  };
}

/**
 * Initialize built-in opaque types in the registry.
 */
export function initializeBuiltinOpaqueTypes(
  opaqueTypes: Record<string, OpaqueTypeInfo>
): void {
  // Primitive opaque types - no pattern matching allowed
  opaqueTypes["Unit"] = {
    name: "Unit",
    moduleName: "__builtin__",
    params: [],
    span: BUILTIN_SPAN,
  };

  opaqueTypes["Int"] = {
    name: "Int",
    moduleName: "__builtin__",
    params: [],
    span: BUILTIN_SPAN,
  };

  opaqueTypes["Float"] = {
    name: "Float",
    moduleName: "__builtin__",
    params: [],
    span: BUILTIN_SPAN,
  };

  opaqueTypes["String"] = {
    name: "String",
    moduleName: "__builtin__",
    params: [],
    span: BUILTIN_SPAN,
  };

  opaqueTypes["Char"] = {
    name: "Char",
    moduleName: "__builtin__",
    params: [],
    span: BUILTIN_SPAN,
  };
}
