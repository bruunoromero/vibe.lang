import { BUILTIN_MODULE_NAME } from "@vibe/syntax";
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
 * Built-in operator type signatures.
 *
 * Most operators are defined via protocols and implementations in user code
 * or the prelude. However, short-circuit operators (&& and ||) are built-in
 * because they require special evaluation semantics (the second operand must
 * not be evaluated if the first operand determines the result).
 *
 * The compiler wraps the second operand in a thunk during IR lowering to
 * enable true short-circuit evaluation.
 */

const BOOL_TYPE: Type = { kind: "con", name: "Bool", args: [] };

export const INFIX_TYPES: Record<string, Type> = {
  // Short-circuit logical operators (Bool -> Bool -> Bool)
  // These are special-cased in IR lowering to wrap the right operand in a thunk
  "&&": {
    kind: "fun",
    from: BOOL_TYPE,
    to: { kind: "fun", from: BOOL_TYPE, to: BOOL_TYPE },
  },
  "||": {
    kind: "fun",
    from: BOOL_TYPE,
    to: { kind: "fun", from: BOOL_TYPE, to: BOOL_TYPE },
  },
};

/**
 * Built-in span for synthetic nodes (builtins have no source location).
 */
export const BUILTIN_SPAN: Span = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 },
};

// Re-export from @vibe/syntax for backwards compatibility
export { SHORT_CIRCUIT_OPERATORS, BUILTIN_OPERATOR_FIXITY } from "@vibe/syntax";
