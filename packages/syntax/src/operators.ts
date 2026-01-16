/**
 * Shared operator character definitions and utilities.
 *
 * This module provides a single source of truth for:
 * - Which characters are valid operator characters
 * - How to sanitize operators to valid identifiers (for codegen)
 * - Builtin operator definitions (short-circuit operators)
 * - Operator fixity declarations
 *
 * Used by the lexer, parser, semantics, IR, and codegen packages
 * for consistent operator handling.
 */

// ============================================================================
// Operator Characters
// ============================================================================

/**
 * Set of characters that can appear in operators.
 * Used by the lexer to recognize multi-character operators.
 */
export const OPERATOR_CHARS = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));

/**
 * Mappings from single operator characters to valid identifier components.
 * Multi-character operators are built by concatenating single-character mappings.
 *
 * Examples:
 * - `==` → `_EQ` + `_EQ` → `_EQ_EQ`
 * - `|>` → `_PIPE` + `_GT` → `_PIPE_GT`
 * - `->` → `_MINUS` + `_GT` → `_MINUS_GT`
 */
export const CHAR_TO_IDENTIFIER: Readonly<Record<string, string>> = {
  ".": "_DOT",
  "+": "_PLUS",
  "-": "_MINUS",
  "*": "_STAR",
  "/": "_SLASH",
  "%": "_PERCENT",
  "^": "_CARET",
  "<": "_LT",
  ">": "_GT",
  "=": "_EQ",
  "|": "_PIPE",
  "&": "_AMP",
  "!": "_BANG",
  ":": "_COLON",
  "~": "_TILDE",
  $: "_DOLLAR",
  "#": "_HASH",
  "@": "_AT",
  "?": "_QUESTION",
  "\\": "_BACKSLASH",
};

/**
 * Convert an operator to a valid identifier by composing character mappings.
 * Unknown characters cause the original lexeme to be returned unchanged.
 *
 * @param lexeme - The operator string to sanitize
 * @returns A valid identifier string, or the original if any character is unmapped
 */
export function sanitizeOperator(lexeme: string): string {
  if (lexeme.length === 0) return lexeme;

  // Try to build from character mappings
  const parts: string[] = [];
  for (const char of lexeme) {
    const mapping = CHAR_TO_IDENTIFIER[char];
    if (mapping) {
      parts.push(mapping);
    } else {
      // If any character is unmapped, return the original lexeme unchanged
      return lexeme;
    }
  }

  return parts.join("");
}

/**
 * Check if a character is a valid operator character.
 *
 * @param char - Single character to check
 * @returns true if char can appear in an operator
 */
export function isOperatorChar(char: string): boolean {
  return OPERATOR_CHARS.has(char);
}

// ============================================================================
// Builtin Short-Circuit Operators
// ============================================================================

/**
 * Operator fixity information.
 */
export interface OperatorFixity {
  /** Operator associativity: left, right, or none */
  associativity: "left" | "right" | "none";
  /** Operator precedence (higher binds tighter) */
  precedence: number;
}

/**
 * Short-circuit operator helper definition for codegen.
 */
export interface ShortCircuitHelper {
  /** The JavaScript helper function name (e.g., "_AMP_AMP") */
  name: string;
  /** The JavaScript implementation (e.g., "(a) => (b) => a && b()") */
  impl: string;
}

/**
 * Complete builtin operator definition.
 */
export interface BuiltinOperator {
  /** The operator symbol (e.g., "&&") */
  symbol: string;
  /** Operator fixity (precedence and associativity) */
  fixity: OperatorFixity;
  /** Whether this operator uses short-circuit evaluation */
  isShortCircuit: boolean;
  /** Helper function info for codegen (only for short-circuit operators) */
  helper?: ShortCircuitHelper;
}

/**
 * All builtin operators.
 *
 * These operators are built into the compiler and require special handling:
 * - They have predefined fixity (precedence and associativity)
 * - Short-circuit operators wrap the right operand in a thunk
 * - They compile to helper functions, not protocol dispatch
 */
export const BUILTIN_OPERATORS: readonly BuiltinOperator[] = [
  {
    symbol: "&&",
    fixity: { associativity: "right", precedence: 3 },
    isShortCircuit: true,
    helper: { name: "_AMP_AMP", impl: "(a) => (b) => a && b()" },
  },
  {
    symbol: "||",
    fixity: { associativity: "right", precedence: 2 },
    isShortCircuit: true,
    helper: { name: "_PIPE_PIPE", impl: "(a) => (b) => a || b()" },
  },
] as const;

/**
 * Set of short-circuit operator symbols.
 *
 * These operators require special handling in IR lowering:
 * - The right operand is wrapped in a thunk (() -> Bool)
 * - At runtime, the thunk is only called if needed
 *
 * This enables true short-circuit evaluation:
 * - `False && expensive()` never evaluates expensive()
 * - `True || expensive()` never evaluates expensive()
 */
export const SHORT_CIRCUIT_OPERATORS: ReadonlySet<string> = new Set(
  BUILTIN_OPERATORS.filter((op) => op.isShortCircuit).map((op) => op.symbol)
);

/**
 * Map from operator symbol to fixity information.
 * Used by the parser and semantics for operator precedence/associativity.
 */
export const BUILTIN_OPERATOR_FIXITY: Readonly<Record<string, OperatorFixity>> =
  Object.fromEntries(BUILTIN_OPERATORS.map((op) => [op.symbol, op.fixity]));

/**
 * Map from short-circuit operator to its helper function info.
 * Used by codegen to emit helper functions.
 */
export const SHORT_CIRCUIT_HELPERS: Readonly<
  Record<string, ShortCircuitHelper>
> = Object.fromEntries(
  BUILTIN_OPERATORS.filter((op) => op.helper).map((op) => [
    op.symbol,
    op.helper!,
  ])
);
