/**
 * Shared operator character definitions and utilities.
 *
 * This module provides a single source of truth for:
 * - Which characters are valid operator characters
 * - How to sanitize operators to valid identifiers (for codegen)
 *
 * Used by both the lexer (to tokenize operators) and other packages
 * (semantics, codegen) for consistent operator handling.
 */

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
