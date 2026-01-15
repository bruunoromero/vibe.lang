/**
 * @vibe/codegen - Identifier Sanitization
 *
 * Utilities for converting Vibe identifiers to valid JavaScript identifiers.
 */

import { sanitizeOperator } from "@vibe/syntax";

/**
 * JavaScript reserved words that need to be escaped.
 */
export const RESERVED_WORDS = new Set([
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "null",
  "return",
  "static",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "undefined",
  "var",
  "void",
  "while",
  "with",
  "yield",
]);

/**
 * Sanitize an identifier for JavaScript.
 *
 * Vibe allows operator-like identifiers that need to be converted:
 * - (+) -> _PLUS
 * - (&&) -> _AND_AND
 * - etc.
 */
export function sanitizeIdentifier(name: string): string {
  // Handle parenthesized operators
  if (name.startsWith("(") && name.endsWith(")")) {
    name = name.slice(1, -1);
  }

  // Check for operator characters - use the shared sanitizeOperator from @vibe/syntax
  if (/^[+\-*/<>=!&|^%:.~$#@?]+$/.test(name)) {
    return sanitizeOperator(name);
  }

  // Check for reserved words
  if (RESERVED_WORDS.has(name)) {
    return `$${name}`;
  }

  return name;
}
