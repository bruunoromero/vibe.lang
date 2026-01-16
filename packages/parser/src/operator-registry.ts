/**
 * Operator Registry - Manages operator precedence and associativity.
 *
 * This module provides utilities for:
 * - Building an operator registry from tokens (pre-scan)
 * - Querying operator info (precedence, associativity)
 * - Merging registries from imports
 *
 * Operators are defined in the language itself via infix/infixl/infixr declarations.
 * This module extracts those declarations from tokens before the main parse pass.
 */

import {
  TokenKind,
  type Token,
  type Span,
  type InfixDeclaration,
  type OperatorInfo,
  type OperatorRegistry,
  BUILTIN_OPERATOR_FIXITY,
} from "@vibe/syntax";

/**
 * Error during infix declaration parsing.
 * Uses the same shape as ParseError for consistency.
 */
export class InfixParseError extends Error {
  constructor(message: string, public readonly span: Span) {
    super(message);
  }
}

/**
 * Default operator info for unknown operators.
 * Used when an operator has no explicit infix declaration.
 *
 * Precedence 9 (high) and left associativity ensures unknown operators
 * bind tightly and behave predictably without explicit declarations.
 */
export const DEFAULT_OPERATOR_INFO: OperatorInfo = {
  precedence: 9,
  associativity: "left",
};

/**
 * Result of building an operator registry from tokens.
 */
export type RegistryBuildResult = {
  /** The built operator registry */
  registry: OperatorRegistry;
  /** Parsed infix declarations (for AST inclusion) */
  declarations: InfixDeclaration[];
  /** Errors encountered during parsing */
  errors: InfixParseError[];
};

/**
 * Build an operator registry from a token stream.
 *
 * This performs a fast pre-scan over tokens to find all infix/infixl/infixr
 * declarations and builds a registry mapping operator strings to their
 * precedence and associativity.
 *
 * @param tokens - Token stream from the lexer
 * @returns Registry, declarations, and any parsing errors
 */
export function buildRegistryFromTokens(tokens: Token[]): RegistryBuildResult {
  const registry: OperatorRegistry = new Map();
  const declarations: InfixDeclaration[] = [];
  const errors: InfixParseError[] = [];

  let i = 0;

  while (i < tokens.length && tokens[i]!.kind !== TokenKind.Eof) {
    const tok = tokens[i]!;

    // Look for infix/infixl/infixr keywords
    if (
      tok.kind === TokenKind.Keyword &&
      (tok.lexeme === "infix" ||
        tok.lexeme === "infixl" ||
        tok.lexeme === "infixr")
    ) {
      const start = tok.span.start;
      const fixity = tok.lexeme as "infix" | "infixl" | "infixr";
      i++;

      // Parse precedence number
      if (i >= tokens.length || tokens[i]!.kind !== TokenKind.Number) {
        errors.push(
          new InfixParseError(
            "Expected precedence number after infix keyword",
            tokens[i - 1]!.span
          )
        );
        continue;
      }
      const precedence = parseInt(tokens[i]!.lexeme, 10);
      i++;

      // Parse operator (optional parens)
      let operator: string;
      let end: Span["end"];

      if (i < tokens.length && tokens[i]!.kind === TokenKind.LParen) {
        i++; // consume (
        if (i >= tokens.length || tokens[i]!.kind !== TokenKind.Operator) {
          errors.push(
            new InfixParseError(
              "Expected operator inside parentheses",
              tokens[i - 1]!.span
            )
          );
          continue;
        }
        operator = tokens[i]!.lexeme;
        i++; // consume operator
        if (i >= tokens.length || tokens[i]!.kind !== TokenKind.RParen) {
          errors.push(
            new InfixParseError(
              "Expected closing parenthesis after operator",
              tokens[i - 1]!.span
            )
          );
          continue;
        }
        end = tokens[i]!.span.end;
        i++; // consume )
      } else if (i < tokens.length && tokens[i]!.kind === TokenKind.Operator) {
        operator = tokens[i]!.lexeme;
        end = tokens[i]!.span.end;
        i++;
      } else {
        errors.push(
          new InfixParseError(
            "Expected operator after precedence in infix declaration",
            tokens[i - 1]!.span
          )
        );
        continue;
      }

      // Convert fixity to associativity
      const associativity: "left" | "right" | "none" =
        fixity === "infixl" ? "left" : fixity === "infixr" ? "right" : "none";

      // Check for duplicate declarations
      if (registry.has(operator)) {
        errors.push(
          new InfixParseError(
            `Duplicate infix declaration for operator '${operator}'`,
            { start, end }
          )
        );
        continue;
      }

      registry.set(operator, { precedence, associativity });
      declarations.push({
        kind: "InfixDeclaration",
        fixity,
        precedence,
        operator,
        span: { start, end },
      });
    } else {
      i++;
    }
  }

  return { registry, declarations, errors };
}

/**
 * Get operator info from a registry, falling back to defaults.
 *
 * @param registry - The operator registry to query
 * @param operator - The operator string to look up
 * @returns Operator info (precedence and associativity)
 */
export function getOperatorInfo(
  registry: OperatorRegistry,
  operator: string
): OperatorInfo {
  return registry.get(operator) ?? DEFAULT_OPERATOR_INFO;
}

/**
 * Merge two operator registries.
 *
 * The second registry's entries override the first's in case of conflict.
 * This is used for composing registries from imports with local declarations.
 *
 * @param base - Base registry (e.g., from imports or defaults)
 * @param override - Override registry (e.g., local declarations)
 * @returns New merged registry
 */
export function mergeRegistries(
  base: OperatorRegistry,
  override: OperatorRegistry
): OperatorRegistry {
  const merged: OperatorRegistry = new Map(base);
  for (const [op, info] of override) {
    merged.set(op, info);
  }
  return merged;
}

/**
 * Create an empty operator registry.
 */
export function createEmptyRegistry(): OperatorRegistry {
  return new Map();
}

/**
 * Builtin operator registry.
 *
 * Contains operators that are built into the compiler and cannot be
 * redefined by user code. These are:
 * - && (logical AND with short-circuit evaluation)
 * - || (logical OR with short-circuit evaluation)
 *
 * The parser uses this as the base registry, and user-defined operator
 * declarations are merged on top.
 *
 * Built from BUILTIN_OPERATOR_FIXITY in @vibe/syntax.
 */
export const BUILTIN_OPERATOR_REGISTRY: OperatorRegistry = new Map(
  Object.entries(BUILTIN_OPERATOR_FIXITY)
);

// Re-export types for convenience
export type { OperatorInfo, OperatorRegistry, InfixDeclaration };
