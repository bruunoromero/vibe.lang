/**
 * Layout preprocessing pass
 *
 * Transforms Newline tokens from the lexer into BlockStart/BlockSep/BlockEnd
 * virtual tokens based on indentation context. This implements Elm/Haskell-style
 * layout rules for keyword-triggered blocks.
 *
 * Layout keywords:
 *   - `let`   → opens a block of bindings (closed by `in` or dedent)
 *   - `of`    → opens a block of case branches (closed by dedent)
 *   - `where` → opens a block of protocol/implement methods (closed by dedent)
 *
 * The pass:
 *   1. Scans the token stream for Newline tokens
 *   2. When a layout keyword is encountered, the column of the next real token
 *      establishes a new layout context
 *   3. Within a layout context:
 *      - Newline followed by token at same column → BlockSep (sibling)
 *      - Newline followed by token at lesser column → BlockEnd (close block)
 *      - Newline followed by token at greater column → continuation (no token)
 *   4. Special tokens (`in`, closing brackets) force-close their matching contexts
 *   5. All Newline tokens are consumed; the parser never sees them
 *
 *       ┌──────────┐     ┌──────────────┐     ┌────────┐
 *       │  Lexer   │────>│ Layout Pass  │────>│ Parser │
 *       │ (tokens  │     │ (inserts     │     │        │
 *       │  + NL)   │     │  Block*)     │     │        │
 *       └──────────┘     └──────────────┘     └────────┘
 */

import { TokenKind, type Token, type Span } from "@vibe/syntax";

type LayoutContext = {
  column: number;
  keyword: "let" | "of" | "where";
  bracketDepth: number;
};

function isLayoutKeyword(lexeme: string): lexeme is "let" | "of" | "where" {
  return lexeme === "let" || lexeme === "of" || lexeme === "where";
}

function makeVirtual(kind: TokenKind, span: Span): Token {
  return { kind, lexeme: "", span };
}

/**
 * Find the next non-Newline token starting from index j.
 * Returns the token and its index, or undefined if only Eof/Newlines remain.
 */
function nextReal(
  tokens: Token[],
  j: number,
): { token: Token; index: number } | undefined {
  while (j < tokens.length) {
    const t = tokens[j]!;
    if (t.kind !== TokenKind.Newline) return { token: t, index: j };
    j++;
  }
  return undefined;
}

/**
 * Insert layout tokens (BlockStart, BlockSep, BlockEnd) into the token stream.
 * Consumes all Newline tokens — the output never contains Newlines.
 */
export function insertLayoutTokens(tokens: Token[]): Token[] {
  const output: Token[] = [];
  const layoutStack: LayoutContext[] = [];
  let bracketDepth = 0;
  let pendingLetCloses = 0;
  let i = 0;

  function layoutTop(): LayoutContext | undefined {
    return layoutStack[layoutStack.length - 1];
  }

  while (i < tokens.length) {
    const token = tokens[i]!;

    // ── EOF ───────────────────────────────────────────────────────────
    if (token.kind === TokenKind.Eof) {
      // Close all remaining layout contexts
      while (layoutStack.length > 0) {
        layoutStack.pop();
        output.push(makeVirtual(TokenKind.BlockEnd, token.span));
      }
      output.push(token);
      break;
    }

    // ── Newline ──────────────────────────────────────────────────────
    if (token.kind === TokenKind.Newline) {
      i++;

      // Find the next real token to determine indentation action
      const next = nextReal(tokens, i);
      if (!next || next.token.kind === TokenKind.Eof) continue;

      const col = next.token.span.start.column;
      const top = layoutTop();

      if (top !== undefined) {
        // Close layout contexts that the new column is below
        while (layoutStack.length > 0) {
          const current = layoutTop()!;
          if (col < current.column) {
            if (current.keyword === "let") pendingLetCloses++;
            layoutStack.pop();
            output.push(makeVirtual(TokenKind.BlockEnd, next.token.span));
          } else {
            break;
          }
        }

        // After closing, check if we're at the same column (sibling)
        const newTop = layoutTop();
        if (newTop !== undefined && col === newTop.column) {
          output.push(makeVirtual(TokenKind.BlockSep, next.token.span));
        }
      }

      continue;
    }

    // ── Bracket tracking ─────────────────────────────────────────────
    if (
      token.kind === TokenKind.LParen ||
      token.kind === TokenKind.LBracket ||
      token.kind === TokenKind.LBrace
    ) {
      bracketDepth++;
      output.push(token);
      i++;
      continue;
    }

    if (
      token.kind === TokenKind.RParen ||
      token.kind === TokenKind.RBracket ||
      token.kind === TokenKind.RBrace
    ) {
      // Close any layout contexts opened inside this bracket pair
      while (
        layoutStack.length > 0 &&
        layoutTop()!.bracketDepth >= bracketDepth
      ) {
        layoutStack.pop();
        output.push(makeVirtual(TokenKind.BlockEnd, token.span));
      }
      bracketDepth = Math.max(0, bracketDepth - 1);
      output.push(token);
      i++;
      continue;
    }

    // ── `in` keyword closes nearest `let` layout ─────────────────────
    if (token.kind === TokenKind.Keyword && token.lexeme === "in") {
      if (pendingLetCloses > 0) {
        // The NL handler already closed this `let` context via dedent.
        pendingLetCloses--;
      } else {
        // Close layouts up to the nearest `let` context still on the stack.
        const hasLetContext = layoutStack.some((ctx) => ctx.keyword === "let");
        if (hasLetContext) {
          while (layoutStack.length > 0) {
            const top = layoutTop()!;
            layoutStack.pop();
            output.push(makeVirtual(TokenKind.BlockEnd, token.span));
            if (top.keyword === "let") break;
          }
        }
      }
      output.push(token);
      i++;
      continue;
    }

    // ── Layout keywords open new blocks ──────────────────────────────
    if (token.kind === TokenKind.Keyword && isLayoutKeyword(token.lexeme)) {
      output.push(token);
      i++;

      // Skip any Newlines after the layout keyword
      while (i < tokens.length && tokens[i]!.kind === TokenKind.Newline) i++;

      // The next real token establishes the new layout column
      if (i < tokens.length && tokens[i]!.kind !== TokenKind.Eof) {
        const nextToken = tokens[i]!;
        layoutStack.push({
          column: nextToken.span.start.column,
          keyword: token.lexeme as "let" | "of" | "where",
          bracketDepth,
        });
        output.push(makeVirtual(TokenKind.BlockStart, nextToken.span));
      }

      continue;
    }

    // ── Regular token ────────────────────────────────────────────────
    output.push(token);
    i++;
  }

  return output;
}
