// @ts-ignore — compiled Vibe module, no TypeScript declarations
import { lexToJson } from "@vibe/vibe-lexer";
import type { Token, Keyword, Position, Span } from "@vibe/syntax";
import { TokenKind } from "@vibe/syntax";

export class LexError extends Error {
  constructor(
    message: string,
    public readonly span: Span,
  ) {
    super(message);
  }
}

export function lex(source: string): Token[] {
  const json = (lexToJson as (s: string) => string)(source);
  const result = JSON.parse(json);
  if (!result.ok) {
    throw new LexError(result.message, result.span);
  }
  return (
    result.tokens as Array<{ kind: string; lexeme: string; span: Span }>
  ).map((t) => ({ kind: t.kind as TokenKind, lexeme: t.lexeme, span: t.span }));
}

export type { Token, TokenKind, Keyword, Position, Span };
