import type { Keyword, Token, Position, Span } from "@vibe/syntax";
import { TokenKind, isKeyword } from "@vibe/syntax";

const operatorCharacters = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));

class LexerState {
  private index = 0;
  private line = 1;
  private column = 1;

  constructor(private readonly source: string) {}

  position(): Position {
    return { offset: this.index, line: this.line, column: this.column };
  }

  offset(): number {
    return this.index;
  }

  isAtEnd(): boolean {
    return this.index >= this.source.length;
  }

  peek(offset = 0): string | undefined {
    return this.source[this.index + offset];
  }

  advance(): string {
    const char = this.source[this.index++];
    if (char === undefined) {
      throw new LexError("Advanced past end of input", {
        start: this.position(),
        end: this.position(),
      });
    }
    if (char === "\n") {
      this.line += 1;
      this.column = 1;
    } else {
      this.column += 1;
    }
    return char;
  }

  slice(start: number, end?: number): string {
    return this.source.slice(start, end);
  }
}

export class LexError extends Error {
  constructor(message: string, public readonly span: Span) {
    super(message);
  }
}

export function lex(source: string): Token[] {
  const state = new LexerState(source);
  const tokens: Token[] = [];

  while (!state.isAtEnd()) {
    const current = state.peek();

    if (current === undefined) {
      break;
    }

    if (isWhitespace(current)) {
      skipWhitespace(state);
      continue;
    }

    if (current === "-" && state.peek(1) === "-") {
      skipLineComment(state);
      continue;
    }

    if (current === "{" && state.peek(1) === "-") {
      skipBlockComment(state);
      continue;
    }

    const startPosition = state.position();

    if (isIdentifierStart(current)) {
      tokens.push(readIdentifierOrKeyword(state, startPosition));
      continue;
    }

    if (isDigit(current)) {
      tokens.push(readNumber(state, startPosition));
      continue;
    }

    if (current === '"') {
      tokens.push(readString(state, startPosition));
      continue;
    }

    if (current === "'") {
      tokens.push(readChar(state, startPosition));
      continue;
    }

    const punctuated = readPunctuationOrOperator(state, startPosition);
    if (punctuated) {
      tokens.push(punctuated);
      continue;
    }

    throw new LexError(`Unexpected character '${current}'`, {
      start: startPosition,
      end: state.position(),
    });
  }

  const endPosition = state.position();
  tokens.push({
    kind: TokenKind.Eof,
    lexeme: "",
    span: { start: endPosition, end: endPosition },
  });

  return tokens;
}

function readIdentifierOrKeyword(state: LexerState, start: Position): Token {
  const startIndex = state.offset();
  const first = state.advance();
  const isUpper = isUppercase(first);

  while (true) {
    const next = state.peek();
    if (next && (isIdentifierPart(next) || next === "'")) {
      state.advance();
      continue;
    }
    break;
  }

  const text = state.slice(startIndex, state.offset());
  const end = state.position();

  if (isKeyword(text)) {
    return { kind: TokenKind.Keyword, lexeme: text, span: { start, end } };
  }

  return {
    kind: isUpper ? TokenKind.UpperIdentifier : TokenKind.LowerIdentifier,
    lexeme: text,
    span: { start, end },
  };
}

function readNumber(state: LexerState, start: Position): Token {
  const startIndex = state.offset();
  consumeDigits(state);

  if (state.peek() === "." && isDigit(state.peek(1) ?? "")) {
    state.advance();
    consumeDigits(state);
  }

  const end = state.position();
  const lexeme = state.slice(startIndex, state.offset());
  return { kind: TokenKind.Number, lexeme, span: { start, end } };
}

function readString(state: LexerState, start: Position): Token {
  const startIndex = state.offset();
  state.advance();

  while (true) {
    const next = state.peek();
    if (next === undefined) {
      throw unterminated("string", start, state);
    }
    if (next === '"') {
      state.advance();
      break;
    }
    if (next === "\n") {
      throw unterminated("string", start, state);
    }
    if (next === "\\") {
      state.advance();
      const escape = state.peek();
      if (!escape || !'nrt"\\'.includes(escape)) {
        throw new LexError(`Invalid escape \\${escape ?? ""} in string`, {
          start,
          end: state.position(),
        });
      }
      state.advance();
      continue;
    }
    state.advance();
  }

  const end = state.position();
  const lexeme = state.slice(startIndex, state.offset());
  return { kind: TokenKind.String, lexeme, span: { start, end } };
}

function readChar(state: LexerState, start: Position): Token {
  const startIndex = state.offset();
  state.advance();

  const next = state.peek();
  if (next === undefined || next === "\n") {
    throw unterminated("char", start, state);
  }

  if (next === "\\") {
    state.advance();
    const escape = state.peek();
    if (!escape || !"nrt'\\".includes(escape)) {
      throw new LexError(`Invalid escape in char literal`, {
        start,
        end: state.position(),
      });
    }
    state.advance();
  } else {
    state.advance();
  }

  if (state.peek() !== "'") {
    throw new LexError("Char literal must contain exactly one character", {
      start,
      end: state.position(),
    });
  }

  state.advance();

  const end = state.position();
  const lexeme = state.slice(startIndex, state.offset());
  return { kind: TokenKind.Char, lexeme, span: { start, end } };
}

function readPunctuationOrOperator(
  state: LexerState,
  start: Position
): Token | null {
  const startIndex = state.offset();
  const current = state.peek();

  if (current === undefined) return null;

  if (current === "(")
    return makeSimpleToken(state, start, TokenKind.LParen, 1);
  if (current === ")")
    return makeSimpleToken(state, start, TokenKind.RParen, 1);
  if (current === "{")
    return makeSimpleToken(state, start, TokenKind.LBrace, 1);
  if (current === "}")
    return makeSimpleToken(state, start, TokenKind.RBrace, 1);
  if (current === "[")
    return makeSimpleToken(state, start, TokenKind.LBracket, 1);
  if (current === "]")
    return makeSimpleToken(state, start, TokenKind.RBracket, 1);
  if (current === ",") return makeSimpleToken(state, start, TokenKind.Comma, 1);

  if (current === ".") {
    if (state.peek(1) === ".") {
      state.advance();
      state.advance();
      return makeToken(state, start, TokenKind.Range, startIndex);
    }
    return makeSimpleToken(state, start, TokenKind.Dot, 1);
  }

  if (current === ":") {
    if (state.peek(1) === ":") {
      state.advance();
      state.advance();
      return makeToken(state, start, TokenKind.Operator, startIndex);
    }
    return makeSimpleToken(state, start, TokenKind.Colon, 1);
  }

  if (current === "=") {
    if (state.peek(1) === "=") {
      state.advance();
      state.advance();
      return makeToken(state, start, TokenKind.Operator, startIndex);
    }
    return makeSimpleToken(state, start, TokenKind.Equals, 1);
  }

  if (current === "|") {
    if (state.peek(1) === ">") {
      state.advance();
      state.advance();
      return makeToken(state, start, TokenKind.Operator, startIndex);
    }
    if (state.peek(1) === "|") {
      state.advance();
      state.advance();
      return makeToken(state, start, TokenKind.Operator, startIndex);
    }
    return makeSimpleToken(state, start, TokenKind.Pipe, 1);
  }

  if (current === "<" && state.peek(1) === "|") {
    state.advance();
    state.advance();
    return makeToken(state, start, TokenKind.Operator, startIndex);
  }

  if (current === "-" && state.peek(1) === ">") {
    state.advance();
    state.advance();
    return makeToken(state, start, TokenKind.Operator, startIndex);
  }

  if (current === "<" && state.peek(1) === "=") {
    state.advance();
    state.advance();
    return makeToken(state, start, TokenKind.Operator, startIndex);
  }

  if (current === ">" && state.peek(1) === "=") {
    state.advance();
    state.advance();
    return makeToken(state, start, TokenKind.Operator, startIndex);
  }

  if (current === "-" && state.peek(1) === "=") {
    // Keep "-=" as operator for future extensions; not Elm, but safe fallback.
    state.advance();
    state.advance();
    return makeToken(state, start, TokenKind.Operator, startIndex);
  }

  if (current === "\\") {
    return makeSimpleToken(state, start, TokenKind.Backslash, 1);
  }

  if (operatorCharacters.has(current)) {
    state.advance();
    while (true) {
      const next = state.peek();
      if (
        next &&
        operatorCharacters.has(next) &&
        !(next === "-" && state.peek(1) === ">")
      ) {
        state.advance();
        continue;
      }
      break;
    }
    return makeToken(state, start, TokenKind.Operator, startIndex);
  }

  return null;
}

function makeToken(
  state: LexerState,
  start: Position,
  kind: TokenKind,
  startIndex: number
): Token {
  const end = state.position();
  const lexeme = state.slice(startIndex, state.offset());
  return { kind, lexeme, span: { start, end } };
}

function makeSimpleToken(
  state: LexerState,
  start: Position,
  kind: TokenKind,
  length: number
): Token {
  const startIndex = state.offset();
  for (let i = 0; i < length; i += 1) {
    state.advance();
  }
  return makeToken(state, start, kind, startIndex);
}

function skipWhitespace(state: LexerState) {
  while (!state.isAtEnd() && isWhitespace(state.peek() ?? "")) {
    state.advance();
  }
}

function skipLineComment(state: LexerState) {
  while (!state.isAtEnd()) {
    const next = state.peek();
    if (next === "\n" || next === undefined) {
      break;
    }
    state.advance();
  }
}

function skipBlockComment(state: LexerState) {
  let depth = 0;
  const start = state.position();

  const tryStart = state.peek() === "{" && state.peek(1) === "-";
  if (tryStart) {
    depth += 1;
    state.advance();
    state.advance();
  }

  while (!state.isAtEnd() && depth > 0) {
    const current = state.peek();
    const next = state.peek(1);

    if (current === "{" && next === "-") {
      depth += 1;
      state.advance();
      state.advance();
      continue;
    }

    if (current === "-" && next === "}") {
      depth -= 1;
      state.advance();
      state.advance();
      continue;
    }

    state.advance();
  }

  if (depth !== 0) {
    throw new LexError("Unterminated block comment", {
      start,
      end: state.position(),
    });
  }
}

function consumeDigits(state: LexerState) {
  while (isDigit(state.peek() ?? "")) {
    state.advance();
  }
}

function isWhitespace(char: string): boolean {
  return char === " " || char === "\t" || char === "\n" || char === "\r";
}

function isDigit(char: string): boolean {
  return char >= "0" && char <= "9";
}

function isUppercase(char: string): boolean {
  return char >= "A" && char <= "Z";
}

function isLowercase(char: string): boolean {
  return char >= "a" && char <= "z";
}

function isIdentifierStart(char: string): boolean {
  return isLowercase(char) || isUppercase(char) || char === "_";
}

function isIdentifierPart(char: string): boolean {
  return isIdentifierStart(char) || isDigit(char);
}

function unterminated(
  kind: "string" | "char",
  start: Position,
  state: LexerState
): LexError {
  return new LexError(`Unterminated ${kind} literal`, {
    start,
    end: state.position(),
  });
}

export type { Token, TokenKind, Keyword, Position, Span };
