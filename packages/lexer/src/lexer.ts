import type {
  Diagnostic,
  SourcePosition,
  Token,
  TokenValue,
} from "@vibe/syntax";
import {
  DiagnosticSeverity,
  TokenType,
  clonePosition,
  createSpan,
} from "@vibe/syntax";

import { CharacterStream, type LexSource } from "./stream";

export interface LexOptions {
  allowShebang?: boolean;
}

export interface LexSummary {
  ok: boolean;
  diagnostics: Diagnostic[];
}

export interface TokenStream extends AsyncIterable<Token> {
  result: Promise<LexSummary>;
}

interface LexerConfig {
  onToken?: (token: Token) => void;
}

export class Lexer {
  private readonly stream: CharacterStream;
  private readonly options: Required<LexOptions>;
  private readonly onToken?: (token: Token) => void;
  private readonly diagnostics: Diagnostic[] = [];
  private offset = 0;
  private line = 0;
  private column = 0;
  private history = "";
  private historyStartOffset = 0;
  private previousWasCarriageReturn = false;

  constructor(
    source: LexSource,
    options: LexOptions = {},
    config: Partial<LexerConfig> = {}
  ) {
    this.stream = new CharacterStream(source);
    this.options = {
      allowShebang: options.allowShebang ?? true,
    };
    this.onToken = config.onToken;
  }

  async lex(): Promise<LexSummary> {
    await this.skipBom();
    await this.skipShebang();
    this.resetHistory();

    while (true) {
      await this.skipWhitespace();
      this.resetHistory();

      if (await this.isAtEnd()) {
        break;
      }

      const start = this.snapshot();
      const ch = await this.peek();

      if (ch === ";") {
        await this.skipComment();
        continue;
      }

      switch (ch) {
        case "(":
          await this.advance();
          this.addToken(TokenType.LeftParen, start);
          continue;
        case ")":
          await this.advance();
          this.addToken(TokenType.RightParen, start);
          continue;
        case "[":
          await this.advance();
          this.addToken(TokenType.LeftBracket, start);
          continue;
        case "]":
          await this.advance();
          this.addToken(TokenType.RightBracket, start);
          continue;
        case "{":
          await this.advance();
          this.addToken(TokenType.LeftBrace, start);
          continue;
        case "}":
          await this.advance();
          this.addToken(TokenType.RightBrace, start);
          continue;
        case "'":
          await this.advance();
          this.addToken(TokenType.Quote, start);
          continue;
        case "`":
          await this.advance();
          this.addToken(TokenType.SyntaxQuote, start);
          continue;
        case "~":
          await this.advance();
          if ((await this.peek()) === "@") {
            await this.advance();
            this.addToken(TokenType.UnquoteSplicing, start);
          } else {
            this.addToken(TokenType.Unquote, start);
          }
          continue;
        case "#":
          await this.advance();
          this.addToken(TokenType.Dispatch, start);
          continue;
        case '"':
          await this.readString(start);
          continue;
        case "\\":
          await this.readCharacter(start);
          continue;
        default:
          break;
      }

      if (await this.isNumberStart(ch)) {
        await this.readNumber(start);
        continue;
      }

      if (ch === ":") {
        await this.readKeyword(start);
        continue;
      }

      if (this.isSymbolStart(ch)) {
        await this.readSymbol(start);
        continue;
      }

      await this.advance();
      this.report(`Unexpected character '${ch}'`, start, this.snapshot());
      this.resetHistory();
    }

    return {
      ok: this.diagnostics.length === 0,
      diagnostics: this.diagnostics,
    };
  }

  private async skipBom(): Promise<void> {
    if (this.offset !== 0) {
      return;
    }
    if ((await this.peek()) === "\uFEFF") {
      await this.advance();
    }
  }

  private async skipShebang(): Promise<void> {
    if (!this.options.allowShebang || this.offset !== 0) {
      return;
    }
    if ((await this.peek()) === "#" && (await this.peek(1)) === "!") {
      while (
        !(await this.isAtEnd()) &&
        !this.isLineTerminator(await this.peek())
      ) {
        await this.advance();
      }
      if (!(await this.isAtEnd())) {
        await this.advance();
      }
    }
  }

  private async skipWhitespace(): Promise<void> {
    while (true) {
      const ch = await this.peek();
      if (ch === "" || (ch !== "," && !this.isWhitespace(ch))) {
        break;
      }
      await this.advance();
    }
  }

  private async skipComment(): Promise<void> {
    await this.advance();
    while (
      !(await this.isAtEnd()) &&
      !this.isLineTerminator(await this.peek())
    ) {
      await this.advance();
    }
    if (!(await this.isAtEnd())) {
      await this.advance();
    }
    this.resetHistory();
  }

  private async readString(start: SourcePosition): Promise<void> {
    await this.advance();
    let value = "";

    while (!(await this.isAtEnd())) {
      const ch = await this.peek();
      if (ch === '"') {
        await this.advance();
        this.addToken(TokenType.String, start, value);
        return;
      }
      if (ch === "\\") {
        await this.advance();
        if (await this.isAtEnd()) {
          break;
        }
        value += await this.readEscapeSequence(start);
        continue;
      }
      if (this.isLineTerminator(ch)) {
        this.report(
          "Unterminated string literal",
          start,
          this.snapshot(),
          "LEX_STRING_UNTERMINATED"
        );
        await this.advance();
        this.addToken(TokenType.String, start, value);
        return;
      }
      value += await this.advance();
    }

    this.report(
      "Unterminated string literal",
      start,
      this.snapshot(),
      "LEX_STRING_UNTERMINATED"
    );
    this.addToken(TokenType.String, start, value);
  }

  private async readEscapeSequence(start: SourcePosition): Promise<string> {
    const ch = await this.advance();
    switch (ch) {
      case '"':
        return '"';
      case "'":
        return "'";
      case "\\":
        return "\\";
      case "n":
        return "\n";
      case "r":
        return "\r";
      case "t":
        return "\t";
      case "b":
        return "\b";
      case "f":
        return "\f";
      case "u": {
        let hex = "";
        for (let i = 0; i < 4; i += 1) {
          const next = await this.peek();
          if (!this.isHexDigit(next)) {
            this.report(
              "Invalid unicode escape sequence",
              start,
              this.snapshot(),
              "LEX_STRING_ESCAPE"
            );
            break;
          }
          hex += await this.advance();
        }
        const codePoint = Number.parseInt(hex, 16);
        if (Number.isNaN(codePoint)) {
          this.report(
            "Invalid unicode escape sequence",
            start,
            this.snapshot(),
            "LEX_STRING_ESCAPE"
          );
          return "";
        }
        return String.fromCodePoint(codePoint);
      }
      default:
        this.report(
          `Unknown escape sequence \\${ch}`,
          start,
          this.snapshot(),
          "LEX_STRING_ESCAPE"
        );
        return ch ?? "";
    }
  }

  private async readCharacter(start: SourcePosition): Promise<void> {
    await this.advance();
    if (await this.isAtEnd()) {
      this.report(
        "Unterminated character literal",
        start,
        this.snapshot(),
        "LEX_CHAR_UNTERMINATED"
      );
      return;
    }

    if ((await this.peek()) === "u") {
      await this.advance();
      let hex = "";
      for (let i = 0; i < 4; i += 1) {
        const next = await this.peek();
        if (!this.isHexDigit(next)) {
          this.report(
            "Invalid unicode character literal",
            start,
            this.snapshot(),
            "LEX_CHAR_INVALID"
          );
          break;
        }
        hex += await this.advance();
      }
      const codePoint = Number.parseInt(hex, 16);
      if (Number.isNaN(codePoint)) {
        this.report(
          "Invalid unicode character literal",
          start,
          this.snapshot(),
          "LEX_CHAR_INVALID"
        );
        return;
      }
      this.addToken(
        TokenType.Character,
        start,
        String.fromCodePoint(codePoint)
      );
      return;
    }

    let literal = "";
    while (!(await this.isAtEnd()) && !this.isDelimiter(await this.peek())) {
      literal += await this.advance();
    }

    if (literal.length === 0) {
      this.report(
        "Invalid character literal",
        start,
        this.snapshot(),
        "LEX_CHAR_INVALID"
      );
      return;
    }

    const value = this.resolveNamedCharacter(literal);
    if (value === undefined) {
      if (literal.length === 1) {
        this.addToken(TokenType.Character, start, literal);
      } else {
        this.report(
          "Unknown character literal",
          start,
          this.snapshot(),
          "LEX_CHAR_UNKNOWN"
        );
      }
      return;
    }

    this.addToken(TokenType.Character, start, value);
  }

  private resolveNamedCharacter(name: string): string | undefined {
    switch (name) {
      case "space":
        return " ";
      case "tab":
        return "\t";
      case "newline":
        return "\n";
      case "return":
        return "\r";
      case "formfeed":
        return "\f";
      case "backspace":
        return "\b";
      default:
        return undefined;
    }
  }

  private async readNumber(start: SourcePosition): Promise<void> {
    if ((await this.peek()) === "+" || (await this.peek()) === "-") {
      await this.advance();
    }

    await this.consumeDigits();

    if ((await this.peek()) === "." && this.isDigit(await this.peek(1))) {
      await this.advance();
      await this.consumeDigits();
    }

    if (
      await this.isExponentStart(
        await this.peek(),
        await this.peek(1),
        await this.peek(2)
      )
    ) {
      await this.advance();
      if ((await this.peek()) === "+" || (await this.peek()) === "-") {
        await this.advance();
      }
      await this.consumeDigits();
    }

    const lexeme = this.sliceFrom(start);
    const value = Number(lexeme);
    if (Number.isNaN(value)) {
      this.report(
        "Invalid numeric literal",
        start,
        this.snapshot(),
        "LEX_NUMBER_INVALID"
      );
      return;
    }
    this.addToken(TokenType.Number, start, value);
  }

  private async readKeyword(start: SourcePosition): Promise<void> {
    await this.advance();
    let doubleColon = false;
    if ((await this.peek()) === ":") {
      doubleColon = true;
      await this.advance();
    }

    if (!this.isSymbolChar(await this.peek())) {
      this.report(
        "Invalid keyword literal",
        start,
        this.snapshot(),
        "LEX_KEYWORD_INVALID"
      );
      return;
    }

    while (this.isSymbolChar(await this.peek())) {
      await this.advance();
    }

    const lexeme = this.sliceFrom(start);
    const value = doubleColon ? lexeme.slice(2) : lexeme.slice(1);
    this.addToken(TokenType.Keyword, start, value);
  }

  private async readSymbol(start: SourcePosition): Promise<void> {
    let length = 0;
    while (this.isSymbolChar(await this.peek())) {
      await this.advance();
      length += 1;
    }

    if (
      length > 0 &&
      (await this.peek()) === "#" &&
      this.isAutoGensymSuffixBoundary(await this.peek(1))
    ) {
      await this.advance();
    }

    const lexeme = this.sliceFrom(start);
    const normalized = lexeme.toLowerCase();

    if (normalized === "nil") {
      this.addToken(TokenType.Nil, start, null);
      return;
    }

    if (normalized === "true" || normalized === "false") {
      this.addToken(TokenType.Boolean, start, normalized === "true");
      return;
    }

    this.addToken(TokenType.Symbol, start, lexeme);
  }

  private async consumeDigits(): Promise<void> {
    while (this.isDigit(await this.peek())) {
      await this.advance();
    }
  }

  private sliceFrom(start: SourcePosition): string {
    const relativeStart = start.offset - this.historyStartOffset;
    const relativeEnd = this.offset - this.historyStartOffset;
    if (relativeStart < 0 || relativeEnd < relativeStart) {
      return "";
    }
    return this.history.slice(relativeStart, relativeEnd);
  }

  private addToken(
    kind: TokenType,
    start: SourcePosition,
    value?: TokenValue
  ): void {
    const span = createSpan(start, this.snapshot());
    const lexeme = this.sliceFrom(start);
    const token: Token = { kind, lexeme, span };
    if (value !== undefined) {
      token.value = value;
    }
    if (this.onToken) {
      this.onToken(token);
    }
    this.resetHistory();
  }

  private report(
    message: string,
    start: SourcePosition,
    end: SourcePosition,
    code?: string
  ): void {
    this.diagnostics.push({
      message,
      span: createSpan(start, end),
      severity: DiagnosticSeverity.Error,
      code,
    });
  }

  private snapshot(): SourcePosition {
    return clonePosition({
      offset: this.offset,
      line: this.line,
      column: this.column,
    });
  }

  private async isAtEnd(): Promise<boolean> {
    return (await this.peek()) === "";
  }

  private peek(offset = 0): Promise<string> {
    return this.stream.peek(offset);
  }

  private async advance(): Promise<string> {
    const ch = await this.stream.advance();
    if (ch === "") {
      return "";
    }

    this.history += ch;
    this.offset += 1;

    if (ch === "\r") {
      this.line += 1;
      this.column = 0;
      this.previousWasCarriageReturn = true;
      return ch;
    }

    if (ch === "\n") {
      if (!this.previousWasCarriageReturn) {
        this.line += 1;
      }
      this.column = 0;
      this.previousWasCarriageReturn = false;
      return ch;
    }

    this.previousWasCarriageReturn = false;
    this.column += 1;
    return ch;
  }

  private resetHistory(): void {
    this.history = "";
    this.historyStartOffset = this.offset;
  }

  private isWhitespace(ch: string): boolean {
    return (
      ch === " " ||
      ch === "\t" ||
      ch === "\n" ||
      ch === "\r" ||
      ch === "\v" ||
      ch === "\f"
    );
  }

  private isLineTerminator(ch: string): boolean {
    return ch === "\n" || ch === "\r";
  }

  private isDigit(ch: string): boolean {
    return ch >= "0" && ch <= "9";
  }

  private isHexDigit(ch: string): boolean {
    return /[0-9a-fA-F]/.test(ch);
  }

  private async isNumberStart(ch: string): Promise<boolean> {
    const next = await this.peek(1);
    if (this.isDigit(ch)) {
      return true;
    }
    if (
      (ch === "+" || ch === "-") &&
      (this.isDigit(next) || (next === "." && this.isDigit(await this.peek(2))))
    ) {
      return true;
    }
    if (ch === "." && this.isDigit(next)) {
      return true;
    }
    return false;
  }

  private async isExponentStart(
    current: string,
    next: string,
    afterNext: string
  ): Promise<boolean> {
    if (current !== "e" && current !== "E") {
      return false;
    }
    if (this.isDigit(next)) {
      return true;
    }
    if ((next === "+" || next === "-") && this.isDigit(afterNext)) {
      return true;
    }
    return false;
  }

  private isDelimiter(ch: string): boolean {
    switch (ch) {
      case "":
      case "(":
      case ")":
      case "[":
      case "]":
      case "{":
      case "}":
      case '"':
      case "'":
      case "`":
      case "~":
      case "@":
      case "#":
      case ":":
      case "\\":
      case ";":
        return true;
      default:
        return this.isWhitespace(ch);
    }
  }

  private isSymbolSuffix(ch: string): boolean {
    // Allow Clojure-style suffixes: ?, !, *, +, -, =, <, >, /, #, etc.
    return /[?!*+\-=<>/#]/.test(ch);
  }

  private isSymbolStart(ch: string): boolean {
    if (!ch) {
      return false;
    }
    if (this.isDigit(ch)) {
      return false;
    }
    if (this.isDelimiter(ch)) {
      return false;
    }
    return true;
  }

  private isSymbolChar(ch: string): boolean {
    if (!ch) {
      return false;
    }
    if (this.isDelimiter(ch)) {
      return false;
    }
    // Allow special suffix characters: ?, !, *, etc.
    if (this.isSymbolSuffix(ch)) {
      return true;
    }
    return true;
  }

  private isAutoGensymSuffixBoundary(next: string): boolean {
    if (next === "{") {
      return false;
    }
    if (next === "") {
      return true;
    }
    return this.isDelimiter(next);
  }
}

export const tokenizeStream = (
  source: LexSource,
  options?: LexOptions
): TokenStream => {
  const queue: Token[] = [];
  let wake: (() => void) | null = null;
  let finished = false;

  const pushToken = (token: Token): void => {
    queue.push(token);
    if (wake) {
      wake();
      wake = null;
    }
  };

  const lexer = new Lexer(source, options, {
    onToken: pushToken,
  });

  const resultPromise = lexer.lex().then(({ ok, diagnostics }) => {
    finished = true;
    if (wake) {
      wake();
      wake = null;
    }
    return { ok, diagnostics } satisfies LexSummary;
  });

  const iterator = (async function* (): AsyncGenerator<Token> {
    while (true) {
      if (queue.length > 0) {
        yield queue.shift()!;
        continue;
      }
      if (finished) {
        break;
      }
      await new Promise<void>((resolve) => {
        wake = resolve;
      });
    }
  })();

  const tokenStream: TokenStream = Object.assign(iterator, {
    result: resultPromise,
  });
  return tokenStream;
};
