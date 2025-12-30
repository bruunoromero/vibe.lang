import { describe, expect, test } from "bun:test";
import { TokenType } from "@vibe/syntax";
import { tokenizeStream, type LexOptions, type LexSource } from "../src";

const collectTokens = async (source: LexSource, options?: LexOptions) => {
  const stream = tokenizeStream(source, options);
  const tokens = [];
  for await (const token of stream) {
    tokens.push(token);
  }
  const summary = await stream.result;
  return { ...summary, tokens };
};

describe("tokenize", () => {
  test("lexes a simple form", async () => {
    const result = await collectTokens("(+ 1 2)");

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.Number,
      TokenType.Number,
      TokenType.RightParen,
    ]);
    expect(result.tokens[2]?.value).toBe(1);
    expect(result.tokens[3]?.value).toBe(2);
  });

  test("supports keywords, booleans, strings, and reader macros", async () => {
    const source = '\'[:ok true false nil "hi\\n" (+ foo) bar]';
    const result = await collectTokens(source);

    expect(result.ok).toBeTrue();
    const kinds = result.tokens.map((token) => token.kind);
    expect(kinds).toEqual([
      TokenType.Quote,
      TokenType.LeftBracket,
      TokenType.Keyword,
      TokenType.Boolean,
      TokenType.Boolean,
      TokenType.Nil,
      TokenType.String,
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.Symbol,
      TokenType.RightParen,
      TokenType.Symbol,
      TokenType.RightBracket,
    ]);
    const keyword = result.tokens[2];
    expect(keyword?.lexeme).toBe(":ok");
    expect(keyword?.value).toBe("ok");
    const str = result.tokens[6];
    expect(str?.value).toBe("hi\n");
  });

  test("skips comments and records diagnostics", async () => {
    const source = '; comment\n(1 "unterminated';
    const result = await collectTokens(source);

    expect(result.ok).toBeFalse();
    expect(result.diagnostics).toHaveLength(1);
    expect(result.diagnostics[0]?.code).toBe("LEX_STRING_UNTERMINATED");
    expect(result.tokens[0]?.kind).toBe(TokenType.LeftParen);
  });

  test("tokenizeStream yields tokens as an async iterable", async () => {
    const stream = tokenizeStream("(def foo :bar)");
    const kinds: TokenType[] = [];
    for await (const token of stream) {
      kinds.push(token.kind);
    }
    expect(kinds).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.Symbol,
      TokenType.Keyword,
      TokenType.RightParen,
    ]);
    const summary = await stream.result;
    expect(summary.ok).toBeTrue();
    expect(summary.diagnostics).toHaveLength(0);
  });

  test("tokenizeStream surfaces diagnostics without buffering tokens", async () => {
    const stream = tokenizeStream('(println "oops');
    const collected: TokenType[] = [];
    for await (const token of stream) {
      collected.push(token.kind);
    }
    expect(collected).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.String,
    ]);
    const summary = await stream.result;
    expect(summary.ok).toBeFalse();
    expect(summary.diagnostics[0]?.code).toBe("LEX_STRING_UNTERMINATED");
  });

  test("handles unicode escapes and string diagnostics", async () => {
    const source = '"ok\\u0041" "\\u00ZZ" "\\z"';
    const result = await collectTokens(source);

    const values = result.tokens
      .filter((token) => token.kind === TokenType.String)
      .map((token) => token.value);
    expect(values).toEqual(["okA", "\u0000ZZ", "z"]);
    expect(result.diagnostics.map((diag) => diag.code)).toEqual([
      "LEX_STRING_ESCAPE",
      "LEX_STRING_ESCAPE",
    ]);
  });

  test("parses numeric edge cases", async () => {
    const source = ".5 -0.25 6.02e23 +42";
    const result = await collectTokens(source);

    const numbers = result.tokens
      .filter((token) => token.kind === TokenType.Number)
      .map((token) => token.value);
    expect(numbers).toEqual([0.5, -0.25, 6.02e23, 42]);
  });

  test("normalizes keywords and identifier literals", async () => {
    const source = ":simple ::auto/ns TRUE False Nil";
    const result = await collectTokens(source);

    const keywords = result.tokens
      .filter((token) => token.kind === TokenType.Keyword)
      .map((token) => token.value);
    expect(keywords).toEqual(["simple", "auto/ns"]);

    const booleans = result.tokens
      .filter((token) => token.kind === TokenType.Boolean)
      .map((token) => token.value);
    expect(booleans).toEqual([true, false]);

    const nilToken = result.tokens.find(
      (token) => token.kind === TokenType.Nil
    );
    expect(nilToken?.value).toBeNull();
  });

  test("skips UTF-8 BOM and treats commas as whitespace", async () => {
    const source = "\uFEFF(def foo [1,2,3])";
    const result = await collectTokens(source);

    expect(result.ok).toBeTrue();
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.Symbol,
      TokenType.LeftBracket,
      TokenType.Number,
      TokenType.Number,
      TokenType.Number,
      TokenType.RightBracket,
      TokenType.RightParen,
    ]);
  });

  test("skips shebang at the beginning of the source", async () => {
    const source = "#!/usr/bin/env vibe\n(def foo [1 2])";
    const result = await collectTokens(source);

    expect(result.ok).toBeTrue();
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.Symbol,
      TokenType.LeftBracket,
      TokenType.Number,
      TokenType.Number,
      TokenType.RightBracket,
      TokenType.RightParen,
    ]);
  });

  test("consumes async iterable sources with chunk boundaries", async () => {
    const chunks = {
      async *[Symbol.asyncIterator]() {
        yield "(prin";
        yield 'tln "hi"\n';
        yield "; comment";
        yield "\r\n)";
      },
    } satisfies AsyncIterable<string>;

    const result = await collectTokens(chunks);
    expect(result.ok).toBeTrue();
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.String,
      TokenType.RightParen,
    ]);
  });

  test("lexes ArrayBufferView chunks", async () => {
    const encoder = new TextEncoder();
    const iterable = {
      async *[Symbol.asyncIterator]() {
        yield encoder.encode("(foo ");
        yield encoder.encode("123)");
      },
    } satisfies AsyncIterable<Uint8Array>;

    const result = await collectTokens(iterable);

    expect(result.ok).toBeTrue();
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.Number,
      TokenType.RightParen,
    ]);
    expect(
      result.tokens.find((token) => token.kind === TokenType.Number)?.value
    ).toBe(123);
  });

  test("supports Clojure-style function names with special suffixes", async () => {
    const result = await collectTokens(
      "(foo? bar! baz* qux+ quux- foo= bar< baz> qux/path)"
    );

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
    const symbols = result.tokens.filter(
      (token) => token.kind === TokenType.Symbol
    );
    expect(symbols).toHaveLength(9);
    expect(symbols.map((s) => s.lexeme)).toEqual([
      "foo?",
      "bar!",
      "baz*",
      "qux+",
      "quux-",
      "foo=",
      "bar<",
      "baz>",
      "qux/path",
    ]);
  });

  test("supports symbols with multiple special characters", async () => {
    const result = await collectTokens("(foo?? bar!! baz*= qux+- foo-bar?)");

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
    const symbols = result.tokens.filter(
      (token) => token.kind === TokenType.Symbol
    );
    expect(symbols).toHaveLength(5);
    expect(symbols.map((s) => s.lexeme)).toEqual([
      "foo??",
      "bar!!",
      "baz*=",
      "qux+-",
      "foo-bar?",
    ]);
  });

  test("treats trailing # as gensym placeholder suffix", async () => {
    const result = await collectTokens("(foo# [1 2] foo#)");

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.LeftParen,
      TokenType.Symbol,
      TokenType.LeftBracket,
      TokenType.Number,
      TokenType.Number,
      TokenType.RightBracket,
      TokenType.Symbol,
      TokenType.RightParen,
    ]);
    const symbols = result.tokens.filter(
      (token) => token.kind === TokenType.Symbol
    );
    expect(symbols.map((token) => token.lexeme)).toEqual(["foo#", "foo#"]);
  });

  test("reports stray deref reader macros", async () => {
    const result = await collectTokens("@foo");

    expect(result.ok).toBeFalse();
    expect(result.diagnostics.map((diag) => diag.message)).toContain(
      "Unexpected character '@'"
    );
    expect(result.tokens.map((token) => token.kind)).toEqual([
      TokenType.Symbol,
    ]);
  });
});
