import { describe, expect, test } from "bun:test";
import { Parser, parseSource } from "../src";
import { tokenizeStream } from "@vibe/lexer";
import type { Diagnostic, Token } from "@vibe/syntax";

const sources = [
  "(def foo [:bar [:baz 1]])",
  "'(println (unquote x) (unquote-splicing xs) [:a :b])",
  "(defrecord User [id ^:required name])",
  "(let [x 1 y 2] (+ x y))",
  '(println "unterminated',
];

const replayTokens = (tokens: readonly Token[]): AsyncIterable<Token> => ({
  async *[Symbol.asyncIterator]() {
    for (const token of tokens) {
      await Promise.resolve();
      yield token;
    }
  },
});

const collectLex = async (
  source: string
): Promise<{ tokens: Token[]; diagnostics: Diagnostic[] }> => {
  const stream = tokenizeStream(source);
  const tokens: Token[] = [];
  for await (const token of stream) {
    tokens.push(token);
  }
  const summary = await stream.result;
  return { tokens, diagnostics: summary.diagnostics };
};

describe("parser streaming consistency", () => {
  test("streaming parser matches buffered token replay", async () => {
    for (const source of sources) {
      const streamed = await parseSource(source);
      const { tokens, diagnostics } = await collectLex(source);
      const parser = new Parser(replayTokens(tokens));
      const bufferedProgram = await parser.parseProgram();
      const bufferedDiagnostics = [...diagnostics, ...parser.diagnostics];
      expect(bufferedProgram).toEqual(streamed.program);
      expect(bufferedDiagnostics).toEqual(streamed.diagnostics);
    }
  });
});
