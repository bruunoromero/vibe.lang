import { describe, expect, test } from "bun:test";
import {
  IDENTIFIER_OPERATOR_MAPPINGS,
  KEYWORDS,
  TokenKind,
  isKeyword,
  sanitizeOperator,
} from "../src/index.ts";

describe("keywords", () => {
  test("detect known keywords", () => {
    for (const word of ["if", "then", "case", "module", "port", "exposing"]) {
      expect(isKeyword(word)).toBe(true);
    }
  });

  test("reject non-keywords", () => {
    for (const word of ["main", "Html", "String"]) {
      expect(isKeyword(word)).toBe(false);
    }
  });
});

describe("operator sanitization", () => {
  test("maps known operators", () => {
    expect(sanitizeOperator("->")).toBe("_ARROW");
    expect(sanitizeOperator("|>")).toBe("_PIPE_FORWARD");
    expect(sanitizeOperator("??")).toBe("??");
  });

  test("produces unique sanitized names", () => {
    const values = Object.values(IDENTIFIER_OPERATOR_MAPPINGS);
    expect(new Set(values).size).toBe(values.length);
  });

  test("token kinds enumerate punctuation", () => {
    expect(TokenKind.Dot).toBe(TokenKind.Dot);
    expect(TokenKind.Range).toBe(TokenKind.Range);
    expect(TokenKind.Backslash).toBe(TokenKind.Backslash);
  });
});
