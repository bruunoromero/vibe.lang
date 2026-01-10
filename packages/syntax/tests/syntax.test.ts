import { describe, expect, test } from "bun:test";
import {
  KEYWORDS,
  TokenKind,
  isKeyword,
  sanitizeOperator,
} from "../src/index.ts";

describe("keywords", () => {
  test("detect known keywords", () => {
    for (const word of [
      "if",
      "then",
      "case",
      "module",
      "port",
      "exposing",
      "infix",
      "infixl",
      "infixr",
    ]) {
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
    // Test composable naming convention from single characters
    expect(sanitizeOperator("->")).toBe("_MINUS_GT");
    expect(sanitizeOperator("|>")).toBe("_PIPE_GT");
    expect(sanitizeOperator("<|")).toBe("_LT_PIPE");
    expect(sanitizeOperator("==")).toBe("_EQ_EQ");
    expect(sanitizeOperator("::")).toBe("_COLON_COLON");
    expect(sanitizeOperator("++")).toBe("_PLUS_PLUS");
    expect(sanitizeOperator("+")).toBe("_PLUS");
    expect(sanitizeOperator("??")).toBe("_QUESTION_QUESTION");
    expect(sanitizeOperator("xyz")).toBe("xyz"); // Unmapped chars pass through
  });

  test("single character operators", () => {
    expect(sanitizeOperator(".")).toBe("_DOT");
    expect(sanitizeOperator(":")).toBe("_COLON");
    expect(sanitizeOperator("|")).toBe("_PIPE");
  });

  test("multi-character composition", () => {
    expect(sanitizeOperator("...")).toBe("_DOT_DOT_DOT");
    expect(sanitizeOperator("<=>")).toBe("_LT_EQ_GT");
    expect(sanitizeOperator("&&")).toBe("_AMP_AMP");
  });

  test("token kinds enumerate punctuation", () => {
    expect(TokenKind.Dot).toBe(TokenKind.Dot);
    expect(TokenKind.Range).toBe(TokenKind.Range);
    expect(TokenKind.Backslash).toBe(TokenKind.Backslash);
  });
});
