import { describe, expect, test } from "bun:test";
import {
  OPERATOR_CHARS,
  CHAR_TO_IDENTIFIER,
  sanitizeOperator,
  isOperatorChar,
} from "../src/operators";

describe("operators", () => {
  describe("OPERATOR_CHARS", () => {
    test("contains common operator characters", () => {
      expect(OPERATOR_CHARS.has("+")).toBe(true);
      expect(OPERATOR_CHARS.has("-")).toBe(true);
      expect(OPERATOR_CHARS.has("*")).toBe(true);
      expect(OPERATOR_CHARS.has("/")).toBe(true);
      expect(OPERATOR_CHARS.has("=")).toBe(true);
      expect(OPERATOR_CHARS.has("<")).toBe(true);
      expect(OPERATOR_CHARS.has(">")).toBe(true);
      expect(OPERATOR_CHARS.has("|")).toBe(true);
      expect(OPERATOR_CHARS.has("&")).toBe(true);
      expect(OPERATOR_CHARS.has(":")).toBe(true);
    });

    test("does not contain alphanumeric characters", () => {
      expect(OPERATOR_CHARS.has("a")).toBe(false);
      expect(OPERATOR_CHARS.has("Z")).toBe(false);
      expect(OPERATOR_CHARS.has("0")).toBe(false);
      expect(OPERATOR_CHARS.has("_")).toBe(false);
    });
  });

  describe("isOperatorChar", () => {
    test("returns true for operator characters", () => {
      expect(isOperatorChar("+")).toBe(true);
      expect(isOperatorChar("|")).toBe(true);
      expect(isOperatorChar(">")).toBe(true);
    });

    test("returns false for non-operator characters", () => {
      expect(isOperatorChar("a")).toBe(false);
      expect(isOperatorChar("1")).toBe(false);
      expect(isOperatorChar(" ")).toBe(false);
    });
  });

  describe("CHAR_TO_IDENTIFIER", () => {
    test("maps common operators to identifiers", () => {
      expect(CHAR_TO_IDENTIFIER["+"]).toBe("_PLUS");
      expect(CHAR_TO_IDENTIFIER["-"]).toBe("_MINUS");
      expect(CHAR_TO_IDENTIFIER["*"]).toBe("_STAR");
      expect(CHAR_TO_IDENTIFIER["/"]).toBe("_SLASH");
      expect(CHAR_TO_IDENTIFIER["="]).toBe("_EQ");
      expect(CHAR_TO_IDENTIFIER["<"]).toBe("_LT");
      expect(CHAR_TO_IDENTIFIER[">"]).toBe("_GT");
      expect(CHAR_TO_IDENTIFIER["|"]).toBe("_PIPE");
      expect(CHAR_TO_IDENTIFIER["&"]).toBe("_AMP");
      expect(CHAR_TO_IDENTIFIER[":"]).toBe("_COLON");
    });
  });

  describe("sanitizeOperator", () => {
    test("sanitizes single-char operators", () => {
      expect(sanitizeOperator("+")).toBe("_PLUS");
      expect(sanitizeOperator("-")).toBe("_MINUS");
      expect(sanitizeOperator("*")).toBe("_STAR");
    });

    test("sanitizes multi-char operators", () => {
      expect(sanitizeOperator("==")).toBe("_EQ_EQ");
      expect(sanitizeOperator("|>")).toBe("_PIPE_GT");
      expect(sanitizeOperator("<|")).toBe("_LT_PIPE");
      expect(sanitizeOperator("->")).toBe("_MINUS_GT");
      expect(sanitizeOperator("::")).toBe("_COLON_COLON");
      expect(sanitizeOperator("++")).toBe("_PLUS_PLUS");
    });

    test("sanitizes complex operators", () => {
      expect(sanitizeOperator("<=")).toBe("_LT_EQ");
      expect(sanitizeOperator(">=")).toBe("_GT_EQ");
      expect(sanitizeOperator("/=")).toBe("_SLASH_EQ");
      expect(sanitizeOperator(">>")).toBe("_GT_GT");
      expect(sanitizeOperator("<<")).toBe("_LT_LT");
      expect(sanitizeOperator("&&")).toBe("_AMP_AMP");
      expect(sanitizeOperator("||")).toBe("_PIPE_PIPE");
    });

    test("returns original for empty string", () => {
      expect(sanitizeOperator("")).toBe("");
    });

    test("returns original for unmapped characters", () => {
      // If any char is unmapped, return original
      expect(sanitizeOperator("abc")).toBe("abc");
      expect(sanitizeOperator("+abc")).toBe("+abc");
    });
  });
});
