import { describe, expect, test } from "bun:test";
import { lex } from "@vibe/lexer";
import {
  buildRegistryFromTokens,
  getOperatorInfo,
  mergeRegistries,
  createEmptyRegistry,
  DEFAULT_OPERATOR_INFO,
} from "../src/operator-registry.js";

describe("operator-registry", () => {
  describe("buildRegistryFromTokens", () => {
    test("parses infixl declaration", () => {
      const tokens = lex("infixl 6 +");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(0);
      expect(result.declarations).toHaveLength(1);
      expect(result.declarations[0]).toMatchObject({
        kind: "InfixDeclaration",
        fixity: "infixl",
        precedence: 6,
        operator: "+",
      });
      expect(result.registry.get("+")).toEqual({
        precedence: 6,
        associativity: "left",
      });
    });

    test("parses infixr declaration", () => {
      const tokens = lex("infixr 5 ::");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(0);
      expect(result.declarations).toHaveLength(1);
      expect(result.declarations[0]).toMatchObject({
        kind: "InfixDeclaration",
        fixity: "infixr",
        precedence: 5,
        operator: "::",
      });
      expect(result.registry.get("::")).toEqual({
        precedence: 5,
        associativity: "right",
      });
    });

    test("parses infix (non-associative) declaration", () => {
      const tokens = lex("infix 4 ==");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(0);
      expect(result.declarations).toHaveLength(1);
      expect(result.declarations[0]).toMatchObject({
        kind: "InfixDeclaration",
        fixity: "infix",
        precedence: 4,
        operator: "==",
      });
      expect(result.registry.get("==")).toEqual({
        precedence: 4,
        associativity: "none",
      });
    });

    test("parses operator in parentheses", () => {
      const tokens = lex("infixl 1 (|>)");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(0);
      expect(result.declarations).toHaveLength(1);
      expect(result.declarations[0]).toMatchObject({
        kind: "InfixDeclaration",
        fixity: "infixl",
        precedence: 1,
        operator: "|>",
      });
      expect(result.registry.get("|>")).toEqual({
        precedence: 1,
        associativity: "left",
      });
    });

    test("parses multiple infix declarations", () => {
      const source = `
        infixl 6 +
        infixl 6 -
        infixl 7 *
        infixl 7 /
      `;
      const tokens = lex(source);
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(0);
      expect(result.declarations).toHaveLength(4);
      expect(result.registry.size).toBe(4);
      expect(result.registry.get("+")).toEqual({
        precedence: 6,
        associativity: "left",
      });
      expect(result.registry.get("*")).toEqual({
        precedence: 7,
        associativity: "left",
      });
    });

    test("reports error for duplicate declarations", () => {
      const tokens = lex("infixl 6 +\ninfixl 7 +");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(1);
      expect(result.errors[0]!.message).toContain(
        "Duplicate infix declaration"
      );
      expect(result.declarations).toHaveLength(1);
    });

    test("reports error for missing precedence", () => {
      const tokens = lex("infixl +");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(1);
      expect(result.errors[0]!.message).toContain("Expected precedence number");
    });

    test("reports error for missing operator", () => {
      const tokens = lex("infixl 6");
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(1);
      expect(result.errors[0]!.message).toContain("Expected operator");
    });

    test("skips non-infix tokens", () => {
      const source = `
        x = 1
        infixl 6 +
        y = 2
      `;
      const tokens = lex(source);
      const result = buildRegistryFromTokens(tokens);

      expect(result.errors).toHaveLength(0);
      expect(result.declarations).toHaveLength(1);
      expect(result.registry.get("+")).toEqual({
        precedence: 6,
        associativity: "left",
      });
    });
  });

  describe("getOperatorInfo", () => {
    test("returns operator info from registry", () => {
      const registry = new Map([
        ["+", { precedence: 6, associativity: "left" as const }],
      ]);

      expect(getOperatorInfo(registry, "+")).toEqual({
        precedence: 6,
        associativity: "left",
      });
    });

    test("returns default for unknown operators", () => {
      const registry = createEmptyRegistry();

      expect(getOperatorInfo(registry, "???")).toEqual(DEFAULT_OPERATOR_INFO);
    });
  });

  describe("mergeRegistries", () => {
    test("merges two registries", () => {
      const base = new Map([
        ["+", { precedence: 6, associativity: "left" as const }],
      ]);
      const override = new Map([
        ["*", { precedence: 7, associativity: "left" as const }],
      ]);

      const merged = mergeRegistries(base, override);

      expect(merged.get("+")).toEqual({ precedence: 6, associativity: "left" });
      expect(merged.get("*")).toEqual({ precedence: 7, associativity: "left" });
    });

    test("override takes precedence in conflicts", () => {
      const base = new Map([
        ["+", { precedence: 6, associativity: "left" as const }],
      ]);
      const override = new Map([
        ["+", { precedence: 9, associativity: "right" as const }],
      ]);

      const merged = mergeRegistries(base, override);

      expect(merged.get("+")).toEqual({
        precedence: 9,
        associativity: "right",
      });
    });

    test("does not mutate original registries", () => {
      const base = new Map([
        ["+", { precedence: 6, associativity: "left" as const }],
      ]);
      const override = new Map([
        ["*", { precedence: 7, associativity: "left" as const }],
      ]);

      mergeRegistries(base, override);

      expect(base.size).toBe(1);
      expect(override.size).toBe(1);
    });
  });

  describe("createEmptyRegistry", () => {
    test("creates empty Map", () => {
      const registry = createEmptyRegistry();
      expect(registry.size).toBe(0);
    });
  });
});
