/**
 * Tests for semantic token generation.
 */
import { describe, test, expect } from "bun:test";
import { TextDocument } from "vscode-languageserver-textdocument";
import { DocumentManager } from "../src/document-manager";
import { provideSemanticTokens, TOKEN_TYPES } from "../src/semantic-tokens";

describe("Semantic Tokens", () => {
  describe("provideSemanticTokens", () => {
    test("should generate tokens for simple value declaration", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

x = 42
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      // Should have some tokens
      expect(tokens.length).toBeGreaterThan(0);
      // Tokens come in groups of 5 (deltaLine, deltaChar, length, type, modifiers)
      expect(tokens.length % 5).toBe(0);
    });

    test("should generate tokens for function declaration", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

add x y = x
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should generate tokens for type declarations", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

type Maybe a = Just a | Nothing
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should generate tokens for type annotations", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

add : Int -> Int -> Int
add x y = x
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should generate tokens for protocol declarations", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

protocol Show a where
  show : a -> String
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should generate tokens for lambda expressions", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

f = \\x -> x
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should generate tokens for case expressions", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

type MyBool = MyTrue | MyFalse

not x =
  case x of
    MyTrue -> MyFalse
    MyFalse -> MyTrue
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should generate tokens for let-in expressions", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

f x =
  let
    y = x
  in
    y
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      expect(tokens.length).toBeGreaterThan(0);
    });

    test("should handle empty document", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create("file:///test.vibe", "vibe", 1, "");

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      // Empty document should produce no tokens (or minimal)
      expect(Array.isArray(tokens)).toBe(true);
    });

    test("should handle document with only comments", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `-- This is a comment
{- This is a
   block comment -}
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      // Comments might not produce tokens depending on implementation
      expect(Array.isArray(tokens)).toBe(true);
    });
  });

  describe("Token encoding", () => {
    test("tokens should be in delta format", () => {
      const manager = new DocumentManager();
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

x = 42
y = 43
`
      );

      const cache = manager.updateDocument(doc);
      const tokens = provideSemanticTokens(cache);

      // First token should have absolute position (as delta from 0,0)
      // All values should be non-negative
      for (let i = 0; i < tokens.length; i += 5) {
        const deltaLine = tokens[i];
        const deltaChar = tokens[i + 1];
        const length = tokens[i + 2];
        const type = tokens[i + 3];
        const modifiers = tokens[i + 4];

        expect(deltaLine).toBeGreaterThanOrEqual(0);
        expect(deltaChar).toBeGreaterThanOrEqual(0);
        expect(length).toBeGreaterThan(0);
        expect(type).toBeGreaterThanOrEqual(0);
        expect(type).toBeLessThan(TOKEN_TYPES.length);
        expect(modifiers).toBeGreaterThanOrEqual(0);
      }
    });
  });
});
