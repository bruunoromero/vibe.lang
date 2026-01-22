/**
 * Tests for the Document Manager and analysis pipeline.
 */
import { describe, test, expect, beforeEach } from "bun:test";
import { TextDocument } from "vscode-languageserver-textdocument";
import { DocumentManager } from "../src/document-manager";
import { SymbolKind } from "../src/types";

describe("DocumentManager", () => {
  let manager: DocumentManager;

  beforeEach(() => {
    manager = new DocumentManager();
  });

  describe("updateDocument", () => {
    test("should cache document content", () => {
      const doc = TextDocument.create("file:///test.vibe", "vibe", 1, "x = 42");

      const cache = manager.updateDocument(doc);

      expect(cache.uri).toBe("file:///test.vibe");
      expect(cache.version).toBe(1);
      expect(cache.content).toBe("x = 42");
    });

    test("should tokenize document", () => {
      const doc = TextDocument.create("file:///test.vibe", "vibe", 1, "x = 42");

      const cache = manager.updateDocument(doc);

      expect(cache.tokens).toBeDefined();
      expect(cache.tokens!.length).toBeGreaterThan(0);
    });

    test("should parse document", () => {
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        "module Test exposing (..)\n\nx = 42"
      );

      const cache = manager.updateDocument(doc);

      expect(cache.parseResult).toBeDefined();
      expect(cache.parseResult!.ast).toBeDefined();
      expect(cache.parseResult!.errors).toHaveLength(0);
    });

    test("should report parse errors", () => {
      const doc = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        "x = = 42" // Invalid syntax
      );

      const cache = manager.updateDocument(doc);

      expect(cache.diagnostics.length).toBeGreaterThan(0);
    });

    test("should return cached version if unchanged", () => {
      const doc = TextDocument.create("file:///test.vibe", "vibe", 1, "x = 42");

      const cache1 = manager.updateDocument(doc);
      const cache2 = manager.updateDocument(doc);

      // Same version should return same cache
      expect(cache1).toBe(cache2);
    });

    test("should update cache on version change", () => {
      const doc1 = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        1,
        "x = 42"
      );

      const doc2 = TextDocument.create(
        "file:///test.vibe",
        "vibe",
        2,
        "x = 43"
      );

      const cache1 = manager.updateDocument(doc1);
      const cache2 = manager.updateDocument(doc2);

      expect(cache1.version).toBe(1);
      expect(cache2.version).toBe(2);
      expect(cache2.content).toBe("x = 43");
    });
  });

  describe("getDocument", () => {
    test("should return cached document", () => {
      const doc = TextDocument.create("file:///test.vibe", "vibe", 1, "x = 42");

      manager.updateDocument(doc);
      const cache = manager.getDocument("file:///test.vibe");

      expect(cache).toBeDefined();
      expect(cache!.content).toBe("x = 42");
    });

    test("should return undefined for unknown document", () => {
      const cache = manager.getDocument("file:///unknown.vibe");
      expect(cache).toBeUndefined();
    });
  });

  describe("removeDocument", () => {
    test("should remove document from cache", () => {
      const doc = TextDocument.create("file:///test.vibe", "vibe", 1, "x = 42");

      manager.updateDocument(doc);
      manager.removeDocument("file:///test.vibe");

      expect(manager.getDocument("file:///test.vibe")).toBeUndefined();
    });
  });

  describe("getSymbolsAtPosition", () => {
    test("should return symbols for analyzed document", () => {
      const doc = TextDocument.create(
        "file:///Test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

add x y = x
`
      );

      manager.updateDocument(doc);
      const symbols = manager.getSymbolsAtPosition("file:///Test.vibe");

      const addSymbol = symbols.find((s) => s.name === "add");
      expect(addSymbol).toBeDefined();
      expect(addSymbol!.kind).toBe(SymbolKind.Function);
    });

    test("should return type declarations", () => {
      const doc = TextDocument.create(
        "file:///Test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

type Maybe a = Just a | Nothing
`
      );

      manager.updateDocument(doc);
      const symbols = manager.getSymbolsAtPosition("file:///Test.vibe");

      const maybeType = symbols.find((s) => s.name === "Maybe");
      expect(maybeType).toBeDefined();
      expect(maybeType!.kind).toBe(SymbolKind.Type);

      const justCtor = symbols.find((s) => s.name === "Just");
      expect(justCtor).toBeDefined();
      expect(justCtor!.kind).toBe(SymbolKind.Constructor);
    });

    test("should return empty array for unknown document", () => {
      const symbols = manager.getSymbolsAtPosition("file:///unknown.vibe");
      expect(symbols).toEqual([]);
    });
  });

  describe("getHoverInfo", () => {
    test("should return type info for values", () => {
      const doc = TextDocument.create(
        "file:///Test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

x = 42
`
      );

      manager.updateDocument(doc);
      // Line 2 (0-indexed), column 0 should be 'x'
      const hover = manager.getHoverInfo("file:///Test.vibe", 2, 0);

      expect(hover).toBeDefined();
      expect(hover!.name).toBe("x");
    });

    test("should return undefined for whitespace", () => {
      const doc = TextDocument.create(
        "file:///Test.vibe",
        "vibe",
        1,
        `module Test exposing (..)

x = 42
`
      );

      manager.updateDocument(doc);
      // Empty line
      const hover = manager.getHoverInfo("file:///Test.vibe", 1, 0);

      expect(hover).toBeUndefined();
    });
  });

  describe("formatTypeScheme", () => {
    test("should format simple types", () => {
      const scheme = {
        vars: new Set<number>(),
        constraints: [],
        type: { kind: "con" as const, name: "Int", args: [] },
      };

      const formatted = manager.formatTypeScheme(scheme);
      expect(formatted).toBe("Int");
    });

    test("should format function types", () => {
      const scheme = {
        vars: new Set<number>(),
        constraints: [],
        type: {
          kind: "fun" as const,
          from: { kind: "con" as const, name: "Int", args: [] },
          to: { kind: "con" as const, name: "Int", args: [] },
        },
      };

      const formatted = manager.formatTypeScheme(scheme);
      expect(formatted).toBe("Int -> Int");
    });

    test("should format polymorphic types", () => {
      const scheme = {
        vars: new Set([0]),
        constraints: [],
        type: {
          kind: "fun" as const,
          from: { kind: "var" as const, id: 0 },
          to: { kind: "var" as const, id: 0 },
        },
      };

      const formatted = manager.formatTypeScheme(scheme);
      expect(formatted).toBe("a -> a");
    });

    test("should format constrained types", () => {
      const scheme = {
        vars: new Set([0]),
        constraints: [
          {
            protocolName: "Num",
            typeArgs: [{ kind: "var" as const, id: 0 }],
          },
        ],
        type: {
          kind: "fun" as const,
          from: { kind: "var" as const, id: 0 },
          to: { kind: "var" as const, id: 0 },
        },
      };

      const formatted = manager.formatTypeScheme(scheme);
      expect(formatted).toBe("Num a => a -> a");
    });
  });
});
