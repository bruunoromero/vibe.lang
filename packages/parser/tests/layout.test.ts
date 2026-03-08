import { describe, expect, test } from "bun:test";
import { parse } from "../src/index.ts";
import { lex } from "@vibe/lexer";
import { insertLayoutTokens } from "../src/layout.ts";
import { TokenKind, type Token } from "@vibe/syntax";
import type { ValueDeclaration } from "@vibe/syntax";

/**
 * Helper to parse test sources.
 * Adds "module Test exposing (..)" if the source doesn't start with a module declaration.
 */
const parseTest = (source: string) => {
  const trimmed = source.trim();
  if (trimmed.startsWith("module ")) {
    return parse(source);
  }
  return parse(`module Test exposing (..)\n${source}`);
};

/** Helper to get layout token stream for debugging */
function layoutTokens(source: string): Array<[string, string]> {
  const raw = lex(source);
  const processed = insertLayoutTokens(raw);
  return processed.map((t) => [t.kind, t.lexeme]);
}

// ===== Layout Pass Unit Tests =====

describe("Layout Pass", () => {
  test("inserts BlockStart/BlockEnd for let block", () => {
    const tokens = layoutTokens(
      `module T exposing (..)\nfoo =\n  let\n    x = 1\n  in\n    x`,
    );
    // Should see: ... let BlockStart x = 1 BlockEnd in ...
    const kinds = tokens.map(([k]) => k);
    const letIdx = kinds.indexOf("Keyword");
    // Find the 'let' keyword
    const letPositions = tokens
      .map(([k, l], i) => (k === "Keyword" && l === "let" ? i : -1))
      .filter((i) => i >= 0);
    expect(letPositions.length).toBe(1);
    const letPos = letPositions[0]!;
    expect(tokens[letPos + 1]![0]).toBe("BlockStart");
    // Find in keyword
    const inPositions = tokens
      .map(([k, l], i) => (k === "Keyword" && l === "in" ? i : -1))
      .filter((i) => i >= 0);
    expect(inPositions.length).toBe(1);
    // BlockEnd should appear before 'in'
    expect(tokens[inPositions[0]! - 1]![0]).toBe("BlockEnd");
  });

  test("inserts BlockSep between let bindings", () => {
    const tokens = layoutTokens(
      `module T exposing (..)\nfoo =\n  let\n    x = 1\n    y = 2\n  in\n    x`,
    );
    // Should contain a BlockSep between x=1 and y=2
    const kinds = tokens.map(([k]) => k);
    expect(kinds.filter((k) => k === "BlockSep").length).toBeGreaterThanOrEqual(
      1,
    );
  });

  test("inserts BlockStart/BlockEnd for case/of", () => {
    const tokens = layoutTokens(
      `module T exposing (..)\nfoo x =\n  case x of\n    True -> 1\n    False -> 0`,
    );
    const kinds = tokens.map(([k]) => k);
    const ofPositions = tokens
      .map(([k, l], i) => (k === "Keyword" && l === "of" ? i : -1))
      .filter((i) => i >= 0);
    expect(ofPositions.length).toBe(1);
    expect(tokens[ofPositions[0]! + 1]![0]).toBe("BlockStart");
    // BlockSep between the two branches
    const sepCount = kinds.filter((k) => k === "BlockSep").length;
    expect(sepCount).toBeGreaterThanOrEqual(1);
  });

  test("inserts BlockStart/BlockEnd for where block", () => {
    const tokens = layoutTokens(
      `module T exposing (..)\nprotocol Foo a where\n  bar : a -> a`,
    );
    const kinds = tokens.map(([k]) => k);
    const wherePositions = tokens
      .map(([k, l], i) => (k === "Keyword" && l === "where" ? i : -1))
      .filter((i) => i >= 0);
    expect(wherePositions.length).toBe(1);
    expect(tokens[wherePositions[0]! + 1]![0]).toBe("BlockStart");
    // BlockEnd before EOF
    const lastNonEof = tokens[tokens.length - 2]!;
    expect(lastNonEof[0]).toBe("BlockEnd");
  });

  test("no Newline tokens in output", () => {
    const tokens = layoutTokens(`module T exposing (..)\nfoo = 1\nbar = 2`);
    const newlines = tokens.filter(([k]) => k === "Newline");
    expect(newlines.length).toBe(0);
  });

  test("handles same-line let/in (no NL)", () => {
    const tokens = layoutTokens(`module T exposing (..)\nfoo = let x = 1 in x`);
    const kinds = tokens.map(([k]) => k);
    expect(kinds).toContain("BlockStart");
    expect(kinds).toContain("BlockEnd");
    // BlockEnd should come before 'in'
    const inIdx = tokens.findIndex(([k, l]) => k === "Keyword" && l === "in");
    expect(tokens[inIdx - 1]![0]).toBe("BlockEnd");
  });

  test("in keyword does not close non-let contexts", () => {
    // This tests the defensive check: if 'let' layout was already closed
    // by NL handler, 'in' should not pop 'where' context
    const tokens = layoutTokens(
      `module T exposing (..)\nimplement Foo Int where\n  bar x =\n    let\n      y = 1\n    in\n      y`,
    );
    const kinds = tokens.map(([k]) => k);
    // Should have exactly 2 BlockStart (where + let) and 2 BlockEnd
    const starts = kinds.filter((k) => k === "BlockStart").length;
    const ends = kinds.filter((k) => k === "BlockEnd").length;
    expect(starts).toBe(2);
    expect(ends).toBe(2);
  });

  test("closing bracket force-closes layout contexts", () => {
    const tokens = layoutTokens(
      `module T exposing (..)\nfoo = (case x of\n  True -> 1\n  False -> 0)`,
    );
    const kinds = tokens.map(([k]) => k);
    // Should have BlockStart after 'of' and BlockEnd before ')'
    expect(kinds).toContain("BlockStart");
    expect(kinds).toContain("BlockEnd");
  });

  test("nested let-in inside case arm of outer let-in", () => {
    // Regression: inner `in` was incorrectly consuming the outer `let` context
    const tokens = layoutTokens(
      `module T exposing (..)\nfoo x =\n  let go xs acc =\n        case xs of\n          [] -> acc\n          x :: rest ->\n            let pad = 1\n            in go rest (acc + pad)\n  in go x 0`,
    );
    const kinds = tokens.map(([k]) => k);
    // Should have 3 BlockStarts: outer let, case of, inner let
    expect(kinds.filter((k) => k === "BlockStart").length).toBe(3);
    expect(kinds.filter((k) => k === "BlockEnd").length).toBe(3);
  });

  test("nested let-in inside case arm parses correctly", () => {
    // Full parse test for nested let-in
    const ast = parseTest(
      `\nfoo x =\n  let go xs acc =\n        case xs of\n          [] -> acc\n          x :: rest ->\n            let pad = 1\n            in go rest (acc + pad)\n  in go x 0`,
    );
    expect(ast.declarations.length).toBe(1);
    const decl = ast.declarations[0]! as ValueDeclaration;
    expect(decl.name).toBe("foo");
    expect(decl.body.kind).toBe("LetIn");
  });
});

// ===== Indentation Enforcement Tests =====

describe("Indentation Enforcement", () => {
  describe("top-level column 1", () => {
    test("module declaration must be at column 1", () => {
      expect(() => parse(`  module Test exposing (..)\nfoo = 1`)).toThrow(
        "must start at column 1",
      );
    });

    test("top-level value declaration must be at column 1", () => {
      expect(() => parse(`module Test exposing (..)\n  foo = 1`)).toThrow(
        "must start at column 1",
      );
    });

    test("import declaration must be at column 1", () => {
      expect(() =>
        parse(`module Test exposing (..)\n  import Foo exposing (..)`),
      ).toThrow("must start at column 1");
    });

    test("accepts valid top-level at column 1", () => {
      const program = parseTest(`foo = 1\nbar = 2`);
      expect(program.declarations.length).toBe(2);
    });
  });

  describe("if/then/else indentation", () => {
    test("then/else on same line as if (no enforcement)", () => {
      const program = parseTest(`foo = if True then 1 else 2`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("If");
    });

    test("then/else on separate lines indented past if", () => {
      const program = parseTest(`foo =\n  if True\n    then 1\n    else 2`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("If");
    });

    test("then/else at same column as if on separate line is accepted", () => {
      const program = parseTest(`foo =\n  if True\n  then 1\n  else 2`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("If");
    });

    test("then less indented than if on separate line is rejected", () => {
      expect(() =>
        parse(`module Test exposing (..)\nfoo =\n  if True\n then 1\n else 2`),
      ).toThrow("'then' must be at least as indented as 'if'");
    });

    test("else less indented than if on separate line is rejected", () => {
      expect(() =>
        parse(
          `module Test exposing (..)\nfoo =\n  if True\n    then 1\n else 2`,
        ),
      ).toThrow("'else' must be at least as indented as 'if'");
    });
  });

  describe("function body indentation", () => {
    test("body on same line as = is valid", () => {
      const program = parseTest(`foo = 42`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("Number");
    });

    test("body on next line indented past name is valid", () => {
      const program = parseTest(`foo =\n  42`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("Number");
    });

    test("body at same column as name on separate line is rejected", () => {
      expect(() => parse(`module Test exposing (..)\nfoo =\nfoo`)).toThrow(
        "Function body must be indented past column 1",
      );
    });
  });

  describe("lambda body indentation", () => {
    test("lambda body on same line as arrow is valid", () => {
      const program = parseTest(`foo = \\x -> x`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("Lambda");
    });

    test("lambda body on next line indented past backslash is valid", () => {
      const program = parseTest(`foo =\n  \\x ->\n    x`);
      const decl = program.declarations[0] as ValueDeclaration;
      expect(decl.body.kind).toBe("Lambda");
    });
  });
});

// ===== Layout-Aware Parsing Tests =====

describe("Layout-Aware Parsing", () => {
  test("let bindings separated by layout", () => {
    const program = parseTest(
      `foo =\n  let\n    x = 1\n    y = 2\n    z = 3\n  in\n    x`,
    );
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("LetIn");
    if (decl.body.kind === "LetIn") {
      expect(decl.body.bindings.length).toBe(3);
    }
  });

  test("case branches separated by layout", () => {
    const program = parseTest(
      `foo x =\n  case x of\n    True -> 1\n    False -> 0`,
    );
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Case");
    if (decl.body.kind === "Case") {
      expect(decl.body.branches.length).toBe(2);
    }
  });

  test("nested let inside case branch", () => {
    const program = parseTest(
      `foo x =\n  case x of\n    True ->\n      let\n        y = 1\n      in\n        y\n    False -> 0`,
    );
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Case");
  });

  test("nested case inside let binding", () => {
    const program = parseTest(
      `foo x =\n  let\n    y =\n      case x of\n        True -> 1\n        False -> 0\n  in\n    y`,
    );
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("LetIn");
  });

  test("protocol with multiple methods", () => {
    const program = parseTest(
      `protocol Foo a where\n  bar : a -> a\n  baz : a -> Int`,
    );
    expect(program.declarations.length).toBe(1);
    expect(program.declarations[0]!.kind).toBe("ProtocolDeclaration");
  });

  test("implement with multiple methods", () => {
    const program = parseTest(
      `implement Foo Int where\n  bar x = x\n  baz x = 42`,
    );
    expect(program.declarations.length).toBe(1);
    expect(program.declarations[0]!.kind).toBe("ImplementationDeclaration");
  });

  test("multiple top-level declarations with layout", () => {
    const program = parseTest(`foo = 1\nbar = 2\nbaz = 3`);
    expect(program.declarations.length).toBe(3);
  });

  test("case expression ends at dedent to top level", () => {
    const program = parse(
      `module Test exposing (..)\n\nfoo x =\n  case x of\n    True -> 1\n    False -> 0\n\nbar = 42`,
    );
    expect(program.declarations.length).toBe(2);
    expect(program.declarations[1]!.kind).toBe("ValueDeclaration");
    expect((program.declarations[1] as ValueDeclaration).name).toBe("bar");
  });
});

// ===== Type Constructor Alignment Tests =====

describe("Type Constructor Alignment", () => {
  test("single-line ADT with | on same line as = is valid", () => {
    const program = parseTest(`type Bool = True | False`);
    const decl = program.declarations[0]!;
    expect(decl.kind).toBe("TypeDeclaration");
    if (decl.kind === "TypeDeclaration") {
      expect(decl.constructors?.length).toBe(2);
    }
  });

  test("multiline ADT with | aligned to = is valid", () => {
    const program = parseTest(`type Maybe a\n    = Just a\n    | Nothing`);
    const decl = program.declarations[0]!;
    expect(decl.kind).toBe("TypeDeclaration");
    if (decl.kind === "TypeDeclaration") {
      expect(decl.constructors?.length).toBe(2);
      expect(decl.constructors?.[0]?.name).toBe("Just");
      expect(decl.constructors?.[1]?.name).toBe("Nothing");
    }
  });

  test("multiline ADT with multiple | aligned to = is valid", () => {
    const program = parseTest(`type Color\n    = Red\n    | Green\n    | Blue`);
    const decl = program.declarations[0]!;
    expect(decl.kind).toBe("TypeDeclaration");
    if (decl.kind === "TypeDeclaration") {
      expect(decl.constructors?.length).toBe(3);
    }
  });

  test("multiline ADT with misaligned | is rejected", () => {
    expect(() =>
      parseTest(`type Maybe a\n    = Just a\n      | Nothing`),
    ).toThrow(/must align with '='/);
  });

  test("multiline ADT with | indented less than = is rejected", () => {
    expect(() => parseTest(`type Maybe a\n    = Just a\n  | Nothing`)).toThrow(
      /must align with '='/,
    );
  });
});
