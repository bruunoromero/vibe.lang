import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index.ts";

const TYPE_PREAMBLE = `
type Maybe a = Just a | Nothing
type Result e a = Ok a | Err e
`;

const dedent = (str: string) => {
  const lines = str.split("\n").filter(l => l.trim().length > 0);
  if (lines.length === 0 || !lines[0]) return str;
  const match = lines[0].match(/^\s*/);
  const indent = match ? match[0].length : 0;
  return lines.map(l => l.substring(indent)).join("\n");
};

const analyzeSource = (source: string) => {
  const fullSource = "module Test exposing (..)\n" + TYPE_PREAMBLE + "\n" + dedent(source);
  return analyze(parse(fullSource), { fileContext: { filePath: "Test", srcDir: "" } });
};

const expectError = (source: string, messagePart: string) => {
  expect(() => analyzeSource(source)).toThrow(messagePart);
};

const expectSuccess = (source: string) => {
  expect(() => analyzeSource(source)).not.toThrow();
};

describe("Exhaustiveness Checking", () => {
  describe("Literals", () => {
    test("Int patterns (always non-exhaustive without wildcard)", () => {
      expectError(`
        f x = case x of
                1 -> "one"
                2 -> "two"
      `, "Non-exhaustive");
    });

    test("Int patterns exhaustive with wildcard", () => {
      expectSuccess(`
        f x = case x of
                1 -> "one"
                _ -> "other"
      `);
    });

    test("Int patterns exhaustive with variable", () => {
      expectSuccess(`
          f x = case x of
                  1 -> "one"
                  n -> "other"
        `);
    });

    test("String patterns", () => {
      expectError(`
            f x = case x of
                    "hello" -> 1
                    "world" -> 2
        `, "Non-exhaustive");

      expectSuccess(`
            f x = case x of
                    "hello" -> 1
                    _ -> 0
        `);
    });

    test("Char patterns", () => {
      expectError(`
            f x = case x of
                    'a' -> 1
                    'b' -> 2
        `, "Non-exhaustive");

      expectSuccess(`
            f x = case x of
                    'a' -> 1
                    _ -> 0
        `);
    });

    test("Float patterns", () => {
      expectError(`
            f x = case x of
                    1.0 -> 1
                    2.0 -> 2
        `, "Non-exhaustive");

      expectSuccess(`
            f x = case x of
                    1.0 -> 1
                    _ -> 0
        `);
    });
  });

  describe("Lists", () => {
    test("Empty list and Cons exhaustive", () => {
      expectSuccess(`
         f xs = case xs of
                  [] -> 0
                  y :: ys -> 1
         `);
    });

    test("Missing empty list", () => {
      expectError(`
         f xs = case xs of
                  y :: ys -> 1
         `, "Non-exhaustive"); // Missing []
    });

    test("Missing Cons", () => {
      expectError(`
         f xs = case xs of
                  [] -> 0
         `, "Non-exhaustive"); // Missing _ :: _
    });

    test("List literal syntax (sugar for nested cons)", () => {
      // [1, 2] is 1 :: 2 :: []
      // To be exhaustive we need to cover all lengths or use wildcard/cons
      expectError(`
        f xs = case xs of
                 [1, 2] -> 1
                 [] -> 0
        `, "Non-exhaustive");
    });

    test("List literal exhaustive with wildcard", () => {
      expectSuccess(`
        f xs = case xs of
                 [1, 2] -> 1
                 _ -> 0
        `);
    });

    test("List literal exhaustive with cons catch-all", () => {
      expectSuccess(`
         f xs = case xs of
                  [1, 2] -> 1
                  [] -> 0
                  _ :: _ -> 2
         `);
    });
  });

  describe("Tuples", () => {
    test("Simple tuple exhaustive", () => {
      expectSuccess(`
          f p = case p of
                  (True, True) -> 1
                  (True, False) -> 2
                  (False, True) -> 3
                  (False, False) -> 4
          `);
    });

    test("Simple tuple non-exhaustive", () => {
      expectError(`
          f p = case p of
                  (True, True) -> 1
          `, "Non-exhaustive");
    });

    test("Tuple with wildcard", () => {
      expectSuccess(`
          f p = case p of
                  (True, _) -> 1
                  (False, _) -> 2
          `);
    });
  });

  describe("ADTs", () => {
    test("Maybe exhaustive", () => {
      expectSuccess(`
          f m = case m of
                  Just x -> x
                  Nothing -> 0
          `);
    });

    test("Maybe non-exhaustive", () => {
      expectError(`
          f m = case m of
                  Just x -> x
          `, "Non-exhaustive");
    });
  });
});
