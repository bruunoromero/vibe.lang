import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index.ts";

const TYPE_PREAMBLE = `
type Maybe a = Just a | Nothing
`;

const expectError = (source: string, message: string) => {
  const fullSource = "module Test exposing (..)\n" + TYPE_PREAMBLE + "\n" + source;
  expect(() => analyze(parse(fullSource), { fileContext: { filePath: "Test", srcDir: "" } })).toThrow(message);
};

describe("exhaustiveness bug reproduction", () => {
  test("fails to detect non-exhaustive case on Bool tuple (the reported bug)", () => {
    // This currently PASSES (no error thrown) because of the shallow check
    // It SHOULD fail with "Non-exhaustive case expression"
    expectError(`
f x y =
  case (x, y) of
    (True, True) -> True
`, "Non-exhaustive case expression");
  });

  test("fails to detect non-exhaustive case on nested ADTs", () => {
    expectError(`
f x =
  case x of
    Just (Just True) -> 1
    Nothing -> 0
`, "Non-exhaustive case expression");
  });
});
