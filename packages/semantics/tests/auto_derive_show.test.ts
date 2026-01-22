
import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "../src/index";

describe("Show Auto-Derivation", () => {

  // Mock Vibe.Basics
  const basicsSource = `
module Vibe.Basics exposing (..)
protocol Show a where
  toString : a -> String

@external "js" "intToString"
intToString : Int -> String

implement Show Int where
  toString = intToString

@external "js" "stringToString"
stringToString : String -> String

implement Show String where
  toString = stringToString

infixr 5 ++
@external "js" "append"
(++) : String -> String -> String
`;
  const basicsProgram = parse(basicsSource);
  const basicsModule = analyze(basicsProgram, { fileContext: { filePath: "Vibe.Basics", srcDir: "" } });
  const dependencies = new Map([["Vibe.Basics", basicsModule]]);

  test("auto-derives Show for ADTs", () => {
    const source = `
module Test exposing (..)
import Vibe.Basics exposing (..)

type Option a = Some a | None

main : String
main = toString (Some 1)
`;
    // Should compile and find instance
    const program = parse(source);
    const result = analyze(program, { dependencies, fileContext: { filePath: "Test", srcDir: "" } });

    // find instance for Option Int
    const inst = result.instances.find(i => i.protocolName === "Show" && i.typeArgs[0]?.kind === "con");
    expect(inst).toBeDefined();
    // In strict sense, typeArgs[0] is 'Option a' (polymorphic instance) or 'Option Int' (concrete if synthesized)?
    // Auto-derive generates polymorphic instance: instance Show a => Show (Option a)
    // So typeArgs[0] is 'Option (var a)'.
    expect(inst?.typeArgs[0]?.kind).toBe("con");
  });

  test("auto-derives Show for Tuples", () => {
    const source = `
module TestTuple exposing (..)
import Vibe.Basics exposing (..)

main : String
main = toString (1, "hello")
`;
    const program = parse(source);
    // This uses the synthetic hook
    const result = analyze(program, { dependencies, fileContext: { filePath: "TestTuple", srcDir: "" } });

    // We expect it to succeed.
    expect(result).toBeDefined();
  });

  test("auto-derives Show for Records", () => {
    // TODO: Records
  });

});
