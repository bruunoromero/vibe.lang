import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "../src/index";
import type { InstanceInfo, TypeCon, Type } from "../src/index";

describe("Primitive Eq Validation", () => {
  // Mock Vibe.Basics with explicit Eq instances for primitives
  const basicsSource = `
module Vibe.Basics exposing (..)

@external "js" "not"
not : Bool -> Bool

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

@external "js" "eqInt"
eqInt : Int -> Int -> Bool

@external "js" "neqInt"
neqInt : Int -> Int -> Bool

implement Eq Int where
  (==) = eqInt
  (/=) = neqInt

type MyContainer = MyContainer Int
`;
  const basicsProgram = parse(basicsSource);
  const basicsModule = analyze(basicsProgram, { fileContext: { filePath: "Vibe.Basics", srcDir: "" } });
  const dependencies = new Map([["Vibe.Basics", basicsModule]]);

  test("auto-derives Eq for type containing Int (with Eq Int present)", () => {
    const source = `
module Test exposing (..)
import Vibe.Basics exposing (..)

type Container = Container Int

implement Eq Container

`;
    const program = parse(source);
    // Should succeed because Eq Int exists
    const result = analyze(program, { dependencies, fileContext: { filePath: "Test", srcDir: "" } });

    const containerEq = result.instances.find(i =>
      i.protocolName === "Eq" &&
      i.typeArgs[0]!.kind === "con" &&
      (i.typeArgs[0] as TypeCon).name === "Container"
    );
    expect(containerEq).toBeDefined();
  });

  test("fails to derive Eq if primitive is missing Eq instance", () => {
    // Mock a scenario where Float has NO Eq instance
    const noFloatSource = `
module Vibe.NoFloat exposing (..)

@external "js" "not"
not : Bool -> Bool

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

type MyFloatContainer = MyFloatContainer Float
`;
    // We expect this to fail or not generate the instance if we tried to implement it
    // But since 'Float' is skipped in the old code, it would pass erroneously if checked against hardcoded list.
    // In new code, it should fail because "Float" has no instance in this scope.

    // However, analyze() throws on errors.
    const program = parse(`
module TestFail exposing (..)
import Vibe.NoFloat exposing (..)

implement Eq MyFloatContainer

`);
    const noFloatProgram = parse(noFloatSource);
    const noFloatModule = analyze(noFloatProgram, { fileContext: { filePath: "Vibe.NoFloat", srcDir: "" } });
    const deps = new Map([["Vibe.NoFloat", noFloatModule]]);

    expect(() => analyze(program, { dependencies: deps, fileContext: { filePath: "TestFail", srcDir: "" } })).toThrow(/Instance is missing implementation for method/);
  });
});
