
import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "../src/index";

describe("Tuple Eq Support", () => {

  test("Can verify equality of tuples", () => {
    // 1. Mock Vibe.Basics
    const basicsSource = `
module Vibe.Basics exposing (..)
protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool

@external "js" "intEq"
intEq : Int -> Int -> Bool

@external "js" "intNeq"
intNeq : Int -> Int -> Bool

implement Eq Int where
  (==) = intEq
  (/=) = intNeq
`;
    const basicsProgram = parse(basicsSource);
    const basicsModule = analyze(basicsProgram);

    // 2. Test code using tuples
    const source = `
module Test exposing (..)
import Vibe.Basics exposing (..)

main : Bool
main = (1, 2) == (1, 2)
`;
    // We expect this to PASS analysis if Eq is implemented/derived for tuples.
    const program = parse(source);
    const result = analyze(program, {
        dependencies: new Map([
            ["Vibe.Basics", basicsModule]
        ])
    });
    
    expect(result).toBeDefined();
  });

});
