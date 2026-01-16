/**
 * Codegen Package Unit Tests
 *
 * Tests for:
 * 1. Multi-parameter protocol dictionary resolution
 * 2. Return type-based constraint resolution
 * 3. Expected return type context propagation
 */

import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, type SemanticModule } from "@vibe/semantics";
import { lower, type IRProgram } from "@vibe/ir";
import { generate } from "../src/index";

// ============================================================================
// Test Helpers
// ============================================================================

/**
 * Compile Vibe source to JavaScript.
 * Uses a mock prelude with basic protocol definitions.
 */
function compileToJS(
  source: string,
  preludeSource?: string
): { code: string; ir: IRProgram } {
  // Parse prelude if provided
  let preludeSemantics: SemanticModule | undefined;
  if (preludeSource) {
    const preludeAst = parse(preludeSource);
    preludeSemantics = analyze(preludeAst, {
      injectPrelude: false,
    });
  }

  const ast = parse(source);
  const semantics = analyze(ast, {
    injectPrelude: false,
    dependencies: preludeSemantics
      ? new Map([["Vibe", preludeSemantics]])
      : undefined,
  });
  const ir = lower(ast, semantics, {
    dependencies: preludeSemantics
      ? new Map([["Vibe", preludeSemantics]])
      : undefined,
  });
  const result = generate(ir);
  return { code: result.code, ir };
}

// ============================================================================
// Multi-Parameter Protocol Dictionary Resolution Tests
// ============================================================================

describe("Multi-Parameter Protocol Dictionary Resolution", () => {
  test("resolves constraint dictionary for return type when annotation provided", () => {
    // This tests the bug fix where a multi-parameter protocol has constraints
    // on the return type position. The constraint dictionary must be resolved
    // using the expected return type from the value's type annotation.
    const preludeSource = `
module Vibe exposing (..)

protocol Appendable a where
  empty : a
  append : a -> a -> a

implement Appendable (List a) where
  empty = []
  append xs ys = xs
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

-- Multi-parameter protocol where second type arg is the return type
protocol Convert a b where
  convert : a -> b

-- Instance with constraint on the return type (second parameter)
implement Appendable b => Convert Int b where
  convert _ = empty

-- Value with explicit type annotation - this provides the return type
main : List Int
main = convert 42
`;

    const { code } = compileToJS(source, preludeSource);

    // Should resolve to the List instance of Appendable, not generate undefined $dict_Appendable
    expect(code).toContain("$dict_Appendable_List");
    // The main value should use the resolved dictionary with module prefix
    expect(code).toContain("Vibe.$dict_Appendable_List");
  });

  test("resolves constraint from operand types for first type parameter", () => {
    // Test that constraints on the first type parameter (operand type) still work
    const preludeSource = `
module Vibe exposing (..)

protocol Show a where
  show : a -> String

implement Show Int where
  show x = "int"
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

-- Protocol with constraint on the first parameter (input type)
protocol Display a b where
  display : a -> b

implement Show a => Display a String where
  display x = show x

result : String
result = display 42
`;

    const { code } = compileToJS(source, preludeSource);

    // Should resolve Show constraint using the operand type (Int)
    expect(code).toContain("$dict_Show_Int");
  });

  test("handles both operand and return type constraints", () => {
    // Test protocol with constraints on both operand and return types
    const preludeSource = `
module Vibe exposing (..)

protocol Eq a where
  eq : a -> a -> Bool

protocol Appendable a where
  empty : a
  append : a -> a -> a

implement Eq Int where
  eq x y = True

implement Appendable (List a) where
  empty = []
  append xs ys = xs
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

-- Protocol with constraints on both parameters
protocol Transform a b where
  transform : a -> b

implement (Eq a, Appendable b) => Transform a b where
  transform _ = empty

result : List Int
result = transform 42
`;

    const { code } = compileToJS(source, preludeSource);

    // Should resolve Eq from operand type and Appendable from return type
    expect(code).toContain("$dict_Eq_Int");
    expect(code).toContain("$dict_Appendable_List");
  });
});

describe("Expected Return Type Context", () => {
  test("propagates return type through nested expressions", () => {
    const preludeSource = `
module Vibe exposing (..)

protocol Appendable a where
  empty : a
  append : a -> a -> a

implement Appendable (List a) where
  empty = []
  append xs ys = xs
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

protocol Make a b where
  make : a -> b

implement Appendable b => Make Int b where
  make _ = empty

-- Helper function
helper : Int -> List Int
helper n = make n

-- Nested usage
main : List Int
main = helper 1
`;

    const { code } = compileToJS(source, preludeSource);

    // The helper function's return type annotation should provide context
    expect(code).toContain("$dict_Appendable_List");
  });

  test("uses value type for non-function values", () => {
    const preludeSource = `
module Vibe exposing (..)

protocol Default a where
  defaultVal : a

implement Default Int where
  defaultVal = 0

implement Default (List a) where
  defaultVal = []
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

-- Simple value with concrete type annotation
value : List Int
value = defaultVal
`;

    const { code } = compileToJS(source, preludeSource);

    // Should use List instance based on the value's type annotation
    expect(code).toContain("$dict_Default_List");
  });
});

describe("Edge Cases", () => {
  test("polymorphic return type uses dictionary parameter", () => {
    const preludeSource = `
module Vibe exposing (..)

protocol Appendable a where
  empty : a
  append : a -> a -> a

implement Appendable (List a) where
  empty = []
  append xs ys = xs
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

protocol Make a b where
  make : a -> b

implement Appendable b => Make Int b where
  make _ = empty

-- Polymorphic function - no concrete return type known
polymorphicMake : Appendable b => Int -> b
polymorphicMake n = make n
`;

    const { code } = compileToJS(source, preludeSource);

    // Should use dictionary parameter for polymorphic context
    expect(code).toContain("($dict_Appendable)");
  });

  test("deeply nested function extracts correct return type", () => {
    const preludeSource = `
module Vibe exposing (..)

protocol Appendable a where
  empty : a
  append : a -> a -> a

implement Appendable (List a) where
  empty = []
  append xs ys = xs
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

protocol Gen a b where
  gen : a -> b

implement Appendable b => Gen Int b where
  gen _ = empty

-- Multi-parameter function - return type is after all params
multiParam : Int -> Int -> Int -> List Int
multiParam a b c = gen c
`;

    const { code } = compileToJS(source, preludeSource);

    // Return type should be extracted correctly through all parameters
    expect(code).toContain("$dict_Appendable_List");
  });
});

// ============================================================================
// Short-Circuit Operator Tests
// ============================================================================

describe("Short-Circuit Operators", () => {
  test("&& operator generates helper function and uses it with thunk", () => {
    const source = `
module Test exposing (..)

test1 : Bool
test1 = True && False
`;

    const { code } = compileToJS(source);

    // Should generate helper function at top of file
    expect(code).toContain("const _AMP_AMP = (a) => (b) => a && b();");
    // Should use the helper with a thunk for the right operand
    expect(code).toContain("_AMP_AMP(true)(() => false)");
  });

  test("|| operator generates helper function and uses it with thunk", () => {
    const source = `
module Test exposing (..)

test1 : Bool
test1 = False || True
`;

    const { code } = compileToJS(source);

    // Should generate helper function at top of file
    expect(code).toContain("const _PIPE_PIPE = (a) => (b) => a || b();");
    // Should use the helper with a thunk for the right operand
    expect(code).toContain("_PIPE_PIPE(false)(() => true)");
  });

  test("chained && operators generate single helper used multiple times", () => {
    const source = `
module Test exposing (..)

test1 : Bool
test1 = True && True && False
`;

    const { code } = compileToJS(source);

    // Should have exactly one helper function definition
    expect(code).toContain("const _AMP_AMP = (a) => (b) => a && b();");
    // With right-associativity, True && True && False becomes:
    // True && (True && False) => nested calls to _AMP_AMP
    expect(code).toContain("_AMP_AMP(true)(() => _AMP_AMP(true)(() => false))");
  });

  test("chained || operators generate single helper used multiple times", () => {
    const source = `
module Test exposing (..)

test1 : Bool
test1 = False || False || True
`;

    const { code } = compileToJS(source);

    // Should have exactly one helper function definition
    expect(code).toContain("const _PIPE_PIPE = (a) => (b) => a || b();");
    // With right-associativity, False || False || True becomes:
    // False || (False || True) => nested calls to _PIPE_PIPE
    expect(code).toContain(
      "_PIPE_PIPE(false)(() => _PIPE_PIPE(false)(() => true))"
    );
  });

  test("mixed && and || operators generate both helpers", () => {
    const source = `
module Test exposing (..)

test1 : Bool
test1 = True || False && True
`;

    const { code } = compileToJS(source);

    // Should contain both helper functions
    expect(code).toContain("const _PIPE_PIPE = (a) => (b) => a || b();");
    expect(code).toContain("const _AMP_AMP = (a) => (b) => a && b();");
  });

  test("short-circuit operators work with variables", () => {
    const source = `
module Test exposing (..)

test1 : Bool -> Bool -> Bool
test1 x y = x && y
`;

    const { code } = compileToJS(source);

    // Should generate helper function
    expect(code).toContain("const _AMP_AMP = (a) => (b) => a && b();");
    // The right operand (y) should be wrapped in a thunk
    expect(code).toContain("_AMP_AMP(x)(() => y)");
  });
});
