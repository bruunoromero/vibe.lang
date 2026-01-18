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
  preludeSource?: string,
): { code: string; ir: IRProgram } {
  // Parse prelude if provided
  let preludeSemantics: SemanticModule | undefined;
  if (preludeSource) {
    const preludeAst = parse(preludeSource);
    preludeSemantics = analyze(preludeAst);
  }

  const ast = parse(source);
  const semantics = analyze(ast, {
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
      "_PIPE_PIPE(false)(() => _PIPE_PIPE(false)(() => true))",
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

// ============================================================================
// Default Protocol Implementation Resolution Tests
// ============================================================================

describe("Default Protocol Implementation Resolution", () => {
  test("resolves protocol methods in default implementations for concrete types", () => {
    // This tests the bug fix where default protocol implementations like `/=`
    // need to call other methods (like `==`) with proper dictionary resolution.
    // For concrete types, the synthetic default impl needs to resolve the
    // protocol constraint from the synthetic value name, not from constraints.
    const source = `
module Test exposing (..)

infix 4 ==
infix 4 /=

protocol Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool
    (/=) x y = not (x == y)

implement Eq Int where
    (==) x y = True

not : Bool -> Bool
not b = if b then False else True

main = 1 == 1
`;

    const { code } = compileToJS(source);

    // The default /= implementation should resolve == to $dict_Eq_Int
    expect(code).toContain("$default_Eq_Int__SLASH_EQ");
    // It should reference the Int dictionary, not an undefined $dict_Eq
    expect(code).toContain("$dict_Eq_Int._EQ_EQ");
    // The dictionary should be properly constructed
    expect(code).toContain("const $dict_Eq_Int");
  });

  test("resolves protocol methods in default implementations for Unit type", () => {
    // Verify Unit type also works with default implementations
    const source = `
module Test exposing (..)

infix 4 ==
infix 4 /=

type Unit = Unit

protocol Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool
    (/=) x y = not (x == y)

implement Eq Unit where
    (==) _ _ = True

not : Bool -> Bool
not b = if b then False else True

main = Unit == Unit
`;

    const { code } = compileToJS(source);

    // The default /= implementation should reference the Unit dictionary
    expect(code).toContain("$default_Eq_Unit__SLASH_EQ");
    expect(code).toContain("$dict_Eq_Unit._EQ_EQ");
  });

  test("handles polymorphic instance default implementations", () => {
    // Polymorphic instances like `Eq (List a)` should pass dictionary parameters
    const source = `
module Test exposing (..)

infix 4 ==
infix 4 /=

protocol Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool
    (/=) x y = not (x == y)

implement Eq a => Eq (List a) where
    (==) xs ys = True

not : Bool -> Bool
not b = if b then False else True
`;

    const { code } = compileToJS(source);

    // The default /= for List should receive a dictionary parameter
    expect(code).toContain("$default_Eq_List");
    // It should use the passed-in dictionary, not reference a concrete one
    expect(code).toMatch(/\$default_Eq_List[^=]+=\s*\(\$dict_Eq\)/);
  });
});

// ============================================================================
// External Function Reference Tests
// ============================================================================

describe("External Function References", () => {
  test("external function calls use the binding name, not the runtime export name", () => {
    // This tests the bug where external function calls were compiled using
    // the runtime export name (e.g., "listCons") instead of the binding name
    // (e.g., "cons"). Since imports alias the runtime name to the binding name
    // (import { listCons as cons }), the binding name must be used in the body.
    const source = `
module Test exposing (..)

@external "@vibe/runtime" "listCons"
cons : a -> List a -> List a

append : List a -> List a -> List a
append xs ys =
    case xs of
        [] -> ys
        x :: xsTail -> cons x (append xsTail ys)
`;

    const { code } = compileToJS(source);

    // Should import with alias: import { listCons as cons }
    expect(code).toContain('import { listCons as cons } from "@vibe/runtime"');

    // Should call "cons" in the body, NOT "listCons"
    expect(code).toContain("cons(x)");
    expect(code).not.toMatch(/[^a-zA-Z]listCons\(/);
  });

  test("external function with same binding and export name works correctly", () => {
    // When the Vibe name matches the runtime export name, no alias is needed
    const source = `
module Test exposing (..)

@external "@vibe/runtime" "listCons"
listCons : a -> List a -> List a

test : List Int
test = listCons 1 []
`;

    const { code } = compileToJS(source);

    // Should import directly: import { listCons }
    expect(code).toContain("listCons");
    // Should call "listCons" in the body
    expect(code).toContain("listCons(1)");
  });

  test("multiple external functions from same module use correct binding names", () => {
    const source = `
module Test exposing (..)

@external "@vibe/runtime" "intAdd"
add : Int -> Int -> Int

@external "@vibe/runtime" "intSub"
sub : Int -> Int -> Int

compute : Int -> Int -> Int
compute x y = add (sub x y) y
`;

    const { code } = compileToJS(source);

    // Should import with aliases
    expect(code).toContain("intAdd as add");
    expect(code).toContain("intSub as sub");

    // Should use binding names in the body
    expect(code).toContain("add(");
    expect(code).toContain("sub(");
    // Should NOT use the runtime export names directly in the body
    expect(code).not.toMatch(/[^a-zA-Z_]intAdd\(/);
    expect(code).not.toMatch(/[^a-zA-Z_]intSub\(/);
  });
});

// ============================================================================
// SSC-Based Dependency Order Tests
// ============================================================================

describe("SSC-Based Dependency Order", () => {
  test("synthetic implementations are emitted before dictionaries that reference them", () => {
    // This test ensures that $impl_* and $default_* values are included in the
    // dependency order so they're emitted before the dictionaries reference them.
    const source = `
module Test exposing (..)

infix 4 ==
infix 4 /=

protocol Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool
    (/=) x y = not (x == y)

implement Eq Int where
    (==) x y = True

not : Bool -> Bool
not b = if b then False else True
`;

    const { code } = compileToJS(source);
    const lines = code.split("\n");

    // Find the line where $impl or $default is defined
    const implLine = lines.findIndex(
      (l) =>
        l.includes("const $impl_Eq_Int") || l.includes("const $default_Eq_Int"),
    );
    // Find the line where the dictionary is defined
    const dictLine = lines.findIndex((l) => l.includes("const $dict_Eq_Int"));

    // Synthetic implementations should be emitted before the dictionary
    expect(implLine).toBeLessThan(dictLine);
    expect(implLine).toBeGreaterThan(-1);
    expect(dictLine).toBeGreaterThan(-1);
  });

  test("values are emitted before dictionaries that reference them", () => {
    // Regular values referenced by dictionary methods should be emitted first
    const source = `
module Test exposing (..)

infix 4 ==

protocol Eq a where
    (==) : a -> a -> Bool

myCompare : Int -> Int -> Bool
myCompare x y = True

implement Eq Int where
    (==) = myCompare
`;

    const { code } = compileToJS(source);
    const lines = code.split("\n");

    // myCompare should be emitted before the dictionary
    const compareLine = lines.findIndex((l) => l.includes("const myCompare"));
    const dictLine = lines.findIndex((l) => l.includes("const $dict_Eq_Int"));

    expect(compareLine).toBeLessThan(dictLine);
  });
});

// ============================================================================
// Type Variable Naming Consistency Tests
// ============================================================================

describe("Type Variable Naming Consistency", () => {
  test("synthetic implementations and dictionaries use consistent type variable names", () => {
    // This test ensures that IR and codegen use the same format for type variable
    // keys (lowercase 'v' prefix) to prevent duplicate dictionary generation.
    const source = `
module Test exposing (..)

infix 4 ==
infix 4 /=

protocol Eq a where
    (==) : a -> a -> Bool
    (/=) : a -> a -> Bool
    (/=) x y = not (x == y)

implement Eq a => Eq (List a) where
    (==) xs ys = True

not : Bool -> Bool
not b = if b then False else True
`;

    const { code } = compileToJS(source);

    // Should use lowercase 'v' for type variables, not 'Var'
    // Check that we don't have inconsistent naming
    expect(code).not.toMatch(/\$dict_Eq_List_Var\d+/);
    expect(code).not.toMatch(/\$impl_Eq_List_Var\d+/);
    expect(code).not.toMatch(/\$default_Eq_List_Var\d+/);

    // Should have consistent lowercase 'v' naming
    expect(code).toMatch(/\$dict_Eq_List_v\d+/);
    expect(code).toMatch(/\$default_Eq_List_v\d+/);
  });
});

// ============================================================================
// Import Path Tests
// ============================================================================

import {
  calculateReExportPath,
  calculateImportPath,
  generateDependencyImports,
} from "../src/imports";

describe("Re-export Path Calculation", () => {
  test("calculates correct path for submodule re-exports within same package", () => {
    const mockProgram = {
      moduleName: "Vibe",
      packageName: "Vibe",
    } as IRProgram;
    const modulePackages = new Map([["Vibe.Maybe", "Vibe"]]);

    const path = calculateReExportPath(
      mockProgram,
      "Vibe.Maybe",
      modulePackages,
    );
    expect(path).toBe("./Vibe/Maybe.js");
  });

  test("calculates correct path for cross-package re-exports", () => {
    const mockProgram = {
      moduleName: "Vibe",
      packageName: "Vibe",
    } as IRProgram;
    const modulePackages = new Map([["ExampleApp", "ExampleApp"]]);

    const path = calculateReExportPath(
      mockProgram,
      "ExampleApp",
      modulePackages,
    );
    expect(path).toBe("../ExampleApp/ExampleApp.js");
  });

  test("falls back to first segment when modulePackages missing entry", () => {
    const mockProgram = {
      moduleName: "Vibe",
      packageName: "Vibe",
    } as IRProgram;
    const modulePackages = new Map();

    const path = calculateReExportPath(
      mockProgram,
      "Vibe.Maybe",
      modulePackages,
    );
    expect(path).toBe("./Vibe/Maybe.js");
  });

  test("handles deeply nested modules correctly", () => {
    const mockProgram = {
      moduleName: "Vibe.Sub.Deep",
      packageName: "Vibe",
    } as IRProgram;
    const modulePackages = new Map([["Vibe.Other", "Vibe"]]);

    const path = calculateReExportPath(
      mockProgram,
      "Vibe.Other",
      modulePackages,
    );
    expect(path).toBe("../../Vibe/Other.js");
  });
});

describe("ADT Constructor Imports", () => {
  test("imports constructors instead of type name for ExportTypeAll", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      sourceProgram: {
        imports: [
          {
            moduleName: "Vibe.Maybe",
            exposing: {
              kind: "Explicit",
              exports: [{ kind: "ExportTypeAll", name: "Maybe" }],
            },
          },
        ],
      },
      adts: {
        Maybe: {
          constructors: ["Just", "Nothing"],
        },
      },
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Maybe", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toContain(
      'import { Just, Nothing } from "../Vibe/Vibe/Maybe.js";',
    );
    expect(imports).not.toContain("import { Maybe }");
  });

  test("imports specific constructors for ExportTypeSome", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      sourceProgram: {
        imports: [
          {
            moduleName: "Vibe.Maybe",
            exposing: {
              kind: "Explicit",
              exports: [
                { kind: "ExportTypeSome", name: "Maybe", members: ["Just"] },
              ],
            },
          },
        ],
      },
      adts: {
        Maybe: {
          constructors: ["Just", "Nothing"],
        },
      },
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Maybe", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toContain('import { Just } from "../Vibe/Vibe/Maybe.js";');
    expect(imports).not.toContain("Nothing");
  });

  test("handles protocol ExportTypeAll without generating imports", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      sourceProgram: {
        imports: [
          {
            moduleName: "Vibe",
            exposing: {
              kind: "Explicit",
              exports: [{ kind: "ExportTypeAll", name: "Eq" }],
            },
          },
        ],
      },
      adts: {},
    } as IRProgram;
    const modulePackages = new Map([["Vibe", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    // Protocols don't have constructors, so only namespace import is generated
    expect(imports.length).toBe(1);
    expect(imports[0]).toContain("import * as Vibe");
  });

  test("generates both namespace and named imports for alias with exposing", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      sourceProgram: {
        imports: [
          {
            moduleName: "Vibe.Bool",
            alias: "Bool",
            exposing: {
              kind: "Explicit",
              exports: [{ kind: "ExportValue", name: "not" }],
            },
          },
        ],
      },
      adts: {},
    } as IRProgram;
    const modulePackages = new Map([["Vibe.Bool", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toContain('import * as Bool from "../Vibe/Vibe/Bool.js";');
    expect(imports).toContain('import { not } from "../Vibe/Vibe/Bool.js";');
  });

  test("generates only namespace import for alias without exposing", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      sourceProgram: {
        imports: [
          {
            moduleName: "Vibe.Bool",
            alias: "Bool",
          },
        ],
      },
      adts: {},
    } as IRProgram;
    const modulePackages = new Map([["Vibe.Bool", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toEqual(['import * as Bool from "../Vibe/Vibe/Bool.js";']);
  });
});
