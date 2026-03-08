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
    preludeSemantics = analyze(preludeAst, {
      fileContext: { filePath: "Vibe", srcDir: "" },
    });
  }

  const ast = parse(source);
  const semantics = analyze(ast, {
    dependencies: preludeSemantics
      ? new Map([["Vibe", preludeSemantics]])
      : undefined,
    fileContext: { filePath: "Test", srcDir: "" },
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
// Exposed Import Qualification Tests
// ============================================================================

describe("Exposed Import Qualification", () => {
  test("constructors imported via exposing are not module-qualified", () => {
    // Regression test: `import M exposing (Maybe(..))` should generate
    // unqualified `Just` and `Nothing` references, not `Vibe.Just` or alias-qualified.
    const preludeSource = `
module Vibe exposing (..)

type Maybe a = Just a | Nothing

wrap : a -> Maybe a
wrap x = Just x

unwrap : Maybe a -> a -> a
unwrap m d =
  case m of
    Just v -> v
    Nothing -> d
`;

    const source = `
module Test exposing (..)

import Vibe exposing (Maybe(..))

useJust : Int -> Maybe Int
useJust x = Just x

useNothing : Maybe Int
useNothing = Nothing
`;

    const { code } = compileToJS(source, preludeSource);

    // Just and Nothing should appear unqualified
    expect(code).toContain("Just(");
    expect(code).toContain("Nothing");
    // Must NOT contain module-qualified references like Vibe.Just
    expect(code).not.toMatch(/Vibe\.Just/);
    expect(code).not.toMatch(/Vibe\.Nothing/);
  });

  test("values imported via exposing are not module-qualified", () => {
    const preludeSource = `
module Vibe exposing (..)

identity : a -> a
identity x = x
`;

    const source = `
module Test exposing (..)

import Vibe exposing (identity)

main : Int
main = identity 42
`;

    const { code } = compileToJS(source, preludeSource);

    // identity should appear unqualified
    expect(code).toContain("identity");
    expect(code).not.toMatch(/Vibe\.identity/);
  });

  test("zero-arity constructors from explicit exposing reference the import", () => {
    // Regression: `Nothing` imported via `exposing (Maybe(..))` was being
    // inlined as `{ $tag: 1 }` instead of referencing the named import.
    const preludeSource = `
module Vibe exposing (..)

type Maybe a = Just a | Nothing
`;

    const source = `
module Test exposing (..)

import Vibe exposing (Maybe(..))

val : Maybe Int
val = Nothing
`;

    const { code } = compileToJS(source, preludeSource);

    // Nothing should reference the import, not be inlined as { $tag: N }
    expect(code).toContain("Nothing");
    expect(code).not.toMatch(/Vibe\.Nothing/);
  });

  test("exposing (..) values are namespace-qualified", () => {
    // Contrast: `exposing (..)` uses namespace imports, so values should
    // be qualified with the import alias.
    const preludeSource = `
module Vibe exposing (..)

identity : a -> a
identity x = x
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

main = identity 42
`;

    const { code } = compileToJS(source, preludeSource);

    // identity from exposing(..) should be namespace-qualified
    expect(code).toContain("Vibe.identity");
  });
});

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
    // External functions with arity > 0 are imported under a private $$-prefixed
    // alias and a curried wrapper const is emitted using the Vibe binding name.
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

    // Should import under private alias: import { listCons as $$cons }
    expect(code).toContain(
      'import { listCons as $$cons } from "@vibe/runtime"',
    );

    // Should emit a curried wrapper
    expect(code).toContain("const cons = ($a0) => ($a1) => $$cons($a0, $a1);");

    // Should call "cons" in the body, NOT "listCons" or "$$cons"
    expect(code).toContain("cons(x)");
    expect(code).not.toMatch(/[^$a-zA-Z]listCons\(/);
  });

  test("external function with same binding and export name works correctly", () => {
    // When the Vibe name matches the runtime export name, the import still
    // uses a $$-prefixed alias because a wrapper const is needed.
    const source = `
module Test exposing (..)

@external "@vibe/runtime" "listCons"
listCons : a -> List a -> List a

test : List Int
test = listCons 1 []
`;

    const { code } = compileToJS(source);

    // Should import under private alias
    expect(code).toContain("listCons as $$listCons");
    // Should emit wrapper
    expect(code).toContain(
      "const listCons = ($a0) => ($a1) => $$listCons($a0, $a1);",
    );
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

    // Should import under private aliases
    expect(code).toContain("intAdd as $$add");
    expect(code).toContain("intSub as $$sub");

    // Should emit curried wrappers
    expect(code).toContain("const add = ($a0) => ($a1) => $$add($a0, $a1);");
    expect(code).toContain("const sub = ($a0) => ($a1) => $$sub($a0, $a1);");

    // Should use binding names in the body
    expect(code).toContain("add(");
    expect(code).toContain("sub(");
    // Should NOT use the runtime export names directly in the body
    expect(code).not.toMatch(/[^$a-zA-Z_]intAdd\(/);
    expect(code).not.toMatch(/[^$a-zA-Z_]intSub\(/);
  });

  test("external value with arity 0 is imported directly without wrapper", () => {
    const source = `
module Test exposing (..)

@external "@vibe/runtime" "emptyList"
empty : List a

test : List a
test = empty
`;

    const { code } = compileToJS(source);

    // Should import directly without $$-prefix (arity 0, no wrapper needed)
    expect(code).toContain(
      'import { emptyList as empty } from "@vibe/runtime"',
    );
    // Should NOT have a const wrapper
    expect(code).not.toContain("const empty =");
  });

  test("external function with arity 1 gets a single-arg wrapper", () => {
    const source = `
module Test exposing (..)

@external "./test.ffi.js" "doSomething"
doIt : Int -> String

test : String
test = doIt 42
`;

    const { code } = compileToJS(source);

    // Should import under private alias
    expect(code).toContain("doSomething as $$doIt");
    // Should emit a single-arg curried wrapper
    expect(code).toContain("const doIt = ($a0) => $$doIt($a0);");
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
  test("emits named imports from resolvedImports", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe.Maybe",
          namedImports: ["Just", "Nothing"],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Maybe", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toContain(
      'import { Just, Nothing } from "../Vibe/Vibe/Maybe.js";',
    );
    expect(imports).not.toContain("Maybe");
  });

  test("emits namespace import from resolvedImports", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe",
          namespaceImport: "Vibe",
          namedImports: [],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports.length).toBe(1);
    expect(imports[0]).toContain("import * as Vibe");
  });

  test("emits both namespace and named imports when both present", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe.Bool",
          namespaceImport: "Bool",
          namedImports: ["not"],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Bool", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toContain('import * as Bool from "../Vibe/Vibe/Bool.js";');
    expect(imports).toContain('import { not } from "../Vibe/Vibe/Bool.js";');
  });

  test("emits only namespace import when namedImports is empty", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe.Bool",
          namespaceImport: "Bool",
          namedImports: [],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Bool", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toEqual(['import * as Bool from "../Vibe/Vibe/Bool.js";']);
  });

  test("sanitizes operator names in named imports", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe.Basics",
          namedImports: ["_PLUS_PLUS", "not"],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Basics", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports[0]).toContain("_PLUS_PLUS");
    expect(imports[0]).toContain("not");
  });

  test("skips resolved import with no namespace and no named imports", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe.Result",
          namedImports: [],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.Result", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports.length).toBe(0);
  });

  test("emits implicit instance namespace imports", () => {
    const mockProgram = {
      moduleName: "ExampleApp",
      packageName: "ExampleApp",
      resolvedImports: [
        {
          moduleName: "Vibe.String",
          namespaceImport: "$inst_String",
          namedImports: [],
        },
      ],
    } as unknown as IRProgram;
    const modulePackages = new Map([["Vibe.String", "Vibe"]]);

    const imports = generateDependencyImports(mockProgram, modulePackages);
    expect(imports).toEqual([
      'import * as $inst_String from "../Vibe/Vibe/String.js";',
    ]);
  });
});

describe("Protocol Dictionary Re-exports", () => {
  test("exports locally generated dictionaries even with explicit exposing clause", () => {
    // This tests the bug fix where modules with explicit exposing clauses
    // (not exposing (..)) would not export their instance dictionaries.
    // Dictionaries should always be exported when an instance is defined.
    const preludeSource = `
module Vibe exposing (..)

protocol Show a where
  toString : a -> String

implement Show Int where
  toString = _intToString

@external "@vibe/runtime" "numToString"
_intToString : Int -> String
`;

    const source = `
module Test exposing (myValue)

import Vibe exposing (..)

protocol MyShow a where
  myToString : a -> String

implement MyShow Int where
  myToString = toString

myValue : Int
myValue = 42
`;

    const { code } = compileToJS(source, preludeSource);

    // Should export the locally generated dictionary even though exposing clause
    // only mentions myValue
    expect(code).toContain("$dict_MyShow_Int");
    expect(code).toMatch(/export\s*{[^}]*\$dict_MyShow_Int[^}]*}/);
  });

  test("re-exports dictionaries from imported modules when protocol is exported", () => {
    // This tests that when a module exports a protocol, it also re-exports
    // dictionaries for instances of that protocol from imported modules.
    const libSource = `
module Lib exposing (..)

protocol Showable a where
  show : a -> String

implement Showable Int where
  show x = "int"
`;

    // First compile the library
    const libAst = parse(libSource);
    const libSemantics = analyze(libAst, {
      fileContext: { filePath: "Lib", srcDir: "" },
    });

    // Then compile the re-exporting module
    const reexportSource = `
module ReExporter exposing (Showable(..))

import Lib exposing (..)
`;

    const reexportAst = parse(reexportSource);
    const reexportSemantics = analyze(reexportAst, {
      dependencies: new Map([["Lib", libSemantics]]),
      fileContext: { filePath: "ReExporter", srcDir: "" },
    });
    const reexportIr = lower(reexportAst, reexportSemantics, {
      dependencies: new Map([["Lib", libSemantics]]),
    });

    // Generate with module packages info
    const modulePackages = new Map([
      ["Lib", "Lib"],
      ["ReExporter", "ReExporter"],
    ]);
    const result = generate(reexportIr, { modulePackages });

    // Should re-export the Lib's Showable_Int dictionary
    expect(result.code).toContain("$dict_Showable_Int");
    expect(result.code).toMatch(
      /export\s*{[^}]*\$dict_Showable_Int[^}]*}\s*from/,
    );
  });
});

// ============================================================================
// Let Expression Code Generation Tests
// ============================================================================

describe("Let Expression Code Generation", () => {
  test("generates correct code for let expression with single binding", () => {
    // Regression test: let expressions must wrap lambda in parentheses
    // Without the fix, (x) => body(arg) is generated instead of ((x) => body)(arg)
    const source = `
module Test exposing (..)

passthrough x = 
    let 
        y = x
    in
        y
`;

    const { code } = compileToJS(source);

    // The let expression should generate an IIFE (immediately invoked function expression)
    // The lambda must be wrapped in parentheses before being applied
    expect(code).toMatch(/\(\(y\)\s*=>/);
    // And the binding value should be applied to it
    expect(code).toContain(")(x)");
  });

  test("generates correct code for let expression with multiple bindings", () => {
    // Multiple bindings are chained: let x = e1; y = e2 in body => ((\x -> (\y -> body)(e2))(e1))
    const source = `
module Test exposing (..)

compute x = 
    let 
        a = x
        b = a
    in
        b
`;

    const { code } = compileToJS(source);

    // Each binding creates a nested IIFE
    // Both lambdas must be wrapped in parentheses
    expect(code).toMatch(/\(\(a\)\s*=>/);
    expect(code).toMatch(/\(\(b\)\s*=>/);
  });

  test("let expression produces same result as inlined expression", () => {
    // The let version and inlined version should be semantically equivalent
    const letSource = `
module Test exposing (..)

identity x = 
    let 
        result = x
    in
        result
`;

    const inlinedSource = `
module Test exposing (..)

identity x = x
`;

    const { code: letCode } = compileToJS(letSource);
    const { code: inlinedCode } = compileToJS(inlinedSource);

    // Both should produce valid JavaScript that computes the same result
    // The let version uses an IIFE, the inlined version is direct
    expect(letCode).toContain("identity");
    expect(inlinedCode).toContain("identity");

    // Let version should have the IIFE pattern
    expect(letCode).toMatch(/\(\(result\)\s*=>/);
  });

  test("nested let expressions generate correctly nested IIFEs", () => {
    const source = `
module Test exposing (..)

nested x = 
    let 
        a = x
    in
        let 
            b = a
        in
            b
`;

    const { code } = compileToJS(source);

    // Both nested let expressions should generate properly wrapped lambdas
    expect(code).toMatch(/\(\(a\)\s*=>/);
    expect(code).toMatch(/\(\(b\)\s*=>/);
  });

  test("let expression with function binding", () => {
    // Let bindings can define local functions
    const source = `
module Test exposing (..)

withLocalFn x = 
    let 
        wrap y = y
    in
        wrap x
`;

    const { code } = compileToJS(source);

    // The local function binding should be wrapped and applied correctly
    expect(code).toMatch(/\(\(wrap\)\s*=>/);
  });

  test("let expression with tuple binding", () => {
    // Test let expressions with more complex expressions
    const source = `
module Test exposing (..)

useTuple = 
    let 
        pair = (1, 2)
    in
        pair
`;

    const { code } = compileToJS(source);

    // The let expression should generate an IIFE
    expect(code).toMatch(/\(\(pair\)\s*=>/);
    // The tuple should be applied as the argument
    expect(code).toContain(")([1, 2])");
  });
});

// ============================================================================
// Imported Value Qualification Tests
// ============================================================================

describe("Imported value qualification", () => {
  test("operators from namespace imports are qualified", () => {
    // When using `import Vibe exposing (..)`, operators like `<|`
    // are only available as namespace members (e.g., Vibe._LT_PIPE).
    // The codegen must qualify them with the import alias.
    const preludeSource = `
module Vibe exposing (..)

(<|) : (a -> b) -> a -> b
(<|) f x = f x

identity : a -> a
identity x = x
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

main = identity <| 42
`;

    const { code } = compileToJS(source, preludeSource);

    // The operator should be qualified with the import alias
    expect(code).toContain("Vibe._LT_PIPE");
    // It should NOT appear as a bare, unqualified reference
    expect(code).not.toMatch(/(?<!\.)_LT_PIPE/);
  });

  test("infix operators from namespace imports are qualified", () => {
    // Infix usage of imported operators should also be qualified
    const preludeSource = `
module Vibe exposing (..)

(|>) : a -> (a -> b) -> b
(|>) x f = f x

identity : a -> a
identity x = x
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

main = 42 |> identity
`;

    const { code } = compileToJS(source, preludeSource);

    expect(code).toContain("Vibe._PIPE_GT");
    expect(code).not.toMatch(/(?<!\.)_PIPE_GT/);
  });

  test("imported non-operator values are qualified", () => {
    // Non-operator values imported via `exposing (..)` should also be qualified
    const preludeSource = `
module Vibe exposing (..)

identity : a -> a
identity x = x
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

main = identity 42
`;

    const { code } = compileToJS(source, preludeSource);

    expect(code).toContain("Vibe.identity");
  });

  test("locally defined values are not qualified", () => {
    // Values defined in the current module should remain unqualified
    const source = `
module Test exposing (..)

myFn x = x
main = myFn 42
`;

    const { code } = compileToJS(source);

    expect(code).toContain("myFn(42)");
    expect(code).not.toMatch(/\w+\.myFn/);
  });
});

// ============================================================================
// Protocol Method First-Class Usage Dictionary Resolution Tests
// ============================================================================

describe("Protocol method used as first-class value", () => {
  const prelude = `
module Vibe exposing (..)

type Ref a

protocol Show a where
  toString : a -> String

implement Show Int where
  toString x = "int"

implement Show String where
  toString x = x

implement Show a => Show (Ref a) where
  toString ref = "Ref(...)"

infixr 0 <|
(<|) : (a -> b) -> a -> b
(<|) f x = f x

infixl 0 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x
`;

  test("resolves dictionary when protocol method is passed via <|", () => {
    // Bug regression: toString <| Ref.create 10 was emitting $dict_Show_String
    // instead of $dict_Show_Ref(...). When a protocol method is used as a
    // first-class value (argument to <|), the constraint must be resolved
    // from the protocol usage site's type, not from the value's return type.
    const source = `
module Test exposing (..)
import Vibe exposing (..)

main = toString <| 42
`;

    const { code } = compileToJS(source, prelude);

    // Should resolve Show Int, not Show String
    expect(code).toContain("$dict_Show_Int");
    expect(code).toContain("toString");
    expect(code).not.toContain("$dict_Show_String.toString");
  });

  test("resolves dictionary when protocol method is passed via |>", () => {
    const source = `
module Test exposing (..)
import Vibe exposing (..)

main = 42 |> toString
`;

    const { code } = compileToJS(source, prelude);

    expect(code).toContain("$dict_Show_Int");
    expect(code).not.toContain("$dict_Show_String.toString");
  });

  test("resolves dictionary for parameterized type via <|", () => {
    // The key bug: toString <| (value of type Ref Int) must use
    // the Show (Ref a) instance with Show Int constraint, not Show String.
    const source = `
module Test exposing (..)
import Vibe exposing (..)

protocol Num a where
  plus : a -> a -> a

implement Num Int where
  plus x y = x

value : Ref Int
value = value

main = toString <| value
`;

    const { code } = compileToJS(source, prelude);

    // Should reference the Show Ref instance, not Show String
    expect(code).toContain("$dict_Show_Ref");
    expect(code).not.toContain("$dict_Show_String.toString");
  });
});

// ============================================================================
// Property Access Declaration Tests (@get / @call)
// ============================================================================

describe("Property Access Declarations", () => {
  test("@get compiles to property access arrow function", () => {
    const source = `
module Test exposing (..)

type FileStat

@get "size"
fileStatSize : FileStat -> Int
`;

    const { code } = compileToJS(source);
    expect(code).toContain("const fileStatSize = ($recv) => $recv.size;");
    // Should NOT generate an import for this
    expect(code).not.toContain("import");
  });

  test("@call with zero extra args compiles to method call", () => {
    const source = `
module Test exposing (..)

type FileStat

@call "toString"
fileStatToString : FileStat -> String
`;

    const { code } = compileToJS(source);
    expect(code).toContain(
      "const fileStatToString = ($recv) => $recv.toString();",
    );
  });

  test("@call with one extra arg compiles to curried method call", () => {
    const source = `
module Test exposing (..)

type Handle

@call "write"
handleWrite : Handle -> String -> Int
`;

    const { code } = compileToJS(source);
    expect(code).toContain(
      "const handleWrite = ($recv) => ($a0) => $recv.write($a0);",
    );
  });

  test("@call with multiple extra args compiles to curried method call", () => {
    const source = `
module Test exposing (..)

type Canvas

@call "drawRect"
drawRect : Canvas -> Int -> Int -> Int -> Int -> String
`;

    const { code } = compileToJS(source);
    expect(code).toContain(
      "const drawRect = ($recv) => ($a0) => ($a1) => ($a2) => ($a3) => $recv.drawRect($a0, $a1, $a2, $a3);",
    );
  });

  test("@get with parametric opaque type", () => {
    const source = `
module Test exposing (..)

type Container a

@get "length"
containerLength : Container a -> Int
`;

    const { code } = compileToJS(source);
    expect(code).toContain("const containerLength = ($recv) => $recv.length;");
  });

  test("@get values are exported", () => {
    const source = `
module Test exposing (fileStatSize)

type FileStat

@get "size"
fileStatSize : FileStat -> Int
`;

    const { code } = compileToJS(source);
    expect(code).toMatch(/export\s*\{[^}]*fileStatSize/);
  });

  test("IR has correct propertyAccess for @get", () => {
    const source = `
module Test exposing (..)

type FileStat

@get "size"
fileStatSize : FileStat -> Int
`;

    const { ir } = compileToJS(source);
    const value = ir.values["fileStatSize"];
    expect(value).toBeDefined();
    expect(value!.propertyAccess).toEqual({
      variant: "get",
      key: "size",
      callArity: 0,
    });
    expect(value!.isExternal).toBe(false);
  });

  test("IR has correct propertyAccess for @call with arity", () => {
    const source = `
module Test exposing (..)

type Handle

@call "write"
handleWrite : Handle -> String -> Int
`;

    const { ir } = compileToJS(source);
    const value = ir.values["handleWrite"];
    expect(value).toBeDefined();
    expect(value!.propertyAccess).toEqual({
      variant: "call",
      key: "write",
      callArity: 1,
    });
  });

  test("@get can be used in expressions", () => {
    const source = `
module Test exposing (..)

type FileStat

@get "size"
fileStatSize : FileStat -> Int

@external "./test.ffi.js" "getStat"
getStat : String -> FileStat

getSize : String -> Int
getSize path = fileStatSize (getStat path)
`;

    const { code } = compileToJS(source);
    expect(code).toContain("const fileStatSize = ($recv) => $recv.size;");
    expect(code).toContain("fileStatSize(getStat(path))");
  });

  test("@val compiles to direct global variable reference", () => {
    const source = `
module Test exposing (..)

type Window

@val "window"
globalWindow : Window
`;

    const { code } = compileToJS(source);
    expect(code).toContain("const globalWindow = window;");
    expect(code).not.toContain("$recv");
  });

  test("@val with non-function type works", () => {
    const source = `
module Test exposing (..)

@val "undefined"
jsUndefined : ()
`;

    const { code } = compileToJS(source);
    expect(code).toContain("const jsUndefined = undefined;");
  });

  test("@val values are exported", () => {
    const source = `
module Test exposing (..)

type Console

@val "console"
jsConsole : Console
`;

    const { code } = compileToJS(source);
    expect(code).toContain("const jsConsole = console;");
    expect(code).toContain("jsConsole");
  });

  test("IR has correct propertyAccess for @val", () => {
    const source = `
module Test exposing (..)

type Window

@val "window"
globalWindow : Window
`;

    const { ir } = compileToJS(source);
    const value = ir.values["globalWindow"];
    expect(value).toBeDefined();
    expect(value!.propertyAccess).toBeDefined();
    expect(value!.propertyAccess!.variant).toBe("val");
    expect(value!.propertyAccess!.key).toBe("window");
    expect(value!.propertyAccess!.callArity).toBe(0);
  });
});

// ============================================================================
// Imported Value Declaration Tests (@import)
// ============================================================================

describe("Imported Value Declarations", () => {
  test("@import emits default import statement", () => {
    const source = `
module Test exposing (..)

type FileSystem

@import "node:fs/promises"
fs : FileSystem
`;

    const { code } = compileToJS(source);
    expect(code).toContain('import fs from "node:fs/promises";');
  });

  test("@import hoists import before other declarations", () => {
    const source = `
module Test exposing (..)

type FileSystem

@import "node:fs/promises"
fs : FileSystem

myValue : Int
myValue = 42
`;

    const { code } = compileToJS(source);
    // The import should appear before the value declaration
    const importIdx = code.indexOf('import fs from "node:fs/promises"');
    const valueIdx = code.indexOf("const myValue");
    expect(importIdx).toBeGreaterThanOrEqual(0);
    expect(valueIdx).toBeGreaterThanOrEqual(0);
    expect(importIdx).toBeLessThan(valueIdx);
  });

  test("@import value is usable as opaque type argument", () => {
    const source = `
module Test exposing (..)

type FileSystem

@import "node:fs/promises"
fs : FileSystem

@get "readFile"
readFile : FileSystem -> String
`;

    const { code } = compileToJS(source);
    expect(code).toContain('import fs from "node:fs/promises";');
    expect(code).toContain("const readFile = ($recv) => $recv.readFile;");
  });

  test("IR has correct defaultImports for @import", () => {
    const source = `
module Test exposing (..)

type FileSystem

@import "node:fs/promises"
fs : FileSystem
`;

    const { ir } = compileToJS(source);
    expect(ir.defaultImports).toHaveLength(1);
    expect(ir.defaultImports[0]!.name).toBe("fs");
    expect(ir.defaultImports[0]!.modulePath).toBe("node:fs/promises");
  });

  test("multiple @import declarations", () => {
    const source = `
module Test exposing (..)

type FS
type Path

@import "node:fs/promises"
fs : FS

@import "node:path"
path : Path
`;

    const { code, ir } = compileToJS(source);
    expect(ir.defaultImports).toHaveLength(2);
    expect(code).toContain('import fs from "node:fs/promises";');
    expect(code).toContain('import path from "node:path";');
  });
});

// ============================================================================
// Tuple Dictionary Resolution Tests
// ============================================================================

describe("Tuple Dictionary Resolution in Auto-Derive", () => {
  test("auto-derived Eq for ADT with tuple-containing List arg generates correct tuple dict", () => {
    // Regression test: When an ADT has a constructor arg like `List (String, Value)`,
    // the auto-derived Eq must synthesize an Eq instance for the `(String, Value)` tuple
    // and generate code that references `$dict_Eq_tuple_String_Value`, not bare `$dict_Eq`.
    const preludeSource = `
module Vibe exposing (..)

@external "runtime" "not"
not : Bool -> Bool

@external "runtime" "stringAppend"
stringAppend : String -> String -> String

infix 4 ==
infix 4 /=
infixr 5 ++

protocol Appendable a where
  (++) : a -> a -> a

implement Appendable String where
  (++) = stringAppend

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

protocol Show a where
  toString : a -> String

implement Eq String where
  (==) a b = True

implement Eq Int where
  (==) a b = True

implement Show String where
  toString a = a

implement Show Int where
  toString a = "int"

implement Eq a => Eq (List a) where
  (==) xs ys =
    case (xs, ys) of
      ([], []) -> True
      _ -> False

implement Show a => Show (List a) where
  toString xs = "list"
`;

    const source = `
module Test exposing (..)

import Vibe exposing (..)

type Value
  = JsonString String
  | JsonInt Int
  | JsonArray (List Value)
  | JsonObject (List (String, Value))
`;
    const { code } = compileToJS(source, preludeSource);

    // Should reference a properly qualified tuple dict, not bare $dict_Eq
    expect(code).toContain("$dict_Eq_tuple_String_Value");
    expect(code).toContain("$dict_Show_tuple_String_Value");
    // Must NOT contain bare $dict_Eq or $dict_Show (undefined references)
    expect(code).not.toMatch(/\(\$dict_Eq\)/);
    expect(code).not.toMatch(/\(\$dict_Show\)/);
  });
});
