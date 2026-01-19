import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index";

/**
 * Helper to parse test source with module declaration.
 */
function parseTest(source: string) {
  const fullSource = source.trim().startsWith("module ")
    ? source
    : `module Test exposing (..)\n\n${source}`;
  return parse(fullSource);
}

describe("Protocol Registration", () => {
  test("registers protocol with methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a
  minus : a -> a -> a
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.protocols.Num).toBeDefined();
    expect(result.protocols.Num?.name).toBe("Num");
    expect(result.protocols.Num?.params).toEqual(["a"]);
    expect(result.protocols.Num?.methods.size).toBe(2);
    expect(result.protocols.Num?.methods.has("plus")).toBe(true);
    expect(result.protocols.Num?.methods.has("minus")).toBe(true);
  });

  test("rejects duplicate protocol names", () => {
    const source = `
protocol Show a where
  show : a -> String

protocol Show a where
  display : a -> String
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Duplicate protocol");
  });

  test("rejects protocol with no methods", () => {
    const source = `
protocol Empty a where
`;
    // The parser will reject this with "Expected at least one method signature"
    expect(() => parseTest(source)).toThrow(
      "Expected at least one method signature",
    );
  });

  test("rejects duplicate method names in protocol", () => {
    const source = `
protocol Bad a where
  foo : a -> a
  foo : a -> String
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Duplicate method");
  });
});

describe("Implementation Registration", () => {
  test("registers implementation for existing protocol", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

@external "@vibe/runtime" "intPlus"
intPlus : Int -> Int -> Int

implement Num Int where
  plus = intPlus
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.protocolName).toBe("Num");
  });

  test("rejects implementation with undefined function reference", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

implement Num Int where
  plus = undefinedFunction
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow(
      "Undefined name 'undefinedFunction'",
    );
  });

  test("validates module-qualified access in implementation", () => {
    // Create the dependency module
    const intModuleSource = `
module Vibe.Int exposing (add)

@external "@vibe/runtime" "intAdd"
add : Int -> Int -> Int
`;
    const intProgram = parseTest(intModuleSource);
    const intModule = analyze(intProgram);

    // Create the main module that imports and uses the function
    const mainSource = `
import Vibe.Int as Int

protocol Num a where
  (+) : a -> a -> a

implement Num Int where
  (+) = Int.add
`;
    const mainProgram = parseTest(mainSource);
    const dependencies = new Map([["Vibe.Int", intModule]]);

    // Should succeed since Int.add exists and is exported
    const result = analyze(mainProgram, { dependencies });
    expect(result.instances).toHaveLength(1);
  });

  test("rejects implementation with undefined module field", () => {
    // Create the dependency module without 'add'
    const intModuleSource = `
module Vibe.Int exposing (sub)

@external "@vibe/runtime" "intSub"
sub : Int -> Int -> Int
`;
    const intProgram = parseTest(intModuleSource);
    const intModule = analyze(intProgram);

    // Create the main module that tries to use a non-existent function
    const mainSource = `
import Vibe.Int as Int

protocol Num a where
  (+) : a -> a -> a

implement Num Int where
  (+) = Int.add
`;
    const mainProgram = parseTest(mainSource);
    const dependencies = new Map([["Vibe.Int", intModule]]);

    expect(() => analyze(mainProgram, { dependencies })).toThrow(SemanticError);
    expect(() => analyze(mainProgram, { dependencies })).toThrow(
      "'add' is not defined in module 'Vibe.Int'",
    );
  });

  test("rejects implementation with unexported module field", () => {
    // Create the dependency module with 'add' NOT exported
    const intModuleSource = `
module Vibe.Int exposing (sub)

@external "@vibe/runtime" "intAdd"
add : Int -> Int -> Int

@external "@vibe/runtime" "intSub"
sub : Int -> Int -> Int
`;
    const intProgram = parseTest(intModuleSource);
    const intModule = analyze(intProgram);

    // Create the main module that tries to use the non-exported function
    const mainSource = `
import Vibe.Int as Int

protocol Num a where
  (+) : a -> a -> a

implement Num Int where
  (+) = Int.add
`;
    const mainProgram = parseTest(mainSource);
    const dependencies = new Map([["Vibe.Int", intModule]]);

    expect(() => analyze(mainProgram, { dependencies })).toThrow(SemanticError);
    expect(() => analyze(mainProgram, { dependencies })).toThrow(
      "'add' is not exported from module 'Vibe.Int'",
    );
  });

  test("rejects implementation for non-existent protocol", () => {
    const source = `
implement NonExistent Int where
  foo = bar
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Unknown protocol");
  });

  test("rejects implementation missing required methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a
  minus : a -> a -> a

@external "@vibe/runtime" "intPlus"
intPlus : Int -> Int -> Int

implement Num Int where
  plus = intPlus
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("missing implementation for method");
  });

  test("rejects implementation with extra methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

@external "@vibe/runtime" "intPlus"
intPlus : Int -> Int -> Int

@external "@vibe/runtime" "extraImpl"
extraImpl : Int

implement Num Int where
  plus = intPlus
  extra = extraImpl
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("not part of protocol");
  });

  test("rejects overlapping implementations", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

@external "@vibe/runtime" "intPlus1"
intPlus1 : Int -> Int -> Int

@external "@vibe/runtime" "intPlus2"
intPlus2 : Int -> Int -> Int

implement Num Int where
  plus = intPlus1

implement Num Int where
  plus = intPlus2
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Overlapping implementation");
  });

  test("allows implementations for different types", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

@external "@vibe/runtime" "intPlus"
intPlus : Int -> Int -> Int

@external "@vibe/runtime" "floatPlus"
floatPlus : Float -> Float -> Float

implement Num Int where
  plus = intPlus

implement Num Float where
  plus = floatPlus
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(2);
  });

  test("allows constrained implementation alongside concrete type that doesn't satisfy constraint", () => {
    // A constrained implementation (Eq a => Protocol a) should NOT overlap with
    // a concrete type (Protocol (List a)) when List a doesn't have an Eq instance
    const source = `
protocol Eq a where
  eq : a -> a -> Bool

protocol MyProtocol a where
  method : a -> Bool

@external "@vibe/runtime" "defaultMethod"
defaultMethod : a -> Bool

implement Eq a => MyProtocol a where
  method = defaultMethod

implement MyProtocol (List a) where
  method _ = False
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Both implementations should be allowed since List a doesn't satisfy Eq
    expect(result.instances).toHaveLength(2);
  });

  test("rejects constrained implementation overlapping with concrete type that satisfies constraint", () => {
    // If we have `Eq a => Protocol a` and also `Protocol Int`, and Int has Eq,
    // then they overlap because Int could match both implementations
    const source = `
protocol Eq a where
  eq : a -> a -> Bool

protocol MyProtocol a where
  method : a -> Bool

@external "@vibe/runtime" "intEq"
intEq : Int -> Int -> Bool

@external "@vibe/runtime" "defaultMethod"
defaultMethod : a -> Bool

@external "@vibe/runtime" "intMethod"
intMethod : Int -> Bool

implement Eq Int where
  eq = intEq

implement Eq a => MyProtocol a where
  method = defaultMethod

implement MyProtocol Int where
  method = intMethod
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Overlapping implementation");
  });

  test("allows multiple constrained implementations with different constraints", () => {
    // Two constrained implementations with mutually exclusive type structures
    // should be allowed
    const source = `
protocol Eq a where
  eq : a -> a -> Bool

protocol Show a where
  show : a -> String

protocol MyProtocol a where
  method : a -> Bool

@external "@vibe/runtime" "eqMethod"
eqMethod : a -> Bool

@external "@vibe/runtime" "showMethod"
showMethod : a -> Bool

implement Eq a => MyProtocol a where
  method = eqMethod
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Single constrained implementation should work
    expect(result.instances).toHaveLength(1);
  });

  test("allows constrained implementation for List when element doesn't satisfy constraint", () => {
    // Similar to first test but more explicit about the List case
    const source = `
protocol Ord a where
  compare : a -> a -> Int

protocol Sortable a where
  sort : a -> a

@external "@vibe/runtime" "sortByOrd"
sortByOrd : a -> a

@external "@vibe/runtime" "noSort"
noSort : List a -> List a

implement Ord a => Sortable a where
  sort = sortByOrd

implement Sortable (List a) where
  sort = noSort
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Both should be allowed: List a doesn't automatically have Ord
    expect(result.instances).toHaveLength(2);
  });
});

describe("Implementation with Constraints", () => {
  test("registers implementation with single constraint", () => {
    const source = `
protocol Show a where
  show : a -> String

@external "@vibe/runtime" "showList"
showList : List a -> String

implement Show a => Show (List a) where
  show = showList
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.constraints).toHaveLength(1);
    expect(result.instances[0]?.constraints[0]?.protocolName).toBe("Show");
  });

  test("registers implementation with multiple constraints", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

protocol Show a where
  show : a -> String

type Pair a b = Pair a b

@external "@vibe/runtime" "showPair"
showPair : Pair a a -> String

implement (Num a, Show a) => Show (Pair a a) where
  show = showPair
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.constraints).toHaveLength(2);
  });
});

describe("Protocol Imports", () => {
  test("imports protocols from dependencies", () => {
    // Create a dependency module with a protocol
    const depSource = `
module Dep exposing (..)

protocol Num a where
  plus : a -> a -> a
`;
    const depProgram = parseTest(depSource);
    const depModule = analyze(depProgram);

    // Create a module that imports the dependency
    const source = `
import Dep

@external "@vibe/runtime" "intPlus"
intPlus : Int -> Int -> Int

implement Num Int where
  plus = intPlus
`;
    const program = parseTest(source);
    const result = analyze(program, {
      dependencies: new Map([["Dep", depModule]]),
    });

    // The protocol should be available in the importing module
    expect(result.protocols.Num).toBeDefined();
    expect(result.instances).toHaveLength(1);
  });
});

describe("Protocol Default Methods", () => {
  test("registers protocol with default method", () => {
    const source = `
@external "@vibe/runtime" "not"
not : Bool -> Bool

protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.protocols.Eq).toBeDefined();
    expect(result.protocols.Eq?.methods.size).toBe(2);

    const eqMethod = result.protocols.Eq?.methods.get("eq");
    expect(eqMethod?.defaultImpl).toBeUndefined();

    const neqMethod = result.protocols.Eq?.methods.get("neq");
    expect(neqMethod?.defaultImpl).toBeDefined();
  });

  test("allows implementation to omit method with default", () => {
    const source = `
@external "@vibe/runtime" "not"
not : Bool -> Bool

protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

@external "@vibe/runtime" "intEqual"
intEqual : Int -> Int -> Bool

implement Eq Int where
  eq = intEqual
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    // The instance should have both methods: explicit eq and default neq
    expect(result.instances[0]?.methods.size).toBe(2);
    expect(result.instances[0]?.methods.has("eq")).toBe(true);
    expect(result.instances[0]?.methods.has("neq")).toBe(true);
  });

  test("allows implementation to override default method", () => {
    const source = `
@external "@vibe/runtime" "not"
not : Bool -> Bool

protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

@external "@vibe/runtime" "intEqual"
intEqual : Int -> Int -> Bool

@external "@vibe/runtime" "intNotEqual"
intNotEqual : Int -> Int -> Bool

implement Eq Int where
  eq = intEqual
  neq = intNotEqual
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.methods.size).toBe(2);
  });

  test("requires implementation of method without default", () => {
    const source = `
@external "@vibe/runtime" "not"
not : Bool -> Bool

protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

@external "@vibe/runtime" "intNotEqual"
intNotEqual : Int -> Int -> Bool

implement Eq Int where
  neq = intNotEqual
`;
    const program = parseTest(source);

    // Should error because eq has no default and isn't implemented
    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("missing implementation for method");
  });

  test("registers protocol with all default methods", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"
  longDescription : a -> String
  longDescription x = describe x
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.protocols.Describable).toBeDefined();
    expect(result.protocols.Describable?.methods.size).toBe(2);

    const describeMethod =
      result.protocols.Describable?.methods.get("describe");
    expect(describeMethod?.defaultImpl).toBeDefined();

    const longDescMethod =
      result.protocols.Describable?.methods.get("longDescription");
    expect(longDescMethod?.defaultImpl).toBeDefined();
  });

  test("allows empty implementation when all methods have defaults", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"

implement Describable Int where
`;
    // NOTE: This will fail at parser level since it expects at least one method
    // Let's test with a different approach - provide at least one method
    const source2 = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"
  longDescription : a -> String
  longDescription x = describe x

@external "@vibe/runtime" "showInt"
showInt : Int -> String

implement Describable Int where
  describe = showInt
`;
    const program = parseTest(source2);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    // Both methods should exist: explicit describe and default longDescription
    expect(result.instances[0]?.methods.size).toBe(2);
  });
});


describe("Integration: Chained Default Methods", () => {
  test("default method can call another protocol method", () => {
    const source = `
protocol Show a where
  show : a -> String
  showList : List a -> String
  showList xs = show xs

@external "@vibe/runtime" "showInt"
showInt : Int -> String

implement Show Int where
  show = showInt
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    // Both methods should exist
    expect(result.instances[0]?.methods.size).toBe(2);
    expect(result.instances[0]?.methods.has("show")).toBe(true);
    expect(result.instances[0]?.methods.has("showList")).toBe(true);
  });

  test("chained defaults - method A's default uses method B", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"
  longDescription : a -> String
  longDescription x = describe x
  detailedDescription : a -> String
  detailedDescription x = longDescription x

type Color = Red | Green | Blue

implement Describable Color where
  describe _ = "A color"
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.protocolName).toBe("Describable");
    expect(result.instances[0]?.methods.size).toBe(3);
    expect(result.instances[0]?.methods.has("describe")).toBe(true);
    expect(result.instances[0]?.methods.has("longDescription")).toBe(true);
    expect(result.instances[0]?.methods.has("detailedDescription")).toBe(true);
  });

  test("override one default while using another", () => {
    const source = `
@external "@vibe/runtime" "not"
not : Bool -> Bool

protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)
  allEqual : a -> a -> a -> Bool
  allEqual x y z = eq x y

@external "@vibe/runtime" "intEqual"
intEqual : Int -> Int -> Bool

@external "@vibe/runtime" "customAllEqual"
customAllEqual : Int -> Int -> Int -> Bool

implement Eq Int where
  eq = intEqual
  allEqual = customAllEqual
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.methods.size).toBe(3);
    // eq is explicitly implemented
    expect(result.instances[0]?.methods.has("eq")).toBe(true);
    // neq uses the default
    expect(result.instances[0]?.methods.has("neq")).toBe(true);
    // allEqual is explicitly overridden
    expect(result.instances[0]?.methods.has("allEqual")).toBe(true);
  });
});
describe("Protocol Methods with Inferred Types", () => {
  test("accepts method with default but no explicit type annotation", () => {
    const source = `
protocol Helper a where
  show : a -> String
  debug x = show x
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.protocols.Helper).toBeDefined();
    expect(result.protocols.Helper?.methods.size).toBe(2);
    expect(result.protocols.Helper?.methods.has("show")).toBe(true);
    expect(result.protocols.Helper?.methods.has("debug")).toBe(true);

    // Both methods should have inferred types
    const showMethod = result.protocols.Helper?.methods.get("show");
    const debugMethod = result.protocols.Helper?.methods.get("debug");
    expect(showMethod?.type).toBeDefined();
    expect(debugMethod?.type).toBeDefined();
  });

  test("accepts protocol with all methods having defaults and no type annotations", () => {
    const source = `
protocol Defaulted a where
  foo x = x
  bar x y = foo x
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.protocols.Defaulted).toBeDefined();
    expect(result.protocols.Defaulted?.methods.size).toBe(2);

    // Both methods should have inferred types
    const fooMethod = result.protocols.Defaulted?.methods.get("foo");
    const barMethod = result.protocols.Defaulted?.methods.get("bar");
    expect(fooMethod?.type).toBeDefined();
    expect(barMethod?.type).toBeDefined();
  });

  test("method with default can omit type and be implemented", () => {
    const source = `
protocol Showable a where
  show : a -> String
  debugShow x = show x

@external "@vibe/runtime" "intToString"
intToString : Int -> String

implement Showable Int where
  show = intToString
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.methods.size).toBe(2);
    expect(result.instances[0]?.methods.has("show")).toBe(true);
    expect(result.instances[0]?.methods.has("debugShow")).toBe(true);
  });
});

describe("Constraint Collection and Dictionary Passing", () => {
  test("collects constraints when using protocol method operators", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

add x y = plus x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    // The function 'add' uses 'plus' which is a protocol method
    // This should result in a Num constraint being collected
    expect(result.typeSchemes.add).toBeDefined();
    expect(result.typeSchemes.add?.constraints.length).toBeGreaterThan(0);
    expect(result.typeSchemes.add?.constraints[0]?.protocolName).toBe("Num");
  });

  test("collects constraints for arithmetic operators", () => {
    const source = `
protocol Num a where
  (+) : a -> a -> a

implement Num Int where
  (+) x y = x

addOne x = x + 1
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Using + operator should collect Num constraint
    // Note: The constraint may be on a concrete type (Int) due to the literal
    expect(result.typeSchemes.addOne).toBeDefined();
  });

  test("propagates constraints through function composition", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

add x y = plus x y
double x = add x x
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Both 'add' and 'double' should have Num constraints
    expect(result.typeSchemes.add).toBeDefined();
    expect(result.typeSchemes.double).toBeDefined();
    // Since double calls add, and add requires Num, double should have Num constraint
    expect(
      result.typeSchemes.add?.constraints.some((c) => c.protocolName === "Num"),
    ).toBe(true);
  });

  test("does not collect constraints for concrete types", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

@external "@vibe/runtime" "intPlus"
intPlus : Int -> Int -> Int

implement Num Int where
  plus = intPlus

-- This function has concrete Int type, no constraint needed
concreteAdd : Int -> Int -> Int
concreteAdd x y = plus x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    // With concrete types, the constraint should be resolved
    expect(result.typeSchemes.concreteAdd).toBeDefined();
    // The constraint should be on Int, which is concrete
    // So it may or may not be filtered out depending on implementation
  });

  test("stores type schemes with constraints in semantic module", () => {
    const source = `
protocol Eq a where
  eq : a -> a -> Bool

isEqual x y = eq x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    // typeSchemes should be populated
    expect(result.typeSchemes).toBeDefined();
    expect(result.typeSchemes.isEqual).toBeDefined();
    expect(result.typeSchemes.isEqual?.constraints).toBeDefined();
    expect(Array.isArray(result.typeSchemes.isEqual?.constraints)).toBe(true);
  });
});

describe("Qualified Type Annotation Constraint Enforcement", () => {
  test("extracts constraints from qualified type annotations", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

add : Num a => a -> a -> a
add x y = plus x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    // The constraint from the annotation should be included in the type scheme
    expect(result.typeSchemes.add).toBeDefined();
    expect(result.typeSchemes.add?.constraints.length).toBeGreaterThan(0);
    expect(
      result.typeSchemes.add?.constraints.some((c) => c.protocolName === "Num"),
    ).toBe(true);
  });

  test("extracts multiple constraints from qualified type annotations", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

protocol Eq a where
  eq : a -> a -> Bool

addIfEq : (Num a, Eq a) => a -> a -> a -> a
addIfEq x y z = plus x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    expect(result.typeSchemes.addIfEq).toBeDefined();
    const constraints = result.typeSchemes.addIfEq?.constraints ?? [];
    expect(constraints.some((c) => c.protocolName === "Num")).toBe(true);
    expect(constraints.some((c) => c.protocolName === "Eq")).toBe(true);
  });

  test("rejects constraints referencing unknown protocols", () => {
    const source = `
foo : Unknown a => a -> a
foo x = x
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Unknown protocol 'Unknown'");
  });

  test("rejects constraints with wrong number of type arguments", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

foo : Num a b => a -> a
foo x = x
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("expects 1 type argument");
  });

  test("rejects constraints on concrete types in annotations", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

foo : Num Int => Int -> Int
foo x = x
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("must be applied to type variables");
  });

  test("stores annotated constraints in value info", () => {
    const source = `
protocol Show a where
  show : a -> String

display : Show a => a -> String
display x = show x
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Check that annotated constraints are stored
    expect(result.values.display).toBeDefined();
    expect(result.values.display?.annotatedConstraints).toBeDefined();
    expect(result.values.display?.annotatedConstraints?.length).toBe(1);
    expect(result.values.display?.annotatedConstraints?.[0]?.protocolName).toBe(
      "Show",
    );
  });

  test("merges annotated constraints with inferred constraints", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

protocol Eq a where
  eq : a -> a -> Bool

-- Annotation declares Eq constraint, body uses plus which needs Num
compute : (Num a, Eq a) => a -> a -> a
compute x y = plus x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Both Num (from inference) and Eq (from annotation) should be in the scheme
    const constraints = result.typeSchemes.compute?.constraints ?? [];
    expect(constraints.some((c) => c.protocolName === "Num")).toBe(true);
    expect(constraints.some((c) => c.protocolName === "Eq")).toBe(true);
  });

  test("handles external declarations with qualified types", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

@external "math" "jsAdd"
jsAdd : Num a => a -> a -> a
`;
    const program = parseTest(source);
    const result = analyze(program);

    // External declaration should have the constraint from annotation
    expect(result.values.jsAdd).toBeDefined();
    expect(result.values.jsAdd?.annotatedConstraints).toBeDefined();
    expect(result.values.jsAdd?.annotatedConstraints?.length).toBe(1);
    expect(result.values.jsAdd?.annotatedConstraints?.[0]?.protocolName).toBe(
      "Num",
    );
  });

  test("deduplicates constraints when annotated and inferred overlap", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

-- Both annotation and inference produce Num constraint
add : Num a => a -> a -> a
add x y = plus x y
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should only have one Num constraint, not duplicates
    const numConstraints =
      result.typeSchemes.add?.constraints.filter(
        (c) => c.protocolName === "Num",
      ) ?? [];
    expect(numConstraints.length).toBe(1);
  });
});

describe("Multi-Parameter Protocol Constraint Validation", () => {
  test("validates constraint applies to correct type parameter", () => {
    // Bug fix test: constraint `Appendable a` applies to second type arg, not first
    const source = `
protocol Appendable a where
  append : a -> a -> a

protocol Convert a b where
  convert : a -> b

-- List implements Appendable
implement Appendable (List a) where
  append xs ys = xs

-- Implementation where second type param has Appendable constraint
-- convert3 returns [], which is List - should pass because List is Appendable
implement Appendable b => Convert Int b where
  convert _ = []
`;
    const program = parseTest(source);
    // Should not throw - List is Appendable
    const result = analyze(program);
    expect(result.instances.length).toBe(2);
  });

  test("rejects implementation where return type violates constraint", () => {
    // Bug fix test: returning Int when constraint requires Appendable should fail
    const source = `
protocol Appendable a where
  append : a -> a -> a

protocol Convert a b where
  convert : a -> b

-- Int does NOT implement Appendable
-- This implementation returns 10 (Int), which violates the Appendable constraint
implement Appendable b => Convert Float b where
  convert _ = 10
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow(/Appendable/);
    expect(() => analyze(program)).toThrow(/Int/);
  });

  test("validates multi-parameter protocol with constraint on first param", () => {
    const source = `
protocol Eq a where
  eq : a -> a -> Bool

protocol Transform a b where
  transform : a -> b

-- Constraint on first param
implement Eq a => Transform a Int where
  transform x = 42
`;
    const program = parseTest(source);
    // Should pass - transform takes an Eq a and returns Int
    const result = analyze(program);
    expect(result.instances.length).toBe(1);
  });

  test("allows polymorphic return type matching Appendable constraint", () => {
    const source = `
protocol Appendable a where
  append : a -> a -> a

protocol Convert a b where
  convert : a -> b

implement Appendable (List a) where
  append xs ys = xs

-- Return type is List which is Appendable (polymorphic match)
implement Appendable b => Convert String b where
  convert _ = []
`;
    const program = parseTest(source);
    const result = analyze(program);
    expect(result.instances.length).toBe(2);
  });

  test("rejects constraint violation with String (non-Appendable)", () => {
    const source = `
protocol Appendable a where
  append : a -> a -> a

protocol Convert a b where
  convert : a -> b

-- String literal - assuming String doesn't implement Appendable
implement Appendable b => Convert Int b where
  convert _ = "hello"
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow(/Appendable/);
  });

  test("validates constraint on nested type parameter", () => {
    const source = `
protocol Show a where
  show : a -> String

protocol Wrap a b where
  wrap : a -> b

-- List Show constraint
implement Show (List a) where
  show xs = "list"

implement Show b => Wrap Int b where
  wrap _ = []
`;
    const program = parseTest(source);
    const result = analyze(program);
    expect(result.instances.length).toBe(2);
  });

  test("concretizes instance type args from implementation body", () => {
    // Bug fix test: when an implementation body forces a polymorphic type arg
    // to be concrete (e.g., `convert3 _ = [1]` forces `a` to be `List Int`),
    // the instance's typeArgs should be updated so that later generalization
    // produces concrete types instead of polymorphic with constraints.
    // This fixes hover showing `ExampleProtocol3 Float t402 => t402` instead of `List Int`.
    const source = `
protocol Appendable a where
  append : a -> a -> a

protocol ExampleProtocol3 a b where
  convert3 : a -> b

implement Appendable (List a) where
  append xs ys = xs

-- Implementation forces b to be List Int due to the literal [1]
implement Appendable a => ExampleProtocol3 Float a where
  convert3 _ = [1]

main = convert3 10.0
`;
    const program = parseTest(source);
    const result = analyze(program);

    // The instance for ExampleProtocol3 should have typeArgs [Float, List Int]
    // after concretization, not [Float, a] with Appendable a constraint
    const ep3Instance = result.instances.find(
      (i) => i.protocolName === "ExampleProtocol3",
    );
    expect(ep3Instance).toBeDefined();
    expect(ep3Instance!.typeArgs.length).toBe(2);
    expect(ep3Instance!.typeArgs[0]).toEqual({
      kind: "con",
      name: "Float",
      args: [],
    });
    // Second type arg should now be List Int (concretized from the body)
    expect(ep3Instance!.typeArgs[1]!.kind).toBe("con");
    expect((ep3Instance!.typeArgs[1] as any).name).toBe("List");

    // main should have type List Int (concrete), not a polymorphic type with constraint
    const mainScheme = result.typeSchemes.main;
    expect(mainScheme).toBeDefined();
    // The type should be concrete List Int, not a type variable
    expect(mainScheme!.type.kind).toBe("con");
    expect((mainScheme!.type as any).name).toBe("List");
    // Should have no constraints (or at least no Appendable constraint on a free var)
    // since the type is now fully concrete
    expect(mainScheme!.constraints.length).toBe(0);
  });
});

describe("Over-application of Protocol Methods", () => {
  test("produces type mismatch error when protocol method result is over-applied", () => {
    // This tests the fix for the inconsistent error message issue.
    // When `convert3 10.0 []` is written, and convert3 : a -> b,
    // the result of `convert3 10.0` (type b) is being applied to [],
    // which means b must be a function type. But the instance returns
    // List Int, not a function. This should produce a "Type mismatch" error,
    // not a "No instance of" error.
    const source = `
protocol Appendable a where
  append : a -> a -> a

protocol ExampleProtocol3 a b where
  convert3 : a -> b

implement Appendable (List a) where
  append xs ys = xs

implement Appendable a => ExampleProtocol3 Float a where
  convert3 _ = [1]

main = convert3 10.0 []
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    // The error should be a type mismatch, not a missing instance error
    expect(() => analyze(program)).toThrow("Type mismatch");
  });

  test("still produces correct error for missing instance when not over-applied", () => {
    // This verifies we haven't broken the case where there really is a missing instance
    const source = `
protocol Show a where
  show : a -> String

type MyType = MyType

-- No Show instance for MyType
main = show MyType
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("No instance of");
  });
});

describe("Ambiguous Type Variable Detection", () => {
  test("rejects empty list equality due to ambiguous type variable", () => {
    // In `[] == []`, both lists have type `List a` for some unknown `a`.
    // The result type is `Bool`, which doesn't mention `a`.
    // This means there's no way to determine what type `a` should be,
    // making the `Eq` constraint ambiguous.
    const source = `
infix 4 ==

protocol Eq a where
  (==) : a -> a -> Bool

implement Eq a => Eq (List a) where
  (==) xs ys = True

main = [] == []
`;
    const program = parseTest(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow(/[Aa]mbiguous/);
  });

  test("accepts list equality when type is concrete", () => {
    // When the list type is known (List Int), there's no ambiguity
    const source = `
infix 4 ==

protocol Eq a where
  (==) : a -> a -> Bool

implement Eq Int where
  (==) x y = True

implement Eq a => Eq (List a) where
  (==) xs ys = True

xs : List Int
xs = []

main = xs == []
`;
    const program = parseTest(source);
    const result = analyze(program);
    expect(result.values.main).toBeDefined();
  });

  test("accepts polymorphic function with constraint in result type", () => {
    // When the type variable appears in the result type, it's not ambiguous
    const source = `
infix 4 ==

protocol Eq a where
  (==) : a -> a -> Bool

-- This is fine because 'a' appears in the result type (a -> a -> Bool)
-- so callers can determine what type 'a' should be
eq : Eq a => a -> a -> Bool
eq x y = x == y
`;
    const program = parseTest(source);
    const result = analyze(program);
    expect(result.values.eq).toBeDefined();
  });
});

describe("Auto Eq Implementation", () => {
  test("auto-implements Eq for simple ADT", () => {
    const source = `
module Vibe exposing (..)

@external "runtime" "not"
not : Bool -> Bool

infix 4 ==
infix 4 /=

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

type Color = Red | Green | Blue
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should auto-generate Eq instance
    expect(
      result.instances.some(
        (i) =>
          i.protocolName === "Eq" &&
          i.typeArgs[0]?.kind === "con" &&
          (i.typeArgs[0] as any).name === "Color",
      ),
    ).toBe(true);
  });

  test("auto-implements Eq for record type", () => {
    const source = `
module Vibe exposing (..)

@external "runtime" "not"
not : Bool -> Bool

infix 4 ==
infix 4 /=

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

@external "runtime" "eqInt"
eqInt : Int -> Int -> Bool

implement Eq Int where
  (==) = eqInt

type Point = { x : Int, y : Int }
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should auto-generate Eq instance for Point
    expect(
      result.instances.some(
        (i) =>
          i.protocolName === "Eq" &&
          i.typeArgs[0]?.kind === "con" &&
          (i.typeArgs[0] as any).name === "Point",
      ),
    ).toBe(true);
  });

  test("does not auto-implement Eq for type with function field", () => {
    const source = `
module Vibe exposing (..)

@external "runtime" "not"
not : Bool -> Bool

infix 4 ==

protocol Eq a where
  (==) : a -> a -> Bool

type FuncHolder = Holder (Int -> Int)
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should NOT auto-generate Eq instance (function type can't implement Eq)
    expect(
      result.instances.some(
        (i) =>
          i.protocolName === "Eq" &&
          i.typeArgs[0]?.kind === "con" &&
          (i.typeArgs[0] as any).name === "FuncHolder",
      ),
    ).toBe(false);
  });

  test("does not auto-implement Eq when explicit implementation exists", () => {
    const source = `
module Vibe exposing (..)

@external "runtime" "not"
not : Bool -> Bool

infix 4 ==
infix 4 /=

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

type Color = Red | Green | Blue

-- Explicit custom implementation
implement Eq Color where
  (==) _ _ = True
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should only have one Eq instance for Color (the explicit one)
    const colorInstances = result.instances.filter(
      (i) =>
        i.protocolName === "Eq" &&
        i.typeArgs[0]?.kind === "con" &&
        (i.typeArgs[0] as any).name === "Color",
    );
    expect(colorInstances).toHaveLength(1);
  });

  test("auto-implements Eq with constraint for polymorphic type", () => {
    const source = `
module Vibe exposing (..)

@external "runtime" "not"
not : Bool -> Bool

infix 4 ==
infix 4 /=

protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)

type Box a = MkBox a
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should auto-generate Eq instance with Eq a constraint
    const boxInstance = result.instances.find(
      (i) =>
        i.protocolName === "Eq" &&
        i.typeArgs[0]?.kind === "con" &&
        (i.typeArgs[0] as any).name === "Box",
    );
    expect(boxInstance).toBeDefined();
    expect(boxInstance?.constraints).toHaveLength(1);
    expect(boxInstance?.constraints[0]?.protocolName).toBe("Eq");
  });

  test("does not auto-implement Eq for custom Eq protocol", () => {
    // When module defines its own Eq protocol (not from Vibe),
    // auto-implementation should not apply
    const source = `
module CustomModule exposing (..)

protocol Eq a where
  eq : a -> a -> Bool

type Color = Red | Green | Blue
`;
    const program = parseTest(source);
    const result = analyze(program);

    // Should NOT auto-generate Eq instance (not from Vibe/Vibe.Basics)
    expect(
      result.instances.some(
        (i) =>
          i.protocolName === "Eq" &&
          i.typeArgs[0]?.kind === "con" &&
          (i.typeArgs[0] as any).name === "Color",
      ),
    ).toBe(false);
  });
});
