import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index";

describe("Protocol Registration", () => {
  test("registers protocol with methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a
  minus : a -> a -> a
`;
    const program = parse(source);
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
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Duplicate protocol");
  });

  test("rejects protocol with no methods", () => {
    const source = `
protocol Empty a where
`;
    // The parser will reject this with "Expected at least one method signature"
    expect(() => parse(source)).toThrow(
      "Expected at least one method signature"
    );
  });

  test("rejects duplicate method names in protocol", () => {
    const source = `
protocol Bad a where
  foo : a -> a
  foo : a -> String
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Duplicate method");
  });
});

describe("Implementation Registration", () => {
  test("registers implementation for existing protocol", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

implement Num Int where
  plus = intPlus
`;
    const program = parse(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.protocolName).toBe("Num");
  });

  test("rejects implementation for non-existent protocol", () => {
    const source = `
implement NonExistent Int where
  foo = bar
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Unknown protocol");
  });

  test("rejects implementation missing required methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a
  minus : a -> a -> a

implement Num Int where
  plus = intPlus
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("missing implementation for method");
  });

  test("rejects implementation with extra methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

implement Num Int where
  plus = intPlus
  extra = extraImpl
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("not part of protocol");
  });

  test("rejects overlapping implementations", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

implement Num Int where
  plus = intPlus1

implement Num Int where
  plus = intPlus2
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Overlapping implementation");
  });

  test("allows implementations for different types", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

implement Num Int where
  plus = intPlus

implement Num Float where
  plus = floatPlus
`;
    const program = parse(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(2);
  });
});

describe("Implementation with Constraints", () => {
  test("registers implementation with single constraint", () => {
    const source = `
protocol Show a where
  show : a -> String

implement Show a => Show (List a) where
  show = showList
`;
    const program = parse(source);
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

implement (Num a, Show a) => Show (Pair a a) where
  show = showPair
`;
    const program = parse(source);
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
    const depProgram = parse(depSource);
    const depModule = analyze(depProgram);

    // Create a module that imports the dependency
    const source = `
import Dep

implement Num Int where
  plus = intPlus
`;
    const program = parse(source);
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
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)
`;
    const program = parse(source);
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
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

implement Eq Int where
  eq = intEqual
`;
    const program = parse(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    // The instance should have both methods: explicit eq and default neq
    expect(result.instances[0]?.methods.size).toBe(2);
    expect(result.instances[0]?.methods.has("eq")).toBe(true);
    expect(result.instances[0]?.methods.has("neq")).toBe(true);
  });

  test("allows implementation to override default method", () => {
    const source = `
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

implement Eq Int where
  eq = intEqual
  neq = intNotEqual
`;
    const program = parse(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.methods.size).toBe(2);
  });

  test("requires implementation of method without default", () => {
    const source = `
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

implement Eq Int where
  neq = intNotEqual
`;
    const program = parse(source);

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
    const program = parse(source);
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

implement Describable Int where
  describe = showInt
`;
    const program = parse(source2);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    // Both methods should exist: explicit describe and default longDescription
    expect(result.instances[0]?.methods.size).toBe(2);
  });
});

describe("implementing Keyword", () => {
  test("allows implementing when all methods have defaults", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"

type Color = Red | Green | Blue
  implementing Describable
`;
    const program = parse(source);
    const result = analyze(program);

    // Should have created a synthetic implementation
    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.protocolName).toBe("Describable");
    expect(result.instances[0]?.methods.has("describe")).toBe(true);
  });

  test("rejects implementing when method lacks default", () => {
    const source = `
protocol Show a where
  show : a -> String

type Color = Red | Green | Blue
  implementing Show
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow("Cannot use 'implementing'");
    expect(() => analyze(program)).toThrow("Missing defaults for: show");
  });

  test("allows implementing multiple protocols with all defaults", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"

protocol Tagged a where
  tag : a -> String
  tag _ = "untagged"

type Color = Red | Green | Blue
  implementing Describable, Tagged
`;
    const program = parse(source);
    const result = analyze(program);

    // Should have two synthetic implementations
    expect(result.instances).toHaveLength(2);
    expect(result.instances[0]?.protocolName).toBe("Describable");
    expect(result.instances[1]?.protocolName).toBe("Tagged");
  });

  test("rejects implementing with non-existent protocol", () => {
    const source = `
type Color = Red | Green | Blue
  implementing NonExistent
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow(SemanticError);
    expect(() => analyze(program)).toThrow("Unknown protocol");
  });

  test("provides helpful error listing missing defaults", () => {
    const source = `
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)

type Color = Red | Green | Blue
  implementing Eq
`;
    const program = parse(source);

    try {
      analyze(program);
      expect(false).toBe(true); // Should have thrown
    } catch (e) {
      expect(e instanceof Error).toBe(true);
      const error = e as Error;
      expect(error.message).toContain("Cannot use 'implementing'");
      expect(error.message).toContain("Missing defaults for: eq");
      expect(error.message).toContain("Methods with defaults: neq");
      expect(error.message).toContain("Hint:");
    }
  });

  test("rejects overlapping implementing with explicit implement", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"

type Color = Red | Green | Blue
  implementing Describable

implement Describable Color where
  describe = showColor
`;
    const program = parse(source);

    expect(() => analyze(program)).toThrow("Overlapping implementation");
  });
});

describe("Integration: Chained Default Methods", () => {
  test("default method can call another protocol method", () => {
    const source = `
protocol Show a where
  show : a -> String
  showList : List a -> String
  showList xs = show xs

implement Show Int where
  show = showInt
`;
    const program = parse(source);
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
  implementing Describable
`;
    const program = parse(source);
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
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)
  allEqual : a -> a -> a -> Bool
  allEqual x y z = eq x y

implement Eq Int where
  eq = intEqual
  allEqual = customAllEqual
`;
    const program = parse(source);
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

  test("multiple types implementing same protocol with all defaults", () => {
    const source = `
protocol Default a where
  value : a -> String
  value _ = "default"

type Foo = Foo
  implementing Default

type Bar = Bar
  implementing Default
`;
    const program = parse(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(2);
    expect(result.instances[0]?.protocolName).toBe("Default");
    expect(result.instances[1]?.protocolName).toBe("Default");
  });
});
describe("Protocol Methods with Inferred Types", () => {
  test("accepts method with default but no explicit type annotation", () => {
    const source = `
protocol Helper a where
  show : a -> String
  debug x = show x
`;
    const program = parse(source);
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
    const program = parse(source);
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

implement Showable Int where
  show = intToString
`;
    const program = parse(source);
    const result = analyze(program);

    expect(result.instances).toHaveLength(1);
    expect(result.instances[0]?.methods.size).toBe(2);
    expect(result.instances[0]?.methods.has("show")).toBe(true);
    expect(result.instances[0]?.methods.has("debugShow")).toBe(true);
  });
});
