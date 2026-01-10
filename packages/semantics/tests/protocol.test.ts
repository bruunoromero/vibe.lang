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
