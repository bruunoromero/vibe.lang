import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "../src/index.ts";

const PREAMBLE = `
module Test exposing (..)
`;

describe("type constraints", () => {
  test("allows constraints on ADT declarations", () => {
    const source = `
${PREAMBLE}
protocol Eq a where
  eq : a -> a -> Bool

type Eq a => Set a = MkSet (List a)
`;
    const { adts, constructorTypes } = analyze(parse(source), { fileContext: { filePath: "Test", srcDir: "" } });
    const setInfo = adts["Set"];
    expect(setInfo).toBeDefined();
    if (!setInfo) return; // Guard for typescript

    if (!setInfo?.constraints[0]) throw new Error("Missing constraints");
    expect(setInfo.constraints).toHaveLength(1);
    expect(setInfo.constraints[0].protocolName).toBe("Eq");
    expect(setInfo.constraints[0].typeArgs[0]?.kind).toBe("var");

    // Constructor scheme should include the constraint
    const ctorScheme = constructorTypes["MkSet"];
    expect(ctorScheme).toBeDefined();
    if (!ctorScheme?.constraints[0]) return;

    expect(ctorScheme.constraints).toHaveLength(1);
    expect(ctorScheme.constraints[0].protocolName).toBe("Eq");
  });

  test("allows constraints on Record declarations", () => {
    const source = `
${PREAMBLE}
protocol Eq a where
  eq : a -> a -> Bool

type Eq a => Set a = { items : List a }
`;
    const { records } = analyze(parse(source), { fileContext: { filePath: "Test", srcDir: "" } });
    const setInfo = records["Set"];
    expect(setInfo).toBeDefined();
    if (!setInfo?.constraints[0]) return;

    expect(setInfo.constraints).toHaveLength(1);
    expect(setInfo.constraints[0].protocolName).toBe("Eq");
  });

  test("rejects constraints on opaque types", () => {
    const source = `
${PREAMBLE}
protocol Eq a where
  eq : a -> a -> Bool

type Eq a => Promise a
`;
    expect(() => analyze(parse(source), { fileContext: { filePath: "Test", srcDir: "" } })).toThrow(/Opaque types cannot have constraints/);
  });

  test("validates constraints during type usage (missing instance)", () => {
    const source = `
${PREAMBLE}
protocol Eq a where
  eq : a -> a -> Bool

type Eq a => Set a = MkSet (List a)

-- Int doesn't implement Eq here (no instance defined)
bad : Set Int
bad = MkSet [1]
`;
    expect(() => analyze(parse(source), { fileContext: { filePath: "Test", srcDir: "" } })).toThrow(/No instance of 'Eq' for type\(s\) 'Int'/);
  });

  test("allows usage when constraint satisfied", () => {
    const source = `
${PREAMBLE}
protocol Eq a where
  eq : a -> a -> Bool

implement Eq Int where
  eq x y = True

type Eq a => Set a = MkSet (List a)

good : Set Int
good = MkSet [1]
`;
    expect(() => analyze(parse(source), { fileContext: { filePath: "Test", srcDir: "" } })).not.toThrow();
  });
});
