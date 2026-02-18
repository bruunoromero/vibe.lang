import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import {
  analyze,
  buildNormalizedNames,
  formatTypeForDisplay,
  formatTypeSchemeForDisplay,
  formatConstraintsForDisplay,
  type Type,
  type TypeScheme,
  type Constraint,
} from "../src/index";

// --- Helpers ---

function con(name: string, ...args: Type[]): Type {
  return { kind: "con", name, args };
}

function tv(id: number): Type {
  return { kind: "var", id };
}

function fun(from: Type, to: Type): Type {
  return { kind: "fun", from, to };
}

function tuple(...elements: Type[]): Type {
  return { kind: "tuple", elements };
}

function record(fields: Record<string, Type>): Type {
  return { kind: "record", fields };
}

function scheme(
  vars: number[],
  constraints: Constraint[],
  type: Type,
  paramNames?: Map<number, string>,
): TypeScheme {
  return {
    vars: new Set(vars),
    constraints,
    type,
    paramNames,
  };
}

function parseTest(source: string) {
  const fullSource = source.trim().startsWith("module ")
    ? source
    : `module Test exposing (..)\n\n${source}`;
  return parse(fullSource);
}

// --- Tests ---

describe("buildNormalizedNames", () => {
  test("assigns sequential letters to type vars in order of appearance", () => {
    const s = scheme([10, 20], [], fun(tv(10), tv(20)));
    const names = buildNormalizedNames(s);
    expect(names.get(10)).toBe("a");
    expect(names.get(20)).toBe("b");
  });

  test("constraint vars get earlier letters", () => {
    // Constraint mentions var 20, type mentions var 10 first
    const s = scheme(
      [10, 20],
      [{ protocolName: "Num", typeArgs: [tv(20)] }],
      fun(tv(10), tv(20)),
    );
    const names = buildNormalizedNames(s);
    // Constraint var 20 scanned first → "a"
    expect(names.get(20)).toBe("a");
    // Type-only var 10 → "b"
    expect(names.get(10)).toBe("b");
  });

  test("handles single type variable", () => {
    const s = scheme([5], [], fun(tv(5), tv(5)));
    const names = buildNormalizedNames(s);
    expect(names.get(5)).toBe("a");
  });

  test("handles many type variables beyond 26", () => {
    const ids = Array.from({ length: 30 }, (_, i) => i + 100);
    const type: Type = fun(tv(ids[0]!), tv(ids[29]!));
    const s = scheme(
      ids,
      ids.slice(0, 28).map((id) => ({ protocolName: "P", typeArgs: [tv(id)] })),
      type,
    );
    const names = buildNormalizedNames(s);
    expect(names.get(ids[0]!)).toBe("a");
    expect(names.get(ids[25]!)).toBe("z");
    // Beyond 26 letters, falls back to tN
    expect(names.get(ids[26]!)).toBe(`t${ids[26]}`);
  });

  test("no duplicate names", () => {
    const s = scheme([1, 2, 3], [], fun(tv(1), fun(tv(2), tv(3))));
    const names = buildNormalizedNames(s);
    const values = Array.from(names.values());
    expect(new Set(values).size).toBe(values.length);
  });
});

describe("formatTypeForDisplay", () => {
  test("renders type variables using provided names", () => {
    const names = new Map([[42, "x"]]);
    expect(formatTypeForDisplay(tv(42), names)).toBe("x");
  });

  test("falls back to tN for unmapped vars", () => {
    const names = new Map<number, string>();
    expect(formatTypeForDisplay(tv(99), names)).toBe("t99");
  });

  test("formats concrete types", () => {
    const names = new Map<number, string>();
    expect(formatTypeForDisplay(con("Int"), names)).toBe("Int");
  });

  test("formats function types", () => {
    const names = new Map([[0, "a"]]);
    expect(formatTypeForDisplay(fun(tv(0), con("Int")), names)).toBe(
      "a -> Int",
    );
  });

  test("wraps function args in parens", () => {
    const names = new Map([
      [0, "a"],
      [1, "b"],
    ]);
    // (a -> b) -> a
    expect(formatTypeForDisplay(fun(fun(tv(0), tv(1)), tv(0)), names)).toBe(
      "(a -> b) -> a",
    );
  });

  test("formats applied constructors with parens on complex args", () => {
    const names = new Map([[0, "a"]]);
    // List (Maybe a)
    expect(formatTypeForDisplay(con("List", con("Maybe", tv(0))), names)).toBe(
      "List (Maybe a)",
    );
  });

  test("formats tuples", () => {
    const names = new Map([
      [0, "a"],
      [1, "b"],
    ]);
    expect(formatTypeForDisplay(tuple(tv(0), tv(1)), names)).toBe("(a, b)");
  });

  test("formats records", () => {
    const names = new Map([[0, "a"]]);
    expect(
      formatTypeForDisplay(
        record({ name: con("String"), value: tv(0) }),
        names,
      ),
    ).toBe("{ name : String, value : a }");
  });

  test("formats error type", () => {
    expect(formatTypeForDisplay({ kind: "error" }, new Map())).toBe("<error>");
  });
});

describe("formatConstraintsForDisplay", () => {
  test("empty constraints return empty string", () => {
    expect(formatConstraintsForDisplay([], new Map())).toBe("");
  });

  test("single constraint without parens", () => {
    const names = new Map([[0, "a"]]);
    const constraints: Constraint[] = [
      { protocolName: "Num", typeArgs: [tv(0)] },
    ];
    expect(formatConstraintsForDisplay(constraints, names)).toBe("Num a");
  });

  test("multiple constraints wrapped in parens", () => {
    const names = new Map([[0, "a"]]);
    const constraints: Constraint[] = [
      { protocolName: "Num", typeArgs: [tv(0)] },
      { protocolName: "Eq", typeArgs: [tv(0)] },
    ];
    expect(formatConstraintsForDisplay(constraints, names)).toBe(
      "(Num a, Eq a)",
    );
  });

  test("multi-param constraints", () => {
    const names = new Map([
      [0, "a"],
      [1, "b"],
    ]);
    const constraints: Constraint[] = [
      { protocolName: "Convert", typeArgs: [tv(0), tv(1)] },
    ];
    expect(formatConstraintsForDisplay(constraints, names)).toBe("Convert a b");
  });
});

describe("formatTypeSchemeForDisplay", () => {
  test("uses annotation paramNames when present", () => {
    const names = new Map([
      [100, "x"],
      [200, "err"],
    ]);
    const s = scheme(
      [100, 200],
      [{ protocolName: "Show", typeArgs: [tv(100)] }],
      fun(tv(100), con("Result", tv(200), tv(100))),
      names,
    );
    expect(formatTypeSchemeForDisplay(s)).toBe("Show x => x -> Result err x");
  });

  test("normalizes when no paramNames", () => {
    const s = scheme([50, 51], [], fun(tv(50), tv(51)));
    expect(formatTypeSchemeForDisplay(s)).toBe("a -> b");
  });

  test("normalizes with constraints when no paramNames", () => {
    const s = scheme(
      [50],
      [{ protocolName: "Eq", typeArgs: [tv(50)] }],
      fun(tv(50), fun(tv(50), con("Bool"))),
    );
    expect(formatTypeSchemeForDisplay(s)).toBe("Eq a => a -> a -> Bool");
  });

  test("monomorphic type", () => {
    const s = scheme([], [], con("Int"));
    expect(formatTypeSchemeForDisplay(s)).toBe("Int");
  });

  test("handles complex nested types", () => {
    const names = new Map([
      [0, "a"],
      [1, "b"],
    ]);
    // (a -> b) -> List a -> List b
    const s = scheme(
      [0, 1],
      [],
      fun(fun(tv(0), tv(1)), fun(con("List", tv(0)), con("List", tv(1)))),
      names,
    );
    expect(formatTypeSchemeForDisplay(s)).toBe(
      "(a -> b) -> (List a) -> List b",
    );
  });

  test("handles unknown/null scheme gracefully", () => {
    expect(formatTypeSchemeForDisplay(null as any)).toBe("<unknown type>");
  });
});

describe("end-to-end: annotation names flow to TypeScheme", () => {
  test("simple qualified type preserves param names", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a

add : Num a => a -> a -> a
add x y = x
`;
    const program = parseTest(source);
    const result = analyze(program, {
      fileContext: { filePath: "Test", srcDir: "" },
    });

    const s = result.typeSchemes["add"]!;
    expect(s).toBeDefined();
    expect(s.paramNames).toBeDefined();

    // The name "a" should appear in the paramNames map
    const names = Array.from(s.paramNames!.values());
    expect(names).toContain("a");

    // And formatTypeSchemeForDisplay should use it
    const display = formatTypeSchemeForDisplay(s);
    expect(display).toContain("a -> a -> a");
    expect(display).toContain("Num a");
  });

  test("multi-param annotation preserves all names", () => {
    const source = `
protocol MyProto a where
  myMethod : a -> a

pair : (MyProto x, MyProto y) => x -> y -> (x, y)
pair a b = (a, b)
`;
    const program = parseTest(source);
    const result = analyze(program, {
      fileContext: { filePath: "Test", srcDir: "" },
    });

    const s = result.typeSchemes["pair"]!;
    expect(s).toBeDefined();
    expect(s.paramNames).toBeDefined();

    const names = Array.from(s.paramNames!.values());
    expect(names).toContain("x");
    expect(names).toContain("y");

    const display = formatTypeSchemeForDisplay(s);
    expect(display).toContain("x -> y");
  });

  test("unannotated function gets normalized names", () => {
    const source = `
identity x = x
`;
    const program = parseTest(source);
    const result = analyze(program, {
      fileContext: { filePath: "Test", srcDir: "" },
    });

    const s = result.typeSchemes["identity"]!;
    expect(s).toBeDefined();
    // No annotation → no paramNames
    expect(s.paramNames).toBeUndefined();

    // Should still display nicely with normalized letters
    const display = formatTypeSchemeForDisplay(s);
    expect(display).toBe("a -> a");
  });

  test("external declaration preserves param names", () => {
    const source = `
@external "js" "console.log"
log : a -> Unit
`;
    const program = parseTest(source);
    const result = analyze(program, {
      fileContext: { filePath: "Test", srcDir: "" },
    });

    const s = result.typeSchemes["log"]!;
    expect(s).toBeDefined();
    expect(s.paramNames).toBeDefined();

    const names = Array.from(s.paramNames!.values());
    expect(names).toContain("a");

    expect(formatTypeSchemeForDisplay(s)).toBe("a -> Unit");
  });

  test("constructor types preserve param names from ADT declaration", () => {
    const source = `
type Box a
  = Box a
`;
    const program = parseTest(source);
    const result = analyze(program, {
      fileContext: { filePath: "Test", srcDir: "" },
    });

    const s = result.constructorTypes["Box"]!;
    expect(s).toBeDefined();
    expect(s.paramNames).toBeDefined();

    const names = Array.from(s.paramNames!.values());
    expect(names).toContain("a");

    expect(formatTypeSchemeForDisplay(s)).toBe("a -> Box a");
  });

  test("multi-param ADT constructor preserves all param names", () => {
    const source = `
type Either a b
  = Left a
  | Right b
`;
    const program = parseTest(source);
    const result = analyze(program, {
      fileContext: { filePath: "Test", srcDir: "" },
    });

    const left = result.constructorTypes["Left"]!;
    expect(left).toBeDefined();
    expect(left.paramNames).toBeDefined();

    const leftNames = Array.from(left.paramNames!.values());
    expect(leftNames).toContain("a");
    expect(leftNames).toContain("b");

    const right = result.constructorTypes["Right"]!;
    const display = formatTypeSchemeForDisplay(right);
    expect(display).toBe("b -> Either a b");
  });
});
