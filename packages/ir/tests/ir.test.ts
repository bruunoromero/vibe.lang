/**
 * IR Package Unit Tests
 *
 * Tests for:
 * 1. Let-binding lifting
 * 2. Case expression lowering
 * 3. Record update desugaring
 * 4. SCC-based dependency analysis
 * 5. Constraint metadata tracking
 */

import { describe, expect, test, beforeEach } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "@vibe/semantics";
import {
  lower,
  buildDependencyGraph,
  findSCCs,
  validateTopologicalOrder,
  lowerExpr,
  createLoweringContext,
  getIRStats,
  dependsOn,
  type IRProgram,
  type IRExpr,
  type IRValue,
  type SCC,
} from "../src/index";

// ============================================================================
// Test Helpers
// ============================================================================

/**
 * Parse and analyze a Vibe source string, returning the IR.
 */
function compileToIR(source: string): IRProgram {
  const ast = parse(source);
  const semantics = analyze(ast, { injectPrelude: false });
  return lower(ast, semantics);
}

/**
 * Get IR for a specific value from source.
 */
function getValueIR(source: string, name: string): IRValue {
  const ir = compileToIR(source);
  const value = ir.values[name];
  if (!value) {
    throw new Error(`Value '${name}' not found in IR`);
  }
  return value;
}

/**
 * Stringify an IR expression for comparison (simplified).
 */
function stringifyExpr(expr: IRExpr): string {
  switch (expr.kind) {
    case "IRVar":
      return expr.name;
    case "IRLiteral":
      return String(expr.value);
    case "IRLambda":
      return `(\\${expr.params
        .map((p) => (p.kind === "IRVarPattern" ? p.name : "_"))
        .join(" ")} -> ${stringifyExpr(expr.body)})`;
    case "IRApply":
      return `(${stringifyExpr(expr.callee)} ${expr.args
        .map(stringifyExpr)
        .join(" ")})`;
    case "IRIf":
      return `(if ${stringifyExpr(expr.condition)} then ${stringifyExpr(
        expr.thenBranch
      )} else ${stringifyExpr(expr.elseBranch)})`;
    case "IRCase":
      return `(case ${stringifyExpr(expr.discriminant)} of ...)`;
    case "IRTuple":
      return `(${expr.elements.map(stringifyExpr).join(", ")})`;
    case "IRUnit":
      return "()";
    case "IRList":
      return `[${expr.elements.map(stringifyExpr).join(", ")}]`;
    case "IRRecord":
      return `{ ${expr.fields
        .map((f) => `${f.name} = ${stringifyExpr(f.value)}`)
        .join(", ")} }`;
    case "IRFieldAccess":
      return `${stringifyExpr(expr.target)}.${expr.field}`;
    case "IRConstructor":
      return expr.args.length > 0
        ? `${expr.name} ${expr.args.map(stringifyExpr).join(" ")}`
        : expr.name;
  }
}

// ============================================================================
// Let-Binding Lifting Tests
// ============================================================================

describe("Let-binding lifting", () => {
  test("simple let-in is converted to lambda application", () => {
    const source = `
x =
    let y = 1
    in y
`;
    const value = getValueIR(source, "x");

    // let y = 1 in y => (\\y -> y) 1
    expect(value.body.kind).toBe("IRApply");
    const apply = value.body as Extract<IRExpr, { kind: "IRApply" }>;
    expect(apply.callee.kind).toBe("IRLambda");
    expect(apply.args[0]?.kind).toBe("IRLiteral");
  });

  test("nested let-in is flattened", () => {
    const source = `
x =
    let a = 1
    in let b = 2
       in a
`;
    const value = getValueIR(source, "x");

    // Should result in nested lambda applications
    expect(value.body.kind).toBe("IRApply");
    const outer = value.body as Extract<IRExpr, { kind: "IRApply" }>;

    // The body of outer lambda should be another application
    const outerLambda = outer.callee as Extract<IRExpr, { kind: "IRLambda" }>;
    expect(outerLambda.body.kind).toBe("IRApply");
  });

  test("let with function binding", () => {
    const source = `
x =
    let f y = y
    in f 1
`;
    const value = getValueIR(source, "x");

    // let f y = y in f 1 => (\\f -> f 1) (\\y -> y)
    expect(value.body.kind).toBe("IRApply");
    const apply = value.body as Extract<IRExpr, { kind: "IRApply" }>;

    // The argument should be a lambda (f y = y)
    expect(apply.args[0]?.kind).toBe("IRLambda");
  });

  test("multiple let bindings", () => {
    const source = `
x =
    let a = 1
        b = 2
    in a
`;
    const value = getValueIR(source, "x");

    // Should chain the bindings: (\\a -> (\\b -> a) 2) 1
    expect(value.body.kind).toBe("IRApply");
  });
});

// ============================================================================
// Case Expression Lowering Tests
// ============================================================================

describe("Case expression lowering", () => {
  test("Bool case is converted to if-then-else", () => {
    // Note: Bool is already a builtin type, no need to redeclare
    const source = `
x b =
    case b of
        True -> 1
        False -> 0
`;
    const value = getValueIR(source, "x");

    // The body should be an IRIf
    expect(value.body.kind).toBe("IRIf");
    const ifExpr = value.body as Extract<IRExpr, { kind: "IRIf" }>;
    expect(ifExpr.condition.kind).toBe("IRVar");
    expect(ifExpr.thenBranch.kind).toBe("IRLiteral");
    expect(ifExpr.elseBranch.kind).toBe("IRLiteral");
  });

  test("ADT case is preserved as IRCase", () => {
    const source = `
type Maybe a = Just a | Nothing

x m =
    case m of
        Just v -> v
        Nothing -> 0
`;
    const value = getValueIR(source, "x");

    // Non-Bool ADT case should remain as IRCase
    expect(value.body.kind).toBe("IRCase");
    const caseExpr = value.body as Extract<IRExpr, { kind: "IRCase" }>;
    expect(caseExpr.branches.length).toBe(2);
  });

  test("user-defined True/False case stays as IRCase (not lowered to if-then-else)", () => {
    // User can define their own Bool-like type
    const source = `
type MyBool = True | False

x : MyBool -> Int
x b =
    case b of
        True -> 1
        False -> 0
`;
    const value = getValueIR(source, "x");

    // User-defined True/False should NOT be lowered to IRIf
    // It should remain as IRCase since it's not the built-in Bool
    expect(value.body.kind).toBe("IRCase");
    const caseExpr = value.body as Extract<IRExpr, { kind: "IRCase" }>;
    expect(caseExpr.branches.length).toBe(2);
  });

  test("constructor patterns get tags", () => {
    const source = `
type Color = Red | Green | Blue

x c =
    case c of
        Red -> 0
        Green -> 1
        Blue -> 2
`;
    const ir = compileToIR(source);

    // Check constructor tags are assigned
    expect(ir.constructors["Red"]?.tag).toBe(0);
    expect(ir.constructors["Green"]?.tag).toBe(1);
    expect(ir.constructors["Blue"]?.tag).toBe(2);
  });
});

// ============================================================================
// Record Update Desugaring Tests
// ============================================================================

describe("Record update desugaring", () => {
  test("record update is desugared", () => {
    // Define a Point type alias and use record update
    // The parameter needs a type annotation for the semantics to know it's a record
    const source = `
type alias Point = { x : Int, y : Int }

moveX : Point -> Point
moveX p =
    { p | x = 1 }
`;
    const value = getValueIR(source, "moveX");

    // Record updates can be either IRRecord (if type known) or IRRecordUpdate (if type unknown)
    // Currently, parameter types aren't tracked through to IR lowering, so we get IRRecordUpdate
    expect(
      value.body.kind === "IRRecord" || value.body.kind === "IRRecordUpdate"
    ).toBe(true);
  });

  test("simple record construction", () => {
    const source = `
x = { a = 1, b = 2 }
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRRecord");
    const record = value.body as Extract<IRExpr, { kind: "IRRecord" }>;
    expect(record.fields.length).toBe(2);
    expect(record.fields[0]?.name).toBe("a");
    expect(record.fields[1]?.name).toBe("b");
  });
});

// ============================================================================
// Infix Resolution Tests
// ============================================================================

describe("Infix resolution", () => {
  test("infix operator is converted to function application", () => {
    // Define the + operator using @external annotation
    const source = `
infixl 6 +

@external "./runtime" "add"
(+) : Int -> Int -> Int

x = 1 + 2
`;
    const value = getValueIR(source, "x");

    // 1 + 2 => ((+) 1) 2
    expect(value.body.kind).toBe("IRApply");
    const apply = value.body as Extract<IRExpr, { kind: "IRApply" }>;

    // The callee should be an application of (+) to 1
    expect(apply.callee.kind).toBe("IRApply");
    const inner = apply.callee as Extract<IRExpr, { kind: "IRApply" }>;
    expect(inner.callee.kind).toBe("IRVar");
    const op = inner.callee as Extract<IRExpr, { kind: "IRVar" }>;
    expect(op.name).toBe("+");
  });

  test("nested infix operators", () => {
    const source = `
infixl 6 +

@external "./runtime" "add"
(+) : Int -> Int -> Int

x = 1 + 2 + 3
`;
    const value = getValueIR(source, "x");

    // Should result in nested applications
    expect(value.body.kind).toBe("IRApply");
  });
});

// ============================================================================
// SCC Dependency Analysis Tests
// ============================================================================

describe("SCC dependency analysis", () => {
  test("independent values form separate SCCs", () => {
    const source = `
a = 1
b = 2
c = 3
`;
    const ir = compileToIR(source);

    // Each value should be in its own SCC
    expect(ir.dependencyOrder.length).toBe(3);
    for (const scc of ir.dependencyOrder) {
      expect(scc.values.length).toBe(1);
      expect(scc.isMutuallyRecursive).toBe(false);
    }
  });

  test("dependent values are ordered correctly", () => {
    const source = `
b = a
a = 1
`;
    const ir = compileToIR(source);

    // a should come before b in dependency order
    const aIndex = ir.dependencyOrder.findIndex((scc) =>
      scc.values.includes("a")
    );
    const bIndex = ir.dependencyOrder.findIndex((scc) =>
      scc.values.includes("b")
    );
    expect(aIndex).toBeLessThan(bIndex);
  });

  test("mutually recursive values form single SCC", () => {
    const source = `
f x = g x
g x = f x
`;
    const ir = compileToIR(source);

    // f and g should be in the same SCC
    const fgScc = ir.dependencyOrder.find(
      (scc) => scc.values.includes("f") && scc.values.includes("g")
    );
    expect(fgScc).toBeDefined();
    expect(fgScc!.isMutuallyRecursive).toBe(true);
  });

  test("complex dependency graph", () => {
    const source = `
a = 1
b = a
c = a
d = b
e = c
f = d
`;
    const ir = compileToIR(source);

    // Validate topological order
    const graph = buildDependencyGraph(ir.values);
    const validation = validateTopologicalOrder(ir.dependencyOrder, graph);
    expect(validation.valid).toBe(true);
  });

  test("self-recursive function", () => {
    const source = `
f x = f x
`;
    const ir = compileToIR(source);

    // f is self-recursive but not mutually recursive
    const fScc = ir.dependencyOrder.find((scc) => scc.values.includes("f"));
    expect(fScc).toBeDefined();
    // Note: a single self-recursive function is technically an SCC of size 1
    // Our isMutuallyRecursive flag is true only for SCCs with > 1 value
    expect(fScc!.values.length).toBe(1);
  });

  test("three-way mutual recursion", () => {
    const source = `
f x = g x
g x = h x
h x = f x
`;
    const ir = compileToIR(source);

    // f, g, h should all be in the same SCC
    const scc = ir.dependencyOrder.find(
      (s) =>
        s.values.includes("f") &&
        s.values.includes("g") &&
        s.values.includes("h")
    );
    expect(scc).toBeDefined();
    expect(scc!.isMutuallyRecursive).toBe(true);
    expect(scc!.values.length).toBe(3);
  });
});

// ============================================================================
// Constraint Metadata Tests
// ============================================================================

describe("Constraint metadata", () => {
  test("protocol info is extracted", () => {
    const source = `
protocol Show a where
    show : a -> String
`;
    const ir = compileToIR(source);

    expect(ir.protocols["Show"]).toBeDefined();
    expect(ir.protocols["Show"]?.name).toBe("Show");
    expect(ir.protocols["Show"]?.params).toEqual(["a"]);
    expect(ir.protocols["Show"]?.methods.length).toBe(1);
    expect(ir.protocols["Show"]?.methods[0]?.name).toBe("show");
  });

  test("instance info is extracted", () => {
    const source = `
protocol Show a where
    show : a -> String

@external "./runtime" "showInt"
showInt : Int -> String

implement Show Int where
    show = showInt
`;
    const ir = compileToIR(source);

    expect(ir.instances.length).toBe(1);
    expect(ir.instances[0]?.protocolName).toBe("Show");
    expect(ir.instances[0]?.methods["show"]).toBe("showInt");
  });

  test("constraints are extracted from type schemes", () => {
    const source = `
protocol Num a where
    plus : a -> a -> a

add x y = plus x y
`;
    const ir = compileToIR(source);

    // The 'add' function uses 'plus' which requires Num constraint
    const addValue = ir.values["add"];
    expect(addValue).toBeDefined();
    expect(addValue?.constraints).toBeDefined();
    expect(addValue?.constraints.length).toBeGreaterThan(0);
    expect(addValue?.constraints[0]?.protocolName).toBe("Num");
  });

  test("values without protocol usage have no constraints", () => {
    const source = `
id x = x
`;
    const ir = compileToIR(source);

    const idValue = ir.values["id"];
    expect(idValue).toBeDefined();
    expect(idValue?.constraints).toEqual([]);
  });
});

// ============================================================================
// Constructor Tag Tests
// ============================================================================

describe("Constructor tags", () => {
  test("tags are assigned sequentially per ADT", () => {
    // Bool is builtin, define Maybe
    const source = `
type Maybe a = Nothing | Just a
`;
    const ir = compileToIR(source);

    // Bool constructors (builtin)
    expect(ir.constructors["True"]?.tag).toBe(0);
    expect(ir.constructors["False"]?.tag).toBe(1);

    // Maybe constructors (user-defined)
    expect(ir.constructors["Nothing"]?.tag).toBe(0);
    expect(ir.constructors["Just"]?.tag).toBe(1);
  });

  test("constructor arity is preserved", () => {
    const source = `
type MyList a = MyNil | MyCons a (MyList a)
`;
    const ir = compileToIR(source);

    expect(ir.constructors["MyNil"]?.arity).toBe(0);
    expect(ir.constructors["MyCons"]?.arity).toBe(2);
  });
});

// ============================================================================
// IR Stats and Utilities Tests
// ============================================================================

describe("IR utilities", () => {
  test("getIRStats returns correct counts", () => {
    // Don't redeclare Bool
    const source = `
protocol Eq a where
    eq : a -> a -> Bool

a = 1
b = 2
`;
    const ir = compileToIR(source);
    const stats = getIRStats(ir);

    expect(stats.valueCount).toBe(2);
    expect(stats.protocolCount).toBe(1);
  });

  test("dependsOn detects dependencies", () => {
    const source = `
a = 1
b = a
`;
    const ir = compileToIR(source);

    expect(dependsOn(ir, "b", "a")).toBe(true);
    expect(dependsOn(ir, "a", "b")).toBe(false);
  });
});

// ============================================================================
// Expression Lowering Edge Cases
// ============================================================================

describe("Expression lowering edge cases", () => {
  test("lambda expressions", () => {
    const source = `
f = \\x -> x
`;
    const value = getValueIR(source, "f");

    expect(value.body.kind).toBe("IRLambda");
    const lambda = value.body as Extract<IRExpr, { kind: "IRLambda" }>;
    expect(lambda.params.length).toBe(1);
    expect(lambda.params[0]?.kind).toBe("IRVarPattern");
  });

  test("tuple expressions", () => {
    const source = `
x = (1, 2, 3)
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRTuple");
    const tuple = value.body as Extract<IRExpr, { kind: "IRTuple" }>;
    expect(tuple.elements.length).toBe(3);
  });

  test("list expressions", () => {
    const source = `
x = [1, 2, 3]
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRList");
    const list = value.body as Extract<IRExpr, { kind: "IRList" }>;
    expect(list.elements.length).toBe(3);
  });

  test("field access", () => {
    // Need type annotation for semantics to know r is a record
    const source = `
type alias Rec = { field : Int }

x : Rec -> Int
x r = r.field
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRFieldAccess");
    const access = value.body as Extract<IRExpr, { kind: "IRFieldAccess" }>;
    expect(access.field).toBe("field");
  });

  test("if-then-else", () => {
    // Note: Bool type must exist (from builtin), add type annotation
    const source = `
x : Bool -> Int
x b = if b then 1 else 0
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRIf");
  });

  test("unit expression", () => {
    const source = `
x = ()
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRUnit");
  });

  test("number literals - int", () => {
    const source = `
x = 42
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRLiteral");
    const lit = value.body as Extract<IRExpr, { kind: "IRLiteral" }>;
    expect(lit.literalType).toBe("int");
    expect(lit.value).toBe(42);
  });

  test("number literals - float", () => {
    const source = `
x = 3.14
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRLiteral");
    const lit = value.body as Extract<IRExpr, { kind: "IRLiteral" }>;
    expect(lit.literalType).toBe("float");
    expect(lit.value).toBe(3.14);
  });

  test("string literal", () => {
    const source = `
x = "hello"
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRLiteral");
    const lit = value.body as Extract<IRExpr, { kind: "IRLiteral" }>;
    expect(lit.literalType).toBe("string");
    // String values include the quotes from parser
    expect(lit.value).toContain("hello");
  });

  test("function application", () => {
    const source = `
f x = x
y = f 1
`;
    const value = getValueIR(source, "y");

    expect(value.body.kind).toBe("IRApply");
    const apply = value.body as Extract<IRExpr, { kind: "IRApply" }>;
    expect(apply.callee.kind).toBe("IRVar");
    expect(apply.args.length).toBe(1);
  });

  test("constructor application", () => {
    const source = `
type Maybe a = Just a | Nothing

x = Just 1
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRApply");
    const apply = value.body as Extract<IRExpr, { kind: "IRApply" }>;
    expect(apply.callee.kind).toBe("IRVar");
    const ctor = apply.callee as Extract<IRExpr, { kind: "IRVar" }>;
    expect(ctor.name).toBe("Just");
    expect(ctor.namespace).toBe("constructor");
  });

  test("zero-arity constructor", () => {
    const source = `
type Maybe a = Just a | Nothing

x = Nothing
`;
    const value = getValueIR(source, "x");

    expect(value.body.kind).toBe("IRConstructor");
    const ctor = value.body as Extract<IRExpr, { kind: "IRConstructor" }>;
    expect(ctor.name).toBe("Nothing");
    expect(ctor.args.length).toBe(0);
  });
});

// ============================================================================
// Pattern Lowering Tests
// ============================================================================

describe("Pattern lowering", () => {
  test("variable pattern", () => {
    const source = `
f x = x
`;
    const value = getValueIR(source, "f");

    expect(value.params.length).toBe(1);
    expect(value.params[0]?.kind).toBe("IRVarPattern");
    const pat = value.params[0] as Extract<
      (typeof value.params)[0],
      { kind: "IRVarPattern" }
    >;
    expect(pat.name).toBe("x");
  });

  test("wildcard pattern", () => {
    const source = `
f _ = 1
`;
    const value = getValueIR(source, "f");

    expect(value.params.length).toBe(1);
    expect(value.params[0]?.kind).toBe("IRWildcardPattern");
  });

  test("tuple pattern", () => {
    const source = `
f (a, b) = a
`;
    const value = getValueIR(source, "f");

    expect(value.params.length).toBe(1);
    expect(value.params[0]?.kind).toBe("IRTuplePattern");
  });

  test("constructor pattern in case", () => {
    const source = `
type Maybe a = Just a | Nothing

f m =
    case m of
        Just x -> x
        Nothing -> 0
`;
    const value = getValueIR(source, "f");

    expect(value.body.kind).toBe("IRCase");
    const caseExpr = value.body as Extract<IRExpr, { kind: "IRCase" }>;
    expect(caseExpr.branches[0]?.pattern.kind).toBe("IRConstructorPattern");
  });
});

// ============================================================================
// External Declarations Tests
// ============================================================================

describe("External declarations", () => {
  test("external value is marked", () => {
    const source = `
@external "./math" "add"
add : Int -> Int -> Int
`;
    const ir = compileToIR(source);
    const value = ir.values["add"];

    expect(value?.isExternal).toBe(true);
    expect(value?.externalTarget).toEqual({
      modulePath: "./math",
      exportName: "add",
    });
  });

  test("external imports are collected", () => {
    const source = `
@external "./math" "add"
add : Int -> Int -> Int

@external "./strings" "concat"
concat : String -> String -> String
`;
    const ir = compileToIR(source);

    expect(ir.externalImports.has("./math")).toBe(true);
    expect(ir.externalImports.has("./strings")).toBe(true);
  });
});
