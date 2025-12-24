import { describe, expect, test } from "bun:test";
import { fileURLToPath } from "node:url";
import { evaluate, createRootEnvironment } from "../src/evaluator";
import { parseSource } from "@vibe/parser";
import type { ExpressionNode } from "@vibe/syntax";

const evalSource = async (source: string, withRuntime = false) => {
  // Prepend runtime import if needed
  const fullSource = withRuntime
    ? `(external runtime "@vibe/runtime")\n${source}`
    : source;

  const parseResult = await parseSource(fullSource);
  if (!parseResult.ok) {
    return { ok: false, diagnostics: parseResult.diagnostics };
  }
  const env = createRootEnvironment();

  // If withRuntime, evaluate the external import first
  if (withRuntime) {
    const importResult = await evaluate(parseResult.program.body[0]!, env);
    if (!importResult.ok) {
      return importResult;
    }
  }

  // Evaluate the actual expression (offset by 1 if we imported runtime)
  const expr = parseResult.program.body[withRuntime ? 1 : 0];
  if (!expr) {
    throw new Error("No expression to evaluate");
  }
  return await evaluate(expr, env);
};

// Helper for evaluating multiple statements and returning the last result
const evalMulti = async (source: string, withRuntime = false) => {
  const fullSource = withRuntime
    ? `(external runtime "@vibe/runtime")\n${source}`
    : source;

  const parseResult = await parseSource(fullSource);
  if (!parseResult.ok) {
    return { ok: false, diagnostics: parseResult.diagnostics };
  }
  const env = createRootEnvironment();

  let startIdx = 0;
  // If withRuntime, evaluate the external import first
  if (withRuntime) {
    const importResult = await evaluate(parseResult.program.body[0]!, env);
    if (!importResult.ok) {
      return importResult;
    }
    startIdx = 1;
  }

  // Evaluate all statements, return the last result
  let lastResult;
  for (let i = startIdx; i < parseResult.program.body.length; i++) {
    const expr = parseResult.program.body[i];
    if (expr) {
      lastResult = await evaluate(expr, env);
      if (!lastResult.ok) {
        return lastResult;
      }
    }
  }

  return lastResult || { ok: false, diagnostics: [] };
};

const IMPORT_FIXTURE = fileURLToPath(
  new URL("./fixtures/import-target.lang", import.meta.url)
);

describe("Interpreter - Literals", () => {
  test("evaluate number", async () => {
    const result = await evalSource("42");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });

  test("evaluate string", async () => {
    const result = await evalSource('"hello"');
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "string", value: "hello" });
  });

  test("evaluate boolean true", async () => {
    const result = await evalSource("true");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("evaluate boolean false", async () => {
    const result = await evalSource("false");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: false });
  });

  test("evaluate nil", async () => {
    const result = await evalSource("nil");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "nil" });
  });
});

describe("Interpreter - Arithmetic", () => {
  test("addition", async () => {
    const result = await evalSource("(runtime/add* 2 3)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 5 });
  });

  test("subtraction", async () => {
    const result = await evalSource("(runtime/sub* 8 3)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 5 });
  });

  test("multiplication", async () => {
    const result = await evalSource("(runtime/mul* 2 3)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 6 });
  });

  test("division", async () => {
    const result = await evalSource("(runtime/div* 12 3)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 4 });
  });

  test("modulo", async () => {
    const result = await evalSource("(runtime/mod* 10 3)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("nested arithmetic", async () => {
    const result = await evalSource(
      "(runtime/add* (runtime/mul* 2 3) (runtime/sub* 10 4))",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 12 });
  });
});

describe("Interpreter - Comparison", () => {
  test("equality - numbers", async () => {
    const result = await evalSource("(runtime/eq* 42 42)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("inequality", async () => {
    const result = await evalSource("(runtime/eq* 1 2)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: false });
  });

  test("less than", async () => {
    const result = await evalSource("(runtime/lt* 1 2)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("greater than", async () => {
    const result = await evalSource("(runtime/gt* 3 2)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("less than or equal", async () => {
    const result = await evalSource("(runtime/lte* 2 2)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("greater than or equal", async () => {
    const result = await evalSource("(runtime/gte* 3 2)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });
});

describe("Interpreter - Runtime Symbols", () => {
  test("runtime/symbol returns tagged symbol values", async () => {
    const result = await evalSource('(runtime/symbol "foo")', true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: "foo" });
  });

  test("runtime/symbol? distinguishes strings", async () => {
    const result = await evalSource(
      `(let [sym (runtime/symbol "foo")]
         (runtime/symbol? sym))`,
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("runtime/eq* compares symbols by name", async () => {
    const result = await evalSource(
      '(runtime/eq* (runtime/symbol "foo") (runtime/symbol "foo"))',
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("runtime/type returns keyword-style symbols", async () => {
    const result = await evalSource(
      '(runtime/type (runtime/symbol "alpha"))',
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":symbol" });
  });
});

describe("Interpreter - Special Forms", () => {
  test("def creates binding", async () => {
    const parseResult = await parseSource("(def x 42) x");
    const env = createRootEnvironment();
    const def = parseResult.program.body[0];
    const ref = parseResult.program.body[1];
    await evaluate(def!, env);
    const result = await evaluate(ref!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });

  test("let creates local scope", async () => {
    const result = await evalSource(
      "(let [x 10 y 20] (runtime/add* x y))",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 30 });
  });

  test("if - truthy branch", async () => {
    const result = await evalSource("(if true 1 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("if - falsy branch", async () => {
    const result = await evalSource("(if false 1 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 2 });
  });

  test("if - nil is falsy", async () => {
    const result = await evalSource("(if nil 1 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 2 });
  });

  test("quote - symbol", async () => {
    const result = await evalSource("(quote x)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: "x" });
  });

  test("quote - list", async () => {
    const result = await evalSource("(quote (1 2 3))");
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("list");
  });

  test("do evaluates multiple forms", async () => {
    const result = await evalSource(
      "(do (def x 1) (def y 2) (runtime/add* x y))",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 3 });
  });

  test("import flattens module bindings", async () => {
    const pulledResult = await evalMulti(`(import "${IMPORT_FIXTURE}") pulled`);
    console.log("pulledResult", pulledResult);
    expect(pulledResult.ok).toBeTrue();
    expect(pulledResult.value).toEqual({ kind: "number", value: 42 });

    const greetResult = await evalMulti(`(import "${IMPORT_FIXTURE}") (greet)`);
    expect(greetResult.ok).toBeTrue();
    expect(greetResult.value).toEqual({ kind: "string", value: "hello" });
  });
});

describe("Interpreter - Functions", () => {
  test("define and call function", async () => {
    const result = await evalMulti(
      "(def add (fn [x y] (runtime/add* x y))) (add 3 4)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 7 });
  });

  test("function with single parameter", async () => {
    const result = await evalMulti(
      "(def double (fn [x] (runtime/mul* x 2))) (double 5)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 10 });
  });

  test("function closes over environment", async () => {
    const result = await evalMulti(
      "(def make-adder (fn [x] (fn [y] (runtime/add* x y)))) (def add5 (make-adder 5)) (add5 3)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 8 });
  });

  test("variadic function with rest parameter", async () => {
    const result = await evalMulti(
      "(def sum (fn [& nums] (runtime/first nums))) (sum 1 2 3)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("function with mixed and rest parameters", async () => {
    const parseResult = await parseSource(
      "(def f (fn [x & rest] x)) (f 42 1 2 3)"
    );
    const env = createRootEnvironment();
    await evaluate(parseResult.program.body[0]!, env);
    const result = await evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });
});

describe("Interpreter - Collections", () => {
  test("evaluate vector", async () => {
    const result = await evalSource("[1 2 3]");
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("vector");
    expect((result.value as any).elements).toHaveLength(3);
  });

  test("evaluate nested vectors", async () => {
    const result = await evalSource("[[1 2] [3 4]]");
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("vector");
  });

  test("vector with expressions", async () => {
    const result = await evalSource(
      "[(runtime/add* 1 2) (runtime/mul* 3 4)]",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("vector");
    const vec = result.value as any;
    expect(vec.elements[0]).toEqual({ kind: "number", value: 3 });
    expect(vec.elements[1]).toEqual({ kind: "number", value: 12 });
  });
});

describe("Interpreter - Sequence Operations", () => {
  test("first of list", async () => {
    const result = await evalSource("(runtime/first (quote (1 2 3)))", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("rest of list", async () => {
    const result = await evalSource("(runtime/rest (quote (1 2 3)))", true);
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("list");
    expect((result.value as any).elements).toHaveLength(2);
  });

  test("count of vector", async () => {
    const result = await evalSource("(runtime/count [1 2 3 4])", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 4 });
  });

  test("cons onto list", async () => {
    const result = await evalSource("(runtime/cons 0 (quote (1 2 3)))", true);
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("list");
    const list = result.value as any;
    expect(list.elements).toHaveLength(4);
    expect(list.elements[0]).toEqual({ kind: "number", value: 0 });
  });

  test("take from list", async () => {
    const result = await evalSource("(runtime/take 2 (quote (1 2 3 4)))", true);
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements).toHaveLength(2);
  });

  test("drop from list", async () => {
    const result = await evalSource("(runtime/drop 2 (quote (1 2 3 4)))", true);
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements).toHaveLength(2);
    expect(list.elements[0]).toEqual({ kind: "number", value: 3 });
  });

  test("reverse list", async () => {
    const result = await evalSource("(runtime/reverse (quote (1 2 3)))", true);
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements[0]).toEqual({ kind: "number", value: 3 });
    expect(list.elements[2]).toEqual({ kind: "number", value: 1 });
  });

  test("concat lists", async () => {
    const result = await evalSource(
      "(runtime/concat (quote (1 2)) (quote (3 4)) (quote (5)))",
      true
    );
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements).toHaveLength(5);
  });
});

describe("Interpreter - Type Predicates", () => {
  test("type returns :number", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource("(runtime/type 42)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":number" });
  });

  test.skip("type returns :string", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource('(runtime/type "hello")', true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":string" });
  });

  test.skip("type returns :nil", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource("(runtime/type nil)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":nil" });
  });

  test.skip("type returns :vector", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource("(runtime/type [1 2 3])", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":vector" });
  });
});

describe("Interpreter - String Operations", () => {
  test("str concatenates values", async () => {
    const result = await evalSource('(runtime/str "hello" " " "world")', true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "string", value: "hello world" });
  });

  test("str with numbers", async () => {
    const result = await evalSource('(runtime/str "Count: " 42)', true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "string", value: "Count: 42" });
  });
});

describe("Interpreter - Utility", () => {
  test("gensym generates unique symbols", async () => {
    const parseResult = await parseSource("(gensym) (gensym)");
    const env = createRootEnvironment();
    const context = { callDepth: 0, gensymCounter: { value: 0 } };
    const result1 = await evaluate(parseResult.program.body[0]!, env, context);
    const result2 = await evaluate(parseResult.program.body[1]!, env, context);
    expect(result1.ok).toBeTrue();
    expect(result2.ok).toBeTrue();
    expect(result1.value?.kind).toBe("symbol");
    expect(result2.value?.kind).toBe("symbol");
    expect((result1.value as any).value).not.toBe((result2.value as any).value);
  });

  test("gensym with hint", async () => {
    const result = await evalSource('(gensym "temp")');
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("symbol");
    expect((result.value as any).value).toContain("temp");
  });
});

describe("Interpreter - Module Imports", () => {
  test("import flattens definitions into current namespace", async () => {
    const source = `(import "${IMPORT_FIXTURE}") pulled`;
    const result = await evalMulti(source);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });

  test("import makes functions available directly", async () => {
    const source = `(import "${IMPORT_FIXTURE}") (greet)`;
    const result = await evalMulti(source);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "string", value: "hello" });
  });

  test("import without alias syntax", async () => {
    const parseResult = await parseSource(`(import "${IMPORT_FIXTURE}")`);
    expect(parseResult.ok).toBeTrue();
    const env = createRootEnvironment();
    const importExpr = parseResult.program.body[0];
    const result = await evaluate(importExpr!, env);
    expect(result.ok).toBeTrue();
    
    // Check that the bindings are directly in the environment
    expect(env.bindings.has("pulled")).toBeTrue();
    expect(env.bindings.has("greet")).toBeTrue();
  });

  test("import makes all module definitions available", async () => {
    const source = `
      (import "${IMPORT_FIXTURE}")
      (def result [pulled (greet)])
      result
    `;
    const result = await evalMulti(source);
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("vector");
    const elements = (result.value as any)?.elements;
    expect(elements).toBeDefined();
    expect(elements).toHaveLength(2);
    expect(elements[0]).toEqual({ kind: "number", value: 42 });
    expect(elements[1]).toEqual({ kind: "string", value: "hello" });
  });

  test("import fails with missing file", async () => {
    const source = `(import "/nonexistent/path.lang")`;
    const result = await evalMulti(source);
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_IMPORT_FAILED");
  });
});

describe("Interpreter - Error Handling", () => {
  test("undefined variable", async () => {
    const result = await evalSource("undefined-var");
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_UNDEFINED_VARIABLE");
  });

  test("arity mismatch - too few args", async () => {
    const parseResult = await parseSource(
      "(def f (fn [x y] (runtime/add* x y))) (f 1)"
    );
    const env = createRootEnvironment();
    await evaluate(parseResult.program.body[0]!, env);
    const result = await evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_ARITY_MISMATCH");
  });

  test("arity mismatch - too many args", async () => {
    const parseResult = await parseSource("(def f (fn [x] (* x 2))) (f 1 2 3)");
    const env = createRootEnvironment();
    await evaluate(parseResult.program.body[0]!, env);
    const result = await evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_ARITY_MISMATCH");
  });

  test("call non-function", async () => {
    const result = await evalSource("(42 1 2)");
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_NOT_CALLABLE");
  });
});
