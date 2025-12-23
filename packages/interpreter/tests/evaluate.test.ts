import { describe, expect, test } from "bun:test";
import { evaluate, createRootEnvironment } from "../src/evaluator";
import { parseSource } from "@vibe/parser";
import type { ExpressionNode } from "@vibe/syntax";

const evalSource = async (source: string) => {
  const parseResult = await parseSource(source);
  if (!parseResult.ok) {
    return { ok: false, diagnostics: parseResult.diagnostics };
  }
  const env = createRootEnvironment();
  const expr = parseResult.program.body[0];
  if (!expr) {
    throw new Error("No expression to evaluate");
  }
  return evaluate(expr, env);
};

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
    const result = await evalSource("(add* 2 3)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 5 });
  });

  test("subtraction", async () => {
    const result = await evalSource("(sub* 8 3)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 5 });
  });

  test("multiplication", async () => {
    const result = await evalSource("(mul* 2 3)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 6 });
  });

  test("division", async () => {
    const result = await evalSource("(div* 12 3)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 4 });
  });

  test("modulo", async () => {
    const result = await evalSource("(mod* 10 3)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("nested arithmetic", async () => {
    const result = await evalSource("(add* (mul* 2 3) (sub* 10 4))");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 12 });
  });
});

describe("Interpreter - Comparison", () => {
  test("equality - numbers", async () => {
    const result = await evalSource("(eq* 42 42)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("inequality", async () => {
    const result = await evalSource("(eq* 1 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: false });
  });

  test("less than", async () => {
    const result = await evalSource("(lt* 1 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("greater than", async () => {
    const result = await evalSource("(gt* 3 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("less than or equal", async () => {
    const result = await evalSource("(lte* 2 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("greater than or equal", async () => {
    const result = await evalSource("(gte* 3 2)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });
});

describe("Interpreter - Special Forms", () => {
  test("def creates binding", async () => {
    const parseResult = await parseSource("(def x 42) x");
    const env = createRootEnvironment();
    const def = parseResult.program.body[0];
    const ref = parseResult.program.body[1];
    evaluate(def!, env);
    const result = evaluate(ref!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });

  test("let creates local scope", async () => {
    const result = await evalSource("(let [x 10 y 20] (add* x y))");
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
    const parseResult = await parseSource(
      "(do (def x 1) (def y 2) (add* x y))"
    );
    const env = createRootEnvironment();
    const result = evaluate(parseResult.program.body[0]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 3 });
  });
});

describe("Interpreter - Functions", () => {
  test("define and call function", async () => {
    const parseResult = await parseSource(
      "(def add (fn [x y] (add* x y))) (add 3 4)"
    );
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    const result = evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 7 });
  });

  test("function with single parameter", async () => {
    const parseResult = await parseSource(
      "(def double (fn [x] (mul* x 2))) (double 5)"
    );
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    const result = evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 10 });
  });

  test("function closes over environment", async () => {
    const parseResult = await parseSource(
      "(def make-adder (fn [x] (fn [y] (add* x y)))) (def add5 (make-adder 5)) (add5 3)"
    );
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    evaluate(parseResult.program.body[1]!, env);
    const result = evaluate(parseResult.program.body[2]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 8 });
  });

  test("variadic function with rest parameter", async () => {
    const parseResult = await parseSource(
      "(def sum (fn [& nums] (first nums))) (sum 1 2 3)"
    );
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    const result = evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("function with mixed and rest parameters", async () => {
    const parseResult = await parseSource(
      "(def f (fn [x & rest] x)) (f 42 1 2 3)"
    );
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    const result = evaluate(parseResult.program.body[1]!, env);
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
    const result = await evalSource("[(add* 1 2) (mul* 3 4)]");
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("vector");
    const vec = result.value as any;
    expect(vec.elements[0]).toEqual({ kind: "number", value: 3 });
    expect(vec.elements[1]).toEqual({ kind: "number", value: 12 });
  });
});

describe("Interpreter - Sequence Operations", () => {
  test("first of list", async () => {
    const result = await evalSource("(first (quote (1 2 3)))");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 1 });
  });

  test("rest of list", async () => {
    const result = await evalSource("(rest (quote (1 2 3)))");
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("list");
    expect((result.value as any).elements).toHaveLength(2);
  });

  test("count of vector", async () => {
    const result = await evalSource("(count [1 2 3 4])");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 4 });
  });

  test("cons onto list", async () => {
    const result = await evalSource("(cons 0 (quote (1 2 3)))");
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("list");
    const list = result.value as any;
    expect(list.elements).toHaveLength(4);
    expect(list.elements[0]).toEqual({ kind: "number", value: 0 });
  });

  test("take from list", async () => {
    const result = await evalSource("(take 2 (quote (1 2 3 4)))");
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements).toHaveLength(2);
  });

  test("drop from list", async () => {
    const result = await evalSource("(drop 2 (quote (1 2 3 4)))");
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements).toHaveLength(2);
    expect(list.elements[0]).toEqual({ kind: "number", value: 3 });
  });

  test("reverse list", async () => {
    const result = await evalSource("(reverse (quote (1 2 3)))");
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements[0]).toEqual({ kind: "number", value: 3 });
    expect(list.elements[2]).toEqual({ kind: "number", value: 1 });
  });

  test("concat lists", async () => {
    const result = await evalSource(
      "(concat (quote (1 2)) (quote (3 4)) (quote (5)))"
    );
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.elements).toHaveLength(5);
  });
});

describe("Interpreter - Type Predicates", () => {
  test("type returns :number", async () => {
    const result = await evalSource("(type 42)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":number" });
  });

  test("type returns :string", async () => {
    const result = await evalSource('(type "hello")');
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":string" });
  });

  test("type returns :nil", async () => {
    const result = await evalSource("(type nil)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":nil" });
  });

  test("type returns :vector", async () => {
    const result = await evalSource("(type [1 2 3])");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: ":vector" });
  });
});

describe("Interpreter - String Operations", () => {
  test("str concatenates values", async () => {
    const result = await evalSource('(str "hello" " " "world")');
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "string", value: "hello world" });
  });

  test("str with numbers", async () => {
    const result = await evalSource('(str "Count: " 42)');
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "string", value: "Count: 42" });
  });
});

describe("Interpreter - Utility", () => {
  test("gensym generates unique symbols", async () => {
    const parseResult = await parseSource("(gensym) (gensym)");
    const env = createRootEnvironment();
    const result1 = evaluate(parseResult.program.body[0]!, env);
    const result2 = evaluate(parseResult.program.body[1]!, env);
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

describe("Interpreter - Error Handling", () => {
  test("undefined variable", async () => {
    const result = await evalSource("undefined-var");
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_UNDEFINED_VARIABLE");
  });

  test("arity mismatch - too few args", async () => {
    const parseResult = await parseSource(
      "(def f (fn [x y] (add* x y))) (f 1)"
    );
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    const result = evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_ARITY_MISMATCH");
  });

  test("arity mismatch - too many args", async () => {
    const parseResult = await parseSource("(def f (fn [x] (* x 2))) (f 1 2 3)");
    const env = createRootEnvironment();
    evaluate(parseResult.program.body[0]!, env);
    const result = evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_ARITY_MISMATCH");
  });

  test("call non-function", async () => {
    const result = await evalSource("(42 1 2)");
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_NOT_CALLABLE");
  });
});
