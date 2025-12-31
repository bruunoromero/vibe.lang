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
    const result = await evalSource('(runtime/symbol* "foo")', true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "symbol", value: "foo" });
  });

  test("runtime/symbol? distinguishes strings", async () => {
    const result = await evalSource(
      `(let [sym (runtime/symbol* "foo")]
         (runtime/symbol? sym))`,
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("runtime/eq* compares symbols by name", async () => {
    const result = await evalSource(
      '(runtime/eq* (runtime/symbol* "foo") (runtime/symbol* "foo"))',
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "boolean", value: true });
  });

  test("runtime/type returns keyword-style symbols", async () => {
    const result = await evalSource(
      '(runtime/type (runtime/symbol* "alpha"))',
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "keyword", value: "symbol" });
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

  test("defp creates a private binding", async () => {
    const parseResult = await parseSource("(defp hidden 99) hidden");
    const env = createRootEnvironment();
    const def = parseResult.program.body[0];
    const ref = parseResult.program.body[1];
    await evaluate(def!, env);
    const result = await evaluate(ref!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 99 });
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

  test("quote produces literal structures", async () => {
    const result = await evalSource("(quote (alpha beta))");
    expect(result.ok).toBeTrue();
    expect(result.value).toMatchObject({
      kind: "list",
      elements: [
        { kind: "symbol", value: "alpha" },
        { kind: "symbol", value: "beta" },
      ],
    });
  });

  test("quote evaluates unquote and splicing helpers", async () => {
    const result = await evalMulti(`
      (def xs (quote (1 2)))
      (quote (list (spread (unquote xs)) (unquote (if true 3 4))))
    `);
    expect(result.ok).toBeTrue();
    expect(result.value).toMatchObject({
      kind: "list",
      elements: [
        { kind: "symbol", value: "list" },
        { kind: "number", value: 1 },
        { kind: "number", value: 2 },
        { kind: "number", value: 3 },
      ],
    });
  });

  test("evaluates multiple forms", async () => {
    const result = await evalMulti(
      `(def x 1) (def y 2) (runtime/add* x y)`,
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

  test("try returns body value when no error", async () => {
    const result = await evalSource("(try 42)");
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });

  test("try catches explicit throw", async () => {
    const result = await evalSource('(try (throw "boom") (catch err err))');
    expect(result.ok).toBeTrue();
    const value = result.value as any;
    expect(value.kind).toBe("error");
    expect(value.error).toBeInstanceOf(Error);
    expect(value.error.message).toBe("boom");
  });

  test("try catches runtime errors", async () => {
    const result = await evalSource(
      "(try (runtime/div* 1 0) (catch err 99))",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 99 });
  });

  test("throw without value reports diagnostics", async () => {
    const missing = await evalSource("(throw)");
    expect(missing.ok).toBeFalse();
    expect(
      missing.diagnostics.some((d) => d.code === "INTERP_THROW_REQUIRES_VALUE")
    ).toBeTrue();
  });

  test("throw without catch surfaces unhandled diagnostic", async () => {
    const result = await evalSource('(throw "catastrophe")');
    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "INTERP_THROW_UNHANDLED")
    ).toBeTrue();
  });
});

describe("Interpreter - Functions", () => {
  test("define and call function", async () => {
    const result = await evalMulti(
      "(def add (fn+ ([x y] (runtime/add* x y)))) (add 3 4)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 7 });
  });

  test("function with single parameter", async () => {
    const result = await evalMulti(
      "(def double (fn+ ([x] (runtime/mul* x 2)))) (double 5)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 10 });
  });

  test("function closes over environment", async () => {
    const result = await evalMulti(
      "(def make-adder (fn+ ([x] (fn+ ([y] (runtime/add* x y)))))) (def add5 (make-adder 5)) (add5 3)",
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 8 });
  });

  // test("variadic function with rest parameter", async () => {
  //   const result = await evalMulti(
  //     "(def sum (fn+ [& nums] (runtime/first nums))) (sum 1 2 3)",
  //     true
  //   );
  //   expect(result.ok).toBeTrue();
  //   expect(result.value).toEqual({ kind: "number", value: 1 });
  // });

  test("function with mixed and rest parameters", async () => {
    const parseResult = await parseSource(
      "(def f (fn+ ([x & rest] x))) (f 42 1 2 3)"
    );
    const env = createRootEnvironment();
    await evaluate(parseResult.program.body[0]!, env);
    const result = await evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 42 });
  });

  test("fn+ rejects inline parameter lists", async () => {
    const result = await evalSource("(fn+ [x] x)");
    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "INTERP_FN_REQUIRES_CLAUSES")
    ).toBeTrue();
  });

  test("multi-arity function dispatches the matching clause", async () => {
    const result = await evalMulti(`
      (def list (fn+ ([& xs] xs)))
      (def picker
        (fn+
          ([x] 1)
          ([x y] 2)
          ([x y & rest] 3)))
      (list (picker 10) (picker 1 2) (picker 1 2 3))
    `);
    expect(result.ok).toBeTrue();
    const listValue = result.value as any;
    expect(listValue.kind).toBe("list");
    expect(listValue.elements.map((elem: any) => elem.value)).toEqual([
      1, 2, 3,
    ]);
  });

  test("multi-arity function reports missing clauses", async () => {
    const result = await evalMulti(`
      (def only-one (fn+ ([x] x)))
      (only-one 1 2)
    `);
    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "INTERP_ARITY_MISMATCH")
    ).toBeTrue();
  });
});

describe("Interpreter - Collections", () => {
  test("bracket syntax behaves like parentheses", async () => {
    const result = await evalSource("[runtime/add* 2 3]", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 5 });
  });

  test("brackets support inline function calls", async () => {
    const result = await evalSource("[(fn+ ([x] (runtime/mul* x 2))) 5]", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 10 });
  });
});

describe("Interpreter - Spread", () => {
  test("function call supports runtime spread in args", async () => {
    const result = await evalMulti(
      `(def nums (quote (1 2 3))) (def f (fn+ ([& xs] (runtime/count xs)))) (f (spread nums))`,
      true
    );
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 3 });
  });

  test("spread form materializes list value", async () => {
    const result = await evalSource("(spread (quote (1 2 3)))", true);
    expect(result.ok).toBeTrue();
    const list = result.value as any;
    expect(list.kind).toBe("list");
    expect(list.elements.map((elem: any) => elem.value)).toEqual([1, 2, 3]);
  });

  test("spread form rejects non-list inputs", async () => {
    const result = await evalSource("(spread 42)", true);
    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some(
        (diagnostic) => diagnostic.code === "INTERP_SPREAD_NOT_SEQUENCE"
      )
    ).toBeTrue();
  });
});

describe("Interpreter - Sequence Operations", () => {
  // test("first of list", async () => {
  //   const result = await evalSource("(runtime/first (quote (1 2 3)))", true);
  //   expect(result.ok).toBeTrue();
  //   expect(result.value).toEqual({ kind: "number", value: 1 });
  // });

  // test("rest of list", async () => {
  //   const result = await evalSource("(runtime/rest (quote (1 2 3)))", true);
  //   expect(result.ok).toBeTrue();
  //   expect(result.value?.kind).toBe("list");
  //   expect((result.value as any).elements).toHaveLength(2);
  // });

  test("count of bracketed list", async () => {
    const result = await evalSource("(runtime/count (quote (1 2 3 4)))", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "number", value: 4 });
  });

  // test("cons onto list", async () => {
  //   const result = await evalSource("(runtime/cons 0 (quote (1 2 3)))", true);
  //   expect(result.ok).toBeTrue();
  //   expect(result.value?.kind).toBe("list");
  //   const list = result.value as any;
  //   expect(list.elements).toHaveLength(4);
  //   expect(list.elements[0]).toEqual({ kind: "number", value: 0 });
  // });

  // test("take from list", async () => {
  //   const result = await evalSource("(runtime/take 2 (quote (1 2 3 4)))", true);
  //   expect(result.ok).toBeTrue();
  //   const list = result.value as any;
  //   expect(list.elements).toHaveLength(2);
  // });

  // test("drop from list", async () => {
  //   const result = await evalSource("(runtime/drop 2 (quote (1 2 3 4)))", true);
  //   expect(result.ok).toBeTrue();
  //   const list = result.value as any;
  //   expect(list.elements).toHaveLength(2);
  //   expect(list.elements[0]).toEqual({ kind: "number", value: 3 });
  // });

  // test("reverse list", async () => {
  //   const result = await evalSource("(runtime/reverse (quote (1 2 3)))", true);
  //   expect(result.ok).toBeTrue();
  //   const list = result.value as any;
  //   expect(list.elements[0]).toEqual({ kind: "number", value: 3 });
  //   expect(list.elements[2]).toEqual({ kind: "number", value: 1 });
  // });

  // test("concat lists", async () => {
  //   const result = await evalSource(
  //     "(runtime/concat (quote (1 2)) (quote (3 4)) (quote (5)))",
  //     true
  //   );
  //   expect(result.ok).toBeTrue();
  //   const list = result.value as any;
  //   expect(list.elements).toHaveLength(5);
  // });
});

describe("Interpreter - Type Predicates", () => {
  test("type returns :number", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource("(runtime/type 42)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "keyword", value: "number" });
  });

  test("type returns :string", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource('(runtime/type "hello")', true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "keyword", value: "string" });
  });

  test("type returns :nil", async () => {
    // TODO: Implement type function in @vibe/runtime
    const result = await evalSource("(runtime/type nil)", true);
    expect(result.ok).toBeTrue();
    expect(result.value).toEqual({ kind: "keyword", value: "nil" });
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

  describe("Interpreter - Destructuring", () => {
    test("let destructures bracketed bindings", async () => {
      const result = await evalMulti(
        `
        (def list (fn+ ([& xs] xs)))
        (let [[x y & rest :as full] (quote (1 2 3 4))
              bonus (runtime/add* x 3)
              meta (quote ())]
          (list x y rest full bonus meta))
      `,
        true
      );

      expect(result.ok).toBeTrue();
      const listValue = result.value as any;
      expect(listValue?.kind).toBe("list");
      const [x, y, rest, full, bonus, meta] = listValue?.elements ?? [];
      expect(x).toMatchObject({ kind: "number", value: 1 });
      expect(y).toMatchObject({ kind: "number", value: 2 });
      expect(rest).toMatchObject({ kind: "list" });
      expect(rest?.elements?.map((element: any) => element.value)).toEqual([
        3, 4,
      ]);
      expect(full).toMatchObject({ kind: "list" });
      expect(full?.elements?.map((element: any) => element.value)).toEqual([
        1, 2, 3, 4,
      ]);
      expect(bonus).toMatchObject({ kind: "number", value: 4 });
      expect(meta).toMatchObject({ kind: "list" });
      expect(meta?.elements?.length).toBe(0);
    });

    test("fn destructures bracketed parameters", async () => {
      const result = await evalMulti(
        `
        (def list (fn+ ([& xs] xs)))
        (def describe
          (fn+ ([[x y & tail] bonus]
            (list x y tail bonus))))
        (describe (quote (10 20 30)) 7)
      `,
        true
      );

      expect(result.ok).toBeTrue();
      const listValue = result.value as any;
      expect(listValue?.kind).toBe("list");
      const [x, y, tail, bonus, extra] = listValue?.elements ?? [];
      expect(x).toMatchObject({ kind: "number", value: 10 });
      expect(y).toMatchObject({ kind: "number", value: 20 });
      expect(tail).toMatchObject({ kind: "list" });
      expect(tail?.elements?.map((element: any) => element.value)).toEqual([
        30,
      ]);
      expect(bonus).toMatchObject({ kind: "number", value: 7 });
    });
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
      (def list (fn+ ([& xs] xs)))
      (import "${IMPORT_FIXTURE}")
      (def result (list pulled (greet)))
      result
    `;
    const result = await evalMulti(source);
    expect(result.ok).toBeTrue();
    expect(result.value?.kind).toBe("list");
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
      "(def f (fn+ ([x y] (runtime/add* x y)))) (f 1)"
    );
    const env = createRootEnvironment();
    await evaluate(parseResult.program.body[0]!, env);
    const result = await evaluate(parseResult.program.body[1]!, env);
    expect(result.ok).toBeFalse();
    expect(result.diagnostics[0]?.code).toBe("INTERP_ARITY_MISMATCH");
  });

  test("arity mismatch - too many args", async () => {
    const parseResult = await parseSource(
      "(def f (fn+ ([x] (* x 2)))) (f 1 2 3)"
    );
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
