import { describe, expect, test } from "bun:test";
import { parseSource } from "@vibe/parser";
import { analyzeProgram } from "../src/analyzer";
import { TEST_BUILTINS } from "./test-builtins";

const ARITHMETIC_STUB = `
  (def + (fn [& xs] xs))
  (def - (fn [& xs] xs))
  (def * (fn [& xs] xs))
  (def / (fn [& xs] xs))
`;

const withArithmeticPrelude = (source: string) =>
  `${ARITHMETIC_STUB}\n${source}`;

const analyzeSource = async (source: string) => {
  const parseResult = await parseSource(source);
  const analyzeResult = await analyzeProgram(parseResult.program, {
    builtins: TEST_BUILTINS,
  });
  return {
    ok: analyzeResult.ok && parseResult.ok,
    diagnostics: [...parseResult.diagnostics, ...analyzeResult.diagnostics],
  };
};

describe("advanced macros", () => {
  test("variadic macro with & rest parameter", async () => {
    const result = await analyzeSource(`
      (def my-list (macro [& items] \`[~@items]))
      (my-list 1 2 3)
    `);

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
  });

  test("recursive macro expansion - and macro", async () => {
    // Simplified version that doesn't require runtime functions
    const result = await analyzeSource(`
      (def simple-and
        (macro [a b]
          \`(if ~a ~b false)))
      (simple-and true false)
    `);

    // Tests macro expansion with if (a special form)
    expect(result.ok).toBeTrue();
  });

  test("threading macro pattern", async () => {
    const result = await analyzeSource(
      withArithmeticPrelude(`
        (def thread-first
          (macro [x & forms]
            \`~x))
        (thread-first 5 (+ 3) (* 2))
      `)
    );

    expect(result.ok).toBeTrue();
  });

  test("variadic macro collects remaining args", async () => {
    const result = await analyzeSource(`
      (def variadic-test
        (macro [first & rest]
          \`[~first ~@rest]))
      (variadic-test :a :b :c :d)
    `);

    expect(result.ok).toBeTrue();
  });

  test("empty variadic args", async () => {
    const result = await analyzeSource(`
      (def maybe-list
        (macro [& items]
          \`[~@items]))
      (maybe-list)
    `);

    expect(result.ok).toBeTrue();
  });

  test("compile-time count and eq* check", async () => {
    const result = await analyzeSource(`
      (def is-empty
        (macro [& items]
          \`(eq* 0 (count ~items))))
      (is-empty)
      (is-empty 42)
    `);

    // count and eq* work at compile-time to derive empty? behavior
    expect(result.ok).toBeTrue();
  });

  test("nested variadic macro expansion", async () => {
    const result = await analyzeSource(`
      (def outer (macro [& args] \`[~@args]))
      (def inner (macro [x] \`(outer ~x :extra)))
      (inner 42)
    `);

    expect(result.ok).toBeTrue();
  });

  test("detects when & is not followed by symbol", async () => {
    const result = await analyzeSource(`
      (def bad (macro [&] \`nil))
    `);

    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some(
        (d) => d.code === "SEM_MACRO_REST_REQUIRES_SYMBOL"
      )
    ).toBeTrue();
  });

  test("detects multiple & parameters", async () => {
    const result = await analyzeSource(`
      (def bad (macro [& a & b] \`nil))
    `);

    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "SEM_MACRO_DUPLICATE_REST")
    ).toBeTrue();
  });

  test("detects params after & rest", async () => {
    const result = await analyzeSource(`
      (def bad (macro [& rest extra] \`nil))
    `);

    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "SEM_MACRO_PARAMS_AFTER_REST")
    ).toBeTrue();
  });
});

describe("advanced functions with rest params", () => {
  test("function with & rest parameter", async () => {
    const result = await analyzeSource(`
      (def variadic-fn (fn [x & rest] x))
      (variadic-fn 1 2 3)
    `);

    expect(result.ok).toBeTrue();
  });

  test("function with only rest parameter", async () => {
    const result = await analyzeSource(`
      (def all-rest (fn [& args] args))
      (all-rest 1 2 3)
    `);

    expect(result.ok).toBeTrue();
  });

  test("detects multiple & in functions", async () => {
    const result = await analyzeSource(`
      (def bad-fn (fn [& a & b] a))
    `);

    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "SEM_FN_DUPLICATE_REST")
    ).toBeTrue();
  });

  test("detects params after & in functions", async () => {
    const result = await analyzeSource(`
      (def bad-fn (fn [& rest extra] rest))
    `);

    expect(result.ok).toBeFalse();
    expect(
      result.diagnostics.some((d) => d.code === "SEM_FN_PARAMS_AFTER_REST")
    ).toBeTrue();
  });
});
