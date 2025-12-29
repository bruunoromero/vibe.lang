import { expect, test } from "bun:test";
import { runRepl } from "../src/repl";

const makeReader = (lines: Array<string | null>) => {
  const copy = lines.slice();
  return () => Promise.resolve(copy.shift() ?? null);
};

test("evals single-line expression", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["(+ 1 2)", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  expect(errs.length).toBe(0);
  expect(outputs.some((o) => o.includes('"result": 3'))).toBeTrue();
});

test("handles multi-line forms", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["(let [x 1", "] x)", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  expect(errs.length).toBe(0);
  expect(outputs.some((o) => o.includes('"result": 1'))).toBeTrue();
});

test("persists defs across inputs", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["(def a 5)", "a", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  // first input defines, second prints value
  expect(outputs.some((o) => o.includes('"result"'))).toBeTrue();
  // one of the outputs should contain 5
  expect(outputs.some((o) => o.includes("5"))).toBeTrue();
});

test("prints diagnostics for unresolved symbols", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["(foo)", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  // Should have at least one diagnostic emitted to stderr
  expect(errs.length).toBeGreaterThan(0);
  expect(
    errs.some((e) => e.includes("SEM_") || e.includes("INTERP_"))
  ).toBeTrue();
});

test("defines functions and allows subsequent calls", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["(def a (fn [x y] (+ x y)))", "(a 1 2)", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  if (errs.length > 0) {
    throw new Error(`REPL diagnostics: ${errs.join("\n")}`);
  }
  expect(errs.length).toBe(0);
  // second input should evaluate to 3
  expect(outputs.some((o) => o.includes('"result": 3'))).toBeTrue();
});

test("handles split definitions across multiple lines", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  // Simulate user splitting a top-level def across lines and then calling it
  const reader = makeReader([
    "(def a (fn [x y]",
    " (+ x y)))",
    "(a 1 2)",
    null,
  ]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  if (errs.length > 0) {
    throw new Error(`REPL diagnostics: ${errs.join("\n")}`);
  }
  expect(outputs.some((o) => o.includes('"result": 3'))).toBeTrue();
});

test("prints friendly function labels when a function value is displayed", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["(def a (fn [x y] (+ x y)))", "a", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  if (errs.length > 0) {
    throw new Error(`REPL diagnostics: ${errs.join("\n")}`);
  }
  // Expect printing of the function value to include our friendly <fn ...> label
  expect(
    outputs.some((o) => o.includes("<fn") && o.includes("arities=[2]"))
  ).toBeTrue();
});

test("prints a friendly message when a bare builtin symbol is entered", async () => {
  const outputs: string[] = [];
  const errs: string[] = [];
  const reader = makeReader(["def", null]);
  await runRepl({
    readLine: reader,
    writeOut: (s) => outputs.push(s),
    writeErr: (s) => errs.push(s),
  });
  expect(
    errs.some((e) => e.includes("cannot evaluate builtin symbol 'def'"))
  ).toBeTrue();
});

// Reproduce analyzer behavior for the def capture strategy
test("analyze evalSource with def capture", async () => {
  const source = "(def a (fn [x y] (+ x y)))\n(def __repl_result_1 a)";
  const { parseSource } = await import("@vibe/parser");
  const { analyzeProgram } = await import("@vibe/semantics");
  const parse = await parseSource(source);
  const analysis = await analyzeProgram(parse.program);
  expect(analysis.diagnostics.map((d) => d.code)).not.toContain(
    "SEM_DUPLICATE_SYMBOL"
  );
});
