import { expect, test } from "bun:test";
import type { Diagnostic } from "@vibe/syntax";
import { formatSource } from "../src";

const stripDiagnostics = (diagnostics: readonly Diagnostic[]): number =>
  diagnostics.length;

test("collapses redundant whitespace and enforces trailing newline", async () => {
  const source = "(def   foo   1)\n(def bar   (+   foo 2))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(stripDiagnostics(result.diagnostics)).toBe(0);
  expect(result.formatted).toBe("(def foo 1)\n\n(def bar (+ foo 2))\n");
});

test("wraps forms that exceed the configured width", async () => {
  const source = "(do (println 1) (println 2) (println 3))";
  const result = await formatSource(source, { maxWidth: 20 });
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(do\n" + "  (println 1)\n" + "  (println 2)\n" + "  (println 3))\n"
  );
});

test("aligns defmacro signatures and canonical binding vectors", async () => {
  const source = "(defmacro a (x) (let (y (name x)) (quote (println :ok y))))";
  const result = await formatSource(source, { maxWidth: 40 });
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defmacro a [x]\n" +
      "  (let [y (name x)]\n" +
      "    (quote\n" +
      "      (println :ok y))))\n"
  );
});

test("reports parser diagnostics instead of rewriting invalid code", async () => {
  const source = "(def foo";
  const result = await formatSource(source);
  expect(result.ok).toBeFalse();
  expect(stripDiagnostics(result.diagnostics)).toBeGreaterThan(0);
  expect(result.formatted).toBe(source);
});

test("groups cond clauses onto single lines", async () => {
  const source =
    '(cond (= favorite-number 42) (println "The answer to life, the universe, and everything.") (= favorite-number 7) (println "A lucky number.") :else (println "Just a number."))';
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(cond\n" +
      '  (= favorite-number 42) (println "The answer to life, the universe, and everything.")\n' +
      '  (= favorite-number 7) (println "A lucky number.")\n' +
      '  :else (println "Just a number."))\n'
  );
});

test("forces multiline bodies for defn", async () => {
  const source = '(defn greet [arg] (a arg) (println "Hello, " arg "!"))';
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defn greet [arg]\n" + "  (a arg)\n" + '  (println "Hello, " arg "!"))\n'
  );
});

test("applies user-provided form config for macros", async () => {
  const source =
    '(defhandler greet (name) (println "Hi " name) (println "Done"))';
  const result = await formatSource(source, {
    formConfig: {
      defhandler: {
        inlineHeadArgCount: 2,
        vectorArgumentIndices: [2],
        forceBodyMultiline: true,
      },
    },
  });
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defhandler greet [name]\n" +
      '  (println "Hi " name)\n' +
      '  (println "Done"))\n'
  );
});
