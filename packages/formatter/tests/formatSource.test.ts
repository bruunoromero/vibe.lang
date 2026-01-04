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
  expect(result.formatted).toBe("(def foo 1)\n\n(def bar\n  (+ foo 2))\n");
});

test("wraps forms that exceed the configured width", async () => {
  const source = "(do (println 1) (println 2) (println 3))";
  const result = await formatSource(source, { maxWidth: 20 });
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(do\n" + "  (println 1)\n" + "  (println 2)\n" + "  (println 3))\n"
  );
});

test("preserves defmacro signatures and binding vectors", async () => {
  const source = "(defmacro a [x] (let [y (name x)] (quote (println :ok y))))";
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

test("keeps explicit parens inside quoted fn clauses", async () => {
  const source =
    "(defmacro defn+ [name & clauses]\n" +
    "  (quote\n" +
    "    (def (unquote name) (fn+ (spread (unquote clauses))))))";
  const result = await formatSource(source, { maxWidth: 80 });
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defmacro defn+ [name & clauses]\n" +
      "  (quote\n" +
      "    (def (unquote name)\n" +
      "      (fn+ (spread (unquote clauses))))))\n"
  );
});

test("keeps clause lists wrapped in parens for multi-arity defn", async () => {
  const source =
    "(defn+ =\n" +
    "  ([x] true)\n" +
    "  ([x y] (runtime/eq* x y))\n" +
    "  ([x y & rest]\n" +
    "    (if (runtime/eq* x y) (apply = (cons y rest)) false)))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defn+ =\n" +
      "  ([x] true)\n" +
      "  ([x y] (runtime/eq* x y))\n" +
      "  ([x y & rest]\n" +
      "    (if (runtime/eq* x y)\n" +
      "      (apply = (cons y rest))\n" +
      "      false)))\n"
  );
});

test("preserves explicit empty list literals", async () => {
  const source =
    "(defn take [n coll]\n" +
    "  (cond\n" +
    "    (<= n 0) []\n" +
    "    (empty? coll) []\n" +
    "    :else (cons (first coll) (take (dec n) (rest coll)))))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defn take [n coll]\n" +
      "  (cond\n" +
      "    (<= n 0) []\n" +
      "    (empty? coll) []\n" +
      "    :else (cons (first coll) (take (dec n) (rest coll)))))\n"
  );
});

test("enforces multiline let bindings and if branches", async () => {
  const source =
    "(defn filter [pred coll]\n" +
    "  (if\n" +
    "    (empty? coll)\n" +
    "    []\n" +
    "    (let [x (first coll) xs (rest coll)]\n" +
    "      (if (pred x) (cons x (filter pred xs)) (filter pred xs)))))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defn filter [pred coll]\n" +
      "  (if (empty? coll)\n" +
      "    []\n" +
      "    (let [x (first coll)\n" +
      "          xs (rest coll)]\n" +
      "      (if (pred x)\n" +
      "        (cons x (filter pred xs))\n" +
      "        (filter pred xs)))))\n"
  );
});

test("breaks multi-arity defmacro definitions onto separate lines", async () => {
  const source =
    "(defmacro+ defmacro ([name args & body]\n" +
    "  (quote\n" +
    "    (defmacro+ (unquote name) ([unquote args] (spread (unquote body)))))))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defmacro+ defmacro\n" +
      "  ([name args & body]\n" +
      "    (quote\n" +
      "      (defmacro+ (unquote name)\n" +
      "        ([unquote args] (spread (unquote body)))))))\n"
  );
});

test("breaks def initializer when value is a call", async () => {
  const source = "(def a (fn-call 1 2 3))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe("(def a\n  (fn-call 1 2 3))\n");
});

test("respects user multiline layout for plain function calls", async () => {
  const source =
    "(defn take-some [coll]\n" +
    "  (cons (first coll)\n" +
    "        (take (dec n)\n" +
    "              (rest coll))))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defn take-some [coll]\n" +
      "  (cons (first coll)\n" +
      "        (take (dec n)\n" +
      "              (rest coll))))\n"
  );
});

test("aligns preserved multiline arguments under the first inline argument", async () => {
  const source = "(-> x\n    (call 1)\n    (call 2))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe("(-> x\n    (call 1)\n    (call 2))\n");
});

test("aligns cond clause bodies with nested calls", async () => {
  const source =
    "(defn drop [n coll]\n" +
    "  (cond\n" +
    "    (<= n 0) coll\n" +
    "    (empty? coll) []\n" +
    "    :else (drop (dec n)\n" +
    "      (rest coll))))";
  const result = await formatSource(source);
  expect(result.ok).toBeTrue();
  expect(result.formatted).toBe(
    "(defn drop [n coll]\n" +
      "  (cond\n" +
      "    (<= n 0) coll\n" +
      "    (empty? coll) []\n" +
      "    :else (drop (dec n)\n" +
      "                (rest coll))))\n"
  );
});
