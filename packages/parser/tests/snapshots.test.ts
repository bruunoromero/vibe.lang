import { describe, expect, test } from "bun:test";
import { parseSource } from "../src";

const snapshotCases = [
  {
    name: "nested quotes and splicing",
    source: "''(foo `(bar (unquote baz) (unquote-splicing (+ 1 2))))",
  },
  {
    name: "metadata with map and def",
    source: '(defn info [] [:doc "hi" :author "me"])',
  },
  {
    name: "macro declaration and usage",
    source: `
      (def with-temp
        (macro+ [expr]
          \`(let [tmp (unquote expr)]
             (println tmp)
             tmp)))

      (def answer (with-temp (+ 1 2)))
    `,
  },
];

describe("parser snapshots", () => {
  for (const testCase of snapshotCases) {
    test(testCase.name, async () => {
      const result = await parseSource(testCase.source);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program).toBeDefined();
    });
  }
});
