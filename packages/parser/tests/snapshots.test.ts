import { describe, expect, test } from "bun:test";
import { parseSource } from "../src";

const snapshotCases = [
  {
    name: "nested quotes and splicing",
    source: "''(foo `(bar ~baz ~@(+ 1 2)))",
  },
  {
    name: "metadata style dispatch",
    source: '#^{:doc "hi" :author "me"} (def foo {:a 1 :b 2})',
  },
  {
    name: "dispatch variants inside collection",
    source: "'[ #(println %1) #(+ 1 state) ]",
  },
  {
    name: "macro declaration and usage",
    source: `
      (def with-temp
        (macro [expr]
          \`(let [tmp ~expr]
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
      expect(result.program).toMatchSnapshot();
    });
  }
});
