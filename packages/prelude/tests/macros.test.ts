import { describe, expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import path from "node:path";
import { parseSource } from "@vibe/parser";
import { NodeKind } from "@vibe/syntax";

const PRELUDE_SOURCE = path.join(import.meta.dir, "../src/prelude.lang");

describe("prelude definition helpers", () => {
  test("exports defmacro-style helpers", async () => {
    const source = readFileSync(PRELUDE_SOURCE, "utf8");
    const result = await parseSource(source);
    if (!result.ok) {
      const messages = result.diagnostics.map((d) => d.message).join("\n");
      throw new Error(`Failed to parse prelude: ${messages}`);
    }

    const exportedNames = result.program.body
      .map((node) => {
        if (node.kind !== NodeKind.List) {
          return null;
        }
        const head = node.elements[0];
        const binding = node.elements[1];
        if (
          head &&
          head.kind === NodeKind.Symbol &&
          (head.value === "def" || head.value === "defmacro") &&
          binding &&
          binding.kind === NodeKind.Symbol
        ) {
          return binding.value;
        }
        return null;
      })
      .filter((name): name is string => Boolean(name));

    expect(exportedNames).toEqual(
      expect.arrayContaining(["defmacro", "defmacrop", "defn", "defnp", "cond"])
    );
  });
});
