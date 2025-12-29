import { describe, expect, test } from "bun:test";
import path from "node:path";
import { parseSource } from "@vibe/parser";
import { NodeKind, type ListNode } from "@vibe/syntax";
import {
  ModuleExportsTable,
  extractTopLevelExports,
  seedModuleExportsFromMetadata,
} from "@vibe/module-resolver";
import type { PackageMetadata } from "@vibe/module-resolver";

describe("module exports helpers", () => {
  const fixturesRoot = path.join(import.meta.dir, "fixtures");

  test("seeds exports from package metadata", async () => {
    const table = new ModuleExportsTable();
    const metadata: PackageMetadata = {
      name: "@vibe/example-fixture",
      rootDir: fixturesRoot,
      vibe: {
        sources: ["./"],
        entry: "./module-a.lang",
      },
    };

    await seedModuleExportsFromMetadata(metadata, table);

    const modulePath = path.join(fixturesRoot, "module-a.lang");
    const exports = table.getExports(modulePath);
    expect(exports).toBeDefined();
    expect(exports).toMatchObject([
      { name: "alpha", kind: "var" },
      {
        name: "beta",
        kind: "macro",
        macro: { clauses: [{ params: [] }] },
      },
    ]);
    expect(exports?.[1]?.macro?.clauses?.[0]?.body.kind).toBe(NodeKind.Number);
  });

  test("extractTopLevelExports returns unique def names", async () => {
    const result = await parseSource(`
      (def foo 1)
      (def build (macro [] foo))
      (def foo 2)
      foo
    `);

    const exports = await extractTopLevelExports(result.program);
    expect(exports.map((entry) => entry.name)).toEqual(["foo", "build"]);
    const build = exports.find((entry) => entry.name === "build");
    expect(build?.kind).toBe("macro");
  });

  test("records macro dependency metadata for externals", async () => {
    const result = await parseSource(`
      (external runtime "@vibe/runtime")
      (def with-runtime (macro [] (runtime/eq* 1 1)))
    `);

    const exports = await extractTopLevelExports(result.program);
    const macro = exports.find((entry) => entry.name === "with-runtime");
    expect(macro?.kind).toBe("macro");
    expect(macro?.macro?.clauses?.length).toBe(1);
    expect(macro?.macro?.dependencies).toEqual([
      {
        kind: "external",
        alias: "runtime",
        specifier: "@vibe/runtime",
      },
    ]);
  });

  test("captures multi-clause macro metadata", async () => {
    const result = await parseSource(`
      (def choose
        (macro
          ([x] x)
          ([x y] y)))
    `);

    const exports = await extractTopLevelExports(result.program);
    const macro = exports.find((entry) => entry.name === "choose");
    expect(
      macro?.macro?.clauses?.map((clause) => clause.params.length)
    ).toEqual([1, 2]);
  });

  test("extracts macros declared via defmacro helper", async () => {
    const result = await parseSource(`
      (def defmacro
        (macro
          ([name & clauses]
            \`(def ~name
               (macro ~@clauses)))))

      (defmacro when
        ([pred body]
          \`(if ~pred ~body nil)))
    `);

    const exports = await extractTopLevelExports(result.program);
    const macro = exports.find((entry) => entry.name === "when");
    expect(macro?.kind).toBe("macro");
    expect(macro?.macro?.clauses?.[0]?.params).toEqual(["pred", "body"]);
  });

  test("captures functions declared via defn helper", async () => {
    const result = await parseSource(`
      (defp defn
        (macro
          ([name & clauses]
            \`(def ~name
               (fn ~@clauses)))))

      (defn add [x y]
        (+ x y))
    `);

    const exports = await extractTopLevelExports(result.program);
    expect(exports.map((entry) => entry.name)).toEqual(["add"]);
    expect(exports[0]?.kind).toBe("var");
  });

  test("skips defp bindings when collecting exports", async () => {
    const result = await parseSource(`
      (def foo 1)
      (defp secret 2)
      (def bar 3)
    `);

    const exports = await extractTopLevelExports(result.program);
    expect(exports.map((entry) => entry.name)).toEqual(["foo", "bar"]);
  });

  test("macro metadata strips scope ids", async () => {
    const result = await parseSource(`
      (def wrap
        (macro [msg]
          (println msg)))
    `);

    const macroLiteral =
      ((result.program.body[0] as ListNode).elements[2] as ListNode) ?? null;
    if (!macroLiteral) {
      throw new Error("Missing macro literal in test setup");
    }
    (macroLiteral as unknown as { scopeId?: string }).scopeId = "scope_def";
    const bodyNode = macroLiteral.elements[2] as ListNode;
    (bodyNode as unknown as { scopeId?: string }).scopeId = "scope_body";
    for (const element of bodyNode.elements) {
      if (element) {
        (element as unknown as { scopeId?: string }).scopeId = "scope_child";
      }
    }

    const exports = await extractTopLevelExports(result.program);
    const macro = exports.find((entry) => entry.name === "wrap");
    const clause = macro?.macro?.clauses?.[0];
    expect(clause).toBeDefined();

    const seenScopeIds: string[] = [];
    const walk = (node: unknown): void => {
      if (!node || typeof node !== "object") {
        return;
      }
      if (Array.isArray(node)) {
        for (const element of node) {
          walk(element);
        }
        return;
      }
      if (Object.prototype.hasOwnProperty.call(node, "scopeId")) {
        const scopeId = (node as { scopeId?: string }).scopeId;
        if (scopeId !== undefined) {
          seenScopeIds.push(scopeId);
        }
      }
      for (const value of Object.values(node as Record<string, unknown>)) {
        walk(value);
      }
    };

    walk(clause?.body);
    expect(seenScopeIds).toEqual([]);
  });
});
