import { describe, expect, test } from "bun:test";
import path from "node:path";
import { parseSource } from "@vibe/parser";
import { NodeKind } from "@vibe/syntax";
import {
  ModuleExportsTable,
  extractTopLevelExports,
  seedModuleExportsFromMetadata,
} from "../src/module-exports";
import type { PackageMetadata } from "../src/module-resolver";

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

    const exports = extractTopLevelExports(result.program);
    expect(exports.map((entry) => entry.name)).toEqual(["foo", "build"]);
    const build = exports.find((entry) => entry.name === "build");
    expect(build?.kind).toBe("macro");
  });

  test("records macro dependency metadata for externals", async () => {
    const result = await parseSource(`
      (external runtime "@vibe/runtime")
      (def with-runtime (macro [] (runtime/eq* 1 1)))
    `);

    const exports = extractTopLevelExports(result.program);
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

    const exports = extractTopLevelExports(result.program);
    const macro = exports.find((entry) => entry.name === "choose");
    expect(
      macro?.macro?.clauses?.map((clause) => clause.params.length)
    ).toEqual([1, 2]);
  });
});
