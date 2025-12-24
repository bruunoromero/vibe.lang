import { describe, expect, test } from "bun:test";
import path from "node:path";
import { parseSource } from "@vibe/parser";
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
    expect(table.getExports(modulePath)).toEqual(["alpha", "beta"]);
  });

  test("extractTopLevelExports returns unique def names", async () => {
    const result = await parseSource(`
      (def foo 1)
      (defmacro build [] foo)
      (def foo 2)
      foo
    `);

    expect(extractTopLevelExports(result.program)).toEqual(["foo", "build"]);
  });
});
