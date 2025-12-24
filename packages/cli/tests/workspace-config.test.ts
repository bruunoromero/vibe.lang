import { describe, expect, test } from "bun:test";
import path from "node:path";
import {
  parseVibeConfig,
  resolveVibePackageConfig,
} from "../src/workspace-config";

describe("workspace config", () => {
  test("parses sources, entry, and outDir declarations", () => {
    const config = parseVibeConfig({
      sources: "./src",
      outDir: "./dist",
      entry: "./src/main.lang",
    });

    expect(config).toBeTruthy();
    expect(config?.sources).toEqual(["./src"]);
    expect(config?.outDir).toBe("./dist");
    expect(config?.entry).toBe("./src/main.lang");
  });

  test("normalizes absolute source roots, entry, and outDir", () => {
    const packageRoot = path.resolve(import.meta.dir, "../../example-app");
    const config = parseVibeConfig({
      sources: ["./src", "./lib"],
      outDir: "./dist",
      entry: "./src/main.lang",
    });
    const resolved = resolveVibePackageConfig(packageRoot, config);

    expect(resolved.sourceRoots).toContain(path.join(packageRoot, "src"));
    expect(resolved.sourceRoots).toContain(path.join(packageRoot, "lib"));
    expect(resolved.outDir).toBe(path.join(packageRoot, "dist"));
    expect(resolved.entry).toBe(path.join(packageRoot, "src/main.lang"));
  });

  test("returns undefined when no actionable fields are present", () => {
    expect(parseVibeConfig(null)).toBeUndefined();
    expect(parseVibeConfig({})).toBeUndefined();
  });
});
