import { describe, expect, test } from "bun:test";
import path from "node:path";
import {
  parseVibeConfig,
  resolveVibePackageConfig,
} from "../src/workspace-config";

describe("workspace config", () => {
  test("parses modules, sources, and outDir declarations", () => {
    const config = parseVibeConfig({
      modules: {
        ".": "./src/main.lang",
      },
      sources: "./src",
      outDir: "./dist",
    });

    expect(config).toBeTruthy();
    expect(config?.sources).toEqual(["./src"]);
    expect(config?.outDir).toBe("./dist");
    expect(config?.modules?.["."]).toBe("./src/main.lang");
  });

  test("normalizes absolute source roots and outDir", () => {
    const packageRoot = path.resolve(import.meta.dir, "../../example-app");
    const config = parseVibeConfig({
      sources: ["./src", "./lib"],
      outDir: "./dist",
    });
    const resolved = resolveVibePackageConfig(packageRoot, config);

    expect(resolved.sourceRoots).toContain(
      path.join(packageRoot, "src")
    );
    expect(resolved.sourceRoots).toContain(
      path.join(packageRoot, "lib")
    );
    expect(resolved.outDir).toBe(path.join(packageRoot, "dist"));
  });

  test("returns undefined when no actionable fields are present", () => {
    expect(parseVibeConfig(null)).toBeUndefined();
    expect(parseVibeConfig({})).toBeUndefined();
  });
});

