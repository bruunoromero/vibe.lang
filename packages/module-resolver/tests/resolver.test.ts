import { describe, expect, test } from "bun:test";
import fs from "node:fs";
import path from "node:path";
import { tmpdir } from "node:os";
import { loadConfig, DEFAULT_CONFIG_NAME } from "@vibe/config";
import {
  moduleNameToRelativePath,
  resolveModule,
  discoverModuleGraph,
} from "../src/index.ts";
import type { Program } from "@vibe/syntax";

function createWorkspace(): string {
  return fs.mkdtempSync(path.join(tmpdir(), "vibe-resolver-"));
}

describe("module resolver", () => {
  test("resolves modules from the project and configured packages", () => {
    const root = createWorkspace();
    const srcDir = path.join(root, "src");
    const packagesDir = path.join(root, "packages", "prelude", "src");

    fs.mkdirSync(srcDir, { recursive: true });
    fs.mkdirSync(packagesDir, { recursive: true });

    const mainPath = path.join(srcDir, "Main.vibe");
    const htmlPath = path.join(packagesDir, "Html.vibe");

    fs.writeFileSync(mainPath, "module Main exposing (..)\n");
    fs.writeFileSync(htmlPath, "module Html exposing (..)\n");

    const configPath = path.join(root, DEFAULT_CONFIG_NAME);
    fs.writeFileSync(
      configPath,
      JSON.stringify(
        {
          name: "ExampleApp",
          src: "src",
          dist: "dist",
          packages: ["@vibe/prelude"],
        },
        null,
        2
      )
    );

    const packageJsonPath = path.join(
      root,
      "packages",
      "prelude",
      "package.json"
    );
    fs.writeFileSync(
      packageJsonPath,
      JSON.stringify({ name: "@vibe/prelude", version: "0.0.0" }, null, 2)
    );

    const config = loadConfig({ path: configPath });

    const main = resolveModule({ config, moduleName: "Main" });
    const html = resolveModule({ config, moduleName: "Html" });

    expect(main.filePath).toBe(mainPath);
    expect(html.filePath).toBe(htmlPath);
    expect(html.packageName).toBe("@vibe/prelude");
  });

  test("rejects invalid module names", () => {
    expect(() => moduleNameToRelativePath("not.Valid")).toThrow("not.Valid");
    expect(() => moduleNameToRelativePath("lowercase")).toThrow("lowercase");
  });

  test("discovers and sorts modules topologically", () => {
    const root = createWorkspace();
    const srcDir = path.join(root, "src");

    fs.mkdirSync(srcDir, { recursive: true });

    // Create a simple dependency graph:
    // Main imports Util
    // Util imports Base
    // Base has no imports

    const basePath = path.join(srcDir, "Base.vibe");
    const utilPath = path.join(srcDir, "Util.vibe");
    const mainPath = path.join(srcDir, "Main.vibe");

    fs.writeFileSync(basePath, "module Base exposing (..)\n\nvalue = 42");
    fs.writeFileSync(
      utilPath,
      "module Util exposing (..)\n\nimport Base\n\nhelper = Base.value"
    );
    fs.writeFileSync(
      mainPath,
      "module Main exposing (..)\n\nimport Util\n\nmain = Util.helper"
    );

    const configPath = path.join(root, DEFAULT_CONFIG_NAME);
    fs.writeFileSync(
      configPath,
      JSON.stringify(
        {
          name: "ExampleApp",
          src: "src",
          dist: "dist",
          packages: [],
        },
        null,
        2
      )
    );

    const config = loadConfig({ path: configPath });

    // Simple parser that extracts imports
    const mockParse = (source: string): Program => {
      const importRegex =
        /^import\s+([A-Z][A-Za-z0-9_]*(?:\.[A-Z][A-Za-z0-9_]*)*)/gm;
      const imports = [];
      let match;

      while ((match = importRegex.exec(source)) !== null) {
        imports.push({
          moduleName: match[1]!,
          span: {
            start: { offset: 0, line: 1, column: 1 },
            end: { offset: 0, line: 1, column: 1 },
          },
        });
      }

      return {
        imports,
        declarations: [],
      };
    };

    const graph = discoverModuleGraph(config, "Main", mockParse);

    // Check that all modules were discovered
    expect(graph.modules.size).toBe(3);
    expect(graph.modules.has("Main")).toBe(true);
    expect(graph.modules.has("Util")).toBe(true);
    expect(graph.modules.has("Base")).toBe(true);

    // Check topological order: Base should come before Util, Util before Main
    expect(graph.sortedModuleNames).toEqual(["Base", "Util", "Main"]);

    // Check dependencies
    const mainNode = graph.modules.get("Main");
    const utilNode = graph.modules.get("Util");
    const baseNode = graph.modules.get("Base");

    expect(mainNode?.dependencies.has("Util")).toBe(true);
    expect(utilNode?.dependencies.has("Base")).toBe(true);
    expect(baseNode?.dependencies.size).toBe(0);
  });

  test("detects circular dependencies", () => {
    const root = createWorkspace();
    const srcDir = path.join(root, "src");

    fs.mkdirSync(srcDir, { recursive: true });

    // Create a circular dependency:
    // A imports B
    // B imports A

    const aPath = path.join(srcDir, "A.vibe");
    const bPath = path.join(srcDir, "B.vibe");

    fs.writeFileSync(
      aPath,
      "module A exposing (..)\n\nimport B\n\nvalue = B.helper"
    );
    fs.writeFileSync(
      bPath,
      "module B exposing (..)\n\nimport A\n\nhelper = A.value"
    );

    const configPath = path.join(root, DEFAULT_CONFIG_NAME);
    fs.writeFileSync(
      configPath,
      JSON.stringify(
        {
          name: "ExampleApp",
          src: "src",
          dist: "dist",
          packages: [],
        },
        null,
        2
      )
    );

    const config = loadConfig({ path: configPath });

    const mockParse = (source: string): Program => {
      const importRegex =
        /^import\s+([A-Z][A-Za-z0-9_]*(?:\.[A-Z][A-Za-z0-9_]*)*)/gm;
      const imports = [];
      let match;

      while ((match = importRegex.exec(source)) !== null) {
        imports.push({
          moduleName: match[1]!,
          span: {
            start: { offset: 0, line: 1, column: 1 },
            end: { offset: 0, line: 1, column: 1 },
          },
        });
      }

      return {
        imports,
        declarations: [],
      };
    };

    expect(() => discoverModuleGraph(config, "A", mockParse)).toThrow(
      "Circular dependency"
    );
  });
});
