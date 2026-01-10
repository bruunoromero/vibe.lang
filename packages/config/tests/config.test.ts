import { describe, expect, test } from "bun:test";
import fs from "node:fs";
import path from "node:path";
import { tmpdir } from "node:os";
import { loadConfig, DEFAULT_CONFIG_NAME } from "../src/index.ts";

function createTempDir(): string {
  return fs.mkdtempSync(path.join(tmpdir(), "vibe-config-"));
}

describe("loadConfig", () => {
  test("loads vibe.json and resolves absolute paths", () => {
    const root = createTempDir();
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

    const config = loadConfig({ cwd: root });

    expect(config.name).toBe("ExampleApp");
    expect(config.configPath).toBe(configPath);
    expect(config.srcDir).toBe(path.join(root, "src"));
    expect(config.distDir).toBe(path.join(root, "dist"));
    expect(config.packages).toEqual(["@vibe/prelude"]);
  });

  test("throws when required fields are missing", () => {
    const root = createTempDir();
    const configPath = path.join(root, DEFAULT_CONFIG_NAME);
    fs.writeFileSync(configPath, JSON.stringify({ name: "ExampleApp" }));

    expect(() => loadConfig({ cwd: root })).toThrow("src");
  });
});
