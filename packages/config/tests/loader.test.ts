import { afterEach, expect, test } from "bun:test";
import { mkdtemp, rm, writeFile, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import {
  clearVibeConfigCache,
  findVibeConfigInDir,
  findVibeConfigPath,
  loadNearestVibeConfig,
  loadVibeConfigForDir,
} from "../src";

afterEach(() => {
  clearVibeConfigCache();
});

test("loads nearest vibe.config.ts when walking parents", async () => {
  const root = await mkdtemp(path.join(tmpdir(), "vibe-config-tests-"));
  const pkgDir = path.join(root, "packages", "app");
  await mkdir(pkgDir, { recursive: true });
  const configPath = path.join(root, "vibe.config.ts");
  await writeFile(
    configPath,
    [
      "export default {",
      "  package: {",
      "    sources: ['./src'],",
      "    outDir: './dist',",
      "    entry: './src/main.lang',",
      "  },",
      "};",
      "",
    ].join("\n"),
    "utf8"
  );
  try {
    const located = findVibeConfigPath(pkgDir);
    expect(located).toBe(configPath);
    const loaded = loadNearestVibeConfig(pkgDir);
    expect(loaded?.config.package?.sources).toEqual(["./src"]);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});

test("finds config only within the provided directory when searchParents is false", async () => {
  const root = await mkdtemp(path.join(tmpdir(), "vibe-config-tests-"));
  const pkgDir = path.join(root, "packages", "app");
  await mkdir(pkgDir, { recursive: true });
  const configPath = path.join(pkgDir, "vibe.config.js");
  await writeFile(
    configPath,
    ["export default {", "  package: { sources: ['./src'] },", "};", ""].join(
      "\n"
    ),
    "utf8"
  );
  try {
    expect(findVibeConfigInDir(pkgDir)).toBe(configPath);
    expect(findVibeConfigPath(pkgDir, { searchParents: false })).toBe(
      configPath
    );
    const loaded = loadVibeConfigForDir(pkgDir);
    expect(loaded?.config.package?.sources).toEqual(["./src"]);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});

test("supports defineFormatterSpec helper when executing configs", async () => {
  const root = await mkdtemp(path.join(tmpdir(), "vibe-config-tests-"));
  try {
    const configPath = path.join(root, "vibe.config.ts");
    await writeFile(
      configPath,
      [
        "import { defineConfig, defineFormatterSpec } from '@vibe/config';",
        "const handler = defineFormatterSpec({ inlineHeadArgCount: 2 });",
        "export default defineConfig({",
        "  formatter: {",
        "    defhandler: handler,",
        "  },",
        "});",
        "",
      ].join("\n"),
      "utf8"
    );
    const loaded = loadVibeConfigForDir(root);
    const formatter = loaded?.config.formatter;
    expect(formatter).toBeDefined();
    const entry = formatter && formatter.defhandler;
    expect(typeof entry).toBe("function");
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
