import { expect, test } from "bun:test";
import { formatSource } from "@vibe/formatter";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

const REPO_ROOT = path.resolve(import.meta.dir, "..", "..", "..");
const CLI_ENTRY = path.resolve(REPO_ROOT, "packages/cli/index.ts");
const FIXTURE_SOURCE = path.resolve(
  REPO_ROOT,
  "packages/cli/tests/fixtures/fmt/messy.lang"
);

const runFmt = async (
  args: readonly string[],
  options: { readonly cwd?: string } = {}
): Promise<{ exitCode: number; stdout: string; stderr: string }> => {
  const proc = Bun.spawn({
    cmd: ["bun", CLI_ENTRY, "fmt", ...args],
    cwd: options.cwd ?? REPO_ROOT,
    stdout: "pipe",
    stderr: "pipe",
  });
  const [stdout, stderr, exitCode] = await Promise.all([
    new Response(proc.stdout).text(),
    new Response(proc.stderr).text(),
    proc.exited,
  ]);
  return { exitCode, stdout, stderr };
};

test("fmt rewrites files by default", async () => {
  const tmpDir = await mkdtemp(path.join(tmpdir(), "vibe-fmt-"));
  const targetFile = path.join(tmpDir, "messy.lang");
  const original = await readFile(FIXTURE_SOURCE, "utf8");
  await writeFile(targetFile, original);
  try {
    const result = await runFmt([targetFile]);
    expect(result.exitCode).toBe(0);
    const formatted = await readFile(targetFile, "utf8");
    const expected = await formatSource(original);
    expect(expected.ok).toBeTrue();
    expect(formatted).toBe(expected.formatted);
  } finally {
    await rm(tmpDir, { recursive: true, force: true });
  }
});

test("fmt --check flags files without rewriting", async () => {
  const tmpDir = await mkdtemp(path.join(tmpdir(), "vibe-fmt-check-"));
  const targetFile = path.join(tmpDir, "messy.lang");
  const original = await readFile(FIXTURE_SOURCE, "utf8");
  await writeFile(targetFile, original);
  try {
    const result = await runFmt([targetFile, "--check"]);
    expect(result.exitCode).toBe(1);
    const after = await readFile(targetFile, "utf8");
    expect(after).toBe(original);
    expect(result.stderr).toContain("not formatted");
  } finally {
    await rm(tmpDir, { recursive: true, force: true });
  }
});

test("fmt honors vibe.config.js formatter specs", async () => {
  const tmpDir = await mkdtemp(path.join(tmpdir(), "vibe-fmt-config-"));
  const targetFile = path.join(tmpDir, "handler.lang");
  const configPath = path.join(tmpDir, "vibe.config.js");
  const source =
    '(defhandler greet (name) (println "Hi" name) (println "Bye" name))\n' +
    '(defhandler+ greet (name) (println "Hi" name) (println "Later" name))';
  await Promise.all([
    writeFile(
      configPath,
      [
        "export default {",
        "  formatter: {",
        "    defhandler: {",
        "      inlineHeadArgCount: 2,",
        "      vectorArgumentIndices: [2],",
        "    },",
        '    "defhandler+": {',
        "      inlineHeadArgCount: 2,",
        "      vectorArgumentIndices: [2],",
        "    },",
        "  },",
        "};",
        "",
      ].join("\n")
    ),
    writeFile(targetFile, source),
  ]);
  try {
    const result = await runFmt([targetFile], { cwd: tmpDir });
    expect(result.exitCode).toBe(0);
    const formatted = await readFile(targetFile, "utf8");
    expect(formatted).toBe(
      "(defhandler greet [name]\n" +
        '  (println "Hi" name)\n' +
        '  (println "Bye" name))\n\n' +
        "(defhandler+ greet [name]\n" +
        '  (println "Hi" name)\n' +
        '  (println "Later" name))\n'
    );
  } finally {
    await rm(tmpDir, { recursive: true, force: true });
  }
});
