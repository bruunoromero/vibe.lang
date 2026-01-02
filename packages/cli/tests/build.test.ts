import { expect, test } from "bun:test";
import { existsSync } from "node:fs";
import { rm } from "node:fs/promises";
import path from "node:path";

const REPO_ROOT = path.resolve(import.meta.dir, "..", "..", "..");
const CLI_ENTRY = path.resolve(REPO_ROOT, "packages/cli/index.ts");
const FIXTURE_PACKAGES = [
  path.resolve(REPO_ROOT, "packages/build-fixture-lib-a"),
  path.resolve(REPO_ROOT, "packages/build-fixture-lib-b"),
  path.resolve(REPO_ROOT, "packages/build-fixture-root"),
];
const BUILD_TARGET = path.resolve(REPO_ROOT, "packages/build-fixture-root");

const cleanupFixtureOutputs = async (): Promise<void> => {
  await Promise.all(
    FIXTURE_PACKAGES.map(async (pkgDir) => {
      const distDir = path.join(pkgDir, ".test-dist");
      await rm(distDir, { recursive: true, force: true });
    })
  );
};

const runBuild = async (): Promise<{
  exitCode: number;
  stdout: string;
  stderr: string;
}> => {
  const proc = Bun.spawn({
    cmd: ["bun", CLI_ENTRY, "build", BUILD_TARGET, "--force"],
    cwd: REPO_ROOT,
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

test.skip("build reports every failing package and still compiles dependents", async () => {
  await cleanupFixtureOutputs();
  try {
    const result = await runBuild();
    expect(result.exitCode).toBe(1);
    expect(result.stderr).toContain("@vibe/build-fixture-lib-a");
    expect(result.stderr).toContain("@vibe/build-fixture-lib-b");
    expect(result.stderr).toContain("Encountered 2 packages with errors");
    const rootOutput = path.join(BUILD_TARGET, ".test-dist", "main.js");
    expect(existsSync(rootOutput)).toBeTrue();
  } finally {
    await cleanupFixtureOutputs();
  }
});
