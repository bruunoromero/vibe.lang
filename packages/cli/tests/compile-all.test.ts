import { expect, test } from "bun:test";
import { existsSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

const REPO_ROOT = path.resolve(import.meta.dir, "..", "..", "..");
const FIXTURE_DIR = path.resolve(
  import.meta.dir,
  "fixtures",
  "diagnostic-recovery"
);

const runCli = async (
  outDir: string
): Promise<{
  exitCode: number;
  stdout: string;
  stderr: string;
}> => {
  const cliEntry = path.resolve(REPO_ROOT, "packages/cli/index.ts");
  const proc = Bun.spawn({
    cmd: ["bun", cliEntry, "compile-all", FIXTURE_DIR, "--out-dir", outDir],
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

test("compile-all continues after parse failures", async () => {
  const outDir = await mkdtemp(path.join(tmpdir(), "vibe-cli-test-"));
  try {
    const result = await runCli(outDir);
    expect(result.exitCode).toBe(1);
    const goodOutput = path.join(outDir, "good.js");
    const badOutput = path.join(outDir, "bad.js");
    expect(existsSync(goodOutput)).toBeTrue();
    expect(existsSync(badOutput)).toBeFalse();
    expect(result.stderr).toContain("bad.lang");
  } finally {
    await rm(outDir, { recursive: true, force: true });
  }
});
