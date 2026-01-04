import { mkdir, rm } from "node:fs/promises";
import { spawn } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const extensionRoot = path.resolve(__dirname, "..");
const lspRoot = path.resolve(extensionRoot, "..", "lsp");
const stageDir = path.join(extensionRoot, ".vsce-stage");
const vsixPath = path.join(extensionRoot, "vibe-vscode.vsix");

async function run(
  command: string,
  args: string[],
  cwd: string
): Promise<void> {
  await new Promise<void>((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      stdio: "inherit",
      env: process.env,
    });
    child.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(
          new Error(`${command} ${args.join(" ")} exited with code ${code}`)
        );
      }
    });
    child.on("error", reject);
  });
}

async function copyIntoStage(): Promise<void> {
  await rm(stageDir, { recursive: true, force: true });
  await mkdir(stageDir, { recursive: true });
  const rsyncArgs = [
    "-a",
    "--copy-links",
    "--safe-links",
    "--delete",
    "--exclude",
    ".vsce-stage",
    "--exclude",
    "vibe-vscode.vsix",
    "./",
    `${stageDir}/`,
  ];
  await run("rsync", rsyncArgs, extensionRoot);
}

async function createPackage(): Promise<void> {
  console.log("▶ Building language server bundle");
  await run("bun", ["run", "build"], lspRoot);

  console.log("▶ Building extension sources");
  await run("bun", ["run", "build"], extensionRoot);

  console.log("▶ Preparing staging directory");
  await copyIntoStage();

  console.log("▶ Creating VSIX via vsce");
  const vsceBin = path.join(
    extensionRoot,
    "node_modules",
    "@vscode",
    "vsce",
    "vsce"
  );
  await run(
    "node",
    [vsceBin, "package", "--out", vsixPath, "--no-dependencies"],
    stageDir
  );

  console.log(`✔ VSIX created at ${vsixPath}`);
}

createPackage()
  .finally(async () => {
    await rm(stageDir, { recursive: true, force: true });
  })
  .catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
