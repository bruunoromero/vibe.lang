import { describe, expect, test } from "bun:test";
import fs from "node:fs";
import path from "node:path";
import { tmpdir } from "node:os";
import { DEFAULT_CONFIG_NAME } from "@vibe/config";
import { run } from "../src/index.ts";

function createWorkspace(): string {
  const root = fs.mkdtempSync(path.join(tmpdir(), "vibe-cli-"));
  const srcDir = path.join(root, "src");
  fs.mkdirSync(srcDir, { recursive: true });

  fs.writeFileSync(
    path.join(root, DEFAULT_CONFIG_NAME),
    JSON.stringify(
      {
        name: "Main",
        src: "src",
        dist: "dist",
        packages: [],
      },
      null,
      2
    )
  );

  fs.writeFileSync(
    path.join(srcDir, "Main.vibe"),
    ["module Main exposing (..)", "", "value = 1", ""].join("\n")
  );

  return root;
}

function createCollector(): NodeJS.WritableStream {
  let buffer = "";
  return {
    write(chunk: unknown) {
      buffer += String(chunk);
      return true;
    },
    toString() {
      return buffer;
    },
  } as unknown as NodeJS.WritableStream;
}

describe("cli", () => {
  test("tokenizes the default module from config", async () => {
    const root = createWorkspace();
    const stdout = createCollector();
    const stderr = createCollector();

    const exitCode = await run(["tokenize"], { cwd: root, stdout, stderr });

    expect(exitCode).toBe(0);
    const tokens = JSON.parse(stdout.toString());
    expect(Array.isArray(tokens)).toBe(true);
    expect(stderr.toString()).toBe("");
  });

  test("parses a module and prints the AST", async () => {
    const root = createWorkspace();
    const stdout = createCollector();
    const stderr = createCollector();

    const exitCode = await run(["parse"], { cwd: root, stdout, stderr });

    expect(exitCode).toBe(0);
    const ast = JSON.parse(stdout.toString());
    expect(ast.module?.name).toBe("Main");
    expect(stderr.toString()).toBe("");
  });

  test("analyzes a module for type errors", async () => {
    const root = createWorkspace();
    const stdout = createCollector();
    const stderr = createCollector();

    const exitCode = await run(["analyze"], { cwd: root, stdout, stderr });

    expect(exitCode).toBe(0);
    const semantic = JSON.parse(stdout.toString());
    expect(semantic.values).toBeDefined();
    expect(semantic.values.value).toBeDefined();
    expect(stderr.toString()).toBe("");
  });

  test("reports errors with source locations", async () => {
    const root = createWorkspace();
    const srcDir = path.join(root, "src");

    // Create a file with an undefined function
    fs.writeFileSync(
      path.join(srcDir, "Error.vibe"),
      ["module Error exposing (..)", "", "value = unknownFunc 1", ""].join("\n")
    );

    const stdout = createCollector();
    const stderr = createCollector();

    const exitCode = await run(["analyze", "Error"], {
      cwd: root,
      stdout,
      stderr,
    });

    expect(exitCode).toBe(1);
    const errorOutput = stderr.toString();
    expect(errorOutput).toContain("Error.vibe");
    expect(errorOutput).toContain("error:");
    expect(errorOutput).toContain("unknownFunc");
    expect(errorOutput).toContain("^");
  });
});
