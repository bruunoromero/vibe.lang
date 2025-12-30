import { describe, expect, test } from "bun:test";
import path from "node:path";
import { findWorkspaceRoot, PackageRegistry } from "@vibe/module-resolver";

const WORKSPACE_ROOT = findWorkspaceRoot(process.cwd());

if (!WORKSPACE_ROOT) {
  throw new Error("Workspace root not found");
}

const PRELUDE_ENTRY = path.join(
  WORKSPACE_ROOT,
  "packages",
  "prelude",
  "src",
  "prelude.lang"
);

describe("package module resolver", () => {
  test("resolves bare package imports via vibe.entry", () => {
    const registry = PackageRegistry.create(WORKSPACE_ROOT);
    const result = registry.resolveModule("@vibe/prelude", WORKSPACE_ROOT);

    expect(result.ok).toBe(true);
    expect(result.moduleId).toBe(PRELUDE_ENTRY);
  });

  test("maps subpaths to files under vibe.sources", () => {
    const registry = PackageRegistry.create(WORKSPACE_ROOT);
    const result = registry.resolveModule(
      "@vibe/example-app/math",
      WORKSPACE_ROOT
    );

    expect(result.ok).toBe(true);
    expect(result.moduleId).toBe(
      path.join(WORKSPACE_ROOT, "packages/example-app/src/math.lang")
    );
  });
});
