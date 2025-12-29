import { describe, expect, test } from "bun:test";
import path from "node:path";
import {
  findWorkspaceRoot,
  PackageRegistry,
  buildPackageGraph,
} from "@vibe/module-resolver";

const WORKSPACE_ROOT = findWorkspaceRoot(process.cwd());

describe("package graph", () => {
  test("topologically sorts example-app dependencies", () => {
    if (!WORKSPACE_ROOT) {
      throw new Error("Workspace root not found");
    }
    const registry = PackageRegistry.create(WORKSPACE_ROOT);
    const exampleAppDir = path.resolve(import.meta.dir, "../../example-app");
    const graph = buildPackageGraph(exampleAppDir, registry);
    const orderedNames = graph.topoOrder.map((node) => node.name);

    expect(graph.entry.name).toBe("@vibe/example-app");
    expect(orderedNames).toEqual(["@vibe/prelude", "@vibe/example-app"]);
  });

  test("skips packages without vibe sources from build targets", () => {
    if (!WORKSPACE_ROOT) {
      throw new Error("Workspace root not found");
    }
    const registry = PackageRegistry.create(WORKSPACE_ROOT);
    const runtimeDir = path.resolve(import.meta.dir, "../../runtime");
    const graph = buildPackageGraph(runtimeDir, registry);
    expect(graph.topoOrder.length).toBe(0);
    expect(graph.buildTargets.length).toBe(0);
  });
});
