import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import type { VibePackageConfig } from "@vibe/config";
import type { PackageRegistry } from "./module-resolver";
import { loadPackageVibeConfig } from "./workspace-config";

const resolveFromModule = createRequire(import.meta.url);

export interface PackageGraphNode {
  readonly name: string;
  readonly rootDir: string;
  readonly manifestPath: string;
  readonly dependencies: readonly string[];
  readonly vibe?: VibePackageConfig;
}

export interface PackageBuildGraph {
  readonly entry: PackageGraphNode;
  readonly nodes: readonly PackageGraphNode[];
  readonly buildTargets: readonly PackageGraphNode[];
  readonly topoOrder: readonly PackageGraphNode[];
}

interface QueueEntry {
  readonly dir: string;
  readonly fromDir?: string;
}

export const buildPackageGraph = (
  entryDir: string,
  registry: PackageRegistry
): PackageBuildGraph => {
  const normalizedEntry = path.resolve(entryDir);
  const visited = new Map<string, PackageGraphNode>();
  const queue: QueueEntry[] = [{ dir: normalizedEntry }];
  let entryNode: PackageGraphNode | null = null;

  while (queue.length > 0) {
    const current = queue.shift();
    if (!current) {
      break;
    }
    const manifest = readPackageManifest(current.dir);
    if (visited.has(manifest.name)) {
      continue;
    }
    visited.set(manifest.name, manifest);
    if (!entryNode) {
      entryNode = manifest;
    }
    for (const depName of manifest.dependencies) {
      if (visited.has(depName)) {
        continue;
      }
      const dependencyDir = resolveDependencyDir(
        depName,
        manifest.rootDir,
        registry
      );
      if (!dependencyDir) {
        throw new Error(
          `Unable to locate dependency ${depName} required by ${manifest.name}`
        );
      }
      queue.push({ dir: dependencyDir, fromDir: manifest.rootDir });
    }
  }

  if (!entryNode) {
    throw new Error(
      `Could not find a package manifest under ${normalizedEntry}`
    );
  }

  const buildTargets = [...visited.values()].filter((node) =>
    Boolean(node.vibe?.sources && node.vibe.sources.length > 0)
  );
  const topoOrder = buildTopologicalOrder(buildTargets);

  return {
    entry: entryNode,
    nodes: [...visited.values()],
    buildTargets,
    topoOrder,
  };
};

const readPackageManifest = (dir: string): PackageGraphNode => {
  const manifestPath = path.join(dir, "package.json");
  if (!existsSync(manifestPath)) {
    throw new Error(`Missing package.json at ${manifestPath}`);
  }

  let manifest: Record<string, unknown>;
  try {
    manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
  } catch (error) {
    throw new Error(`Failed to parse ${manifestPath}: ${String(error)}`);
  }

  const name = manifest.name;
  if (typeof name !== "string" || name.length === 0) {
    throw new Error(`Package at ${manifestPath} is missing a name`);
  }

  const dependencies = extractDependencyNames(manifest.dependencies);
  const vibe = loadPackageVibeConfig(dir);

  return {
    name,
    rootDir: dir,
    manifestPath,
    dependencies,
    ...(vibe ? { vibe } : {}),
  };
};

const extractDependencyNames = (value: unknown): readonly string[] => {
  if (!value || typeof value !== "object") {
    return [];
  }
  return Object.keys(value as Record<string, unknown>).filter(
    (key) => typeof key === "string" && key.length > 0
  );
};

const resolveDependencyDir = (
  packageName: string,
  fromDir: string,
  registry: PackageRegistry
): string | null => {
  const metadata = registry.getPackageMetadata(packageName, fromDir);
  if (metadata) {
    return metadata.rootDir;
  }
  try {
    const manifestPath = resolveFromModule.resolve(
      `${packageName}/package.json`,
      {
        paths: [fromDir],
      }
    );
    return path.dirname(manifestPath);
  } catch {
    return null;
  }
};

const buildTopologicalOrder = (
  nodes: readonly PackageGraphNode[]
): PackageGraphNode[] => {
  const nodeMap = new Map(nodes.map((node) => [node.name, node]));
  const incoming = new Map<string, number>();
  const adjacency = new Map<string, Set<string>>();

  for (const node of nodes) {
    incoming.set(node.name, 0);
    adjacency.set(node.name, new Set());
  }

  for (const node of nodes) {
    for (const dependency of node.dependencies) {
      if (!nodeMap.has(dependency)) {
        continue;
      }
      adjacency.get(dependency)?.add(node.name);
      incoming.set(node.name, (incoming.get(node.name) ?? 0) + 1);
    }
  }

  const queue = [...incoming.entries()]
    .filter(([, count]) => count === 0)
    .map(([name]) => name);
  const order: PackageGraphNode[] = [];

  while (queue.length > 0) {
    const name = queue.shift();
    if (!name) {
      break;
    }

    const node = nodeMap.get(name);
    if (!node) {
      continue;
    }

    order.push(node);
    for (const dependent of adjacency.get(name) ?? []) {
      const nextCount = (incoming.get(dependent) ?? 0) - 1;
      incoming.set(dependent, nextCount);
      if (nextCount === 0) {
        queue.push(dependent);
      }
    }
  }

  if (order.length !== nodes.length) {
    const remaining = [...incoming.entries()]
      .filter(([, count]) => count > 0)
      .map(([name]) => name)
      .join(", ");
    throw new Error(
      `Detected a dependency cycle among Vibe packages: ${remaining}`
    );
  }

  return order;
};
