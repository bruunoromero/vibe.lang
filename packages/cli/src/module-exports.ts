import { parseSource } from "@vibe/parser";
import { NodeKind, type ListNode, type ProgramNode } from "@vibe/syntax";
import type { ModuleExportsLookup } from "@vibe/semantics";
import { existsSync, readFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import path from "node:path";
import type { PackageMetadata } from "./module-resolver";
import { parseVibeConfig, resolveVibePackageConfig } from "./workspace-config";
import { LANG_EXTENSION } from "./specifiers";

export class ModuleExportsTable implements ModuleExportsLookup {
  private readonly cache = new Map<string, readonly string[]>();

  getExports(moduleId: string): readonly string[] | undefined {
    return this.cache.get(path.resolve(moduleId));
  }

  has(moduleId: string): boolean {
    return this.cache.has(path.resolve(moduleId));
  }

  register(moduleId: string, symbols: readonly string[]): void {
    const normalized = path.resolve(moduleId);
    const unique = Array.from(new Set(symbols));
    this.cache.set(normalized, unique);
  }
}

export const extractTopLevelExports = (
  program: ProgramNode
): readonly string[] => {
  const exports: string[] = [];
  const seen = new Set<string>();
  for (const form of program.body) {
    if (!form || form.kind !== NodeKind.List) {
      continue;
    }
    const list = form as ListNode;
    const head = list.elements[0];
    if (!head || head.kind !== NodeKind.Symbol) {
      continue;
    }
    if (head.value !== "def" && head.value !== "defmacro") {
      continue;
    }
    const binding = list.elements[1];
    if (!binding || binding.kind !== NodeKind.Symbol) {
      continue;
    }
    if (seen.has(binding.value)) {
      continue;
    }
    seen.add(binding.value);
    exports.push(binding.value);
  }
  return exports;
};

export const discoverModuleExports = async (
  modulePath: string
): Promise<readonly string[] | undefined> => {
  if (!modulePath.endsWith(LANG_EXTENSION)) {
    return undefined;
  }
  try {
    const source = await readFile(modulePath, "utf8");
    const result = await parseSource(source);
    return extractTopLevelExports(result.program);
  } catch {
    return undefined;
  }
};

export const seedModuleExportsFromMetadata = async (
  metadata: PackageMetadata,
  table: ModuleExportsTable
): Promise<void> => {
  const resolved = resolveVibePackageConfig(metadata.rootDir, metadata.vibe);
  const moduleEntries = Object.values(resolved.modules);
  if (moduleEntries.length === 0) {
    return;
  }
  for (const entry of moduleEntries) {
    const modulePath = path.resolve(metadata.rootDir, entry);
    if (table.has(modulePath)) {
      continue;
    }
    const exports = await discoverModuleExports(modulePath);
    if (exports !== undefined) {
      table.register(modulePath, exports);
    }
  }
};

export const seedModuleExportsFromPackageJson = async (
  packageRoot: string,
  table: ModuleExportsTable
): Promise<void> => {
  const metadata = readPackageMetadata(packageRoot);
  if (!metadata) {
    return;
  }
  await seedModuleExportsFromMetadata(metadata, table);
};

const readPackageMetadata = (dir: string): PackageMetadata | null => {
  const manifestPath = path.join(dir, "package.json");
  if (!existsSync(manifestPath)) {
    return null;
  }
  try {
    const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
    const name =
      typeof manifest.name === "string" && manifest.name.length > 0
        ? manifest.name
        : path.basename(dir);
    const vibe = parseVibeConfig(manifest.vibe);
    return {
      name,
      rootDir: dir,
      ...(vibe ? { vibe } : {}),
    } satisfies PackageMetadata;
  } catch {
    return null;
  }
};
