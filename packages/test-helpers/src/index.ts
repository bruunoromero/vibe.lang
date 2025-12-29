import { readFileSync } from "node:fs";
import path from "node:path";
import type { ModuleResolver, ModuleExportsLookup } from "@vibe/semantics";
import {
  createProjectModuleResolver,
  findWorkspaceRoot,
  PackageRegistry,
} from "../../cli/src/module-resolver.ts";
import {
  ModuleExportsTable,
  seedModuleExportsFromMetadata,
} from "../../cli/src/module-exports.ts";

const PRELUDE_REL = path.join("packages", "prelude", "src", "prelude.lang");
const RUNTIME_SRC_REL = path.join("packages", "runtime", "src", "index.ts");

const extractPreludeExports = (
  text: string
): { name: string; kind: "var" | "macro" }[] => {
  const exports: { name: string; kind: "var" | "macro" }[] = [];
  const re = /\(\s*(defmacro|defmacrop|defn|defnp|def)\s+([^\s\)]+)/g;
  let m: RegExpExecArray | null;
  while ((m = re.exec(text))) {
    const kind = m[1].startsWith("defmacro") ? "macro" : "var";
    const name = m[2];
    exports.push({ name, kind });
  }
  return exports;
};

export const createDefaultTestResolvers = async (): Promise<{
  moduleResolver: ModuleResolver;
  moduleExports: ModuleExportsLookup;
  preludeModuleId: string;
}> => {
  const workspaceRoot = findWorkspaceRoot(process.cwd());
  const registry = PackageRegistry.create(workspaceRoot);
  const moduleResolver = createProjectModuleResolver({
    packageRegistry: registry,
    workspaceRoot,
  });

  const preludeModuleId = path.resolve(PRELUDE_REL);

  const table = new ModuleExportsTable();
  // Seed module exports for workspace packages (this will parse .lang and extract macros/vars)
  const pkgs = registry.getWorkspacePackages();
  for (const metadata of pkgs) {
    // Only seed packages that declare vibe sources — seedModuleExportsFromMetadata will noop otherwise
    // We call it for each package so prelude and others are prepared for tests
    // eslint-disable-next-line no-await-in-loop
    await seedModuleExportsFromMetadata(metadata, table, { moduleResolver });
  }

  const wrapper: ModuleExportsLookup = {
    getExports(moduleId: string) {
      if (!moduleId) return undefined;
      // Try direct match first
      const direct = table.getExports(moduleId);
      if (direct) return direct;
      // Fallback: match by filename (e.g., "/workspace/prelude.lang")
      const base = path.basename(moduleId);
      for (const candidate of table.getExports?.(preludeModuleId)
        ? [preludeModuleId]
        : []) {
        // noop: keep TS happy
      }
      for (const metadata of registry.getWorkspacePackages()) {
        const resolved = path.resolve(
          metadata.rootDir,
          metadata.vibe?.entry ?? ""
        );
        if (resolved && path.basename(resolved) === base) {
          const found = table.getExports(resolved);
          if (found) return found;
        }
      }
      // As last resort, search all cached exports for matching basename
      for (const pkg of registry.getWorkspacePackages()) {
        const resolved = path.resolve(pkg.rootDir, pkg.vibe?.entry ?? "");
        const found = table.getExports(resolved);
        if (found && path.basename(resolved) === base) return found;
      }
      return undefined;
    },
  };

  return { moduleResolver, moduleExports: wrapper, preludeModuleId };
};

export const getRuntimeSourceImportSpecifier = (): string => {
  // return an absolute path that points at the runtime source entry so tests and generated modules import the same file
  return path.resolve(RUNTIME_SRC_REL);
};
