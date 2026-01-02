import path from "node:path";
import type { Connection } from "vscode-languageserver";
import {
  PackageRegistry,
  ModuleExportsTable,
  seedModuleExportsFromMetadata,
  createProjectModuleResolver,
  findWorkspaceRoot,
  type PackageMetadata,
} from "@vibe/module-resolver";
import type { ModuleResolver } from "@vibe/semantics";
import {
  loadFormatterFormConfig,
  type FormFormattingConfig,
} from "@vibe/formatter";

const ORPHAN_WORKSPACE_KEY = "__orphan__";

export interface WorkspaceContext {
  readonly rootDir: string | null;
  readonly packageRegistry: PackageRegistry;
  readonly moduleResolver: ModuleResolver;
  readonly moduleExports: ModuleExportsTable;
  readonly knownModules: Set<string>;
}

export class WorkspaceManager {
  private readonly contexts = new Map<string, Promise<WorkspaceContext>>();
  private readonly formatterCache = new Map<
    string,
    FormFormattingConfig | undefined
  >();

  constructor(private readonly connection: Connection) {}

  async getContextForFile(filePath: string): Promise<WorkspaceContext> {
    const rootDir = findWorkspaceRoot(path.dirname(filePath)) ?? null;
    const key = rootDir ?? ORPHAN_WORKSPACE_KEY;
    const existing = this.contexts.get(key);
    if (existing) {
      return existing;
    }
    const pending = this.initializeContext(rootDir);
    this.contexts.set(key, pending);
    return pending;
  }

  async getFormatterConfig(
    dir: string
  ): Promise<FormFormattingConfig | undefined> {
    const normalized = path.resolve(dir);
    if (this.formatterCache.has(normalized)) {
      return this.formatterCache.get(normalized);
    }
    const config = loadFormatterFormConfig(normalized);
    this.formatterCache.set(normalized, config);
    return config;
  }

  private async initializeContext(
    rootDir: string | null
  ): Promise<WorkspaceContext> {
    const packageRegistry = PackageRegistry.create(rootDir);
    const moduleExports = new ModuleExportsTable();
    const knownModules = new Set<string>();
    const moduleResolver = createProjectModuleResolver({
      packageRegistry,
      workspaceRoot: rootDir,
      knownModules,
    });
    const context: WorkspaceContext = {
      rootDir,
      packageRegistry,
      moduleResolver,
      moduleExports,
      knownModules,
    };

    const packages = packageRegistry.getWorkspacePackages();
    await Promise.all(
      packages.map((metadata) =>
        this.seedExportsForPackage(metadata, moduleExports, moduleResolver)
      )
    );

    return context;
  }

  private async seedExportsForPackage(
    metadata: PackageMetadata,
    moduleExports: ModuleExportsTable,
    moduleResolver: ModuleResolver
  ): Promise<void> {
    try {
      await seedModuleExportsFromMetadata(metadata, moduleExports, {
        moduleResolver,
      });
    } catch (error) {
      this.connection.console.error(
        `[vibe][lsp] Failed to seed exports for ${metadata.name}: ${String(
          error
        )}`
      );
    }
  }
}
