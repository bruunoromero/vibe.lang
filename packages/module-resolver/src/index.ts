import fs from "node:fs";
import path from "node:path";
import {
  DEFAULT_CONFIG_NAME,
  loadConfig,
  type ResolvedVibeConfig,
} from "@vibe/config";
import type { Program } from "@vibe/syntax";

export interface ResolveModuleInput {
  config: ResolvedVibeConfig;
  moduleName: string;
  preferDist?: boolean;
}

export interface ResolvedModule {
  moduleName: string;
  packageName: string;
  filePath: string;
}

/**
 * Represents a module with its dependencies and parsed AST.
 * Used for building the module dependency graph.
 */
export interface ModuleNode {
  moduleName: string;
  packageName: string;
  filePath: string;
  source: string;
  ast: Program;
  /** Module names this module imports */
  dependencies: Set<string>;
}

/**
 * Result of discovering and sorting modules topologically.
 */
export interface ModuleGraph {
  /** Map from module name to module node */
  modules: Map<string, ModuleNode>;
  /** Module names in topological order (dependencies before dependents) */
  sortedModuleNames: string[];
}

export function resolveModule(input: ResolveModuleInput): ResolvedModule {
  const { config, moduleName, preferDist = false } = input;
  const relativePath = moduleNameToRelativePath(moduleName);

  const candidates: Array<{ packageName: string; baseDir: string }> = [
    {
      packageName: config.name,
      baseDir: preferDist ? config.distDir : config.srcDir,
    },
  ];

  for (const pkgName of config.packages) {
    const pkgRoot = resolvePackageRoot(pkgName, config.rootDir);
    if (!pkgRoot) {
      continue;
    }

    const pkgConfigPath = path.join(pkgRoot, DEFAULT_CONFIG_NAME);
    let pkgSrcDir = path.join(pkgRoot, "src");
    let pkgDistDir = path.join(pkgRoot, "dist");
    // Use the vibe.json name if available, otherwise fall back to npm package name
    let vibePackageName = pkgName;

    if (fs.existsSync(pkgConfigPath)) {
      const pkgConfig = loadConfig({ path: pkgConfigPath });
      pkgSrcDir = pkgConfig.srcDir;
      pkgDistDir = pkgConfig.distDir;
      vibePackageName = pkgConfig.name;
    }

    candidates.push({
      packageName: vibePackageName,
      baseDir: preferDist ? pkgDistDir : pkgSrcDir,
    });
  }

  for (const candidate of candidates) {
    const fullPath = path.join(candidate.baseDir, relativePath);
    if (fs.existsSync(fullPath)) {
      return {
        moduleName,
        packageName: candidate.packageName,
        filePath: fullPath,
      };
    }
  }

  throw new Error(`Module "${moduleName}" not found in configured packages`);
}

export function moduleNameToRelativePath(moduleName: string): string {
  const segments = moduleName.split(".");
  if (segments.length === 0) {
    throw new Error("Module name must not be empty");
  }

  for (const segment of segments) {
    if (!/^[A-Z][A-Za-z0-9_]*$/.test(segment)) {
      throw new Error(
        `Invalid module segment "${segment}" in module name "${moduleName}"`
      );
    }
  }

  return path.join(...segments) + ".vibe";
}

function resolvePackageRoot(pkgName: string, startDir: string): string | null {
  const fromNodeModules = findUpward(startDir, (dir) => {
    const candidate = path.join(dir, "node_modules", pkgName);
    return fs.existsSync(path.join(candidate, "package.json"))
      ? candidate
      : null;
  });
  if (fromNodeModules) {
    return fromNodeModules;
  }

  const workspaceRoot = findWorkspaceRoot(startDir);
  if (workspaceRoot) {
    const folderName = pkgName.includes("/")
      ? pkgName.split("/").at(-1) ?? pkgName
      : pkgName;
    const candidate = path.join(workspaceRoot, "packages", folderName);
    if (fs.existsSync(path.join(candidate, "package.json"))) {
      return candidate;
    }
  }

  return null;
}

function findWorkspaceRoot(startDir: string): string | null {
  return findUpward(startDir, (dir) => {
    const pkgDir = path.join(dir, "packages");
    return fs.existsSync(pkgDir) && fs.statSync(pkgDir).isDirectory()
      ? dir
      : null;
  });
}

function findUpward(
  startDir: string,
  matcher: (dir: string) => string | null
): string | null {
  let dir = path.resolve(startDir);
  while (true) {
    const match = matcher(dir);
    if (match) {
      return match;
    }

    const parent = path.dirname(dir);
    if (parent === dir) {
      return null;
    }
    dir = parent;
  }
}

/**
 * Discover all modules transitively imported from the entry module,
 * parse them, and return them sorted in topological order.
 *
 * This ensures that dependencies are always analyzed before modules that depend on them.
 *
 * @param config - The resolved Vibe configuration
 * @param entryModuleName - The entry point module to start discovery from
 * @param parseFunction - Function to parse source code into an AST
 * @param preferDist - Whether to prefer dist over src directories
 * @returns ModuleGraph with all discovered modules and topological ordering
 * @throws Error if a module cannot be found or if there's a circular dependency
 */
export function discoverModuleGraph(
  config: ResolvedVibeConfig,
  entryModuleName: string,
  parseFunction: (source: string) => Program,
  preferDist = false,
  injectPrelude = true
): ModuleGraph {
  const modules = new Map<string, ModuleNode>();
  const visiting = new Set<string>(); // For cycle detection

  // Recursively discover all dependencies
  function discoverModule(moduleName: string): void {
    // Already processed
    if (modules.has(moduleName)) {
      return;
    }

    // Cycle detection: if we're currently visiting this module, we have a cycle
    if (visiting.has(moduleName)) {
      throw new Error(
        `Circular dependency detected: ${[...visiting, moduleName].join(
          " -> "
        )}`
      );
    }

    visiting.add(moduleName);

    // Resolve and read module
    const resolved = resolveModule({ config, moduleName, preferDist });
    const source = fs.readFileSync(resolved.filePath, "utf8");

    // Parse to get imports
    const ast = parseFunction(source);

    // Extract dependency names
    const dependencies = new Set<string>();
    for (const imp of ast.imports) {
      dependencies.add(imp.moduleName);
    }

    // Auto-inject Vibe dependency if enabled and this is not Vibe itself
    const isPreludeModule = moduleName === "Vibe";
    const hasExplicitPreludeImport = ast.imports?.some(
      (imp) => imp.moduleName === "Vibe"
    );

    if (injectPrelude && !isPreludeModule && !hasExplicitPreludeImport) {
      // Try to add Vibe, but don't fail if it can't be found
      // This allows using builtin types without requiring Vibe to be available
      try {
        resolveModule({ config, moduleName: "Vibe", preferDist });
        dependencies.add("Vibe");
      } catch (error) {
        // Vibe not found - this is OK, builtin types are available without it
        // Silently skip adding Prelude to dependencies
      }
    }

    // Recursively discover dependencies BEFORE storing this module
    // This ensures we detect cycles properly
    for (const depName of dependencies) {
      discoverModule(depName);
    }

    // Store module node AFTER processing dependencies
    modules.set(moduleName, {
      moduleName,
      packageName: resolved.packageName,
      filePath: resolved.filePath,
      source,
      ast,
      dependencies,
    });

    visiting.delete(moduleName);
  }

  // Start discovery from entry module
  discoverModule(entryModuleName);

  // Perform topological sort
  const sortedModuleNames = topologicalSort(modules);

  return {
    modules,
    sortedModuleNames,
  };
}

/**
 * Discover all .vibe source files in a directory recursively.
 *
 * @param srcDir - The source directory to scan
 * @returns Array of module names found
 */
export function discoverSourceModules(srcDir: string): string[] {
  const modules: string[] = [];

  function scan(dir: string, prefix: string): void {
    if (!fs.existsSync(dir)) {
      return;
    }

    const entries = fs.readdirSync(dir, { withFileTypes: true });

    for (const entry of entries) {
      if (entry.isDirectory()) {
        // Recurse into subdirectories
        const newPrefix = prefix ? `${prefix}.${entry.name}` : entry.name;
        scan(path.join(dir, entry.name), newPrefix);
      } else if (entry.isFile() && entry.name.endsWith(".vibe")) {
        // Convert filename to module name
        const baseName = entry.name.slice(0, -5); // Remove .vibe extension
        const moduleName = prefix ? `${prefix}.${baseName}` : baseName;
        modules.push(moduleName);
      }
    }
  }

  scan(srcDir, "");
  return modules;
}

/**
 * Discover all modules from a package's source directory and their dependencies.
 * Unlike discoverModuleGraph which starts from a single entry point, this discovers
 * ALL source modules in the package and their transitive dependencies.
 *
 * @param config - The resolved Vibe configuration
 * @param parseFunction - Function to parse source code into an AST
 * @returns ModuleGraph with all discovered modules and topological ordering
 */
export function discoverAllModules(
  config: ResolvedVibeConfig,
  parseFunction: (source: string) => Program
): ModuleGraph {
  // First, discover all source modules in the config's source directory
  const sourceModules = discoverSourceModules(config.srcDir);

  // Now build the graph starting from all source modules
  const modules = new Map<string, ModuleNode>();
  const visiting = new Set<string>();

  function discoverModule(moduleName: string): void {
    if (modules.has(moduleName)) {
      return;
    }

    if (visiting.has(moduleName)) {
      throw new Error(
        `Circular dependency detected: ${[...visiting, moduleName].join(
          " -> "
        )}`
      );
    }

    visiting.add(moduleName);

    // Resolve and read module
    const resolved = resolveModule({ config, moduleName });
    const source = fs.readFileSync(resolved.filePath, "utf8");
    const ast = parseFunction(source);

    // Extract dependencies
    const dependencies = new Set<string>();
    for (const imp of ast.imports) {
      dependencies.add(imp.moduleName);
    }

    // Auto-inject Vibe dependency if this is not Vibe itself
    const isPreludeModule = moduleName === "Vibe";
    const hasExplicitPreludeImport = ast.imports?.some(
      (imp) => imp.moduleName === "Vibe"
    );

    if (!isPreludeModule && !hasExplicitPreludeImport) {
      try {
        resolveModule({ config, moduleName: "Vibe" });
        dependencies.add("Vibe");
      } catch {
        // Vibe not found - OK, builtin types are available
      }
    }

    // Recursively discover dependencies
    for (const depName of dependencies) {
      discoverModule(depName);
    }

    // Store module node
    modules.set(moduleName, {
      moduleName,
      packageName: resolved.packageName,
      filePath: resolved.filePath,
      source,
      ast,
      dependencies,
    });

    visiting.delete(moduleName);
  }

  // Discover all source modules and their dependencies
  for (const moduleName of sourceModules) {
    discoverModule(moduleName);
  }

  // Topologically sort
  const sortedModuleNames = topologicalSort(modules);

  return {
    modules,
    sortedModuleNames,
  };
}

/**
 * Perform topological sort on the module graph using depth-first search.
 *
 * @param modules - Map of all discovered modules
 * @returns Array of module names in topological order (dependencies first)
 */
function topologicalSort(modules: Map<string, ModuleNode>): string[] {
  const sorted: string[] = [];
  const visited = new Set<string>();

  function visit(moduleName: string): void {
    if (visited.has(moduleName)) {
      return;
    }

    visited.add(moduleName);

    const node = modules.get(moduleName);
    if (!node) {
      throw new Error(`Module "${moduleName}" not found in graph`);
    }

    // Visit all dependencies first
    for (const depName of node.dependencies) {
      visit(depName);
    }

    // Add this module after its dependencies
    sorted.push(moduleName);
  }

  // Visit all modules
  for (const moduleName of modules.keys()) {
    visit(moduleName);
  }

  return sorted;
}
