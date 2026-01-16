import fs from "node:fs";
import path from "node:path";
import {
  DEFAULT_CONFIG_NAME,
  loadConfig,
  type ResolvedVibeConfig,
} from "@vibe/config";
import type { Program, OperatorRegistry } from "@vibe/syntax";

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
  /** Operator precedence/associativity declarations from this module */
  operatorRegistry: OperatorRegistry;
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
 * Options for module discovery functions.
 */
export interface DiscoverOptions {
  /**
   * Function to extract infix declarations from source code.
   * Used in the first pass to build operator registries.
   */
  collectInfixDeclarations: (source: string) => { registry: OperatorRegistry };

  /**
   * Function to parse source code into an AST.
   * May optionally accept an operator registry for precedence-aware parsing.
   */
  parseFunction: (
    source: string,
    operatorRegistry?: OperatorRegistry
  ) => Program;

  /**
   * Whether to prefer dist directories over src directories.
   */
  preferDist?: boolean;
}

/**
 * Merge multiple operator registries into one.
 * Later registries override earlier ones for the same operator.
 *
 * @param registries - Registries to merge (in order, later overrides earlier)
 * @returns Combined registry
 */
export function mergeOperatorRegistries(
  ...registries: OperatorRegistry[]
): OperatorRegistry {
  const merged: OperatorRegistry = new Map();
  for (const registry of registries) {
    for (const [op, info] of registry) {
      merged.set(op, info);
    }
  }
  return merged;
}

/**
 * Discover all modules transitively imported from the entry module,
 * parse them, and return them sorted in topological order.
 *
 * This implements a two-pass algorithm:
 * 1. First pass: Collect imports and operator declarations for all modules
 * 2. Second pass: Parse modules in topological order with merged operator registries
 *
 * This ensures that dependencies are always analyzed before modules that depend on them,
 * and that operator precedence from dependencies is available during parsing.
 *
 * @param config - The resolved Vibe configuration
 * @param entryModuleName - The entry point module to start discovery from
 * @param options - Discovery options including parse and infix collection functions
 * @returns ModuleGraph with all discovered modules and topological ordering
 * @throws Error if a module cannot be found or if there's a circular dependency
 */
export function discoverModuleGraph(
  config: ResolvedVibeConfig,
  entryModuleName: string,
  options: DiscoverOptions
): ModuleGraph;

/**
 * @deprecated Use the options object overload instead
 */
export function discoverModuleGraph(
  config: ResolvedVibeConfig,
  entryModuleName: string,
  parseFunction: (source: string) => Program,
  preferDist?: boolean
): ModuleGraph;

export function discoverModuleGraph(
  config: ResolvedVibeConfig,
  entryModuleName: string,
  optionsOrParseFunction: DiscoverOptions | ((source: string) => Program),
  preferDistArg = false
): ModuleGraph {
  // Handle both old and new API signatures
  let options: DiscoverOptions;
  if (typeof optionsOrParseFunction === "function") {
    // Legacy API: wrap parse function and provide a no-op infix collector
    const parseFunction = optionsOrParseFunction;
    options = {
      collectInfixDeclarations: () => ({ registry: new Map() }),
      parseFunction,
      preferDist: preferDistArg,
    };
  } else {
    options = optionsOrParseFunction;
  }

  const {
    collectInfixDeclarations,
    parseFunction,
    preferDist = false,
  } = options;

  // ============================================================
  // PASS 1: Discover modules and collect operator declarations
  // ============================================================
  // We use a minimal parse just to extract imports and infix declarations.
  // This avoids needing operator precedence during import discovery.

  interface PreliminaryModule {
    moduleName: string;
    packageName: string;
    filePath: string;
    source: string;
    dependencies: Set<string>;
    operatorRegistry: OperatorRegistry;
  }

  const preliminaryModules = new Map<string, PreliminaryModule>();
  const visiting = new Set<string>(); // For cycle detection

  function discoverModulePreliminary(moduleName: string): void {
    if (preliminaryModules.has(moduleName)) {
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
    const resolved = resolveModule({ config, moduleName, preferDist });
    const source = fs.readFileSync(resolved.filePath, "utf8");

    // Collect infix declarations (quick scan, no full parse needed)
    const { registry: operatorRegistry } = collectInfixDeclarations(source);

    // Parse minimally to get imports - use the module's own operators only
    // (imports don't need complex expression parsing with operator precedence)
    const ast = parseFunction(source, operatorRegistry);

    // Extract dependency names
    const dependencies = new Set<string>();
    for (const imp of ast.imports) {
      dependencies.add(imp.moduleName);
    }

    // Recursively discover dependencies BEFORE storing this module
    for (const depName of dependencies) {
      discoverModulePreliminary(depName);
    }

    preliminaryModules.set(moduleName, {
      moduleName,
      packageName: resolved.packageName,
      filePath: resolved.filePath,
      source,
      dependencies,
      operatorRegistry,
    });

    visiting.delete(moduleName);
  }

  // Start discovery from entry module
  discoverModulePreliminary(entryModuleName);

  // Perform topological sort
  const sortedModuleNames = topologicalSortPreliminary(preliminaryModules);

  // ============================================================
  // PASS 2: Re-parse modules with combined operator registries
  // ============================================================
  // Process in topological order so dependency registries are available

  const modules = new Map<string, ModuleNode>();

  for (const moduleName of sortedModuleNames) {
    const preliminary = preliminaryModules.get(moduleName)!;

    // Merge operator registries from all dependencies
    const dependencyRegistries: OperatorRegistry[] = [];
    for (const depName of preliminary.dependencies) {
      const depModule = modules.get(depName);
      if (depModule) {
        dependencyRegistries.push(depModule.operatorRegistry);
      }
    }

    // Combined registry: dependencies first, then module's own (can override)
    const combinedRegistry = mergeOperatorRegistries(
      ...dependencyRegistries,
      preliminary.operatorRegistry
    );

    // Re-parse with combined operator registry for correct precedence
    const ast = parseFunction(preliminary.source, combinedRegistry);

    modules.set(moduleName, {
      moduleName: preliminary.moduleName,
      packageName: preliminary.packageName,
      filePath: preliminary.filePath,
      source: preliminary.source,
      ast,
      dependencies: preliminary.dependencies,
      operatorRegistry: preliminary.operatorRegistry, // Store module's own declarations
    });
  }

  return {
    modules,
    sortedModuleNames,
  };
}

/**
 * Helper for topological sort on preliminary modules
 */
function topologicalSortPreliminary(
  modules: Map<string, { moduleName: string; dependencies: Set<string> }>
): string[] {
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

    for (const depName of node.dependencies) {
      visit(depName);
    }

    sorted.push(moduleName);
  }

  for (const moduleName of modules.keys()) {
    visit(moduleName);
  }

  return sorted;
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
 * This implements a two-pass algorithm:
 * 1. First pass: Collect imports and operator declarations for all modules
 * 2. Second pass: Parse modules in topological order with merged operator registries
 *
 * @param config - The resolved Vibe configuration
 * @param options - Discovery options including parse and infix collection functions
 * @returns ModuleGraph with all discovered modules and topological ordering
 */
export function discoverAllModules(
  config: ResolvedVibeConfig,
  options: DiscoverOptions
): ModuleGraph;

/**
 * @deprecated Use the options object overload instead
 */
export function discoverAllModules(
  config: ResolvedVibeConfig,
  parseFunction: (source: string) => Program
): ModuleGraph;

export function discoverAllModules(
  config: ResolvedVibeConfig,
  optionsOrParseFunction: DiscoverOptions | ((source: string) => Program)
): ModuleGraph {
  // Handle both old and new API signatures
  let options: DiscoverOptions;
  if (typeof optionsOrParseFunction === "function") {
    // Legacy API: wrap parse function and provide a no-op infix collector
    const parseFunction = optionsOrParseFunction;
    options = {
      collectInfixDeclarations: () => ({ registry: new Map() }),
      parseFunction,
      preferDist: false,
    };
  } else {
    options = optionsOrParseFunction;
  }

  const { collectInfixDeclarations, parseFunction } = options;

  // First, discover all source modules in the config's source directory
  const sourceModules = discoverSourceModules(config.srcDir);

  // ============================================================
  // PASS 1: Discover modules and collect operator declarations
  // ============================================================

  interface PreliminaryModule {
    moduleName: string;
    packageName: string;
    filePath: string;
    source: string;
    dependencies: Set<string>;
    operatorRegistry: OperatorRegistry;
  }

  const preliminaryModules = new Map<string, PreliminaryModule>();
  const visiting = new Set<string>();

  function discoverModulePreliminary(moduleName: string): void {
    if (preliminaryModules.has(moduleName)) {
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

    // Collect infix declarations
    const { registry: operatorRegistry } = collectInfixDeclarations(source);

    // Parse minimally to get imports
    const ast = parseFunction(source, operatorRegistry);

    // Extract dependencies
    const dependencies = new Set<string>();
    for (const imp of ast.imports) {
      dependencies.add(imp.moduleName);
    }

    // Recursively discover dependencies
    for (const depName of dependencies) {
      discoverModulePreliminary(depName);
    }

    preliminaryModules.set(moduleName, {
      moduleName,
      packageName: resolved.packageName,
      filePath: resolved.filePath,
      source,
      dependencies,
      operatorRegistry,
    });

    visiting.delete(moduleName);
  }

  // Discover all source modules and their dependencies
  for (const moduleName of sourceModules) {
    discoverModulePreliminary(moduleName);
  }

  // Perform topological sort
  const sortedModuleNames = topologicalSortPreliminary(preliminaryModules);

  // ============================================================
  // PASS 2: Re-parse modules with combined operator registries
  // ============================================================

  const modules = new Map<string, ModuleNode>();

  for (const moduleName of sortedModuleNames) {
    const preliminary = preliminaryModules.get(moduleName)!;

    // Merge operator registries from all dependencies
    const dependencyRegistries: OperatorRegistry[] = [];
    for (const depName of preliminary.dependencies) {
      const depModule = modules.get(depName);
      if (depModule) {
        dependencyRegistries.push(depModule.operatorRegistry);
      }
    }

    // Combined registry: dependencies first, then module's own (can override)
    const combinedRegistry = mergeOperatorRegistries(
      ...dependencyRegistries,
      preliminary.operatorRegistry
    );

    // Re-parse with combined operator registry for correct precedence
    const ast = parseFunction(preliminary.source, combinedRegistry);

    modules.set(moduleName, {
      moduleName: preliminary.moduleName,
      packageName: preliminary.packageName,
      filePath: preliminary.filePath,
      source: preliminary.source,
      ast,
      dependencies: preliminary.dependencies,
      operatorRegistry: preliminary.operatorRegistry,
    });
  }

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
