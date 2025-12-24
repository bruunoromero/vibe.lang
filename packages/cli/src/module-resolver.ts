import { existsSync, readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import type {
  ModuleResolutionRequest,
  ModuleResolutionResult,
  ModuleResolver,
} from "@vibe/semantics";
import { isRelativeSpecifier, normalizeRequireTarget } from "./specifiers";
import { parseVibeConfig, type VibePackageConfig } from "./workspace-config";

export interface PackageMetadata {
  readonly name: string;
  readonly rootDir: string;
  readonly vibe?: VibePackageConfig;
}

interface PackageModuleResolutionResult {
  readonly ok: boolean;
  readonly moduleId?: string;
  readonly reason?: string;
}

type WorkspacePackageMap = Map<string, PackageMetadata>;

export const findWorkspaceRoot = (startDir: string): string | null => {
  let current = path.resolve(startDir);
  while (true) {
    const manifestPath = path.join(current, "package.json");
    if (existsSync(manifestPath)) {
      try {
        const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
        if (hasWorkspaces(manifest)) {
          return current;
        }
      } catch {
        // Ignore JSON errors and continue traversing upward
      }
    }
    const parent = path.dirname(current);
    if (parent === current) {
      return null;
    }
    current = parent;
  }
};

const hasWorkspaces = (manifest: unknown): boolean => {
  if (!manifest || typeof manifest !== "object") {
    return false;
  }
  const workspaces = (manifest as Record<string, unknown>).workspaces;
  if (Array.isArray(workspaces)) {
    return workspaces.length > 0;
  }
  if (
    workspaces &&
    typeof workspaces === "object" &&
    Array.isArray((workspaces as { packages?: unknown }).packages)
  ) {
    return (workspaces as { packages: unknown[] }).packages.length > 0;
  }
  return false;
};

const collectWorkspacePackages = (
  workspaceRoot: string
): WorkspacePackageMap => {
  const manifestPath = path.join(workspaceRoot, "package.json");
  if (!existsSync(manifestPath)) {
    return new Map();
  }
  let manifest: Record<string, unknown>;
  try {
    manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
  } catch {
    return new Map();
  }
  const patterns = extractWorkspacePatterns(manifest.workspaces);
  const packages: WorkspacePackageMap = new Map();
  for (const pattern of patterns) {
    const dirs = expandWorkspacePattern(workspaceRoot, pattern);
    for (const dir of dirs) {
      const metadata = readPackageMetadata(dir);
      if (metadata) {
        packages.set(metadata.name, metadata);
      }
    }
  }
  return packages;
};

const extractWorkspacePatterns = (workspaces: unknown): string[] => {
  if (Array.isArray(workspaces)) {
    return workspaces.filter(
      (entry): entry is string => typeof entry === "string"
    );
  }
  if (
    workspaces &&
    typeof workspaces === "object" &&
    Array.isArray((workspaces as { packages?: unknown }).packages)
  ) {
    return (workspaces as { packages: unknown[] }).packages.filter(
      (entry): entry is string => typeof entry === "string"
    );
  }
  return [];
};

const expandWorkspacePattern = (
  workspaceRoot: string,
  pattern: string
): string[] => {
  const normalized = pattern.replace(/\\/g, "/");
  if (normalized.endsWith("/*")) {
    const baseDir = path.resolve(workspaceRoot, normalized.slice(0, -2));
    if (!existsSync(baseDir)) {
      return [];
    }
    return readdirSync(baseDir, { withFileTypes: true })
      .filter((entry) => entry.isDirectory())
      .map((entry) => path.join(baseDir, entry.name));
  }
  const target = path.resolve(workspaceRoot, normalized);
  return existsSync(target) ? [target] : [];
};

const readPackageMetadata = (dir: string): PackageMetadata | null => {
  const manifestPath = path.join(dir, "package.json");
  if (!existsSync(manifestPath)) {
    return null;
  }
  try {
    const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
    const name = typeof manifest.name === "string" ? manifest.name : null;
    if (!name) {
      return null;
    }
    const vibe = parseVibeConfig(manifest.vibe);
    return { name, rootDir: dir, vibe } satisfies PackageMetadata;
  } catch {
    return null;
  }
};

const splitPackageName = (packageName: string): string[] => {
  if (packageName.startsWith("@")) {
    const segments = packageName.split("/");
    if (segments.length >= 2) {
      return [segments[0]!, segments[1]!];
    }
    return [packageName];
  }
  return packageName.split("/");
};

interface PackageSpecifier {
  readonly packageName: string;
  readonly subpath: string;
}

const parsePackageSpecifier = (specifier: string): PackageSpecifier | null => {
  if (
    !specifier ||
    specifier.startsWith("./") ||
    specifier.startsWith("../") ||
    specifier.startsWith("/") ||
    specifier.includes(":")
  ) {
    return null;
  }
  const normalized = specifier.replace(/\\/g, "/");
  const segments = normalized.split("/").filter(Boolean);
  if (segments.length === 0) {
    return null;
  }
  if (normalized.startsWith("@")) {
    if (segments.length < 2) {
      return null;
    }
    const packageName = `${segments[0]!}/${segments[1]!}`;
    const remainder = segments.slice(2).join("/");
    return {
      packageName,
      subpath: normalizePackageSubpath(remainder),
    } satisfies PackageSpecifier;
  }
  const packageName = segments[0]!;
  const rest = segments.slice(1);
  return {
    packageName,
    subpath: normalizePackageSubpath(rest.join("/")),
  } satisfies PackageSpecifier;
};

const normalizePackageSubpath = (value: string): string => {
  if (!value) {
    return ".";
  }
  const trimmed = value.startsWith("/") ? value : `/${value}`;
  return `.${trimmed}`;
};

export class PackageRegistry {
  private readonly packageCache = new Map<string, PackageMetadata | null>();

  constructor(
    private readonly workspaceRoot: string | null,
    private readonly workspacePackages: WorkspacePackageMap
  ) {}

  static create(workspaceRoot: string | null): PackageRegistry {
    const workspacePackages = workspaceRoot
      ? collectWorkspacePackages(workspaceRoot)
      : new Map();
    return new PackageRegistry(workspaceRoot, workspacePackages);
  }

  resolveModule(
    specifier: string,
    fromDir: string | undefined
  ): PackageModuleResolutionResult {
    const parsed = parsePackageSpecifier(specifier);
    if (!parsed) {
      return { ok: false, reason: "module specifier is not a package import" };
    }
    const pkg = this.lookupPackage(parsed.packageName, fromDir);
    if (!pkg) {
      return { ok: false, reason: `package not found (${parsed.packageName})` };
    }
    const targetMap = pkg.vibe?.modules;
    if (!targetMap) {
      return {
        ok: false,
        reason: `${parsed.packageName} does not declare any vibe.modules`,
      };
    }
    const relativePath = targetMap[parsed.subpath];
    if (!relativePath) {
      return {
        ok: false,
        reason: `${parsed.packageName} does not expose ${parsed.subpath}`,
      };
    }
    const absolutePath = path.resolve(pkg.rootDir, relativePath);
    if (!existsSync(absolutePath)) {
      return {
        ok: false,
        reason: `module not found (${relativePath})`,
      };
    }
    return { ok: true, moduleId: absolutePath };
  }

  getPackageMetadata(
    packageName: string,
    fromDir: string | undefined
  ): PackageMetadata | null {
    return this.lookupPackage(packageName, fromDir);
  }

  getWorkspacePackages(): readonly PackageMetadata[] {
    return Array.from(this.workspacePackages.values());
  }

  private lookupPackage(
    packageName: string,
    fromDir: string | undefined
  ): PackageMetadata | null {
    const workspaceHit = this.workspacePackages.get(packageName);
    if (workspaceHit) {
      return workspaceHit;
    }
    const searchStart = fromDir ?? this.workspaceRoot ?? process.cwd();
    return this.findPackageInNodeModules(packageName, searchStart);
  }

  private findPackageInNodeModules(
    packageName: string,
    startDir: string
  ): PackageMetadata | null {
    const nameParts = splitPackageName(packageName);
    let current = path.resolve(startDir);
    while (true) {
      const candidateDir = path.join(current, "node_modules", ...nameParts);
      const metadata = this.readPackageMetadataWithCache(candidateDir);
      if (metadata) {
        return metadata;
      }
      const parent = path.dirname(current);
      if (parent === current) {
        return null;
      }
      current = parent;
    }
  }

  private readPackageMetadataWithCache(dir: string): PackageMetadata | null {
    const cached = this.packageCache.get(dir);
    if (cached !== undefined) {
      return cached;
    }
    const metadata = readPackageMetadata(dir);
    this.packageCache.set(dir, metadata ?? null);
    return metadata;
  }
}

interface ProjectModuleResolverOptions {
  readonly knownModules?: Set<string>;
  readonly packageRegistry?: PackageRegistry | null;
  readonly workspaceRoot?: string | null;
}

class ProjectModuleResolver implements ModuleResolver {
  constructor(private readonly options: ProjectModuleResolverOptions) {}

  resolve(request: ModuleResolutionRequest): ModuleResolutionResult {
    if (request.kind !== "require" && request.kind !== "import") {
      return { ok: true };
    }
    if (isRelativeSpecifier(request.specifier)) {
      return this.resolveRelative(request);
    }
    return this.resolvePackage(request);
  }

  private resolveRelative(
    request: ModuleResolutionRequest
  ): ModuleResolutionResult {
    const { fromModuleId, specifier } = request;
    if (!fromModuleId) {
      return {
        ok: false,
        reason: `${request.kind} needs a moduleId for relative paths`,
      };
    }
    const normalized = normalizeRequireTarget(fromModuleId, specifier);
    if (!normalized) {
      return {
        ok: false,
        reason: `${request.kind} expects a relative .lang specifier`,
      };
    }
    if (
      this.options.knownModules &&
      !this.options.knownModules.has(normalized)
    ) {
      return { ok: false, reason: `module not found (${specifier})` };
    }
    if (!existsSync(normalized)) {
      return { ok: false, reason: `module not found (${specifier})` };
    }
    return { ok: true, moduleId: normalized };
  }

  private resolvePackage(
    request: ModuleResolutionRequest
  ): ModuleResolutionResult {
    const { packageRegistry, workspaceRoot } = this.options;
    if (!packageRegistry) {
      return {
        ok: false,
        reason: "package imports require a workspace-aware resolver",
      };
    }
    const fromDir = request.fromModuleId
      ? path.dirname(request.fromModuleId)
      : workspaceRoot ?? process.cwd();
    const resolution = packageRegistry.resolveModule(
      request.specifier,
      fromDir
    );
    if (!resolution.ok || !resolution.moduleId) {
      return { ok: false, reason: resolution.reason };
    }
    return { ok: true, moduleId: resolution.moduleId };
  }
}

export const createProjectModuleResolver = (
  options: ProjectModuleResolverOptions = {}
): ModuleResolver => {
  return new ProjectModuleResolver(options);
};
