import path from "node:path";
import { loadVibeConfigForDir, type VibePackageConfig } from "@vibe/config";

export interface ResolvedVibePackageConfig {
  readonly sourceRoots: readonly string[];
  readonly outDir?: string;
  readonly entry?: string;
}

const PACKAGE_CONFIG_CACHE = new Map<string, VibePackageConfig | null>();

export const loadPackageVibeConfig = (
  rootDir: string
): VibePackageConfig | undefined => {
  const normalized = path.resolve(rootDir);
  if (PACKAGE_CONFIG_CACHE.has(normalized)) {
    return PACKAGE_CONFIG_CACHE.get(normalized) ?? undefined;
  }
  const loaded = loadVibeConfigForDir(normalized);
  if (!loaded?.config.package) {
    PACKAGE_CONFIG_CACHE.set(normalized, null);
    return undefined;
  }
  const normalizedConfig = normalizePackageConfig(loaded.config.package);
  PACKAGE_CONFIG_CACHE.set(normalized, normalizedConfig ?? null);
  return normalizedConfig;
};

export const resolveVibePackageConfig = (
  rootDir: string,
  vibe?: VibePackageConfig
): ResolvedVibePackageConfig => {
  const sourceRoots = (vibe?.sources ?? []).map((source) =>
    path.resolve(rootDir, source)
  );
  const outDir = vibe?.outDir ? path.resolve(rootDir, vibe.outDir) : undefined;
  const entry = vibe?.entry ? path.resolve(rootDir, vibe.entry) : undefined;
  return {
    sourceRoots,
    ...(outDir ? { outDir } : {}),
    ...(entry ? { entry } : {}),
  };
};

export const hasVibeSources = (config?: VibePackageConfig): boolean => {
  return Boolean(config?.sources && config.sources.length > 0);
};

const normalizePackageConfig = (
  config: VibePackageConfig
): VibePackageConfig | undefined => {
  const sources = normalizeStringArray(config.sources);
  const outDir = normalizeString(config.outDir);
  const entry = normalizeString(config.entry);
  if (!sources && !outDir && !entry) {
    return undefined;
  }
  return {
    ...(sources ? { sources } : {}),
    ...(outDir ? { outDir } : {}),
    ...(entry ? { entry } : {}),
  } satisfies VibePackageConfig;
};

const normalizeStringArray = (
  value: readonly string[] | string | undefined
): readonly string[] | undefined => {
  if (Array.isArray(value)) {
    const entries = value.filter(
      (entry): entry is string => typeof entry === "string" && entry.length > 0
    );
    return entries.length > 0 ? entries : undefined;
  }
  if (typeof value === "string" && value.length > 0) {
    return [value];
  }
  return undefined;
};

const normalizeString = (value: string | undefined): string | undefined => {
  return typeof value === "string" && value.length > 0 ? value : undefined;
};
