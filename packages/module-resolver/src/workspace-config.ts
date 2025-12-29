import path from "node:path";

export interface VibePackageConfig {
  readonly sources?: readonly string[];
  readonly outDir?: string;
  readonly entry?: string;
}

export interface ResolvedVibePackageConfig {
  readonly sourceRoots: readonly string[];
  readonly outDir?: string;
  readonly entry?: string;
}

export const parseVibeConfig = (
  value: unknown
): VibePackageConfig | undefined => {
  if (!value || typeof value !== "object") {
    return undefined;
  }

  const record = value as Record<string, unknown>;

  const sources = normalizeStringArray(record.sources);
  const outDir =
    typeof record.outDir === "string" && record.outDir.length > 0
      ? record.outDir
      : undefined;
  const entry =
    typeof record.entry === "string" && record.entry.length > 0
      ? record.entry
      : undefined;

  if ((!sources || sources.length === 0) && !outDir && !entry) {
    return undefined;
  }

  return {
    ...(sources ? { sources } : {}),
    ...(outDir ? { outDir } : {}),
    ...(entry ? { entry } : {}),
  };
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

const normalizeStringArray = (
  value: unknown
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
