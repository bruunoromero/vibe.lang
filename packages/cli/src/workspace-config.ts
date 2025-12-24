import path from "node:path";

export interface VibePackageConfig {
  readonly modules?: Record<string, string>;
  readonly sources?: readonly string[];
  readonly outDir?: string;
}

export interface ResolvedVibePackageConfig {
  readonly modules: Record<string, string>;
  readonly sourceRoots: readonly string[];
  readonly outDir?: string;
}

export const parseVibeConfig = (
  value: unknown
): VibePackageConfig | undefined => {
  if (!value || typeof value !== "object") {
    return undefined;
  }

  const record = value as Record<string, unknown>;
  let modules: Record<string, string> | undefined;
  if (record.modules && typeof record.modules === "object") {
    modules = {};
    for (const [key, modulePath] of Object.entries(
      record.modules as Record<string, unknown>
    )) {
      if (typeof modulePath === "string") {
        modules[key] = modulePath;
      }
    }
    if (Object.keys(modules).length === 0) {
      modules = undefined;
    }
  }

  const sources = normalizeStringArray(record.sources);
  const outDir =
    typeof record.outDir === "string" && record.outDir.length > 0
      ? record.outDir
      : undefined;

  if (!modules && (!sources || sources.length === 0) && !outDir) {
    return undefined;
  }

  return {
    ...(modules ? { modules } : {}),
    ...(sources ? { sources } : {}),
    ...(outDir ? { outDir } : {}),
  };
};

export const resolveVibePackageConfig = (
  rootDir: string,
  vibe?: VibePackageConfig
): ResolvedVibePackageConfig => {
  const modules = { ...(vibe?.modules ?? {}) };
  const sourceRoots = (vibe?.sources ?? []).map((source) =>
    path.resolve(rootDir, source)
  );
  const outDir = vibe?.outDir ? path.resolve(rootDir, vibe.outDir) : undefined;
  return {
    modules,
    sourceRoots,
    ...(outDir ? { outDir } : {}),
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
