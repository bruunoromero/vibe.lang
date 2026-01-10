import fs from "node:fs";
import path from "node:path";

export interface VibeConfig {
  name: string;
  src: string;
  dist: string;
  packages: string[];
}

export interface ResolvedVibeConfig extends VibeConfig {
  rootDir: string;
  configPath: string;
  srcDir: string;
  distDir: string;
}

export interface LoadConfigOptions {
  cwd?: string;
  path?: string;
}

export const DEFAULT_CONFIG_NAME = "vibe.json";

export function loadConfig(
  options: LoadConfigOptions = {}
): ResolvedVibeConfig {
  const cwd = options.cwd ?? process.cwd();
  const configPath = resolveConfigPath(options.path, cwd);

  if (!fs.existsSync(configPath)) {
    throw new Error(`No ${DEFAULT_CONFIG_NAME} found at ${configPath}`);
  }

  const rawText = fs.readFileSync(configPath, "utf8");
  let rawConfig: unknown;

  try {
    rawConfig = JSON.parse(rawText);
  } catch (error) {
    throw new Error(
      `Invalid JSON in ${configPath}: ${(error as Error).message}`
    );
  }

  const config = validateConfig(rawConfig, configPath);
  const rootDir = path.dirname(configPath);

  return {
    ...config,
    rootDir,
    configPath,
    srcDir: path.resolve(rootDir, config.src),
    distDir: path.resolve(rootDir, config.dist),
  };
}

function resolveConfigPath(
  explicitPath: string | undefined,
  cwd: string
): string {
  if (explicitPath) {
    return path.isAbsolute(explicitPath)
      ? explicitPath
      : path.resolve(cwd, explicitPath);
  }
  return path.join(cwd, DEFAULT_CONFIG_NAME);
}

function validateConfig(config: unknown, sourcePath: string): VibeConfig {
  if (!config || typeof config !== "object") {
    throw new Error(`Config at ${sourcePath} must be an object`);
  }

  const shape = config as Record<string, unknown>;
  const name = expectString(shape, "name", sourcePath);
  const src = expectString(shape, "src", sourcePath);
  const dist = expectString(shape, "dist", sourcePath);
  const packages = expectStringArray(shape, "packages", sourcePath);

  return { name, src, dist, packages };
}

function expectString(
  config: Record<string, unknown>,
  key: string,
  sourcePath: string
): string {
  const value = config[key];
  if (typeof value !== "string" || value.trim() === "") {
    throw new Error(
      `Config field "${key}" in ${sourcePath} must be a non-empty string`
    );
  }
  return value;
}

function expectStringArray(
  config: Record<string, unknown>,
  key: string,
  sourcePath: string
): string[] {
  const value = config[key];
  if (!Array.isArray(value)) {
    throw new Error(
      `Config field "${key}" in ${sourcePath} must be an array of package names`
    );
  }

  const items = value.map((entry) => {
    if (typeof entry !== "string" || entry.trim() === "") {
      throw new Error(
        `Config field "${key}" in ${sourcePath} must only contain non-empty strings`
      );
    }
    return entry;
  });

  return items;
}
