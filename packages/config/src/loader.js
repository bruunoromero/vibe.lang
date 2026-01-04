import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import vm from "node:vm";
import { transformSync } from "esbuild";
const CONFIG_BASENAME = "vibe.config";
const CONFIG_EXTENSIONS = [
    ".ts",
    ".mts",
    ".cts",
    ".js",
    ".mjs",
    ".cjs",
    ".json",
];
const ESBUILD_LOADERS = {
    ".ts": "ts",
    ".mts": "ts",
    ".cts": "ts",
    ".js": "js",
    ".mjs": "js",
    ".cjs": "js",
    ".json": "json",
};
const CONFIG_CACHE = new Map();
const identityDefineConfig = (config) => config;
const cloneNumberArray = (values) => {
    if (!values) {
        return undefined;
    }
    return [...values];
};
const cloneClauseGrouping = (clause) => {
    if (!clause) {
        return undefined;
    }
    return { ...clause };
};
const cloneFormatterSpec = (spec) => {
    const vectorArgumentIndices = cloneNumberArray(spec.vectorArgumentIndices);
    const clauseGrouping = cloneClauseGrouping(spec.clauseGrouping);
    return {
        ...spec,
        ...(vectorArgumentIndices ? { vectorArgumentIndices } : {}),
        ...(clauseGrouping ? { clauseGrouping } : {}),
    };
};
const identityDefineFormatterSpec = (spec) => {
    const template = cloneFormatterSpec(spec);
    return () => cloneFormatterSpec(template);
};
const isPlainObject = (value) => {
    return Boolean(value && typeof value === "object" && !Array.isArray(value));
};
const resolveConfigExport = (exports) => {
    if (isPlainObject(exports)) {
        if (isPlainObject(exports.default)) {
            return exports.default;
        }
        return exports;
    }
    return undefined;
};
const compileModule = (code, filePath, loader) => {
    const result = transformSync(code, {
        loader,
        format: "cjs",
        platform: "node",
        target: "es2020",
        sourcefile: filePath,
        sourcemap: "inline",
    });
    const moduleDir = path.dirname(filePath);
    const moduleObject = { exports: {} };
    const baseRequire = createRequire(filePath);
    const wrappedRequire = (request) => {
        if (request === "@vibe/config") {
            return {
                defineConfig: identityDefineConfig,
                defineFormatterSpec: identityDefineFormatterSpec,
                defineSpec: identityDefineFormatterSpec,
            };
        }
        return baseRequire(request);
    };
    const wrapped = `(function (exports, require, module, __filename, __dirname) { ${result.code}
})`;
    const script = new vm.Script(wrapped, { filename: filePath });
    const fn = script.runInThisContext();
    fn(moduleObject.exports, wrappedRequire, moduleObject, filePath, moduleDir);
    return moduleObject.exports;
};
const loadModuleFromFile = (configPath) => {
    const ext = path.extname(configPath).toLowerCase();
    const loader = ESBUILD_LOADERS[ext] ?? "js";
    const source = readFileSync(configPath, "utf8");
    if (loader === "json") {
        return JSON.parse(source);
    }
    return compileModule(source, configPath, loader);
};
const checkDirForConfig = (dir) => {
    for (const ext of CONFIG_EXTENSIONS) {
        const candidate = path.join(dir, `${CONFIG_BASENAME}${ext}`);
        if (existsSync(candidate)) {
            return candidate;
        }
    }
    return null;
};
export const findVibeConfigPath = (startDir, options = {}) => {
    const searchParents = options.searchParents !== false;
    let current = path.resolve(startDir);
    while (true) {
        const hit = checkDirForConfig(current);
        if (hit) {
            return hit;
        }
        if (!searchParents) {
            return null;
        }
        const parent = path.dirname(current);
        if (parent === current) {
            return null;
        }
        current = parent;
    }
};
export const findVibeConfigInDir = (dir) => {
    return checkDirForConfig(path.resolve(dir));
};
export const loadVibeConfigFromPath = (configPath) => {
    const normalized = path.resolve(configPath);
    if (CONFIG_CACHE.has(normalized)) {
        return CONFIG_CACHE.get(normalized) ?? undefined;
    }
    if (!existsSync(normalized)) {
        CONFIG_CACHE.set(normalized, null);
        return undefined;
    }
    try {
        const exports = loadModuleFromFile(normalized);
        const config = resolveConfigExport(exports);
        if (config) {
            CONFIG_CACHE.set(normalized, config);
            return config;
        }
        CONFIG_CACHE.set(normalized, null);
        return undefined;
    }
    catch (error) {
        CONFIG_CACHE.delete(normalized);
        throw new Error(`Failed to load ${CONFIG_BASENAME} at ${normalized}: ${String(error)}`);
    }
};
export const loadNearestVibeConfig = (startDir, options = {}) => {
    const configPath = findVibeConfigPath(startDir, options);
    if (!configPath) {
        return undefined;
    }
    const config = loadVibeConfigFromPath(configPath);
    if (!config) {
        return undefined;
    }
    return {
        path: configPath,
        dir: path.dirname(configPath),
        config,
    };
};
export const loadVibeConfigForDir = (dir) => {
    return loadNearestVibeConfig(dir, { searchParents: false });
};
export const clearVibeConfigCache = () => {
    CONFIG_CACHE.clear();
};
//# sourceMappingURL=loader.js.map