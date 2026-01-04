import { loadNearestVibeConfig, } from "@vibe/config";
import { FORM_PRESETS, } from "./form-config";
const cloneFormSpec = (spec) => {
    if (!spec) {
        return undefined;
    }
    return {
        ...spec,
        ...(spec.vectorArgumentIndices
            ? { vectorArgumentIndices: [...spec.vectorArgumentIndices] }
            : {}),
        ...(spec.clauseGrouping
            ? { clauseGrouping: { ...spec.clauseGrouping } }
            : {}),
    };
};
const isPlainObject = (value) => {
    return Boolean(value && typeof value === "object" && !Array.isArray(value));
};
const cloneVectorArgumentIndices = (value) => {
    if (!Array.isArray(value)) {
        return undefined;
    }
    const indices = value.filter((entry) => typeof entry === "number" && Number.isFinite(entry));
    if (indices.length > 0) {
        return indices;
    }
    return value.length === 0 ? [] : undefined;
};
const cloneClauseGrouping = (value) => {
    if (!isPlainObject(value)) {
        return undefined;
    }
    const size = value.groupSize;
    if (typeof size === "number" && Number.isFinite(size)) {
        return { groupSize: size };
    }
    return undefined;
};
const readNumericField = (value, keys) => {
    for (const key of keys) {
        if (key in value &&
            typeof value[key] === "number" &&
            Number.isFinite(value[key])) {
            return value[key];
        }
    }
    return undefined;
};
const readBooleanField = (value, keys) => {
    for (const key of keys) {
        if (key in value && typeof value[key] === "boolean") {
            return value[key];
        }
    }
    return undefined;
};
const readVectorField = (value, keys) => {
    for (const key of keys) {
        if (key in value) {
            const indices = cloneVectorArgumentIndices(value[key]);
            if (indices !== undefined) {
                return indices;
            }
        }
    }
    return undefined;
};
const normalizeFormSpecObject = (value, options = {}) => {
    if (!isPlainObject(value)) {
        return undefined;
    }
    const spec = {};
    const inlineHeadArgCount = readNumericField(value, ["inlineHeadArgCount"]);
    if (inlineHeadArgCount !== undefined) {
        spec.inlineHeadArgCount = inlineHeadArgCount;
    }
    const vectorIndices = readVectorField(value, ["vectorArgumentIndices"]);
    if (vectorIndices !== undefined) {
        spec.vectorArgumentIndices = vectorIndices;
    }
    const forceBody = readBooleanField(value, ["forceBodyMultiline"]);
    if (forceBody !== undefined) {
        spec.forceBodyMultiline = forceBody;
    }
    else if (options.defaultForceBody) {
        spec.forceBodyMultiline = true;
    }
    if ("clauseGrouping" in value) {
        const grouping = cloneClauseGrouping(value.clauseGrouping);
        if (grouping) {
            spec.clauseGrouping = grouping;
        }
    }
    return spec;
};
const buildPresetTable = () => {
    const table = {};
    for (const [name, preset] of Object.entries(FORM_PRESETS)) {
        table[name] = cloneFormSpec(preset) ?? {};
    }
    return table;
};
const materializeSpecEntry = (entry) => {
    if (!entry) {
        return undefined;
    }
    if (typeof entry === "string") {
        return undefined;
    }
    const value = typeof entry === "function" ? entry() : entry;
    return normalizeFormSpecObject(value, { defaultForceBody: true });
};
const resolveEntrySpec = (entry, table) => {
    if (typeof entry === "string") {
        return table[entry];
    }
    return materializeSpecEntry(entry);
};
export const buildFormConfigFromFormatter = (formatter) => {
    if (!formatter) {
        return undefined;
    }
    const table = buildPresetTable();
    const formConfig = {};
    for (const [formName, entry] of Object.entries(formatter)) {
        const spec = resolveEntrySpec(entry, table);
        if (!spec) {
            console.warn(`[vibe][fmt] Skipping formatter form "${formName}" because it is invalid or references an unknown spec.`);
            continue;
        }
        formConfig[formName] = spec;
    }
    return Object.keys(formConfig).length > 0 ? formConfig : undefined;
};
export const loadFormatterFormConfig = (sourceRoot) => {
    const loaded = loadNearestVibeConfig(sourceRoot);
    if (!loaded) {
        return undefined;
    }
    return buildFormConfigFromFormatter(loaded.config.formatter);
};
//# sourceMappingURL=config-loader.js.map