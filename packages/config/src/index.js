export { findVibeConfigPath, findVibeConfigInDir, loadNearestVibeConfig, loadVibeConfigForDir, loadVibeConfigFromPath, clearVibeConfigCache, } from "./loader";
export const defineConfig = (config) => config;
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
export const defineFormatterSpec = (spec) => {
    const template = cloneFormatterSpec(spec);
    return () => cloneFormatterSpec(template);
};
export const defineSpec = defineFormatterSpec;
//# sourceMappingURL=index.js.map