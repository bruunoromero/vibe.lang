const cloneVectorArgumentIndices = (values) => {
    if (!values) {
        return undefined;
    }
    return [...values];
};
const cloneClauseGrouping = (spec) => {
    if (!spec) {
        return undefined;
    }
    return { ...spec };
};
const cloneSpec = (spec) => ({
    ...spec,
    vectorArgumentIndices: cloneVectorArgumentIndices(spec.vectorArgumentIndices),
    clauseGrouping: cloneClauseGrouping(spec.clauseGrouping),
});
const defineSpec = (spec) => {
    const template = cloneSpec(spec);
    return () => cloneSpec(template);
};
const SIMPLE_INLINE_DEFINITION = defineSpec({ inlineHeadArgCount: 1 });
const DEFINITION_WITH_BINDINGS = defineSpec({
    inlineHeadArgCount: 2,
    vectorArgumentIndices: [2],
    forceBodyMultiline: true,
});
const LAMBDA_WITH_BINDINGS = defineSpec({
    inlineHeadArgCount: 1,
    vectorArgumentIndices: [1],
    forceBodyMultiline: true,
});
const LEXICAL_BINDING_FORM = defineSpec({
    inlineHeadArgCount: 1,
    vectorArgumentIndices: [1],
});
const COND_CLAUSE_GROUPING = defineSpec({
    clauseGrouping: {
        groupSize: 2,
    },
});
export const FORM_PRESETS = {
    definitionWithBindings: DEFINITION_WITH_BINDINGS(),
    lambdaWithBindings: LAMBDA_WITH_BINDINGS(),
    lexicalBindingForm: LEXICAL_BINDING_FORM(),
    condClausePairs: COND_CLAUSE_GROUPING(),
};
const DEFAULT_FORM_ENTRIES = [
    ["def", SIMPLE_INLINE_DEFINITION],
    ["defp", SIMPLE_INLINE_DEFINITION],
    ["defmacro", DEFINITION_WITH_BINDINGS],
    ["defmacrop", DEFINITION_WITH_BINDINGS],
    ["defmacro+", DEFINITION_WITH_BINDINGS],
    ["defmacrop+", DEFINITION_WITH_BINDINGS],
    ["defn", DEFINITION_WITH_BINDINGS],
    ["defn+", DEFINITION_WITH_BINDINGS],
    ["defnp", DEFINITION_WITH_BINDINGS],
    ["defnp+", DEFINITION_WITH_BINDINGS],
    ["fn", LAMBDA_WITH_BINDINGS],
    ["fn+", LAMBDA_WITH_BINDINGS],
    ["let", LEXICAL_BINDING_FORM],
    ["cond", COND_CLAUSE_GROUPING],
];
export const DEFAULT_FORM_CONFIG = DEFAULT_FORM_ENTRIES.reduce((config, [head, factory]) => {
    config[head] = factory();
    return config;
}, {});
export const cloneFormConfig = (config) => {
    const cloned = {};
    for (const [head, spec] of Object.entries(config)) {
        cloned[head] = cloneSpec(spec);
    }
    return cloned;
};
const mergeSpecs = (base, override) => {
    const vectorArgumentIndices = "vectorArgumentIndices" in override
        ? cloneVectorArgumentIndices(override.vectorArgumentIndices)
        : base?.vectorArgumentIndices
            ? cloneVectorArgumentIndices(base.vectorArgumentIndices)
            : undefined;
    const clauseGrouping = "clauseGrouping" in override
        ? cloneClauseGrouping(override.clauseGrouping)
        : base?.clauseGrouping
            ? cloneClauseGrouping(base.clauseGrouping)
            : undefined;
    return {
        ...(base ?? {}),
        ...override,
        ...(vectorArgumentIndices ? { vectorArgumentIndices } : {}),
        ...(clauseGrouping ? { clauseGrouping } : {}),
    };
};
export const mergeFormConfigs = (base, override) => {
    const merged = cloneFormConfig(base);
    if (!override) {
        return merged;
    }
    for (const [head, spec] of Object.entries(override)) {
        if (!spec) {
            continue;
        }
        const existing = merged[head];
        merged[head] = mergeSpecs(existing, spec);
    }
    return merged;
};
export const cloneFormSpec = (spec) => cloneSpec(spec);
//# sourceMappingURL=form-config.js.map