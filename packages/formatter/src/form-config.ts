export interface ClauseGroupingSpec {
  readonly groupSize: number;
}

export interface VectorArgumentGroupingSpec {
  readonly argumentIndex: number;
  readonly groupSize: number;
  readonly inlineHangRelativeColumns?: number;
}

export interface FormFormattingSpec {
  readonly inlineHeadArgCount?: number;
  readonly vectorArgumentIndices?: readonly number[];
  readonly forceBodyMultiline?: boolean;
  readonly forceListBodyMultiline?: boolean;
  readonly clauseGrouping?: ClauseGroupingSpec;
  readonly vectorArgumentGroupings?: readonly VectorArgumentGroupingSpec[];
}

export type FormFormattingConfig = Record<string, FormFormattingSpec>;

const cloneVectorArgumentIndices = (
  values: readonly number[] | undefined
): readonly number[] | undefined => {
  if (!values) {
    return undefined;
  }
  return [...values];
};

const cloneClauseGrouping = (
  spec: ClauseGroupingSpec | undefined
): ClauseGroupingSpec | undefined => {
  if (!spec) {
    return undefined;
  }
  return { ...spec } satisfies ClauseGroupingSpec;
};

const cloneVectorArgumentGroupings = (
  specs: readonly VectorArgumentGroupingSpec[] | undefined
): readonly VectorArgumentGroupingSpec[] | undefined => {
  if (!specs) {
    return undefined;
  }
  return specs.map(
    (spec) => ({ ...spec } satisfies VectorArgumentGroupingSpec)
  );
};

const cloneSpec = (spec: FormFormattingSpec): FormFormattingSpec => ({
  ...spec,
  vectorArgumentIndices: cloneVectorArgumentIndices(spec.vectorArgumentIndices),
  clauseGrouping: cloneClauseGrouping(spec.clauseGrouping),
  vectorArgumentGroupings: cloneVectorArgumentGroupings(
    spec.vectorArgumentGroupings
  ),
});

type FormSpecFactory = () => FormFormattingSpec;

const defineSpec = (spec: FormFormattingSpec): FormSpecFactory => {
  const template = cloneSpec(spec);
  return () => cloneSpec(template);
};

const SIMPLE_INLINE_DEFINITION = defineSpec({
  inlineHeadArgCount: 1,
  forceListBodyMultiline: true,
});

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
  vectorArgumentGroupings: [
    { argumentIndex: 1, groupSize: 2, inlineHangRelativeColumns: 1 },
  ],
});

const COND_CLAUSE_GROUPING = defineSpec({
  clauseGrouping: {
    groupSize: 2,
  },
});

const IF_FORM = defineSpec({
  inlineHeadArgCount: 1,
  forceBodyMultiline: true,
});

export const FORM_PRESETS = {
  definitionWithBindings: DEFINITION_WITH_BINDINGS(),
  lambdaWithBindings: LAMBDA_WITH_BINDINGS(),
  lexicalBindingForm: LEXICAL_BINDING_FORM(),
  condClausePairs: COND_CLAUSE_GROUPING(),
} as const;

const DEFAULT_FORM_ENTRIES: readonly (readonly [string, FormSpecFactory])[] = [
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
  ["if", IF_FORM],
  ["cond", COND_CLAUSE_GROUPING],
];

export const DEFAULT_FORM_CONFIG: FormFormattingConfig =
  DEFAULT_FORM_ENTRIES.reduce((config, [head, factory]) => {
    config[head] = factory();
    return config;
  }, {} as FormFormattingConfig);

export const cloneFormConfig = (
  config: FormFormattingConfig
): FormFormattingConfig => {
  const cloned: FormFormattingConfig = {};
  for (const [head, spec] of Object.entries(config)) {
    cloned[head] = cloneSpec(spec);
  }
  return cloned;
};

const mergeSpecs = (
  base: FormFormattingSpec | undefined,
  override: FormFormattingSpec
): FormFormattingSpec => {
  const vectorArgumentIndices =
    "vectorArgumentIndices" in override
      ? cloneVectorArgumentIndices(override.vectorArgumentIndices)
      : base?.vectorArgumentIndices
      ? cloneVectorArgumentIndices(base.vectorArgumentIndices)
      : undefined;

  const clauseGrouping =
    "clauseGrouping" in override
      ? cloneClauseGrouping(override.clauseGrouping)
      : base?.clauseGrouping
      ? cloneClauseGrouping(base.clauseGrouping)
      : undefined;

  const vectorArgumentGroupings =
    "vectorArgumentGroupings" in override
      ? cloneVectorArgumentGroupings(override.vectorArgumentGroupings)
      : base?.vectorArgumentGroupings
      ? cloneVectorArgumentGroupings(base.vectorArgumentGroupings)
      : undefined;

  return {
    ...(base ?? {}),
    ...override,
    ...(vectorArgumentIndices ? { vectorArgumentIndices } : {}),
    ...(clauseGrouping ? { clauseGrouping } : {}),
    ...(vectorArgumentGroupings ? { vectorArgumentGroupings } : {}),
  } satisfies FormFormattingSpec;
};

export const mergeFormConfigs = (
  base: FormFormattingConfig,
  override?: FormFormattingConfig
): FormFormattingConfig => {
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

export const cloneFormSpec = (spec: FormFormattingSpec): FormFormattingSpec =>
  cloneSpec(spec);
