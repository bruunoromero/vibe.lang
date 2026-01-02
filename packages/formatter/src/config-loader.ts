import {
  loadNearestVibeConfig,
  type VibeFormatterConfig,
  type VibeFormatterSpecEntry,
} from "@vibe/config";
import {
  FORM_PRESETS,
  type FormFormattingConfig,
  type FormFormattingSpec,
} from "./form-config";

type FormatterSpecEntry = VibeFormatterSpecEntry;

type MutableFormFormattingSpec = {
  inlineHeadArgCount?: number;
  vectorArgumentIndices?: readonly number[];
  forceBodyMultiline?: boolean;
  clauseGrouping?: { readonly groupSize: number };
};

const cloneFormSpec = (
  spec: FormFormattingSpec | undefined
): FormFormattingSpec | undefined => {
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
  } satisfies FormFormattingSpec;
};

const isPlainObject = (value: unknown): value is Record<string, unknown> => {
  return Boolean(value && typeof value === "object" && !Array.isArray(value));
};

const cloneVectorArgumentIndices = (
  value: unknown
): readonly number[] | undefined => {
  if (!Array.isArray(value)) {
    return undefined;
  }
  const indices = value.filter(
    (entry): entry is number =>
      typeof entry === "number" && Number.isFinite(entry)
  );
  if (indices.length > 0) {
    return indices;
  }
  return value.length === 0 ? ([] as number[]) : undefined;
};

const cloneClauseGrouping = (
  value: unknown
): { readonly groupSize: number } | undefined => {
  if (!isPlainObject(value)) {
    return undefined;
  }
  const size = value.groupSize;
  if (typeof size === "number" && Number.isFinite(size)) {
    return { groupSize: size };
  }
  return undefined;
};

const readNumericField = (
  value: Record<string, unknown>,
  keys: readonly string[]
): number | undefined => {
  for (const key of keys) {
    if (
      key in value &&
      typeof value[key] === "number" &&
      Number.isFinite(value[key] as number)
    ) {
      return value[key] as number;
    }
  }
  return undefined;
};

const readBooleanField = (
  value: Record<string, unknown>,
  keys: readonly string[]
): boolean | undefined => {
  for (const key of keys) {
    if (key in value && typeof value[key] === "boolean") {
      return value[key] as boolean;
    }
  }
  return undefined;
};

const readVectorField = (
  value: Record<string, unknown>,
  keys: readonly string[]
): readonly number[] | undefined => {
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

interface NormalizeOptions {
  readonly defaultForceBody?: boolean;
}

const normalizeFormSpecObject = (
  value: unknown,
  options: NormalizeOptions = {}
): FormFormattingSpec | undefined => {
  if (!isPlainObject(value)) {
    return undefined;
  }
  const spec: MutableFormFormattingSpec = {};
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
  } else if (options.defaultForceBody) {
    spec.forceBodyMultiline = true;
  }
  if ("clauseGrouping" in value) {
    const grouping = cloneClauseGrouping(value.clauseGrouping);
    if (grouping) {
      spec.clauseGrouping = grouping;
    }
  }
  return spec as FormFormattingSpec;
};

const buildPresetTable = (): Record<string, FormFormattingSpec> => {
  const table: Record<string, FormFormattingSpec> = {};
  for (const [name, preset] of Object.entries(FORM_PRESETS)) {
    table[name] = cloneFormSpec(preset) ?? {};
  }
  return table;
};

const materializeSpecEntry = (
  entry: FormatterSpecEntry | undefined
): FormFormattingSpec | undefined => {
  if (!entry) {
    return undefined;
  }
  if (typeof entry === "string") {
    return undefined;
  }
  const value = typeof entry === "function" ? entry() : entry;
  return normalizeFormSpecObject(value, { defaultForceBody: true });
};

const resolveEntrySpec = (
  entry: FormatterSpecEntry,
  table: Record<string, FormFormattingSpec>
): FormFormattingSpec | undefined => {
  if (typeof entry === "string") {
    return table[entry];
  }
  return materializeSpecEntry(entry);
};

export const buildFormConfigFromFormatter = (
  formatter: VibeFormatterConfig | undefined
): FormFormattingConfig | undefined => {
  if (!formatter) {
    return undefined;
  }
  const table = buildPresetTable();
  const formConfig: FormFormattingConfig = {};
  for (const [formName, entry] of Object.entries(formatter)) {
    const spec = resolveEntrySpec(entry, table);
    if (!spec) {
      console.warn(
        `[vibe][fmt] Skipping formatter form "${formName}" because it is invalid or references an unknown spec.`
      );
      continue;
    }
    formConfig[formName] = spec;
  }
  return Object.keys(formConfig).length > 0 ? formConfig : undefined;
};

export const loadFormatterFormConfig = (
  sourceRoot: string
): FormFormattingConfig | undefined => {
  const loaded = loadNearestVibeConfig(sourceRoot);
  if (!loaded) {
    return undefined;
  }
  return buildFormConfigFromFormatter(loaded.config.formatter);
};
