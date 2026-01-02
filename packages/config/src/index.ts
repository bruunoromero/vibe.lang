export type {
  VibeConfig,
  VibeFormatterConfig,
  VibeFormatterSpec,
  VibeFormatterSpecFactory,
  VibeFormatterSpecEntry,
  VibeFormatterClauseGrouping,
  VibePackageConfig,
  LoadedVibeConfig,
} from "./types";

export {
  findVibeConfigPath,
  findVibeConfigInDir,
  loadNearestVibeConfig,
  loadVibeConfigForDir,
  loadVibeConfigFromPath,
  clearVibeConfigCache,
} from "./loader";

import type {
  VibeConfig,
  VibeFormatterSpec,
  VibeFormatterSpecFactory,
} from "./types";

export const defineConfig = <T extends VibeConfig>(config: T): T => config;

const cloneNumberArray = (
  values: readonly number[] | undefined
): readonly number[] | undefined => {
  if (!values) {
    return undefined;
  }
  return [...values];
};

const cloneClauseGrouping = (
  clause: VibeFormatterSpec["clauseGrouping"]
): VibeFormatterSpec["clauseGrouping"] => {
  if (!clause) {
    return undefined;
  }
  return { ...clause };
};

const cloneFormatterSpec = (spec: VibeFormatterSpec): VibeFormatterSpec => {
  const vectorArgumentIndices = cloneNumberArray(spec.vectorArgumentIndices);
  const clauseGrouping = cloneClauseGrouping(spec.clauseGrouping);
  return {
    ...spec,
    ...(vectorArgumentIndices ? { vectorArgumentIndices } : {}),
    ...(clauseGrouping ? { clauseGrouping } : {}),
  } satisfies VibeFormatterSpec;
};

export const defineFormatterSpec = (
  spec: VibeFormatterSpec
): VibeFormatterSpecFactory => {
  const template = cloneFormatterSpec(spec);
  return () => cloneFormatterSpec(template);
};

export const defineSpec = defineFormatterSpec;
