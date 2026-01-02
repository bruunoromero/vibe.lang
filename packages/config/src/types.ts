export interface VibePackageConfig {
  readonly sources?: readonly string[];
  readonly outDir?: string;
  readonly entry?: string;
}

export interface VibeFormatterClauseGrouping {
  readonly groupSize: number;
}

export interface VibeFormatterSpec {
  /**
   * Number of arguments (after the head symbol) that should remain inline as part of
   * the signature before the formatter breaks onto new lines.
   */
  readonly inlineHeadArgCount?: number;
  /**
   * Argument indices that should always be rendered using vector notation (e.g. binding
   * vectors). Indices are zero-based relative to the form head.
   */
  readonly vectorArgumentIndices?: readonly number[];
  /** Forces the remaining body clauses to be emitted on separate lines. */
  readonly forceBodyMultiline?: boolean;
  readonly clauseGrouping?: VibeFormatterClauseGrouping;
}

export type VibeFormatterSpecFactory = () => VibeFormatterSpec;

export type VibeFormatterSpecEntry =
  | VibeFormatterSpec
  | VibeFormatterSpecFactory
  | string;

export type VibeFormatterConfig = Record<string, VibeFormatterSpecEntry>;

export interface VibeConfig {
  readonly package?: VibePackageConfig;
  readonly formatter?: VibeFormatterConfig;
}

export interface LoadedVibeConfig {
  readonly path: string;
  readonly dir: string;
  readonly config: VibeConfig;
}
