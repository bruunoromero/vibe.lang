import { parseSource } from "@vibe/parser";
import {
  NodeKind,
  type Diagnostic,
  type ExpressionNode,
  type ListNode,
  type NamespaceImportNode,
  type ProgramNode,
} from "@vibe/syntax";
import {
  DEFAULT_FORM_CONFIG,
  mergeFormConfigs,
  type FormFormattingConfig,
  type FormFormattingSpec,
  type VectorArgumentGroupingSpec,
} from "./form-config";

export type { FormFormattingConfig, FormFormattingSpec } from "./form-config";
export {
  DEFAULT_FORM_CONFIG,
  FORM_PRESETS,
  mergeFormConfigs,
} from "./form-config";
export {
  loadFormatterFormConfig,
  buildFormConfigFromFormatter,
} from "./config-loader";

export interface FormatSourceOptions {
  readonly indent?: string;
  readonly newline?: string;
  readonly maxWidth?: number;
  readonly formConfig?: FormFormattingConfig;
  readonly sourceText?: string;
}

export interface FormatSourceResult {
  readonly ok: boolean;
  readonly formatted: string;
  readonly diagnostics: readonly Diagnostic[];
}

interface NormalizedFormatOptions {
  readonly indent: string;
  readonly newline: string;
  readonly maxWidth: number;
  readonly formConfig: FormFormattingConfig;
  readonly sourceText?: string;
}

interface FormatChunk {
  readonly text: string;
  readonly inlineLength: number;
  readonly isMultiline: boolean;
  readonly inlineHangRelativeColumns?: number;
}

type SequenceDelimiter = "()" | "[]";

interface ExpressionFormatOverrides {
  readonly delimiter?: SequenceDelimiter;
  readonly vectorElementIndices?: readonly number[];
  readonly elementGroupingSize?: number;
  readonly inlineHangRelativeColumns?: number;
}

interface ListFormattingConfig {
  readonly delimiter?: SequenceDelimiter;
  readonly inlineTailCount?: number;
  readonly forceMultiline?: boolean;
  readonly alignBodyWithInlineHead?: boolean;
  readonly inlineTailFromUserLayout?: boolean;
}

interface UserLayoutPreference {
  readonly inlineTailCount: number;
  readonly forceMultiline: boolean;
}

type ListLikeNode = ListNode | NamespaceImportNode;

const resolveListDelimiter = (
  node: ListLikeNode,
  override?: SequenceDelimiter
): SequenceDelimiter => {
  if (override) {
    return override;
  }
  if (node.elements.length === 0) {
    return "[]";
  }
  return "()";
};

export const DEFAULT_FORMAT_OPTIONS: NormalizedFormatOptions = {
  indent: "  ",
  newline: "\n",
  maxWidth: 80,
  formConfig: DEFAULT_FORM_CONFIG,
  sourceText: undefined,
};

export const formatSource = async (
  source: string,
  options: FormatSourceOptions = {}
): Promise<FormatSourceResult> => {
  const normalized = normalizeOptions({
    ...options,
    sourceText: options.sourceText ?? source,
  });
  const parse = await parseSource(source);
  if (!parse.ok) {
    return {
      ok: false,
      formatted: source,
      diagnostics: parse.diagnostics,
    } satisfies FormatSourceResult;
  }
  const formatted = formatProgramWithOptions(parse.program, normalized);
  return {
    ok: true,
    formatted,
    diagnostics: parse.diagnostics,
  } satisfies FormatSourceResult;
};

export const formatProgram = (
  program: ProgramNode,
  options: FormatSourceOptions = {}
): string => {
  const normalized = normalizeOptions(options);
  return formatProgramWithOptions(program, normalized);
};

const formatProgramWithOptions = (
  program: ProgramNode,
  options: NormalizedFormatOptions
): string => {
  if (program.body.length === 0) {
    return "";
  }
  const separator = `${options.newline}${options.newline}`;
  const body = program.body
    .map((expression) => formatExpression(expression, 0, options).text)
    .join(separator);
  return body.length > 0 ? `${body}${options.newline}` : "";
};

const formatExpression = (
  node: ExpressionNode,
  level: number,
  options: NormalizedFormatOptions,
  overrides: ExpressionFormatOverrides = {}
): FormatChunk => {
  let chunk: FormatChunk;
  switch (node.kind) {
    case NodeKind.List:
      chunk = formatListNode(node, level, options, overrides);
      break;
    case NodeKind.NamespaceImport:
      chunk = formatListNode(node, level, options, overrides);
      break;
    case NodeKind.Quote: {
      const children: FormatChunk[] = [createAtomChunk("quote", options)];
      const forceMultiline = Boolean(
        node.target && isComplexQuoteTarget(node.target)
      );
      if (node.target) {
        children.push(formatExpression(node.target, level + 1, options));
      }
      chunk = formatListChunks(children, level, options, {
        forceMultiline,
      });
      break;
    }
    case NodeKind.Symbol:
    case NodeKind.Keyword:
    case NodeKind.Number:
    case NodeKind.String:
    case NodeKind.Boolean:
    case NodeKind.Nil:
      chunk = createAtomChunk(node.lexeme, options);
      break;
    default:
      chunk = createAtomChunk("", options);
      break;
  }
  return applyChunkOverrides(chunk, overrides);
};

const formatListNode = (
  node: ListLikeNode,
  level: number,
  options: NormalizedFormatOptions,
  overrides: ExpressionFormatOverrides = {}
): FormatChunk => {
  const delimiterOverride = overrides?.delimiter;
  const headSymbol = getHeadSymbol(node.elements[0]);
  const formSpec = headSymbol ? options.formConfig[headSymbol] : undefined;
  const clauseVectorStart = resolveClauseVectorStart(formSpec, node);
  const hasStructuralOverrides = Boolean(
    overrides?.delimiter ||
      (overrides?.vectorElementIndices &&
        overrides.vectorElementIndices.length > 0) ||
      overrides?.elementGroupingSize
  );
  const userLayoutPreference =
    !formSpec && !hasStructuralOverrides && options.sourceText
      ? captureUserLayoutPreference(node, options.sourceText)
      : undefined;
  let inlineTailCount =
    formSpec?.inlineHeadArgCount ?? userLayoutPreference?.inlineTailCount ?? 0;
  if (clauseVectorStart !== null && inlineTailCount > 0) {
    inlineTailCount -= 1;
  }
  const inlineTailFromUserLayout = Boolean(!formSpec && userLayoutPreference);
  if (formSpec?.clauseGrouping) {
    return formatClauseGroupedList(
      node,
      level,
      options,
      overrides,
      delimiterOverride,
      formSpec,
      clauseVectorStart
    );
  }
  const chunks = formatListElements(
    node,
    level,
    options,
    formSpec,
    overrides,
    clauseVectorStart,
    headSymbol
  );
  const groupedChunks = applyElementGrouping(
    chunks,
    overrides.elementGroupingSize,
    options.newline
  );
  const delimiter = resolveListDelimiter(node, delimiterOverride);
  const forceGroupingMultiline = shouldForceGroupingMultiline(
    overrides.elementGroupingSize,
    groupedChunks
  );
  const alignBodyWithInlineHead = Boolean(
    !formSpec &&
      userLayoutPreference?.forceMultiline &&
      (userLayoutPreference?.inlineTailCount ?? 0) > 0
  );
  return formatListChunks(groupedChunks, level, options, {
    delimiter,
    inlineTailCount,
    forceMultiline:
      shouldForceMultilineBody(formSpec, node, inlineTailCount) ||
      forceGroupingMultiline ||
      Boolean(userLayoutPreference?.forceMultiline),
    alignBodyWithInlineHead,
    inlineTailFromUserLayout,
  });
};

const formatClauseGroupedList = (
  node: ListLikeNode,
  level: number,
  options: NormalizedFormatOptions,
  overrides: ExpressionFormatOverrides = {},
  delimiterOverride: SequenceDelimiter | undefined,
  spec: FormFormattingSpec,
  clauseVectorStart: number | null
): FormatChunk => {
  const chunks = formatListElements(
    node,
    level,
    options,
    spec,
    overrides,
    clauseVectorStart,
    getHeadSymbol(node.elements[0])
  );
  const groupedChunks = applyElementGrouping(
    chunks,
    overrides.elementGroupingSize,
    options.newline
  );
  if (groupedChunks.length === 0) {
    return formatListChunks(groupedChunks, level, options, {
      delimiter: delimiterOverride ?? "()",
    });
  }
  const head = groupedChunks[0]!;
  const rest = groupedChunks.slice(1);
  const groupSize = Math.max(1, spec.clauseGrouping?.groupSize ?? 2);
  const clauseChunks: FormatChunk[] = [];
  for (let index = 0; index < rest.length; index += groupSize) {
    const group = rest.slice(index, index + groupSize);
    if (group.length < groupSize) {
      clauseChunks.push(...group);
      break;
    }
    clauseChunks.push(
      joinChunksInlineGroup(
        group as [FormatChunk, ...FormatChunk[]],
        options.newline
      )
    );
  }
  const combined = [head, ...clauseChunks];
  const delimiter = resolveListDelimiter(node, delimiterOverride);
  const forceGroupingMultiline = shouldForceGroupingMultiline(
    overrides.elementGroupingSize,
    groupedChunks
  );
  return formatListChunks(combined, level, options, {
    delimiter,
    forceMultiline: clauseChunks.length > 0 || forceGroupingMultiline,
  });
};

const formatListElements = (
  node: ListLikeNode,
  level: number,
  options: NormalizedFormatOptions,
  formSpec: FormFormattingSpec | undefined,
  overrides: ExpressionFormatOverrides = {},
  clauseVectorStart: number | null,
  headSymbol: string | null
): FormatChunk[] => {
  const vectorOverrideIndices = overrides.vectorElementIndices ?? [];
  return node.elements.map((child, index) => {
    const clauseContext =
      clauseVectorStart !== null &&
      index >= clauseVectorStart &&
      isClauseListNode(child);
    const forceVectorFromSpec =
      !clauseContext && shouldForceVector(formSpec, index, child);
    const forceVectorFromOverride = vectorOverrideIndices.includes(index);
    const nestedVectorIndices = clauseContext ? ([0] as const) : undefined;
    const vectorGrouping = clauseContext
      ? undefined
      : resolveVectorGroupingSpec(formSpec, index);
    const vectorGroupingSize =
      vectorGrouping && vectorGrouping.groupSize > 1
        ? vectorGrouping.groupSize
        : undefined;
    const childOverrides = createChildOverrides(
      forceVectorFromSpec || forceVectorFromOverride,
      nestedVectorIndices,
      vectorGroupingSize,
      resolveInlineHangRelativeColumns(
        formSpec,
        index,
        headSymbol,
        vectorGrouping
      )
    );
    return formatExpression(child, level + 1, options, childOverrides);
  });
};

const resolveClauseVectorStart = (
  spec: FormFormattingSpec | undefined,
  node: ListLikeNode
): number | null => {
  if (
    !spec?.vectorArgumentIndices?.length ||
    spec.inlineHeadArgCount === undefined
  ) {
    return null;
  }
  const clauseStartIndex = spec.inlineHeadArgCount;
  if (!spec.vectorArgumentIndices.includes(clauseStartIndex)) {
    return null;
  }
  if (!areFunctionClauses(node, clauseStartIndex)) {
    return null;
  }
  return clauseStartIndex;
};

const areFunctionClauses = (
  node: ListLikeNode,
  startIndex: number
): boolean => {
  if (node.elements.length <= startIndex) {
    return false;
  }
  for (let index = startIndex; index < node.elements.length; index += 1) {
    if (!isClauseListNode(node.elements[index])) {
      return false;
    }
  }
  return true;
};

const isClauseListNode = (
  node: ExpressionNode | undefined
): node is ListNode => {
  if (!node || node.kind !== NodeKind.List) {
    return false;
  }
  const clauseHead = node.elements[0];
  return Boolean(clauseHead && clauseHead.kind === NodeKind.List);
};

const createChildOverrides = (
  forceVector: boolean,
  vectorElementIndices: readonly number[] | undefined,
  elementGroupingSize?: number,
  inlineHangRelativeColumns?: number
): ExpressionFormatOverrides | undefined => {
  if (
    !forceVector &&
    !vectorElementIndices &&
    !elementGroupingSize &&
    !inlineHangRelativeColumns
  ) {
    return undefined;
  }
  return {
    ...(forceVector ? { delimiter: "[]" as SequenceDelimiter } : {}),
    ...(vectorElementIndices ? { vectorElementIndices } : {}),
    ...(elementGroupingSize ? { elementGroupingSize } : {}),
    ...(inlineHangRelativeColumns ? { inlineHangRelativeColumns } : {}),
  } satisfies ExpressionFormatOverrides;
};

const applyElementGrouping = (
  chunks: readonly FormatChunk[],
  groupSize: number | undefined,
  newline: string
): readonly FormatChunk[] => {
  if (!groupSize || groupSize <= 1 || chunks.length === 0) {
    return chunks;
  }
  const grouped: FormatChunk[] = [];
  for (let index = 0; index < chunks.length; index += groupSize) {
    const group = chunks.slice(index, index + groupSize);
    grouped.push(joinChunksInlineGroup(group, newline));
  }
  return grouped;
};

const shouldForceGroupingMultiline = (
  groupSize: number | undefined,
  chunks: readonly FormatChunk[]
): boolean => Boolean(groupSize && groupSize > 1 && chunks.length > 1);

const applyInlineHangIndentToChunk = (
  chunk: FormatChunk,
  chunkStartColumn: number,
  newline: string
): string => {
  if (!chunk.inlineHangRelativeColumns || !chunk.isMultiline) {
    return chunk.text;
  }
  const targetColumn = chunkStartColumn + chunk.inlineHangRelativeColumns;
  return applyHangIndent(chunk.text, newline, targetColumn);
};

const applyHangIndent = (
  text: string,
  newline: string,
  targetColumn: number
): string => {
  const lines = text.split(newline);
  if (lines.length <= 1) {
    return text;
  }
  const adjusted = lines.map((line, index) => {
    if (index === 0) {
      return line;
    }
    const existingIndent = countLeadingSpaces(line);
    if (existingIndent >= targetColumn) {
      return line;
    }
    const needed = targetColumn - existingIndent;
    return `${" ".repeat(needed)}${line}`;
  });
  return adjusted.join(newline);
};

const countLeadingSpaces = (text: string): number => {
  let count = 0;
  while (count < text.length && text[count] === " ") {
    count += 1;
  }
  return count;
};

const shouldForceVector = (
  spec: FormFormattingSpec | undefined,
  elementIndex: number,
  child: ExpressionNode | undefined
): boolean => {
  if (!spec?.vectorArgumentIndices?.includes(elementIndex)) {
    return false;
  }
  if (!child || child.kind !== NodeKind.List) {
    return false;
  }
  const head = getHeadSymbol(child.elements[0]);
  if (head === "spread") {
    return false;
  }
  return true;
};

const resolveVectorGroupingSpec = (
  spec: FormFormattingSpec | undefined,
  elementIndex: number
): VectorArgumentGroupingSpec | undefined =>
  spec?.vectorArgumentGroupings?.find(
    (item) => item.argumentIndex === elementIndex
  );

const resolveInlineHangRelativeColumns = (
  spec: FormFormattingSpec | undefined,
  elementIndex: number,
  headSymbol: string | null,
  vectorGrouping: VectorArgumentGroupingSpec | undefined
): number | undefined => {
  if (!headSymbol || !vectorGrouping) {
    return undefined;
  }
  if (!vectorGrouping.inlineHangRelativeColumns) {
    return undefined;
  }
  const inlineHeadArgs = spec?.inlineHeadArgCount ?? 0;
  if (elementIndex > inlineHeadArgs) {
    return undefined;
  }
  return vectorGrouping.inlineHangRelativeColumns;
};

const formatListChunks = (
  chunks: readonly FormatChunk[],
  level: number,
  options: NormalizedFormatOptions,
  config: ListFormattingConfig = {}
): FormatChunk => {
  const delimiter = config.delimiter ?? "()";
  const [open, close] = delimiter.split("") as [string, string];
  if (chunks.length === 0) {
    const empty = `${open}${close}`;
    return createListChunk(empty, empty.length, false);
  }
  const inlineInner = chunks.map((chunk) => chunk.text).join(" ");
  const inlineText = `${open}${inlineInner}${close}`;
  const inlineLength = inlineText.length;
  const forceMultiline = Boolean(config.forceMultiline && chunks.length > 1);
  const requiresMultiline =
    inlineLength > options.maxWidth ||
    chunks.some((chunk) => chunk.isMultiline);
  if ((!requiresMultiline && !forceMultiline) || chunks.length === 1) {
    return createListChunk(inlineText, inlineLength, false);
  }
  const head = chunks[0]!;
  const tail = chunks.slice(1);
  if (tail.length === 0) {
    return createListChunk(inlineText, inlineLength, false);
  }
  let inlineTailCount = Math.min(config.inlineTailCount ?? 0, tail.length);
  if (
    config.inlineTailFromUserLayout &&
    !config.alignBodyWithInlineHead &&
    inlineLength > options.maxWidth
  ) {
    inlineTailCount = 0;
  }
  const inlineChunks = tail.slice(0, inlineTailCount);
  const bodyChunks = tail.slice(inlineTailCount);
  const inlineChunkTexts: string[] = [];
  let inlinePrefixLength = open.length + head.text.length;
  inlineChunks.forEach((chunk) => {
    inlinePrefixLength += 1; // space before chunk
    inlineChunkTexts.push(
      applyInlineHangIndentToChunk(chunk, inlinePrefixLength, options.newline)
    );
    inlinePrefixLength += chunk.inlineLength;
  });
  const headLineParts = [head.text, ...inlineChunkTexts];
  const headLine = `${open}${headLineParts.join(" ")}`.trimEnd();
  if (bodyChunks.length === 0) {
    return createListChunk(`${headLine}${close}`, inlineLength, false);
  }
  const newline = options.newline;
  const baseHangIndent =
    config.alignBodyWithInlineHead && inlineChunks.length > 0
      ? createHangIndentString(open, head)
      : undefined;
  const indentInner = baseHangIndent ?? options.indent;
  const bodyLines = bodyChunks.map((chunk, index) => {
    const block = indentBlock(chunk.text, indentInner, newline);
    if (index === bodyChunks.length - 1) {
      return `${block}${close}`;
    }
    return block;
  });
  const text = `${headLine}${newline}${bodyLines.join(newline)}`;
  const inlineHangColumns = baseHangIndent
    ? baseHangIndent.length
    : options.indent.length;
  return createListChunk(text, inlineLength, true, inlineHangColumns);
};

const getHeadSymbol = (node: ExpressionNode | undefined): string | null => {
  if (node && node.kind === NodeKind.Symbol) {
    return node.value;
  }
  return null;
};

const isComplexQuoteTarget = (node: ExpressionNode): boolean => {
  return node.kind === NodeKind.List || node.kind === NodeKind.NamespaceImport;
};

const isListLikeExpression = (node: ExpressionNode): boolean =>
  isComplexQuoteTarget(node);

const joinChunksInline = (
  left: FormatChunk,
  right: FormatChunk,
  newline: string
): FormatChunk => {
  const startColumn = getLastLineLength(left.text, newline) + 1;
  const hangColumns =
    right.inlineHangRelativeColumns ?? inferBodyIndent(right, newline);
  const adjustedRightText = applyHangIndent(
    right.text,
    newline,
    startColumn + hangColumns
  );
  const text = `${left.text} ${adjustedRightText}`;
  return {
    text,
    inlineLength: left.inlineLength + 1 + right.inlineLength,
    isMultiline: left.isMultiline || right.isMultiline,
  } satisfies FormatChunk;
};

const joinChunksInlineGroup = (
  chunks: readonly FormatChunk[],
  newline: string
): FormatChunk => {
  let current = chunks[0]!;
  for (let index = 1; index < chunks.length; index += 1) {
    current = joinChunksInline(current, chunks[index]!, newline);
  }
  return current;
};

const createHangIndentString = (
  openDelimiter: string,
  headChunk: FormatChunk
): string => {
  const baseColumns = Math.max(
    0,
    openDelimiter.length + headChunk.inlineLength + 1
  );
  if (baseColumns === 0) {
    return "";
  }
  return " ".repeat(baseColumns);
};

const getLastLineLength = (text: string, newline: string): number => {
  const lines = text.split(newline);
  const last = lines[lines.length - 1];
  return last ? last.length : 0;
};

const inferBodyIndent = (chunk: FormatChunk, newline: string): number => {
  if (!chunk.isMultiline) {
    return 0;
  }
  const lines = chunk.text.split(newline);
  if (lines.length <= 1) {
    return 0;
  }
  return countLeadingSpaces(lines[1] ?? "");
};

const indentBlock = (text: string, indent: string, newline: string): string => {
  const lines = text.split(newline);
  return lines.map((line) => indent + line).join(newline);
};

const createAtomChunk = (
  text: string,
  options: NormalizedFormatOptions
): FormatChunk => {
  return {
    text,
    inlineLength: text.length,
    isMultiline: text.includes(options.newline),
  } satisfies FormatChunk;
};

const createListChunk = (
  text: string,
  inlineLength: number,
  isMultiline: boolean,
  inlineHangRelativeColumns?: number
): FormatChunk => ({
  text,
  inlineLength,
  isMultiline,
  ...(inlineHangRelativeColumns !== undefined
    ? { inlineHangRelativeColumns }
    : {}),
});

const applyChunkOverrides = (
  chunk: FormatChunk,
  overrides: ExpressionFormatOverrides | undefined
): FormatChunk => {
  if (!overrides?.inlineHangRelativeColumns) {
    return chunk;
  }
  if (chunk.inlineHangRelativeColumns === overrides.inlineHangRelativeColumns) {
    return chunk;
  }
  return {
    ...chunk,
    inlineHangRelativeColumns: overrides.inlineHangRelativeColumns,
  } satisfies FormatChunk;
};

const shouldForceMultilineBody = (
  spec: FormFormattingSpec | undefined,
  node: ListLikeNode,
  inlineTailCount: number
): boolean => {
  const bodyLength = node.elements.length - 1 - inlineTailCount;
  if (bodyLength <= 0) {
    return false;
  }
  if (spec?.forceBodyMultiline) {
    return true;
  }
  if (spec?.forceListBodyMultiline) {
    const firstBodyIndex = 1 + inlineTailCount;
    const firstBodyNode = node.elements[firstBodyIndex];
    if (firstBodyNode && isListLikeExpression(firstBodyNode)) {
      return true;
    }
  }
  return false;
};

const captureUserLayoutPreference = (
  node: ListLikeNode,
  sourceText: string
): UserLayoutPreference | undefined => {
  const elementCount = node.elements.length;
  if (elementCount <= 1) {
    return undefined;
  }
  let inlineTailCount = 0;
  let observedLineBreak = false;
  for (let index = 1; index < elementCount; index += 1) {
    const previous = node.elements[index - 1];
    const current = node.elements[index];
    if (!previous || !current) {
      continue;
    }
    if (
      hasLineBreakBetweenOffsets(
        sourceText,
        previous.span.end.offset,
        current.span.start.offset
      )
    ) {
      observedLineBreak = true;
      break;
    }
    inlineTailCount += 1;
  }
  if (!observedLineBreak) {
    inlineTailCount = Math.max(0, elementCount - 1);
  }
  return {
    inlineTailCount,
    forceMultiline: observedLineBreak,
  } satisfies UserLayoutPreference;
};

const hasLineBreakBetweenOffsets = (
  sourceText: string,
  startOffset: number,
  endOffset: number
): boolean => {
  if (startOffset >= endOffset) {
    return false;
  }
  const safeStart = Math.max(0, Math.min(sourceText.length, startOffset));
  const safeEnd = Math.max(safeStart, Math.min(sourceText.length, endOffset));
  for (let index = safeStart; index < safeEnd; index += 1) {
    if (sourceText[index] === "\n") {
      return true;
    }
  }
  return false;
};

const normalizeOptions = (
  options: FormatSourceOptions
): NormalizedFormatOptions => ({
  indent: options.indent ?? DEFAULT_FORMAT_OPTIONS.indent,
  newline: options.newline ?? DEFAULT_FORMAT_OPTIONS.newline,
  maxWidth: options.maxWidth ?? DEFAULT_FORMAT_OPTIONS.maxWidth,
  formConfig: normalizeFormConfig(options.formConfig),
  sourceText: options.sourceText,
});

const normalizeFormConfig = (
  formConfig: FormFormattingConfig | undefined
): FormFormattingConfig => mergeFormConfigs(DEFAULT_FORM_CONFIG, formConfig);
