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
  FORM_PRESETS,
  mergeFormConfigs,
  type FormFormattingConfig,
  type FormFormattingSpec,
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
}

interface FormatChunk {
  readonly text: string;
  readonly inlineLength: number;
  readonly isMultiline: boolean;
}

type SequenceDelimiter = "()" | "[]";

interface ExpressionFormatOverrides {
  readonly delimiter?: SequenceDelimiter;
}

interface ListFormattingConfig {
  readonly delimiter?: SequenceDelimiter;
  readonly inlineTailCount?: number;
  readonly forceMultiline?: boolean;
}

type ListLikeNode = ListNode | NamespaceImportNode;

export const DEFAULT_FORMAT_OPTIONS: NormalizedFormatOptions = {
  indent: "  ",
  newline: "\n",
  maxWidth: 80,
  formConfig: DEFAULT_FORM_CONFIG,
};

export const formatSource = async (
  source: string,
  options: FormatSourceOptions = {}
): Promise<FormatSourceResult> => {
  const normalized = normalizeOptions(options);
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
  switch (node.kind) {
    case NodeKind.List:
      return formatListNode(node, level, options, overrides.delimiter);
    case NodeKind.NamespaceImport:
      return formatListNode(node, level, options, overrides.delimiter);
    case NodeKind.Quote: {
      const children: FormatChunk[] = [createAtomChunk("quote", options)];
      const forceMultiline = Boolean(
        node.target && isComplexQuoteTarget(node.target)
      );
      if (node.target) {
        children.push(formatExpression(node.target, level + 1, options));
      }
      return formatListChunks(children, level, options, {
        forceMultiline,
      });
    }
    case NodeKind.Symbol:
    case NodeKind.Keyword:
    case NodeKind.Number:
    case NodeKind.String:
    case NodeKind.Boolean:
    case NodeKind.Nil:
      return createAtomChunk(node.lexeme, options);
    default:
      return createAtomChunk("", options);
  }
};

const formatListNode = (
  node: ListLikeNode,
  level: number,
  options: NormalizedFormatOptions,
  delimiterOverride?: SequenceDelimiter
): FormatChunk => {
  const headSymbol = getHeadSymbol(node.elements[0]);
  const formSpec = headSymbol ? options.formConfig[headSymbol] : undefined;
  if (formSpec?.clauseGrouping) {
    return formatClauseGroupedList(
      node,
      level,
      options,
      delimiterOverride,
      formSpec
    );
  }
  const inlineTailCount = formSpec?.inlineHeadArgCount ?? 0;
  const chunks = node.elements.map((child, index) => {
    const forceVector = shouldForceVector(formSpec, index);
    return formatExpression(child, level + 1, options, {
      delimiter: forceVector ? "[]" : undefined,
    });
  });
  return formatListChunks(chunks, level, options, {
    delimiter: delimiterOverride ?? "()",
    inlineTailCount,
    forceMultiline: shouldForceMultilineBody(formSpec, node, inlineTailCount),
  });
};

const formatClauseGroupedList = (
  node: ListLikeNode,
  level: number,
  options: NormalizedFormatOptions,
  delimiterOverride: SequenceDelimiter | undefined,
  spec: FormFormattingSpec
): FormatChunk => {
  const chunks = node.elements.map((child, index) => {
    const forceVector = shouldForceVector(spec, index);
    return formatExpression(child, level + 1, options, {
      delimiter: forceVector ? "[]" : undefined,
    });
  });
  if (chunks.length === 0) {
    return formatListChunks(chunks, level, options, {
      delimiter: delimiterOverride ?? "()",
    });
  }
  const head = chunks[0]!;
  const rest = chunks.slice(1);
  const groupSize = Math.max(1, spec.clauseGrouping?.groupSize ?? 2);
  const clauseChunks: FormatChunk[] = [];
  for (let index = 0; index < rest.length; index += groupSize) {
    const group = rest.slice(index, index + groupSize);
    if (group.length < groupSize) {
      clauseChunks.push(...group);
      break;
    }
    clauseChunks.push(
      joinChunksInlineGroup(group as [FormatChunk, ...FormatChunk[]])
    );
  }
  const combined = [head, ...clauseChunks];
  return formatListChunks(combined, level, options, {
    delimiter: delimiterOverride ?? "()",
    forceMultiline: clauseChunks.length > 0,
  });
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
  const inlineTailCount = Math.min(config.inlineTailCount ?? 0, tail.length);
  const inlineChunks = tail.slice(0, inlineTailCount);
  const bodyChunks = tail.slice(inlineTailCount);
  const headLineParts = [head.text, ...inlineChunks.map((chunk) => chunk.text)];
  const headLine = `${open}${headLineParts.join(" ")}`.trimEnd();
  if (bodyChunks.length === 0) {
    return createListChunk(`${headLine}${close}`, inlineLength, false);
  }
  const newline = options.newline;
  const indentInner = options.indent;
  const bodyLines = bodyChunks.map((chunk, index) => {
    const block = indentBlock(chunk.text, indentInner, newline);
    if (index === bodyChunks.length - 1) {
      return `${block}${close}`;
    }
    return block;
  });
  const text = `${headLine}${newline}${bodyLines.join(newline)}`;
  return createListChunk(text, inlineLength, true);
};

const getHeadSymbol = (node: ExpressionNode | undefined): string | null => {
  if (node && node.kind === NodeKind.Symbol) {
    return node.value;
  }
  return null;
};

const shouldForceVector = (
  spec: FormFormattingSpec | undefined,
  elementIndex: number
): boolean => {
  if (!spec?.vectorArgumentIndices) {
    return false;
  }
  return spec.vectorArgumentIndices.includes(elementIndex);
};

const isComplexQuoteTarget = (node: ExpressionNode): boolean => {
  return node.kind === NodeKind.List || node.kind === NodeKind.NamespaceImport;
};

const joinChunksInline = (
  left: FormatChunk,
  right: FormatChunk
): FormatChunk => {
  const text = `${left.text} ${right.text}`;
  return {
    text,
    inlineLength: left.inlineLength + 1 + right.inlineLength,
    isMultiline: left.isMultiline || right.isMultiline,
  } satisfies FormatChunk;
};

const joinChunksInlineGroup = (chunks: readonly FormatChunk[]): FormatChunk => {
  let current = chunks[0]!;
  for (let index = 1; index < chunks.length; index += 1) {
    current = joinChunksInline(current, chunks[index]!);
  }
  return current;
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
  isMultiline: boolean
): FormatChunk => ({
  text,
  inlineLength,
  isMultiline,
});

const shouldForceMultilineBody = (
  spec: FormFormattingSpec | undefined,
  node: ListLikeNode,
  inlineTailCount: number
): boolean => {
  if (!spec?.forceBodyMultiline) {
    return false;
  }
  const bodyLength = node.elements.length - 1 - inlineTailCount;
  return bodyLength > 0;
};

const normalizeOptions = (
  options: FormatSourceOptions
): NormalizedFormatOptions => ({
  indent: options.indent ?? DEFAULT_FORMAT_OPTIONS.indent,
  newline: options.newline ?? DEFAULT_FORMAT_OPTIONS.newline,
  maxWidth: options.maxWidth ?? DEFAULT_FORMAT_OPTIONS.maxWidth,
  formConfig: normalizeFormConfig(options.formConfig),
});

const normalizeFormConfig = (
  formConfig: FormFormattingConfig | undefined
): FormFormattingConfig => mergeFormConfigs(DEFAULT_FORM_CONFIG, formConfig);
