import { parseSource } from "@vibe/parser";
import { NodeKind, } from "@vibe/syntax";
import { DEFAULT_FORM_CONFIG, mergeFormConfigs, } from "./form-config";
export { DEFAULT_FORM_CONFIG, FORM_PRESETS, mergeFormConfigs, } from "./form-config";
export { loadFormatterFormConfig, buildFormConfigFromFormatter, } from "./config-loader";
const resolveListDelimiter = (node, override) => {
    if (override) {
        return override;
    }
    if (node.elements.length === 0) {
        return "[]";
    }
    return "()";
};
export const DEFAULT_FORMAT_OPTIONS = {
    indent: "  ",
    newline: "\n",
    maxWidth: 80,
    formConfig: DEFAULT_FORM_CONFIG,
};
export const formatSource = async (source, options = {}) => {
    const normalized = normalizeOptions(options);
    const parse = await parseSource(source);
    if (!parse.ok) {
        return {
            ok: false,
            formatted: source,
            diagnostics: parse.diagnostics,
        };
    }
    const formatted = formatProgramWithOptions(parse.program, normalized);
    return {
        ok: true,
        formatted,
        diagnostics: parse.diagnostics,
    };
};
export const formatProgram = (program, options = {}) => {
    const normalized = normalizeOptions(options);
    return formatProgramWithOptions(program, normalized);
};
const formatProgramWithOptions = (program, options) => {
    if (program.body.length === 0) {
        return "";
    }
    const separator = `${options.newline}${options.newline}`;
    const body = program.body
        .map((expression) => formatExpression(expression, 0, options).text)
        .join(separator);
    return body.length > 0 ? `${body}${options.newline}` : "";
};
const formatExpression = (node, level, options, overrides = {}) => {
    switch (node.kind) {
        case NodeKind.List:
            return formatListNode(node, level, options, overrides);
        case NodeKind.NamespaceImport:
            return formatListNode(node, level, options, overrides);
        case NodeKind.Quote: {
            const children = [createAtomChunk("quote", options)];
            const forceMultiline = Boolean(node.target && isComplexQuoteTarget(node.target));
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
const formatListNode = (node, level, options, overrides = {}) => {
    const delimiterOverride = overrides?.delimiter;
    const headSymbol = getHeadSymbol(node.elements[0]);
    const formSpec = headSymbol ? options.formConfig[headSymbol] : undefined;
    if (formSpec?.clauseGrouping) {
        return formatClauseGroupedList(node, level, options, overrides, delimiterOverride, formSpec);
    }
    const inlineTailCount = formSpec?.inlineHeadArgCount ?? 0;
    const chunks = formatListElements(node, level, options, formSpec, overrides);
    const delimiter = resolveListDelimiter(node, delimiterOverride);
    return formatListChunks(chunks, level, options, {
        delimiter,
        inlineTailCount,
        forceMultiline: shouldForceMultilineBody(formSpec, node, inlineTailCount),
    });
};
const formatClauseGroupedList = (node, level, options, overrides = {}, delimiterOverride, spec) => {
    const chunks = formatListElements(node, level, options, spec, overrides);
    if (chunks.length === 0) {
        return formatListChunks(chunks, level, options, {
            delimiter: delimiterOverride ?? "()",
        });
    }
    const head = chunks[0];
    const rest = chunks.slice(1);
    const groupSize = Math.max(1, spec.clauseGrouping?.groupSize ?? 2);
    const clauseChunks = [];
    for (let index = 0; index < rest.length; index += groupSize) {
        const group = rest.slice(index, index + groupSize);
        if (group.length < groupSize) {
            clauseChunks.push(...group);
            break;
        }
        clauseChunks.push(joinChunksInlineGroup(group));
    }
    const combined = [head, ...clauseChunks];
    const delimiter = resolveListDelimiter(node, delimiterOverride);
    return formatListChunks(combined, level, options, {
        delimiter,
        forceMultiline: clauseChunks.length > 0,
    });
};
const formatListElements = (node, level, options, formSpec, overrides = {}) => {
    const vectorOverrideIndices = overrides.vectorElementIndices ?? [];
    const clauseVectorStart = resolveClauseVectorStart(formSpec, node);
    return node.elements.map((child, index) => {
        const clauseContext = clauseVectorStart !== null &&
            index >= clauseVectorStart &&
            isClauseListNode(child);
        const forceVectorFromSpec = !clauseContext && shouldForceVector(formSpec, index, child);
        const forceVectorFromOverride = vectorOverrideIndices.includes(index);
        const nestedVectorIndices = clauseContext ? [0] : undefined;
        const childOverrides = createChildOverrides(forceVectorFromSpec || forceVectorFromOverride, nestedVectorIndices);
        return formatExpression(child, level + 1, options, childOverrides);
    });
};
const resolveClauseVectorStart = (spec, node) => {
    if (!spec?.vectorArgumentIndices?.length ||
        spec.inlineHeadArgCount === undefined) {
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
const areFunctionClauses = (node, startIndex) => {
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
const isClauseListNode = (node) => {
    if (!node || node.kind !== NodeKind.List) {
        return false;
    }
    const clauseHead = node.elements[0];
    return Boolean(clauseHead && clauseHead.kind === NodeKind.List);
};
const createChildOverrides = (forceVector, vectorElementIndices) => {
    if (!forceVector && !vectorElementIndices) {
        return undefined;
    }
    return {
        ...(forceVector ? { delimiter: "[]" } : {}),
        ...(vectorElementIndices ? { vectorElementIndices } : {}),
    };
};
const shouldForceVector = (spec, elementIndex, child) => {
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
const formatListChunks = (chunks, level, options, config = {}) => {
    const delimiter = config.delimiter ?? "()";
    const [open, close] = delimiter.split("");
    if (chunks.length === 0) {
        const empty = `${open}${close}`;
        return createListChunk(empty, empty.length, false);
    }
    const inlineInner = chunks.map((chunk) => chunk.text).join(" ");
    const inlineText = `${open}${inlineInner}${close}`;
    const inlineLength = inlineText.length;
    const forceMultiline = Boolean(config.forceMultiline && chunks.length > 1);
    const requiresMultiline = inlineLength > options.maxWidth ||
        chunks.some((chunk) => chunk.isMultiline);
    if ((!requiresMultiline && !forceMultiline) || chunks.length === 1) {
        return createListChunk(inlineText, inlineLength, false);
    }
    const head = chunks[0];
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
const getHeadSymbol = (node) => {
    if (node && node.kind === NodeKind.Symbol) {
        return node.value;
    }
    return null;
};
const isComplexQuoteTarget = (node) => {
    return node.kind === NodeKind.List || node.kind === NodeKind.NamespaceImport;
};
const joinChunksInline = (left, right) => {
    const text = `${left.text} ${right.text}`;
    return {
        text,
        inlineLength: left.inlineLength + 1 + right.inlineLength,
        isMultiline: left.isMultiline || right.isMultiline,
    };
};
const joinChunksInlineGroup = (chunks) => {
    let current = chunks[0];
    for (let index = 1; index < chunks.length; index += 1) {
        current = joinChunksInline(current, chunks[index]);
    }
    return current;
};
const indentBlock = (text, indent, newline) => {
    const lines = text.split(newline);
    return lines.map((line) => indent + line).join(newline);
};
const createAtomChunk = (text, options) => {
    return {
        text,
        inlineLength: text.length,
        isMultiline: text.includes(options.newline),
    };
};
const createListChunk = (text, inlineLength, isMultiline) => ({
    text,
    inlineLength,
    isMultiline,
});
const shouldForceMultilineBody = (spec, node, inlineTailCount) => {
    if (!spec?.forceBodyMultiline) {
        return false;
    }
    const bodyLength = node.elements.length - 1 - inlineTailCount;
    return bodyLength > 0;
};
const normalizeOptions = (options) => ({
    indent: options.indent ?? DEFAULT_FORMAT_OPTIONS.indent,
    newline: options.newline ?? DEFAULT_FORMAT_OPTIONS.newline,
    maxWidth: options.maxWidth ?? DEFAULT_FORMAT_OPTIONS.maxWidth,
    formConfig: normalizeFormConfig(options.formConfig),
});
const normalizeFormConfig = (formConfig) => mergeFormConfigs(DEFAULT_FORM_CONFIG, formConfig);
//# sourceMappingURL=index.js.map