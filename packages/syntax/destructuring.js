import { NodeKind, } from "./index";
export const parseBindingPattern = (node) => {
    const context = {
        names: new Map(),
        errors: [],
    };
    const pattern = parsePattern(node, context);
    if (!pattern || context.errors.length > 0) {
        return {
            ok: false,
            errors: context.errors.length > 0
                ? context.errors
                : [
                    {
                        kind: "PatternUnsupportedNode",
                        message: "Binding pattern must be a symbol or list",
                        span: node.span,
                    },
                ],
        };
    }
    return { ok: true, pattern };
};
const parsePattern = (node, context) => {
    switch (node.kind) {
        case NodeKind.Symbol:
            return registerSymbolPattern(node, context);
        case NodeKind.List:
            return parseSequencePattern(node, context);
        default:
            context.errors.push({
                kind: "PatternUnsupportedNode",
                message: "Binding pattern must be a symbol or list",
                span: node.span,
            });
            return null;
    }
};
const registerSymbolPattern = (node, context) => {
    if (node.value !== "&" &&
        node.value !== "_" &&
        context.names.has(node.value)) {
        context.errors.push({
            kind: "PatternDuplicateBinding",
            message: `Duplicate binding ${node.value}`,
            span: node.span,
        });
    }
    else if (node.value !== "_" && !context.names.has(node.value)) {
        context.names.set(node.value, node);
    }
    return { kind: "symbol", node };
};
const parseSequencePattern = (node, context) => {
    const elements = [];
    let rest;
    let asAlias;
    const items = node.elements.filter((element) => Boolean(element));
    for (let index = 0; index < items.length; index += 1) {
        const element = items[index];
        if (isRestMarker(element)) {
            if (rest) {
                context.errors.push({
                    kind: "PatternRestDuplicate",
                    message: "Sequence pattern allows only one & rest binding",
                    span: element.span,
                });
                continue;
            }
            const target = items[index + 1];
            if (!target) {
                context.errors.push({
                    kind: "PatternRestRequiresTarget",
                    message: "& must be followed by a binding pattern",
                    span: element.span,
                });
                break;
            }
            const parsed = parsePattern(target, context);
            if (parsed) {
                rest = parsed;
            }
            index += 1;
            continue;
        }
        if (isKeywordNode(element) && stripKeywordPrefix(element.value) === "as") {
            const aliasNode = items[index + 1];
            if (!aliasNode || aliasNode.kind !== NodeKind.Symbol) {
                context.errors.push({
                    kind: "PatternAsRequiresSymbol",
                    message: ":as requires a symbol alias",
                    span: element.span,
                });
            }
            else if (asAlias) {
                context.errors.push({
                    kind: "PatternAsDuplicate",
                    message: "Sequence pattern allows only one :as alias",
                    span: aliasNode.span,
                });
            }
            else {
                registerSymbolPattern(aliasNode, context);
                asAlias = aliasNode;
            }
            index += 1;
            continue;
        }
        const parsed = parsePattern(element, context);
        if (parsed) {
            elements.push(parsed);
        }
    }
    return {
        kind: "sequence",
        node,
        elements,
        ...(rest ? { rest } : {}),
        ...(asAlias ? { as: asAlias } : {}),
    };
};
/* Map-related helpers removed */
const keywordFromSymbol = (value) => stripKeywordPrefix(value);
const stripKeywordPrefix = (value) => value.startsWith(":") ? value.slice(1) : value;
const isRestMarker = (node) => node.kind === NodeKind.Symbol && node.value === "&";
const isKeywordNode = (node) => node.kind === NodeKind.Keyword;
const undefinedSpan = () => ({
    start: { offset: 0, line: 0, column: 0 },
    end: { offset: 0, line: 0, column: 0 },
});
//# sourceMappingURL=destructuring.js.map