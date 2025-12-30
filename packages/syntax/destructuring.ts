import {
  NodeKind,
  type ExpressionNode,
  type KeywordNode,
  type SourceSpan,
  type SymbolNode,
  type ListNode,
} from "./index";

export type BindingPattern = SymbolBindingPattern | SequenceBindingPattern;

export interface SymbolBindingPattern {
  readonly kind: "symbol";
  readonly node: SymbolNode;
}

export interface SequenceBindingPattern {
  readonly kind: "sequence";
  readonly node: ListNode;
  readonly elements: readonly BindingPattern[];
  readonly rest?: BindingPattern;
  readonly as?: SymbolNode;
}

/* Map binding patterns removed */

export type PatternParseResult =
  | { ok: true; pattern: BindingPattern }
  | { ok: false; errors: readonly PatternError[] };

export interface PatternError {
  readonly kind: PatternErrorKind;
  readonly message: string;
  readonly span: SourceSpan;
}

export type PatternErrorKind =
  | "PatternUnsupportedNode"
  | "PatternRestRequiresTarget"
  | "PatternRestDuplicate"
  | "PatternAsRequiresSymbol"
  | "PatternAsDuplicate"
  | "PatternMapKeyMissingValue"
  | "PatternMapKeyUnsupported"
  | "PatternMapKeysRequiresVector"
  | "PatternMapStringsRequiresVector"
  | "PatternMapSymbolsRequiresVector"
  | "PatternMapDefaultsRequireMap"
  | "PatternDuplicateBinding";

interface PatternContext {
  readonly names: Map<string, SymbolNode>;
  readonly errors: PatternError[];
}

export const parseBindingPattern = (
  node: ExpressionNode
): PatternParseResult => {
  const context: PatternContext = {
    names: new Map<string, SymbolNode>(),
    errors: [],
  };
  const pattern = parsePattern(node, context);
  if (!pattern || context.errors.length > 0) {
    return {
      ok: false,
      errors:
        context.errors.length > 0
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

const parsePattern = (
  node: ExpressionNode,
  context: PatternContext
): BindingPattern | null => {
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

const registerSymbolPattern = (
  node: SymbolNode,
  context: PatternContext
): SymbolBindingPattern => {
  if (
    node.value !== "&" &&
    node.value !== "_" &&
    context.names.has(node.value)
  ) {
    context.errors.push({
      kind: "PatternDuplicateBinding",
      message: `Duplicate binding ${node.value}`,
      span: node.span,
    });
  } else if (node.value !== "_" && !context.names.has(node.value)) {
    context.names.set(node.value, node);
  }
  return { kind: "symbol", node };
};

const parseSequencePattern = (
  node: ListNode,
  context: PatternContext
): SequenceBindingPattern | null => {
  const elements: BindingPattern[] = [];
  let rest: BindingPattern | undefined;
  let asAlias: SymbolNode | undefined;

  const items = node.elements.filter((element): element is ExpressionNode =>
    Boolean(element)
  );

  for (let index = 0; index < items.length; index += 1) {
    const element = items[index]!;
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
      } else if (asAlias) {
        context.errors.push({
          kind: "PatternAsDuplicate",
          message: "Sequence pattern allows only one :as alias",
          span: aliasNode.span,
        });
      } else {
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
  } satisfies SequenceBindingPattern;
};

/* Map-related helpers removed */

const keywordFromSymbol = (value: string): string => stripKeywordPrefix(value);

const stripKeywordPrefix = (value: string): string =>
  value.startsWith(":") ? value.slice(1) : value;

const isRestMarker = (node: ExpressionNode): node is SymbolNode =>
  node.kind === NodeKind.Symbol && node.value === "&";

const isKeywordNode = (node: ExpressionNode): node is KeywordNode =>
  node.kind === NodeKind.Keyword;

const undefinedSpan = (): SourceSpan => ({
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 },
});
