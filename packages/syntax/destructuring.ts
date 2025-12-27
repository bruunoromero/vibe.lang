import {
  NodeKind,
  type ExpressionNode,
  type KeywordNode,
  type MapNode,
  type SourceSpan,
  type SymbolNode,
  type VectorNode,
} from "./index";

export type BindingPattern =
  | SymbolBindingPattern
  | VectorBindingPattern
  | MapBindingPattern;

export interface SymbolBindingPattern {
  readonly kind: "symbol";
  readonly node: SymbolNode;
}

export interface VectorBindingPattern {
  readonly kind: "vector";
  readonly node: VectorNode;
  readonly elements: readonly BindingPattern[];
  readonly rest?: BindingPattern;
  readonly as?: SymbolNode;
}

export interface MapBindingPattern {
  readonly kind: "map";
  readonly node: MapNode;
  readonly properties: readonly MapPropertyPattern[];
  readonly defaults: readonly MapDefaultEntry[];
  readonly as?: SymbolNode;
}

export interface MapPropertyPattern {
  readonly key: MapKeyDescriptor;
  readonly pattern: BindingPattern;
}

export type MapKeyDescriptor =
  | KeywordKeyDescriptor
  | SymbolKeyDescriptor
  | StringKeyDescriptor;

export interface KeywordKeyDescriptor {
  readonly kind: "keyword";
  readonly value: string;
  readonly span: SourceSpan;
}

export interface SymbolKeyDescriptor {
  readonly kind: "symbol";
  readonly value: string;
  readonly span: SourceSpan;
}

export interface StringKeyDescriptor {
  readonly kind: "string";
  readonly value: string;
  readonly span: SourceSpan;
}

export interface MapDefaultEntry {
  readonly binding: string;
  readonly value: ExpressionNode;
  readonly span: SourceSpan;
}

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
                message: "Binding pattern must be a symbol, vector, or map",
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
    case NodeKind.Vector:
      return parseVectorPattern(node, context);
    case NodeKind.Map:
      return parseMapPattern(node, context);
    default:
      context.errors.push({
        kind: "PatternUnsupportedNode",
        message: "Binding pattern must be a symbol, vector, or map",
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

const parseVectorPattern = (
  node: VectorNode,
  context: PatternContext
): VectorBindingPattern | null => {
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
          message: "Vector pattern allows only one & rest binding",
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
          message: "Vector pattern allows only one :as alias",
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
    kind: "vector",
    node,
    elements,
    ...(rest ? { rest } : {}),
    ...(asAlias ? { as: asAlias } : {}),
  } satisfies VectorBindingPattern;
};

const parseMapPattern = (
  node: MapNode,
  context: PatternContext
): MapBindingPattern | null => {
  const properties: MapPropertyPattern[] = [];
  const defaults: MapDefaultEntry[] = [];
  let asAlias: SymbolNode | undefined;

  for (const entry of node.entries) {
    const keyNode = entry.key;
    const valueNode = entry.value;
    if (!keyNode) {
      continue;
    }

    if (isKeywordNode(keyNode)) {
      const keywordName = stripKeywordPrefix(keyNode.value);
      switch (keywordName) {
        case "as": {
          if (!valueNode || valueNode.kind !== NodeKind.Symbol) {
            context.errors.push({
              kind: "PatternAsRequiresSymbol",
              message: ":as requires a symbol alias",
              span: keyNode.span,
            });
            continue;
          }
          if (asAlias) {
            context.errors.push({
              kind: "PatternAsDuplicate",
              message: "Map pattern allows only one :as alias",
              span: valueNode.span,
            });
            continue;
          }
          registerSymbolPattern(valueNode, context);
          asAlias = valueNode;
          continue;
        }
        case "keys": {
          const list = collectSymbolVector(
            valueNode,
            context,
            "PatternMapKeysRequiresVector"
          );
          for (const symbol of list) {
            registerSymbolPattern(symbol, context);
            properties.push({
              key: {
                kind: "keyword",
                value: keywordFromSymbol(symbol.value),
                span: symbol.span,
              },
              pattern: { kind: "symbol", node: symbol },
            });
          }
          continue;
        }
        case "strs": {
          const list = collectSymbolVector(
            valueNode,
            context,
            "PatternMapStringsRequiresVector"
          );
          for (const symbol of list) {
            registerSymbolPattern(symbol, context);
            properties.push({
              key: {
                kind: "string",
                value: symbol.value,
                span: symbol.span,
              },
              pattern: { kind: "symbol", node: symbol },
            });
          }
          continue;
        }
        case "syms": {
          const list = collectSymbolVector(
            valueNode,
            context,
            "PatternMapSymbolsRequiresVector"
          );
          for (const symbol of list) {
            registerSymbolPattern(symbol, context);
            properties.push({
              key: {
                kind: "symbol",
                value: symbol.value,
                span: symbol.span,
              },
              pattern: { kind: "symbol", node: symbol },
            });
          }
          continue;
        }
        case "or": {
          if (!valueNode || valueNode.kind !== NodeKind.Map) {
            context.errors.push({
              kind: "PatternMapDefaultsRequireMap",
              message: ":or requires a map of default values",
              span: keyNode.span,
            });
            continue;
          }
          collectDefaultEntries(valueNode, defaults, context);
          continue;
        }
        default:
          break;
      }
    }

    if (!valueNode) {
      context.errors.push({
        kind: "PatternMapKeyMissingValue",
        message: "Map pattern entry requires a value pattern",
        span: keyNode.span,
      });
      continue;
    }
    const descriptor = resolveMapKeyDescriptor(keyNode);
    if (!descriptor) {
      context.errors.push({
        kind: "PatternMapKeyUnsupported",
        message: "Map pattern keys must be keywords, strings, or symbols",
        span: keyNode.span,
      });
      continue;
    }
    const parsed = parsePattern(valueNode, context);
    if (parsed) {
      properties.push({ key: descriptor, pattern: parsed });
    }
  }

  return {
    kind: "map",
    node,
    properties,
    defaults,
    ...(asAlias ? { as: asAlias } : {}),
  } satisfies MapBindingPattern;
};

const collectSymbolVector = (
  node: ExpressionNode | null | undefined,
  context: PatternContext,
  errorKind:
    | "PatternMapKeysRequiresVector"
    | "PatternMapStringsRequiresVector"
    | "PatternMapSymbolsRequiresVector"
): SymbolNode[] => {
  if (!node || node.kind !== NodeKind.Vector) {
    context.errors.push({
      kind: errorKind,
      message: "Expected a vector of symbols",
      span: node ? node.span : undefinedSpan(),
    });
    return [];
  }
  const symbols: SymbolNode[] = [];
  for (const element of node.elements) {
    if (!element || element.kind !== NodeKind.Symbol) {
      context.errors.push({
        kind: errorKind,
        message: "Expected a symbol inside vector",
        span: element ? element.span : node.span,
      });
      continue;
    }
    symbols.push(element);
  }
  return symbols;
};

const collectDefaultEntries = (
  mapNode: MapNode,
  defaults: MapDefaultEntry[],
  context: PatternContext
): void => {
  for (const entry of mapNode.entries) {
    if (!entry.key || !entry.value) {
      continue;
    }
    const binding = resolveDefaultBindingName(entry.key);
    if (!binding) {
      context.errors.push({
        kind: "PatternMapDefaultsRequireMap",
        message: ":or keys must be symbols or keywords",
        span: entry.key.span,
      });
      continue;
    }
    defaults.push({
      binding,
      value: entry.value,
      span: entry.key.span,
    });
  }
};

const resolveDefaultBindingName = (node: ExpressionNode): string | null => {
  if (node.kind === NodeKind.Symbol) {
    return node.value;
  }
  if (node.kind === NodeKind.Keyword) {
    return stripKeywordPrefix(node.value);
  }
  return null;
};

const resolveMapKeyDescriptor = (
  node: ExpressionNode
): MapKeyDescriptor | null => {
  if (node.kind === NodeKind.Keyword) {
    return {
      kind: "keyword",
      value: stripKeywordPrefix(node.value),
      span: node.span,
    };
  }
  if (node.kind === NodeKind.String) {
    return { kind: "string", value: node.value, span: node.span };
  }
  if (node.kind === NodeKind.Symbol) {
    return { kind: "symbol", value: node.value, span: node.span };
  }
  return null;
};

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
