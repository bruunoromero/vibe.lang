import type {
  ExpressionNode,
  NodeKind,
  SourceSpan,
  SymbolNode,
  NumberNode,
  StringNode,
  BooleanNode,
  NilNode,
  ListNode,
  VectorNode,
  SetNode,
  MapNode,
  MapEntryNode,
} from "@vibe/syntax";
import { NodeKind as NK } from "@vibe/syntax";
import type { Environment } from "./environment";

/**
 * Runtime values produced by the interpreter.
 * These represent the results of evaluating AST nodes.
 */
export type Value =
  | NumberValue
  | StringValue
  | BooleanValue
  | NilValue
  | SymbolValue
  | ListValue
  | VectorValue
  | SetValue
  | MapValue
  | FunctionValue
  | BuiltinValue;

export interface NumberValue {
  readonly kind: "number";
  readonly value: number;
}

export interface StringValue {
  readonly kind: "string";
  readonly value: string;
}

export interface BooleanValue {
  readonly kind: "boolean";
  readonly value: boolean;
}

export interface NilValue {
  readonly kind: "nil";
}

export interface SymbolValue {
  readonly kind: "symbol";
  readonly value: string;
}

export interface ListValue {
  readonly kind: "list";
  readonly elements: readonly Value[];
}

export interface VectorValue {
  readonly kind: "vector";
  readonly elements: readonly Value[];
}

export interface SetValue {
  readonly kind: "set";
  readonly elements: readonly Value[];
}

export interface MapValue {
  readonly kind: "map";
  readonly entries: ReadonlyMap<string, Value>;
}

/**
 * User-defined function with closure.
 * Captures the lexical environment at definition time.
 */
export interface FunctionValue {
  readonly kind: "function";
  readonly params: readonly string[];
  readonly rest?: string;
  readonly body: readonly ExpressionNode[];
  readonly closure: Environment;
  readonly span: SourceSpan;
}

/**
 * Built-in primitive operation.
 */
export interface BuiltinValue {
  readonly kind: "builtin";
  readonly name: string;
  readonly fn: BuiltinFunction;
}

export type BuiltinFunction = (
  args: readonly Value[],
  span: SourceSpan
) => Value;

// Type guards

export const isNumber = (value: Value): value is NumberValue =>
  value.kind === "number";

export const isString = (value: Value): value is StringValue =>
  value.kind === "string";

export const isBoolean = (value: Value): value is BooleanValue =>
  value.kind === "boolean";

export const isNil = (value: Value): value is NilValue => value.kind === "nil";

export const isSymbol = (value: Value): value is SymbolValue =>
  value.kind === "symbol";

export const isList = (value: Value): value is ListValue =>
  value.kind === "list";

export const isVector = (value: Value): value is VectorValue =>
  value.kind === "vector";

export const isSet = (value: Value): value is SetValue => value.kind === "set";

export const isMap = (value: Value): value is MapValue => value.kind === "map";

export const isFunction = (value: Value): value is FunctionValue =>
  value.kind === "function";

export const isBuiltin = (value: Value): value is BuiltinValue =>
  value.kind === "builtin";

export const isCallable = (
  value: Value
): value is FunctionValue | BuiltinValue =>
  isFunction(value) || isBuiltin(value);

export const isSequence = (
  value: Value
): value is ListValue | VectorValue | SetValue =>
  isList(value) || isVector(value) || isSet(value);

// Truthiness: nil and false are falsy, everything else is truthy

export const isTruthy = (value: Value): boolean =>
  !(isNil(value) || (isBoolean(value) && !value.value));

// Equality

export const valuesEqual = (a: Value, b: Value): boolean => {
  if (a.kind !== b.kind) return false;

  switch (a.kind) {
    case "number":
      return a.value === (b as NumberValue).value;
    case "string":
      return a.value === (b as StringValue).value;
    case "boolean":
      return a.value === (b as BooleanValue).value;
    case "nil":
      return true;
    case "symbol":
      return a.value === (b as SymbolValue).value;
    case "list":
    case "vector":
    case "set": {
      const bSeq = b as ListValue | VectorValue | SetValue;
      if (a.elements.length !== bSeq.elements.length) return false;
      return a.elements.every((elem, i) =>
        valuesEqual(elem, bSeq.elements[i]!)
      );
    }
    case "map": {
      const bMap = b as MapValue;
      if (a.entries.size !== bMap.entries.size) return false;
      for (const [key, value] of a.entries) {
        const bValue = bMap.entries.get(key);
        if (!bValue || !valuesEqual(value, bValue)) return false;
      }
      return true;
    }
    case "function":
    case "builtin":
      // Functions are compared by reference (identity)
      return a === b;
    default:
      return false;
  }
};

// Conversion: Value → AST Node

export const valueToNode = (value: Value, span: SourceSpan): ExpressionNode => {
  switch (value.kind) {
    case "number": {
      const node: NumberNode = {
        kind: NK.Number,
        span,
        lexeme: value.value.toString(),
        value: value.value,
      };
      return node;
    }
    case "string": {
      const node: StringNode = {
        kind: NK.String,
        span,
        lexeme: `"${value.value}"`,
        value: value.value,
      };
      return node;
    }
    case "boolean": {
      const node: BooleanNode = {
        kind: NK.Boolean,
        span,
        lexeme: value.value ? "true" : "false",
        value: value.value,
      };
      return node;
    }
    case "nil": {
      const node: NilNode = {
        kind: NK.Nil,
        span,
        lexeme: "nil",
        value: null,
      };
      return node;
    }
    case "symbol": {
      const node: SymbolNode = {
        kind: NK.Symbol,
        span,
        lexeme: value.value,
        value: value.value,
      };
      return node;
    }
    case "list": {
      const node: ListNode = {
        kind: NK.List,
        span,
        elements: value.elements.map((elem) => valueToNode(elem, span)),
      };
      return node;
    }
    case "vector": {
      const node: VectorNode = {
        kind: NK.Vector,
        span,
        elements: value.elements.map((elem) => valueToNode(elem, span)),
      };
      return node;
    }
    case "set": {
      const node: SetNode = {
        kind: NK.Set,
        span,
        elements: value.elements.map((elem) => valueToNode(elem, span)),
      };
      return node;
    }
    case "map": {
      const entries: MapEntryNode[] = [];
      for (const [keyStr, val] of value.entries) {
        const key: SymbolNode = {
          kind: NK.Symbol,
          span,
          lexeme: keyStr,
          value: keyStr,
        };
        entries.push({
          kind: NK.MapEntry,
          span,
          key,
          value: valueToNode(val, span),
        });
      }
      const node: MapNode = {
        kind: NK.Map,
        span,
        entries,
      };
      return node;
    }
    case "function":
      throw new Error(
        "Cannot convert function value to AST node (functions are not serializable)"
      );
    case "builtin":
      throw new Error(
        "Cannot convert builtin value to AST node (builtins are not serializable)"
      );
    default:
      throw new Error(`Unsupported value kind: ${(value as Value).kind}`);
  }
};

// Conversion: AST Node → Value (for literals only)

export const nodeToValue = (node: ExpressionNode): Value => {
  switch (node.kind) {
    case NK.Number:
      return { kind: "number", value: node.value };
    case NK.String:
      return { kind: "string", value: node.value };
    case NK.Boolean:
      return { kind: "boolean", value: node.value };
    case NK.Nil:
      return { kind: "nil" };
    case NK.Symbol:
      return { kind: "symbol", value: node.value };
    case NK.List:
      return {
        kind: "list",
        elements: node.elements
          .filter((e): e is ExpressionNode => e !== null)
          .map(nodeToValue),
      };
    case NK.Vector:
      return {
        kind: "vector",
        elements: node.elements
          .filter((e): e is ExpressionNode => e !== null)
          .map(nodeToValue),
      };
    case NK.Set:
      return {
        kind: "set",
        elements: node.elements
          .filter((e): e is ExpressionNode => e !== null)
          .map(nodeToValue),
      };
    case NK.Map: {
      const entries = new Map<string, Value>();
      for (const entry of node.entries) {
        if (entry.key && entry.value) {
          const key = nodeToValue(entry.key);
          if (!isSymbol(key) && !isString(key)) {
            throw new Error("Map keys must be symbols or strings");
          }
          const keyStr = isSymbol(key) ? key.value : key.value;
          entries.set(keyStr, nodeToValue(entry.value));
        }
      }
      return { kind: "map", entries };
    }
    default:
      throw new Error(
        `Cannot convert AST node kind ${node.kind} to literal value`
      );
  }
};

// Helper constructors

export const makeNumber = (value: number): NumberValue => ({
  kind: "number",
  value,
});

export const makeString = (value: string): StringValue => ({
  kind: "string",
  value,
});

export const makeBoolean = (value: boolean): BooleanValue => ({
  kind: "boolean",
  value,
});

export const makeNil = (): NilValue => ({ kind: "nil" });

export const makeSymbol = (value: string): SymbolValue => ({
  kind: "symbol",
  value,
});

export const makeList = (elements: readonly Value[]): ListValue => ({
  kind: "list",
  elements,
});

export const makeVector = (elements: readonly Value[]): VectorValue => ({
  kind: "vector",
  elements,
});

export const makeSet = (elements: readonly Value[]): SetValue => ({
  kind: "set",
  elements,
});

export const makeMap = (entries: Map<string, Value>): MapValue => ({
  kind: "map",
  entries,
});

export const makeFunction = (
  params: readonly string[],
  body: readonly ExpressionNode[],
  closure: Environment,
  span: SourceSpan,
  rest?: string
): FunctionValue => ({
  kind: "function",
  params,
  rest,
  body,
  closure,
  span,
});

export const makeBuiltin = (
  name: string,
  fn: BuiltinFunction
): BuiltinValue => ({
  kind: "builtin",
  name,
  fn,
});
