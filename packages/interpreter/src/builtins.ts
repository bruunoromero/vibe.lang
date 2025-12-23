import type { SourceSpan } from "@vibe/syntax";
import * as runtime from "@vibe/runtime";
import type { Value } from "./values";
import {
  isNumber,
  isString,
  isBoolean,
  isNil,
  isSymbol,
  isList,
  isVector,
  isSet,
  isMap,
  isFunction,
  isSequence,
  makeNumber,
  makeString,
  makeBoolean,
  makeNil,
  makeSymbol,
  makeList,
  makeVector,
  makeMap,
  makeBuiltin,
  valuesEqual,
} from "./values";

/**
 * Convert interpreter Value to runtime JS value
 */
const valueToJS = (value: Value): unknown => {
  switch (value.kind) {
    case "number":
      return value.value;
    case "string":
      return value.value;
    case "boolean":
      return value.value;
    case "nil":
      return null;
    case "symbol":
      return value.value;
    case "list":
    case "vector":
      return value.elements.map(valueToJS);
    case "set":
      return new Set(value.elements.map(valueToJS));
    case "map": {
      const obj: Record<string, unknown> = {};
      for (const [key, val] of value.entries) {
        obj[key] = valueToJS(val);
      }
      return obj;
    }
    default:
      throw new Error(`Cannot convert ${(value as Value).kind} to JS value`);
  }
};

/**
 * Convert runtime JS value to interpreter Value
 */
const jsToValue = (value: unknown): Value => {
  if (value === null || value === undefined) return makeNil();
  if (typeof value === "number") return makeNumber(value);
  if (typeof value === "string") return makeString(value);
  if (typeof value === "boolean") return makeBoolean(value);
  if (Array.isArray(value)) return makeList(value.map(jsToValue));
  if (value instanceof Set) {
    return makeList(Array.from(value).map(jsToValue));
  }
  if (typeof value === "object") {
    const entries = new Map<string, Value>();
    for (const [k, v] of Object.entries(value)) {
      entries.set(k, jsToValue(v));
    }
    return makeMap(entries);
  }
  throw new Error(`Cannot convert JS value of type ${typeof value} to Value`);
};

/**
 * Create a record of builtin functions.
 */
export const createBuiltins = (): Record<string, Value> => {
  return {
    // Arithmetic (binary operations)
    "add*": makeBuiltin("add*", builtinAdd),
    "sub*": makeBuiltin("sub*", builtinSubtract),
    "mul*": makeBuiltin("mul*", builtinMultiply),
    "div*": makeBuiltin("div*", builtinDivide),
    "mod*": makeBuiltin("mod*", builtinMod),

    // Comparison (binary operations)
    "eq*": makeBuiltin("eq*", builtinEqual),
    "lt*": makeBuiltin("lt*", builtinLessThan),
    "gt*": makeBuiltin("gt*", builtinGreaterThan),
    "lte*": makeBuiltin("lte*", builtinLessThanOrEqual),
    "gte*": makeBuiltin("gte*", builtinGreaterThanOrEqual),

    // Sequence operations
    first: makeBuiltin("first", builtinFirst),
    rest: makeBuiltin("rest", builtinRest),
    cons: makeBuiltin("cons", builtinCons),
    count: makeBuiltin("count", builtinCount),
    nth: makeBuiltin("nth", builtinNth),
    map: makeBuiltin("map", builtinMap),
    filter: makeBuiltin("filter", builtinFilter),
    reduce: makeBuiltin("reduce", builtinReduce),
    concat: makeBuiltin("concat", builtinConcat),
    take: makeBuiltin("take", builtinTake),
    drop: makeBuiltin("drop", builtinDrop),
    reverse: makeBuiltin("reverse", builtinReverse),

    // Map operations
    get: makeBuiltin("get", builtinGet),
    assoc: makeBuiltin("assoc", builtinAssoc),
    dissoc: makeBuiltin("dissoc", builtinDissoc),
    keys: makeBuiltin("keys", builtinKeys),
    vals: makeBuiltin("vals", builtinVals),

    // Type introspection
    type: makeBuiltin("type", builtinType),

    // String operations
    str: makeBuiltin("str", builtinStr),

    // Collection constructors
    list: makeBuiltin("list", builtinListConstructor),
    vector: makeBuiltin("vector", builtinVectorConstructor),

    // Utility
    gensym: makeBuiltin("gensym", builtinGensym),
  };
};

// Arithmetic operations (binary)

const builtinAdd = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`add* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.add_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeNumber(result);
};

const builtinSubtract = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`sub* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.sub_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeNumber(result);
};

const builtinMultiply = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`mul* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.mul_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeNumber(result);
};

const builtinDivide = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`div* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.div_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeNumber(result);
};

const builtinMod = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`mod* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.mod_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeNumber(result);
};

// Comparison operations (binary)

const builtinEqual = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`eq* requires exactly 2 arguments, got ${args.length}`);
  }
  return makeBoolean(valuesEqual(args[0]!, args[1]!));
};

const builtinLessThan = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`lt* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.lt_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeBoolean(result);
};

const builtinGreaterThan = (
  args: readonly Value[],
  span: SourceSpan
): Value => {
  if (args.length !== 2) {
    throw new Error(`gt* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.gt_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeBoolean(result);
};

const builtinLessThanOrEqual = (
  args: readonly Value[],
  span: SourceSpan
): Value => {
  if (args.length !== 2) {
    throw new Error(`lte* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.lte_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeBoolean(result);
};

const builtinGreaterThanOrEqual = (
  args: readonly Value[],
  span: SourceSpan
): Value => {
  if (args.length !== 2) {
    throw new Error(`gte* requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.gte_STAR(valueToJS(args[0]!), valueToJS(args[1]!));
  return makeBoolean(result);
};

// Sequence operations

const builtinFirst = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`first requires exactly 1 argument, got ${args.length}`);
  }
  const result = runtime.first(valueToJS(args[0]!));
  return jsToValue(result);
};

const builtinRest = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`rest requires exactly 1 argument, got ${args.length}`);
  }
  const result = runtime.rest(valueToJS(args[0]!));
  return jsToValue(result);
};

const builtinCons = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`cons requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.cons(valueToJS(args[0]!), valueToJS(args[1]!));
  return jsToValue(result);
};

const builtinCount = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`count requires exactly 1 argument, got ${args.length}`);
  }
  const result = runtime.count(valueToJS(args[0]!));
  return makeNumber(result);
};

const builtinNth = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length < 2 || args.length > 3) {
    throw new Error(`nth requires 2 or 3 arguments, got ${args.length}`);
  }
  const jsArgs = args.map(valueToJS);
  const result =
    args.length === 2
      ? runtime.nth(jsArgs[0], jsArgs[1])
      : runtime.nth(jsArgs[0], jsArgs[1], jsArgs[2]);
  return jsToValue(result);
};

const builtinMap = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`map requires exactly 2 arguments, got ${args.length}`);
  }
  const [fn, seq] = args;
  if (!isFunction(fn!) && !isFunction(fn!)) {
    throw new Error("map requires a function as first argument");
  }
  if (!isSequence(seq!)) {
    throw new Error("map requires a sequence as second argument");
  }

  // Note: This is a simplified version that doesn't actually call the function
  // A full implementation would need access to the evaluator's applyFunction
  throw new Error(
    "map is not yet fully implemented in the interpreter builtins"
  );
};

const builtinFilter = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`filter requires exactly 2 arguments, got ${args.length}`);
  }
  // Similar limitation as map
  throw new Error(
    "filter is not yet fully implemented in the interpreter builtins"
  );
};

const builtinReduce = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length < 2 || args.length > 3) {
    throw new Error(`reduce requires 2 or 3 arguments, got ${args.length}`);
  }
  // Similar limitation as map
  throw new Error(
    "reduce is not yet fully implemented in the interpreter builtins"
  );
};

const builtinConcat = (args: readonly Value[], span: SourceSpan): Value => {
  const jsArgs = args.map(valueToJS);
  const result = runtime.concat(...jsArgs);
  return jsToValue(result);
};

const builtinTake = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`take requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.take(valueToJS(args[0]!), valueToJS(args[1]!));
  return jsToValue(result);
};

const builtinDrop = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 2) {
    throw new Error(`drop requires exactly 2 arguments, got ${args.length}`);
  }
  const result = runtime.drop(valueToJS(args[0]!), valueToJS(args[1]!));
  return jsToValue(result);
};

const builtinReverse = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`reverse requires exactly 1 argument, got ${args.length}`);
  }
  const result = runtime.reverse(valueToJS(args[0]!));
  return jsToValue(result);
};

// Map operations

const builtinGet = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length < 2 || args.length > 3) {
    throw new Error(`get requires 2 or 3 arguments, got ${args.length}`);
  }
  const jsArgs = args.map(valueToJS);
  const result =
    args.length === 2
      ? runtime.get(jsArgs[0], jsArgs[1])
      : runtime.get(jsArgs[0], jsArgs[1], jsArgs[2]);
  return jsToValue(result);
};

const builtinAssoc = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length < 3 || args.length % 2 === 0) {
    throw new Error(
      `assoc requires an odd number of arguments (map key val ...), got ${args.length}`
    );
  }
  const jsArgs = args.map(valueToJS);
  const result = runtime.assoc(...(jsArgs as [any, ...any[]]));
  return jsToValue(result);
};

const builtinDissoc = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length < 2) {
    throw new Error(`dissoc requires at least 2 arguments, got ${args.length}`);
  }
  const jsArgs = args.map(valueToJS);
  const result = runtime.dissoc(...(jsArgs as [any, ...any[]]));
  return jsToValue(result);
};

const builtinKeys = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`keys requires exactly 1 argument, got ${args.length}`);
  }
  const result = runtime.keys(valueToJS(args[0]!));
  return jsToValue(result);
};

const builtinVals = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`vals requires exactly 1 argument, got ${args.length}`);
  }
  const result = runtime.vals(valueToJS(args[0]!));
  return jsToValue(result);
};

// Type introspection

const builtinType = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length !== 1) {
    throw new Error(`type requires exactly 1 argument, got ${args.length}`);
  }
  const value = args[0]!;
  // Return the type as a keyword symbol
  return makeSymbol(`:${value.kind}`);
};

// String operations

const builtinStr = (args: readonly Value[], span: SourceSpan): Value => {
  const jsArgs = args.map(valueToJS);
  const result = runtime.str(...jsArgs);
  return makeString(result);
};

// Collection constructors

const builtinListConstructor = (
  args: readonly Value[],
  span: SourceSpan
): Value => {
  return makeList(args);
};

const builtinVectorConstructor = (
  args: readonly Value[],
  span: SourceSpan
): Value => {
  return makeVector(args);
};

// Utility

let gensymCounter = 0;

const builtinGensym = (args: readonly Value[], span: SourceSpan): Value => {
  if (args.length > 1) {
    throw new Error(`gensym requires 0 or 1 argument, got ${args.length}`);
  }
  const hint = args[0];
  let prefix = "g";
  if (hint) {
    if (isString(hint)) {
      prefix = hint.value;
    } else if (isSymbol(hint)) {
      prefix = hint.value;
    }
  }
  const sym = `${prefix}__${gensymCounter++}`;
  return makeSymbol(sym);
};
