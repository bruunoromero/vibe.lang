export interface RuntimeSymbol {
  readonly __vibeType: "symbol";
  readonly name: string;
}

export interface RuntimeKeyword {
  readonly __vibeType: "keyword";
  readonly name: string;
}

const isRuntimeSymbol = (value: unknown): value is RuntimeSymbol => {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const candidate = value as Partial<RuntimeSymbol>;
  return (
    candidate.__vibeType === "symbol" && typeof candidate.name === "string"
  );
};

const isRuntimeKeyword = (value: unknown): value is RuntimeKeyword => {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const candidate = value as Partial<RuntimeKeyword>;
  return (
    candidate.__vibeType === "keyword" && typeof candidate.name === "string"
  );
};

const createRuntimeSymbol = (name: string): RuntimeSymbol => ({
  __vibeType: "symbol",
  name,
});

const keywordCache = new Map<string, RuntimeKeyword>();

const normalizeKeywordName = (label: string): string =>
  label.startsWith(":") ? label.slice(1) : label;

const createRuntimeKeyword = (label: string): RuntimeKeyword => {
  const name = normalizeKeywordName(label);
  const cached = keywordCache.get(name);
  if (cached) {
    return cached;
  }
  const keyword: RuntimeKeyword = {
    __vibeType: "keyword",
    name,
  };
  keywordCache.set(name, keyword);
  return keyword;
};

const coerceKey = (value: unknown): string => {
  if (isRuntimeSymbol(value)) {
    return value.name;
  }
  if (isRuntimeKeyword(value)) {
    return value.name;
  }
  return String(value);
};

export const symbol_STAR = (name: unknown): RuntimeSymbol => {
  if (typeof name !== "string") {
    throw new Error("symbol requires a string argument");
  }
  return createRuntimeSymbol(name);
};

export const symbol_QMARK = (value: unknown): value is RuntimeSymbol =>
  isRuntimeSymbol(value);

export const keyword_STAR = (label: unknown): RuntimeKeyword => {
  if (typeof label !== "string") {
    throw new Error("keyword requires a string argument");
  }
  return createRuntimeKeyword(label);
};

export const keyword_QMARK = (value: unknown): value is RuntimeKeyword =>
  isRuntimeKeyword(value);

export const println = (...args: unknown[]) => {
  // Default runtime println delegates to console.log and returns the last arg or null
  // to match previous inline behavior used by codegen.
  // Keep signature minimal and side-effect free beyond logging.
  // Consumers may override or extend runtime behavior later.
  // eslint-disable-next-line no-console
  console.log(...(args as any[]));
  return args.length === 0 ? null : args[args.length - 1];
};

export const type = (v: unknown): RuntimeKeyword => {
  if (v === null) return createRuntimeKeyword("nil");
  if (isRuntimeSymbol(v)) return createRuntimeKeyword("symbol");
  if (isRuntimeKeyword(v)) return createRuntimeKeyword("keyword");
  if (Array.isArray(v)) return createRuntimeKeyword("list");
  if (v instanceof Set) return createRuntimeKeyword("set");
  if (v instanceof Map) return createRuntimeKeyword("map");
  if (typeof v === "boolean") return createRuntimeKeyword("boolean");
  if (typeof v === "number") return createRuntimeKeyword("number");
  if (typeof v === "string") return createRuntimeKeyword("string");
  if (typeof v === "function") return createRuntimeKeyword("function");
  if (typeof v === "undefined") return createRuntimeKeyword("undefined");
  if (typeof v === "bigint") return createRuntimeKeyword("bigint");
  if (typeof v === "symbol") return createRuntimeKeyword("js-symbol");
  return createRuntimeKeyword("object");
};

// Equality check
export const eq_STAR = (a: unknown, b: unknown): boolean => {
  if (isRuntimeSymbol(a) && isRuntimeSymbol(b)) {
    return a.name === b.name;
  }
  if (isRuntimeKeyword(a) && isRuntimeKeyword(b)) {
    return a.name === b.name;
  }
  return a === b;
};

// Sequence helpers for macro operations
export const seq_QMARK = (v: unknown): boolean => {
  return (
    Array.isArray(v) ||
    (typeof v === "object" && v !== null && Symbol.iterator in v)
  );
};

export const count = (v: unknown): number => {
  if (Array.isArray(v)) {
    return v.length;
  }
  if (typeof v === "object" && v !== null && Symbol.iterator in v) {
    return Array.from(v as Iterable<unknown>).length;
  }
  return 0;
};

// Arithmetic operations (binary)
const expectBinaryNumbers = (
  label: string,
  left: unknown,
  right: unknown
): [number, number] => {
  if (typeof left !== "number" || typeof right !== "number") {
    throw new Error(`${label} requires numeric arguments`);
  }
  return [left, right];
};

export const add_STAR = (a: unknown, b: unknown): number => {
  const [left, right] = expectBinaryNumbers("add*", a, b);
  return left + right;
};

export const sub_STAR = (a: unknown, b: unknown): number => {
  const [left, right] = expectBinaryNumbers("sub*", a, b);
  return left - right;
};

export const mul_STAR = (a: unknown, b: unknown): number => {
  const [left, right] = expectBinaryNumbers("mul*", a, b);
  return left * right;
};

export const div_STAR = (a: unknown, b: unknown): number => {
  const [left, right] = expectBinaryNumbers("div*", a, b);
  if (right === 0) {
    throw new Error("Division by zero");
  }
  return left / right;
};

export const mod_STAR = (a: unknown, b: unknown): number => {
  const [left, right] = expectBinaryNumbers("mod*", a, b);
  return left % right;
};

// Comparison operations (binary)
export const lt_STAR = (a: unknown, b: unknown): boolean => {
  if (typeof a !== "number" || typeof b !== "number") {
    throw new Error("lt* requires numeric arguments");
  }
  return a < b;
};

export const gt_STAR = (a: unknown, b: unknown): boolean => {
  if (typeof a !== "number" || typeof b !== "number") {
    throw new Error("gt* requires numeric arguments");
  }
  return a > b;
};

export const lte_STAR = (a: unknown, b: unknown): boolean => {
  if (typeof a !== "number" || typeof b !== "number") {
    throw new Error("lte* requires numeric arguments");
  }
  return a <= b;
};

export const gte_STAR = (a: unknown, b: unknown): boolean => {
  if (typeof a !== "number" || typeof b !== "number") {
    throw new Error("gte* requires numeric arguments");
  }
  return a >= b;
};

// String operations
export const str = (...args: unknown[]): string => {
  return args
    .map((arg) => {
      if (arg === null) return "nil";
      if (typeof arg === "boolean") return arg ? "true" : "false";
      if (isRuntimeSymbol(arg) || isRuntimeKeyword(arg)) return arg.name;
      return String(arg);
    })
    .join("");
};

// Map operations
export const get_STAR = (target: unknown, key: unknown): unknown => {
  if (target instanceof Map) {
    const direct = target.get(key);
    if (direct !== undefined) {
      return direct;
    }
    const fallback = target.get(coerceKey(key));
    if (fallback !== undefined) {
      return fallback;
    }
    return null;
  }
  if (typeof target === "object" && target !== null && !Array.isArray(target)) {
    const value = (target as Record<string, unknown>)[coerceKey(key)];
    return value !== undefined ? value : null;
  }
  throw new Error("get requires a map or namespace object as first argument");
};

export const assoc_STAR = (
  map: unknown,
  key: unknown,
  value: unknown
): Map<unknown, unknown> => {
  if (!(map instanceof Map)) {
    throw new Error("assoc requires a map as first argument");
  }

  const newMap = new Map(map);
  newMap.set(key, value);
  return newMap;
};

export const dissoc_STAR = (
  map: unknown,
  key: unknown
): Map<unknown, unknown> => {
  if (!(map instanceof Map)) {
    throw new Error("dissoc requires a map as first argument");
  }

  const newMap = new Map(map);
  newMap.delete(key);
  return newMap;
};

export const keys_STAR = (map: unknown): unknown[] => {
  if (!(map instanceof Map)) {
    throw new Error("keys requires a map");
  }

  return Array.from(map.keys());
};

export const vals_STAR = (map: unknown): unknown[] => {
  if (!(map instanceof Map)) {
    throw new Error("keys requires a map");
  }

  return Array.from(map.values());
};

export const apply = (f: unknown, args: unknown[]): unknown => {
  if (typeof f !== "function") {
    throw new Error("apply requires a function as first argument");
  }
  if (!Array.isArray(args)) {
    throw new Error("apply requires a list of arguments as second argument");
  }
  return f(...args);
};

export default {
  symbol_STAR,
  symbol_QMARK,
  keyword_STAR,
  keyword_QMARK,
  println,
  type,
  seq_QMARK,
  count,
  add_STAR,
  sub_STAR,
  mul_STAR,
  div_STAR,
  mod_STAR,
  eq_STAR,
  lt_STAR,
  gt_STAR,
  lte_STAR,
  gte_STAR,
  str,
  get_STAR,
  assoc_STAR,
  dissoc_STAR,
  keys_STAR,
  vals_STAR,
  apply,
};
