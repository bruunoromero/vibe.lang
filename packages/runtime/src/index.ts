export interface RuntimeSymbol {
  readonly __vibeType: "symbol";
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

const createRuntimeSymbol = (name: string): RuntimeSymbol => ({
  __vibeType: "symbol",
  name,
});

const coerceKey = (value: unknown): string =>
  isRuntimeSymbol(value) ? value.name : String(value);

const keyword = (label: string): RuntimeSymbol =>
  createRuntimeSymbol(label.startsWith(":") ? label : `:${label}`);

export const symbol = (name: unknown): RuntimeSymbol => {
  if (typeof name !== "string") {
    throw new Error("symbol requires a string argument");
  }
  return createRuntimeSymbol(name);
};

export const symbol_QMARK = (value: unknown): value is RuntimeSymbol =>
  isRuntimeSymbol(value);

export const println = (...args: unknown[]) => {
  // Default runtime println delegates to console.log and returns the last arg or null
  // to match previous inline behavior used by codegen.
  // Keep signature minimal and side-effect free beyond logging.
  // Consumers may override or extend runtime behavior later.
  // eslint-disable-next-line no-console
  console.log(...(args as any[]));
  return args.length === 0 ? null : args[args.length - 1];
};

export const type = (v: unknown): RuntimeSymbol => {
  if (v === null) return keyword("nil");
  if (isRuntimeSymbol(v)) return keyword("symbol");
  if (Array.isArray(v)) return keyword("list");
  if (v instanceof Set) return keyword("set");
  if (v instanceof Map) return keyword("map");
  if (typeof v === "boolean") return keyword("boolean");
  if (typeof v === "number") return keyword("number");
  if (typeof v === "string") return keyword("string");
  if (typeof v === "function") return keyword("function");
  if (typeof v === "undefined") return keyword("undefined");
  if (typeof v === "bigint") return keyword("bigint");
  if (typeof v === "symbol") return keyword("js-symbol");
  return keyword("object");
};

// Equality check
export const eq_STAR = (a: unknown, b: unknown): boolean => {
  if (isRuntimeSymbol(a) && isRuntimeSymbol(b)) {
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

export const first = (v: unknown): unknown => {
  if (Array.isArray(v)) {
    return v.length > 0 ? v[0] : null;
  }
  if (typeof v === "object" && v !== null && Symbol.iterator in v) {
    const iterator = (v as Iterable<unknown>)[Symbol.iterator]();
    const result = iterator.next();
    return result.done ? null : result.value;
  }
  return null;
};

export const next = (v: unknown): unknown => {
  if (Array.isArray(v)) {
    return v.length > 1 ? v.slice(1) : null;
  }
  if (typeof v === "object" && v !== null && Symbol.iterator in v) {
    const arr = Array.from(v as Iterable<unknown>);
    return arr.length > 1 ? arr.slice(1) : null;
  }
  return null;
};

export const rest = (v: unknown): unknown => {
  if (Array.isArray(v)) {
    return v.slice(1);
  }
  if (typeof v === "object" && v !== null && Symbol.iterator in v) {
    const arr = Array.from(v as Iterable<unknown>);
    return arr.slice(1);
  }
  return [];
};

export const list = (...args: unknown[]): unknown[] => args;

export const cons = (item: unknown, coll: unknown): unknown[] => {
  if (Array.isArray(coll)) {
    return [item, ...coll];
  }
  if (typeof coll === "object" && coll !== null && Symbol.iterator in coll) {
    return [item, ...Array.from(coll as Iterable<unknown>)];
  }
  return [item];
};

export const conj = (coll: unknown, ...items: unknown[]): unknown[] => {
  if (Array.isArray(coll)) {
    return [...coll, ...items];
  }
  if (typeof coll === "object" && coll !== null && Symbol.iterator in coll) {
    return [...Array.from(coll as Iterable<unknown>), ...items];
  }
  return items;
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

// Additional sequence operations
export const nth = (
  seq: unknown,
  index: unknown,
  defaultValue?: unknown
): unknown => {
  if (typeof index !== "number") {
    throw new Error("nth requires a number as second argument");
  }
  if (Array.isArray(seq)) {
    if (index < 0 || index >= seq.length) {
      if (defaultValue !== undefined) return defaultValue;
      throw new Error(`Index out of bounds: ${index}`);
    }
    return seq[index];
  }
  throw new Error("nth requires a sequence as first argument");
};

export const take = (n: unknown, seq: unknown): unknown[] => {
  if (typeof n !== "number") {
    throw new Error("take requires a number as first argument");
  }
  if (!Array.isArray(seq)) {
    throw new Error("take requires a sequence as second argument");
  }
  return seq.slice(0, Math.max(0, n));
};

export const drop = (n: unknown, seq: unknown): unknown[] => {
  if (typeof n !== "number") {
    throw new Error("drop requires a number as first argument");
  }
  if (!Array.isArray(seq)) {
    throw new Error("drop requires a sequence as second argument");
  }
  return seq.slice(Math.max(0, n));
};

export const reverse = (seq: unknown): unknown[] => {
  if (!Array.isArray(seq)) {
    throw new Error("reverse requires a sequence");
  }
  return [...seq].reverse();
};

export const concat = (...seqs: unknown[]): unknown[] => {
  const result: unknown[] = [];
  for (const seq of seqs) {
    if (!Array.isArray(seq)) {
      throw new Error("concat requires sequence arguments");
    }
    result.push(...seq);
  }
  return result;
};

// String operations
export const str = (...args: unknown[]): string => {
  return args
    .map((arg) => {
      if (arg === null) return "nil";
      if (typeof arg === "boolean") return arg ? "true" : "false";
      if (isRuntimeSymbol(arg)) return arg.name;
      return String(arg);
    })
    .join("");
};

// Map operations
export const get = (
  target: unknown,
  key: unknown,
  defaultValue?: unknown
): unknown => {
  if (target instanceof Map) {
    const direct = target.get(key);
    if (direct !== undefined) {
      return direct;
    }
    const fallback = target.get(coerceKey(key));
    if (fallback !== undefined) {
      return fallback;
    }
    return defaultValue ?? null;
  }
  if (typeof target === "object" && target !== null && !Array.isArray(target)) {
    const value = (target as Record<string, unknown>)[coerceKey(key)];
    return value !== undefined ? value : defaultValue ?? null;
  }
  throw new Error("get requires a map or namespace object as first argument");
};

export const assoc = (
  map: unknown,
  ...kvs: unknown[]
): Record<string, unknown> => {
  if (kvs.length % 2 !== 0) {
    throw new Error("assoc requires an even number of key-value arguments");
  }
  if (typeof map !== "object" || map === null || Array.isArray(map)) {
    throw new Error("assoc requires a map as first argument");
  }
  const result = { ...(map as Record<string, unknown>) };
  for (let i = 0; i < kvs.length; i += 2) {
    const key = kvs[i];
    const value = kvs[i + 1];
    result[coerceKey(key)] = value;
  }
  return result;
};

export const dissoc = (
  map: unknown,
  ...keys: unknown[]
): Record<string, unknown> => {
  if (typeof map !== "object" || map === null || Array.isArray(map)) {
    throw new Error("dissoc requires a map as first argument");
  }
  const result = { ...(map as Record<string, unknown>) };
  for (const key of keys) {
    delete result[coerceKey(key)];
  }
  return result;
};

export const keys = (map: unknown): unknown[] => {
  if (typeof map !== "object" || map === null || Array.isArray(map)) {
    throw new Error("keys requires a map");
  }
  return Object.keys(map);
};

export const vals = (map: unknown): unknown[] => {
  if (typeof map !== "object" || map === null || Array.isArray(map)) {
    throw new Error("vals requires a map");
  }
  return Object.values(map);
};

export default {
  symbol,
  symbol_QMARK,
  println,
  type,
  seq_QMARK,
  first,
  next,
  rest,
  list,
  cons,
  conj,
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
  nth,
  take,
  drop,
  reverse,
  concat,
  str,
  get,
  assoc,
  dissoc,
  keys,
  vals,
};
