export const println = (...args: unknown[]) => {
  // Default runtime println delegates to console.log and returns the last arg or null
  // to match previous inline behavior used by codegen.
  // Keep signature minimal and side-effect free beyond logging.
  // Consumers may override or extend runtime behavior later.
  // eslint-disable-next-line no-console
  console.log(...(args as any[]));
  return args.length === 0 ? null : args[args.length - 1];
};

export const isTruthy = (v: unknown): boolean => v !== null && v !== false;

// Equality check
export const eq_STAR = (a: unknown, b: unknown): boolean => a === b;

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

export const empty_QMARK = (v: unknown): boolean => count(v) === 0;

// Type predicates
export const list_QMARK = (v: unknown): boolean => Array.isArray(v);
export const vector_QMARK = (v: unknown): boolean => Array.isArray(v);
export const map_QMARK = (v: unknown): boolean =>
  typeof v === "object" &&
  v !== null &&
  !Array.isArray(v) &&
  v.constructor === Object;
export const set_QMARK = (v: unknown): boolean => v instanceof Set;
export const nil_QMARK = (v: unknown): boolean => v === null;
export const symbol_QMARK = (v: unknown): boolean => typeof v === "symbol";
export const string_QMARK = (v: unknown): boolean => typeof v === "string";
export const number_QMARK = (v: unknown): boolean => typeof v === "number";
export const fn_QMARK = (v: unknown): boolean => typeof v === "function";

export default {
  println,
  isTruthy,
  seq_QMARK,
  first,
  next,
  rest,
  list,
  cons,
  conj,
  count,
  empty_QMARK,
  list_QMARK,
  vector_QMARK,
  map_QMARK,
  set_QMARK,
  nil_QMARK,
  symbol_QMARK,
  string_QMARK,
  number_QMARK,
  fn_QMARK,
};
