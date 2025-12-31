import { describe, expect, test } from "bun:test";
import {
  symbol_STAR as symbol,
  symbol_QMARK,
  keyword_STAR as keyword,
  keyword_QMARK,
  eq_STAR,
  assoc_STAR as assoc,
  get_STAR as get,
  type_STAR as type,
} from "../src/index";

describe("runtime symbols", () => {
  test("symbol creates tagged objects", () => {
    const foo = symbol("foo");
    expect(symbol_QMARK(foo)).toBeTrue();
    expect(foo).toEqual({ __vibeType: "symbol", name: "foo" });
  });

  test("eq* compares symbols by name", () => {
    const left = symbol("alpha");
    const right = symbol("alpha");
    const other = symbol("beta");
    expect(eq_STAR(left, right)).toBeTrue();
    expect(eq_STAR(left, other)).toBeFalse();
  });

  // test("str prints readable symbol names", () => {
  //   const printable = symbol("demo");
  //   expect(str(printable)).toBe("demo");
  // });

  test("map helpers treat symbols as keys", () => {
    const key = symbol("answer");
    const updated = assoc(new Map(), key, 42);
    expect(get(updated, key)).toBe(42);
  });

  test("get reads namespace-like objects", () => {
    const namespace = { "path-separator": ":", value: 10 };
    expect(get(namespace, "path-separator")).toBe(":");
    expect(get(namespace, "missing")).toBeNull();
  });

  test("get supports Map targets", () => {
    const entries = new Map<unknown, unknown>();
    entries.set("alpha", 1);
    entries.set("beta", 2);
    expect(get(entries, "alpha")).toBe(1);
    expect(get(entries, symbol("beta"))).toBe(2);
  });

  test("type returns keywords", () => {
    const result = type(symbol("foo"));
    expect(keyword_QMARK(result)).toBeTrue();
    expect(result).toEqual({ __vibeType: "keyword", name: "symbol" });
  });
});

describe("runtime keywords", () => {
  test("keyword creates tagged objects and interns names", () => {
    const alpha = keyword("alpha");
    const beta = keyword("alpha");
    expect(keyword_QMARK(alpha)).toBeTrue();
    expect(alpha).toEqual({ __vibeType: "keyword", name: "alpha" });
    expect(alpha).toBe(beta);
  });

  test("eq* compares keywords by name", () => {
    const left = keyword("foo");
    const right = keyword("foo");
    const other = keyword("bar");
    expect(eq_STAR(left, right)).toBeTrue();
    expect(eq_STAR(left, other)).toBeFalse();
  });

  test("get resolves keyword map keys", () => {
    const key = keyword("answer");
    const entries = new Map<unknown, unknown>();
    entries.set(key, 42);
    expect(get(entries, keyword("answer"))).toBe(42);
  });
});
