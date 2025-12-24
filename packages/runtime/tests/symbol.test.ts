import { describe, expect, test } from "bun:test";
import {
  symbol,
  symbol_QMARK,
  eq_STAR,
  str,
  assoc,
  get,
  type,
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

  test("str prints readable symbol names", () => {
    const printable = symbol("demo");
    expect(str(printable)).toBe("demo");
  });

  test("map helpers treat symbols as keys", () => {
    const key = symbol("answer");
    const updated = assoc({}, key, 42);
    expect(get(updated, key)).toBe(42);
  });

  test("type returns keyword-like symbols", () => {
    const result = type(symbol("foo"));
    expect(symbol_QMARK(result)).toBeTrue();
    expect(result.name).toBe(":symbol");
  });
});
