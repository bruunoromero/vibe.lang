import { describe, it, expect } from "bun:test";
import { deepEqual } from "../src/index";

describe("deepEqual", () => {
  it("should return true for identical primitives", () => {
    expect(deepEqual(1)(1)).toBe(true);
    expect(deepEqual("hello")("hello")).toBe(true);
    expect(deepEqual(true)(true)).toBe(true);
    expect(deepEqual(null)(null)).toBe(true);
    expect(deepEqual(undefined)(undefined)).toBe(true);
  });

  it("should return false for different primitives", () => {
    expect(deepEqual(1)(2)).toBe(false);
    expect(deepEqual("hello")("world")).toBe(false);
    expect(deepEqual(true)(false)).toBe(false);
  });

  it("should return true for identical arrays", () => {
    expect(deepEqual([1, 2, 3])([1, 2, 3])).toBe(true);
    expect(deepEqual([])([])).toBe(true);
  });

  it("should return false for different arrays", () => {
    expect(deepEqual([1, 2, 3])([1, 2, 4])).toBe(false);
    expect(deepEqual([1, 2])([1, 2, 3])).toBe(false);
    expect(deepEqual([1, 2, 3])([1, 2])).toBe(false);
  });

  it("should return true for identical objects", () => {
    expect(deepEqual({ a: 1, b: 2 })({ a: 1, b: 2 })).toBe(true);
    expect(deepEqual({})({})).toBe(true);
  });

  it("should return false for different objects", () => {
    expect(deepEqual({ a: 1, b: 2 })({ a: 1, b: 3 })).toBe(false);
    // Use 'as any' to test runtime behavior with mismatched types
    expect(deepEqual({ a: 1 } as any)({ a: 1, b: 2 } as any)).toBe(false);
    expect(deepEqual({ a: 1, b: 2 } as any)({ a: 1 } as any)).toBe(false);
  });

  it("should handle nested structures", () => {
    const obj1 = { a: [1, 2], b: { c: 3 } };
    const obj2 = { a: [1, 2], b: { c: 3 } };
    const obj3 = { a: [1, 2], b: { c: 4 } };

    expect(deepEqual(obj1)(obj2)).toBe(true);
    expect(deepEqual(obj1)(obj3)).toBe(false);
  });

  it("should handle mixed types", () => {
    // Use 'as any' to test runtime behavior with mixed types
    expect(deepEqual(1 as any)([1] as any)).toBe(false);
    expect(deepEqual({ a: 1 } as any)("string" as any)).toBe(false);
  });
});
