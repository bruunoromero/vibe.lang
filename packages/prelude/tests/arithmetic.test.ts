import { describe, expect, test } from "bun:test";
import { _PLUS, _DASH, _STAR, _SLASH, get } from "../src/prelude.js";

describe("prelude arithmetic operators", () => {
  test("+ mirrors identity, unary, and variadic semantics", () => {
    expect(_PLUS()).toBe(0);
    expect(_PLUS(6)).toBe(6);
    expect(_PLUS(1, 2, 3, 4)).toBe(10);
  });

  test("* covers multiplicative identity and folds", () => {
    expect(_STAR()).toBe(1);
    expect(_STAR(5)).toBe(5);
    expect(_STAR(2, 3, 4)).toBe(24);
  });

  test("- handles zero, unary negation, and chained subtraction", () => {
    expect(_DASH()).toBe(0);
    expect(_DASH(7)).toBe(-7);
    expect(_DASH(10, 3, 2)).toBe(5);
  });

  test("/ mirrors zero, unary reciprocal, and chained division", () => {
    expect(_SLASH()).toBe(0);
    expect(_SLASH(4)).toBe(0.25);
    expect(_SLASH(20, 2, 2)).toBe(5);
    expect(() => _SLASH(5, 0)).toThrow("Division by zero");
  });

  // test("get delegates to runtime helper", () => {
  //   const namespace = { foo: 1 };
  //   expect(get(namespace, "foo")).toBe(1);
  //   expect(get(namespace, "missing")).toBeNull();
  //   expect(get(namespace, "missing", 99)).toBe(99);
  // });
});
