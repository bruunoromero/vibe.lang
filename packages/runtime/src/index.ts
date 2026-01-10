/**
 * @vibe/runtime - Runtime Support Library for Vibe
 *
 * This module provides the low-level implementations for built-in operations
 * that are exposed to Vibe code via @external declarations in the Prelude.
 */

// =============================================================================
// Logical Operations
// =============================================================================

/** Logical AND with short-circuit evaluation */
export const and =
  (a: boolean) =>
  (b: boolean): boolean =>
    a && b;

/** Logical OR with short-circuit evaluation */
export const or =
  (a: boolean) =>
  (b: boolean): boolean =>
    a || b;

/** Logical NOT */
export const not = (a: boolean): boolean => !a;

// =============================================================================
// Function Composition and Application
// =============================================================================

/** Forward pipe: x |> f = f(x) */
export const pipeForward =
  <A, B>(x: A) =>
  (f: (a: A) => B): B =>
    f(x);

/** Backward pipe: f <| x = f(x) */
export const pipeBackward =
  <A, B>(f: (a: A) => B) =>
  (x: A): B =>
    f(x);

/** Forward composition: (f >> g)(x) = g(f(x)) */
export const composeForward =
  <A, B, C>(f: (a: A) => B) =>
  (g: (b: B) => C) =>
  (x: A): C =>
    g(f(x));

/** Backward composition: (g << f)(x) = g(f(x)) */
export const composeBackward =
  <B, C, A>(g: (b: B) => C) =>
  (f: (a: A) => B) =>
  (x: A): C =>
    g(f(x));

// =============================================================================
// List Operations
// =============================================================================

/** Cons: prepend element to list */
export const cons =
  <A>(head: A) =>
  (tail: A[]): A[] =>
    [head, ...tail];

/** Append: concatenate two lists or strings */
export const append =
  <A>(xs: A[] | string) =>
  (ys: A[] | string): A[] | string => {
    if (typeof xs === "string" && typeof ys === "string") {
      return xs + ys;
    }
    return [...(xs as A[]), ...(ys as A[])];
  };

// =============================================================================
// Int Operations
// =============================================================================

/** Integer addition */
export const intAdd =
  (a: number) =>
  (b: number): number =>
    (a + b) | 0;

/** Integer subtraction */
export const intSub =
  (a: number) =>
  (b: number): number =>
    (a - b) | 0;

/** Integer multiplication */
export const intMul =
  (a: number) =>
  (b: number): number =>
    (a * b) | 0;

/** Integer division (truncated toward zero) */
export const intDiv =
  (a: number) =>
  (b: number): number =>
    Math.trunc(a / b);

/** Integer modulo */
export const intMod =
  (a: number) =>
  (b: number): number =>
    a % b;

/** Integer negation */
export const intNegate = (a: number): number => -a | 0;

/** Integer equality */
export const intEq =
  (a: number) =>
  (b: number): boolean =>
    a === b;

/** Integer inequality */
export const intNeq =
  (a: number) =>
  (b: number): boolean =>
    a !== b;

/** Integer less than */
export const intLt =
  (a: number) =>
  (b: number): boolean =>
    a < b;

/** Integer less than or equal */
export const intLte =
  (a: number) =>
  (b: number): boolean =>
    a <= b;

/** Integer greater than */
export const intGt =
  (a: number) =>
  (b: number): boolean =>
    a > b;

/** Integer greater than or equal */
export const intGte =
  (a: number) =>
  (b: number): boolean =>
    a >= b;

/** Integer exponentiation */
export const intPow =
  (base: number) =>
  (exp: number): number =>
    Math.pow(base, exp) | 0;

// =============================================================================
// Float Operations
// =============================================================================

/** Float addition */
export const floatAdd =
  (a: number) =>
  (b: number): number =>
    a + b;

/** Float subtraction */
export const floatSub =
  (a: number) =>
  (b: number): number =>
    a - b;

/** Float multiplication */
export const floatMul =
  (a: number) =>
  (b: number): number =>
    a * b;

/** Float division */
export const floatDiv =
  (a: number) =>
  (b: number): number =>
    a / b;

/** Float negation */
export const floatNegate = (a: number): number => -a;

/** Float equality */
export const floatEq =
  (a: number) =>
  (b: number): boolean =>
    a === b;

/** Float inequality */
export const floatNeq =
  (a: number) =>
  (b: number): boolean =>
    a !== b;

/** Float less than */
export const floatLt =
  (a: number) =>
  (b: number): boolean =>
    a < b;

/** Float less than or equal */
export const floatLte =
  (a: number) =>
  (b: number): boolean =>
    a <= b;

/** Float greater than */
export const floatGt =
  (a: number) =>
  (b: number): boolean =>
    a > b;

/** Float greater than or equal */
export const floatGte =
  (a: number) =>
  (b: number): boolean =>
    a >= b;

/** Float exponentiation */
export const floatPow =
  (base: number) =>
  (exp: number): number =>
    Math.pow(base, exp);

// =============================================================================
// Debug / Development
// =============================================================================

/** Print a value to console (for debugging) */
export const println = <A>(value: A): A => {
  console.log(value);
  return value;
};
