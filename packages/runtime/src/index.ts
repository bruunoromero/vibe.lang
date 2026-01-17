/**
 * @vibe/runtime - Runtime Support Library for Vibe
 *
 * This module provides the low-level implementations for built-in operations
 * that are exposed to Vibe code via @external declarations in the Prelude.
 */

// =============================================================================
// Logical Operations
// =============================================================================

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
export const listCons =
  <A>(head: A) =>
  (tail: A[]): A[] => [head, ...tail];

/** Append: concatenate two lists (exported as 'append' for prelude (++) operator) */
export const listAppend =
  <A>(xs: A[]) =>
  (ys: A[]): A[] => {
    return [...xs, ...ys];
  };

// =============================================================================
// String Operations
// =============================================================================

export const stringAppend =
  (a: string) =>
  (b: string): string =>
    a + b;

// =============================================================================
// Generic Number Operations
// =============================================================================

/** Number equality */
export const numEq =
  (a: number) =>
  (b: number): boolean =>
    a === b;

/** Number not equal */
export const numNeq =
  (a: number) =>
  (b: number): boolean =>
    a !== b;

/** Number less than */
export const numLt =
  (a: number) =>
  (b: number): boolean =>
    a < b;

/** Number less than or equal */
export const numLte =
  (a: number) =>
  (b: number): boolean =>
    a <= b;

/** Number greater than */
export const numGt =
  (a: number) =>
  (b: number): boolean =>
    a > b;

/** Number greater than or equal */
export const numGte =
  (a: number) =>
  (b: number): boolean =>
    a >= b;

export const numToString = (n: number): string => n.toString();

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

/** Float exponentiation */
export const floatPow =
  (base: number) =>
  (exp: number): number =>
    Math.pow(base, exp);

// =============================================================================
// Int Operations
// =============================================================================

/** Integer addition */
export const intAdd =
  (a: number) =>
  (b: number): number =>
    floatAdd(a)(b) | 0;

/** Integer subtraction */
export const intSub =
  (a: number) =>
  (b: number): number =>
    floatSub(a)(b) | 0;

/** Integer multiplication */
export const intMul =
  (a: number) =>
  (b: number): number =>
    floatMul(a)(b) | 0;

/** Integer division (truncated toward zero) */
export const intDiv =
  (a: number) =>
  (b: number): number =>
    Math.trunc(floatDiv(a)(b));

/** Integer modulo */
export const intMod =
  (a: number) =>
  (b: number): number =>
    a % b;

/** Integer negation */
export const intNegate = (a: number): number => floatNegate(a) | 0;

/** Integer exponentiation */
export const intPow =
  (base: number) =>
  (exp: number): number =>
    floatPow(base)(exp) | 0;

// =============================================================================
// Generic Operations
// =============================================================================

export const deepEqual =
  <A>(a: A) =>
  (b: A): boolean => {
    // Handle primitives and null/undefined
    if (a === b) return true;

    // Handle null/undefined cases
    if (a == null || b == null) return false;

    // Handle arrays
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false;
      for (let i = 0; i < a.length; i++) {
        if (!deepEqual(a[i])(b[i])) return false;
      }
      return true;
    }

    // Handle objects
    if (typeof a === "object" && typeof b === "object") {
      const keysA = Object.keys(a);
      const keysB = Object.keys(b);

      if (keysA.length !== keysB.length) return false;

      for (const key of keysA) {
        if (!(key in b)) return false;
        if (!deepEqual((a as any)[key])((b as any)[key])) return false;
      }

      return true;
    }

    return false;
  };

// =============================================================================
// Debug / Development
// =============================================================================

/** Print a value to console (for debugging) */
export const println = <A>(value: A): A => {
  console.log(value);
  return value;
};

export const null_ = null;
