/**
 * @vibe/runtime - Runtime Support Library for Vibe
 *
 * This module provides the low-level implementations for built-in operations
 * that are exposed to Vibe code via @external declarations in the Prelude.
 */

// =============================================================================
// Bool Operations
// =============================================================================

/** Logical NOT */
export const not = (a: boolean): boolean => !a;

export const boolEq =
  (a: boolean) =>
  (b: boolean): boolean =>
    a === b;

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

/** Map over a list */
export const listMap =
  <A, B>(f: (a: A) => B) =>
  (xs: A[]): B[] =>
    xs.map(f);

/** Left fold over a list */
export const listFoldl =
  <A, B>(f: (acc: B, a: A) => B) =>
  (init: B) =>
  (xs: A[]): B =>
    xs.reduce(f, init);

/** Filter a list */
export const listFilter =
  <A>(predicate: (a: A) => boolean) =>
  (xs: A[]): A[] =>
    xs.filter(predicate);

// =============================================================================
// String Operations
// =============================================================================

export const stringAppend =
  (a: string) =>
  (b: string): string =>
    a + b;

export const stringEq =
  (a: string) =>
  (b: string): boolean =>
    a === b;

// =============================================================================
// Char Operations
// =============================================================================

export const charToString = (a: string): string => a;

// =============================================================================
// Generic Number Operations
// =============================================================================

/** Number equality */
export const numEq =
  (a: number) =>
  (b: number): boolean =>
    a === b;

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
// Debug / Development
// =============================================================================

/** Print a value to console (for debugging) */
export const println = <A>(value: A): A => {
  console.log(value);
  return value;
};
