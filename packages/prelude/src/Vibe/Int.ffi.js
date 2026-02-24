export const intAdd = (a, b) => (a + b) | 0;

export const intSub = (a, b) => (a - b) | 0;

export const intMul = (a, b) => (a * b) | 0;

export const intDiv = (a, b) => Math.trunc(a / b);

export const intMod = (a, b) => a % b;

export const numEq = (a, b) => a === b;

export const numLt = (a, b) => a < b;

export const numGt = (a, b) => a > b;

export const numToString = (n) => n.toString();
