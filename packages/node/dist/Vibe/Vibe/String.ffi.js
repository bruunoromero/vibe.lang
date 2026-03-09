export const stringAppend = (a, b) => a + b;

export const stringEq = (a, b) => a === b;

export const stringLt = (a, b) => a < b;

export const stringGt = (a, b) => a > b;

export const parseInt = (just, nothing, s) => {
  const n = Number.parseInt(s, 10);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};

export const parseFloat = (just, nothing, s) => {
  const n = Number.parseFloat(s);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};

export const stringCharAt = (just, nothing, i, s) => {
  if (i >= 0 && i < s.length) {
    return just(s[i]);
  }
  return nothing;
};

export const unsafeCharAt = (i, s) => s[i];

export const stringToList = (s) => [...s];

export const stringFromChar = (c) => c;
