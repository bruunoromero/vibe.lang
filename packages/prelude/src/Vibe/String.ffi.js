export const stringAppend = (a) => (b) => a + b;

export const stringEq = (a) => (b) => a === b;

export const parseInt = (just) => (nothing) => (s) => {
  const n = Number.parseInt(s, 10);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};

export const parseFloat = (just) => (nothing) => (s) => {
  const n = Number.parseFloat(s);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
