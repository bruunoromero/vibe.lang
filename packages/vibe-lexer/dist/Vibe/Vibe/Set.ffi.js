export const setEmpty = new Set();

export const setInsert = (value, s) => {
  const ns = new Set(s);
  ns.add(value);
  return ns;
};

export const setRemove = (value, s) => {
  const ns = new Set(s);
  ns.delete(value);
  return ns;
};

export const setFromList = (lst) => new Set(lst);

export const setToList = (s) => [...s];

export const setFoldl = (fn, acc, s) => {
  let result = acc;
  for (const v of s) {
    result = fn(v)(result);
  }
  return result;
};

export const setMap = (fn, s) => {
  const ns = new Set();
  for (const v of s) {
    ns.add(fn(v));
  }
  return ns;
};

export const setFilter = (pred, s) => {
  const ns = new Set();
  for (const v of s) {
    if (pred(v)) {
      ns.add(v);
    }
  }
  return ns;
};

export const setUnion = (a, b) => {
  const ns = new Set(b);
  for (const v of a) {
    ns.add(v);
  }
  return ns;
};

export const setIntersect = (a, b) => {
  const ns = new Set();
  for (const v of a) {
    if (b.has(v)) {
      ns.add(v);
    }
  }
  return ns;
};

export const setDiff = (a, b) => {
  const ns = new Set(a);
  for (const v of b) {
    ns.delete(v);
  }
  return ns;
};
