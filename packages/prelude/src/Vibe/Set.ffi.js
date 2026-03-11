export const setEmpty = new Set();

export const setFromList = (lst) => new Set(lst);

export const setInsert = (v, s) => {
  const copy = new Set(s);
  copy.add(v);
  return copy;
};

export const setRemove = (v, s) => {
  const copy = new Set(s);
  copy.delete(v);
  return copy;
};

export const setMember = (v, s) => s.has(v);

export const setToList = (s) => [...s];

export const setFoldl = (fn, acc, s) => {
  let result = acc;
  s.forEach((v) => {
    result = fn(v)(result);
  });
  return result;
};

export const setMap = (fn, s) => {
  const result = new Set();
  s.forEach((v) => {
    result.add(fn(v));
  });
  return result;
};

export const setFilter = (pred, s) => {
  const result = new Set();
  s.forEach((v) => {
    if (pred(v)) {
      result.add(v);
    }
  });
  return result;
};

export const setUnion = (a, b) => {
  const result = new Set(b);
  a.forEach((v) => result.add(v));
  return result;
};

export const setIntersect = (a, b) => {
  const result = new Set();
  a.forEach((v) => {
    if (b.has(v)) result.add(v);
  });
  return result;
};

export const setDiff = (a, b) => {
  const result = new Set(a);
  b.forEach((v) => result.delete(v));
  return result;
};
