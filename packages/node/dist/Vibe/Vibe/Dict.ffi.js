export const dictEmpty = new Map();

export const dictInsert = (k, v, d) => {
  const m = new Map(d);
  m.set(k, v);
  return m;
};

export const dictRemove = (k, d) => {
  const m = new Map(d);
  m.delete(k);
  return m;
};

export const dictFoldl = (fn, acc, d) => {
  let result = acc;
  d.forEach((v, k) => {
    result = fn(k)(v)(result);
  });
  return result;
};

export const dictKeys = (d) => [...d.keys()];

export const dictValues = (d) => [...d.values()];

export const dictToList = (toPair, d) => {
  const result = [];
  d.forEach((v, k) => {
    result.push(toPair(k)(v));
  });
  return result;
};

export const dictFromList = (getKey, getValue, lst) => {
  const m = new Map();
  for (const pair of lst) {
    m.set(getKey(pair), getValue(pair));
  }
  return m;
};

export const dictMap = (fn, d) => {
  const m = new Map();
  d.forEach((v, k) => {
    m.set(k, fn(k)(v));
  });
  return m;
};

export const dictFilter = (pred, d) => {
  const m = new Map();
  d.forEach((v, k) => {
    if (pred(k)(v)) {
      m.set(k, v);
    }
  });
  return m;
};

export const dictUnion = (left, right) => {
  const m = new Map(right);
  left.forEach((v, k) => {
    m.set(k, v);
  });
  return m;
};
