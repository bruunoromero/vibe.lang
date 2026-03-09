export const listSort = (lt, lst) => {
  const copy = [...lst];
  copy.sort((a, b) => {
    if (lt(a)(b)) return -1;
    if (lt(b)(a)) return 1;
    return 0;
  });
  return copy;
};

export const listReverse = (lst) => [...lst].reverse();

export const listNth = (just, nothing, n, lst) => {
  if (n >= 0 && n < lst.length) {
    return just(lst[n]);
  }
  return nothing;
};
