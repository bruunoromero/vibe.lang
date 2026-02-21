export const listCons = (head) => (tail) => [head, ...tail];

export const listAppend = (xs) => (ys) => [...xs, ...ys];

export const listMap = (f) => (xs) => xs.map(f);

export const listFoldl = (f) => (init) => (xs) => xs.reduce(f, init);

export const listFilter = (predicate) => (xs) => xs.filter(predicate);
