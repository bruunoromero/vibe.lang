export const createRef = (value) => {
  return { current: value };
};

export const getRef = (ref) => {
  return ref.current;
};

export const setRef = (ref, value) => {
  ref.current = value;
};

export const refEq = (a, b) => a === b;
