export const fromNullable = (just, nothing, value) => {
  return value === null || value === undefined ? nothing : just(value);
};
