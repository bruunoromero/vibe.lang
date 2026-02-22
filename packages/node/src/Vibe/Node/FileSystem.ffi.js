import fs from "node:fs/promises";

export const makeDir = (path) => {
  return fs.mkdir(path, { recursive: true });
};

export const exists = (path) => {
  return fs
    .access(path)
    .then(() => true)
    .catch(() => false);
};
