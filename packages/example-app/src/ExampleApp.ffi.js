import fs from "node:fs/promises";

export const readFile = (path) => {
  return fs.readFile(path);
};
