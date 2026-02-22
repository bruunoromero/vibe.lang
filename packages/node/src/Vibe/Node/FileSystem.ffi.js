import fs from "node:fs/promises";

export const readFile = (path) => {
  return fs.readFile(path, "utf-8");
};

export const writeFile = (path) => (contents) => {
  return fs.writeFile(path, contents, "utf-8");
};

export const appendFile = (path) => (contents) => {
  return fs.appendFile(path, contents, "utf-8");
};

export const removeFile = (path) => {
  return fs.unlink(path);
};

export const readDir = (path) => {
  return fs.readdir(path);
};

export const makeDir = (path) => {
  return fs.mkdir(path, { recursive: true });
};

export const stat = (path) => {
  return fs.stat(path).then((s) => ({
    size: s.size,
    isFile: s.isFile(),
    isDirectory: s.isDirectory(),
  }));
};

export const exists = (path) => {
  return fs
    .access(path)
    .then(() => true)
    .catch(() => false);
};
