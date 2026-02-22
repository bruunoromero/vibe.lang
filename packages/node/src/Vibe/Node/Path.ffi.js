import path from "node:path";

export const pathJoin = (segments) => {
  return path.join(...segments);
};

export const pathResolve = (segments) => {
  return path.resolve(...segments);
};

export const pathBasename = (p) => {
  return path.basename(p);
};

export const pathDirname = (p) => {
  return path.dirname(p);
};

export const pathExtname = (p) => {
  return path.extname(p);
};

export const pathNormalize = (p) => {
  return path.normalize(p);
};

export const pathIsAbsolute = (p) => {
  return path.isAbsolute(p);
};

export const pathSeparator = path.sep;
