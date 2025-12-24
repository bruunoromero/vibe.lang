import path from "node:path";

export const LANG_EXTENSION = ".lang";

export const isRelativeSpecifier = (specifier: string): boolean => {
  return specifier.startsWith("./") || specifier.startsWith("../");
};

export const ensureLangSpecifier = (specifier: string): string | null => {
  if (specifier.endsWith(LANG_EXTENSION)) {
    return specifier;
  }
  if (specifier.endsWith(".js")) {
    return null;
  }
  return `${specifier}${LANG_EXTENSION}`;
};

export const normalizeRequireTarget = (
  fromModuleId: string,
  specifier: string
): string | null => {
  if (!isRelativeSpecifier(specifier)) {
    return null;
  }
  const langSpecifier = ensureLangSpecifier(specifier);
  if (!langSpecifier) {
    return null;
  }
  const fromDir = path.dirname(fromModuleId);
  return path.resolve(fromDir, langSpecifier);
};
