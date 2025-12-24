import { BUILTIN_SYMBOLS } from "@vibe/syntax";

const EXTRA_SYMBOLS = [
  "+",
  "-",
  "*",
  "/",
  "=",
  "<",
  ">",
  "<=",
  ">=",
  "add*",
  "sub*",
  "mul*",
  "div*",
  "mod*",
  "eq*",
  "lt*",
  "gt*",
  "lte*",
  "gte*",
  "first",
  "rest",
  "cons",
  "count",
  "nth",
  "assoc",
  "dissoc",
  "keys",
  "vals",
  "type",
  "str",
  "gensym",
  "println",
] as const;

export const TEST_BUILTINS: readonly string[] = [
  ...new Set<string>([
    ...(BUILTIN_SYMBOLS as readonly string[]),
    ...EXTRA_SYMBOLS,
  ]),
];
