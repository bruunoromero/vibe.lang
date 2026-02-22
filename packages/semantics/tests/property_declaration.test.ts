import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index.ts";

function analyzeSource(source: string) {
  const fullSource = source.trim().startsWith("module ")
    ? source
    : `module Test exposing (..)\n\n${source}`;
  const ast = parse(fullSource);
  return analyze(ast, {
    fileContext: { filePath: "Test", srcDir: "" },
  });
}

function expectError(source: string, expectedMessage: string) {
  const fullSource = source.trim().startsWith("module ")
    ? source
    : `module Test exposing (..)\n\n${source}`;
  try {
    const ast = parse(fullSource);
    analyze(ast, {
      fileContext: { filePath: "Test", srcDir: "" },
    });
    throw new Error(`Expected SemanticError but analysis succeeded`);
  } catch (e) {
    if (e instanceof SemanticError) {
      expect(e.message).toContain(expectedMessage);
    } else {
      throw e;
    }
  }
}

describe("@get declaration", () => {
  test("accepts @get with opaque type", () => {
    const result = analyzeSource(`
type FileStat

@get "size"
fileStatSize : FileStat -> Int
`);
    expect(result.types["fileStatSize"]).toBeDefined();
  });

  test("rejects @get with non-opaque (ADT) first argument", () => {
    expectError(
      `
type Color = Red | Blue

@get "size"
getSize : Color -> Int
`,
      "is not an opaque type",
    );
  });

  test("rejects @get with type variable first argument", () => {
    expectError(
      `
@get "size"
getSize : a -> Int
`,
      "first argument must be an opaque type",
    );
  });

  test("rejects @get with more than one argument", () => {
    expectError(
      `
type FileStat

@get "size"
fileStatSize : FileStat -> Int -> String
`,
      "must have type A -> B (exactly one argument)",
    );
  });
});

describe("@call declaration", () => {
  test("accepts @call with opaque type, zero extra args", () => {
    const result = analyzeSource(`
type FileStat

@call "toString"
fileStatToString : FileStat -> String
`);
    expect(result.types["fileStatToString"]).toBeDefined();
  });

  test("accepts @call with opaque type, multiple args", () => {
    const result = analyzeSource(`
type Handle

@call "write"
handleWrite : Handle -> String -> Int
`);
    expect(result.types["handleWrite"]).toBeDefined();
  });

  test("rejects @call with non-opaque first argument", () => {
    expectError(
      `
type Color = Red | Blue

@call "show"
showColor : Color -> String
`,
      "is not an opaque type",
    );
  });

  test("rejects @call with type variable first argument", () => {
    expectError(
      `
@call "show"
showThing : a -> String
`,
      "first argument must be an opaque type",
    );
  });

  test("rejects duplicate definition for property declaration", () => {
    expectError(
      `
type FileStat

fileStatSize : FileStat -> Int
fileStatSize x = 42

@get "size"
fileStatSize : FileStat -> Int
`,
      "Duplicate definition",
    );
  });

  test("rejects separate type annotation for property declaration", () => {
    expectError(
      `
type FileStat

@get "size"
fileStatSize : FileStat -> Int

fileStatSize : FileStat -> Int
`,
      "already includes a type annotation",
    );
  });
});
