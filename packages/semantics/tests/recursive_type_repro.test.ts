import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "../src/index.ts";

const TYPE_PREAMBLE = `
type Maybe a = Just a | Nothing
`;

const analyzeNoPrelude = (source: string) => {
  let fullSource = source;
  const trimmed = source.trim();
  let moduleName = "Test";
  if (trimmed.startsWith("module")) {
    const moduleMatch = trimmed.match(/^module\s+(\S+)/);
    if (moduleMatch) moduleName = moduleMatch[1]!;
    const lines = source.split("\n");
    let insertIndex = 0;
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      if (line.startsWith("module") || line.startsWith("import")) {
        insertIndex = i + 1;
      } else if (line) {
        break;
      }
    }
    fullSource =
      lines.slice(0, insertIndex).join("\n") +
      "\n" +
      TYPE_PREAMBLE +
      "\n" +
      lines.slice(insertIndex).join("\n");
  } else {
    fullSource = "module Test exposing (..)\n" + TYPE_PREAMBLE + "\n" + source;
  }
  return analyze(parse(fullSource), {
    fileContext: { filePath: moduleName, srcDir: "" },
  });
};

describe("recursive type repro", () => {
  test("map identity - direct application", () => {
    expect(() =>
      analyzeNoPrelude(`
type Box a = Box a

mapBox : (a -> b) -> Box a -> Box b
mapBox f (Box x) = Box (f x)

identity : a -> a
identity x = x

test1 : Box a -> Box a
test1 box = mapBox identity box
`),
    ).not.toThrow();
  });

  test("map identity with pipe", () => {
    expect(() =>
      analyzeNoPrelude(`
type Box a = Box a

mapBox : (a -> b) -> Box a -> Box b
mapBox f (Box x) = Box (f x)

identity : a -> a
identity x = x

infixl 1 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x

test2 : Box a -> Box a
test2 box = box |> mapBox identity
`),
    ).not.toThrow();
  });

  test("map identity with opaque type", () => {
    expect(() =>
      analyzeNoPrelude(`
type Promise a

@external "@vibe/runtime" "mapPromise"
promiseMap : (a -> b) -> Promise a -> Promise b

identity : a -> a
identity x = x

infixl 1 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x

test3 : Promise a -> Promise a
test3 p = p |> promiseMap identity
`),
    ).not.toThrow();
  });

  test("cross-module opaque type with map identity", () => {
    const promiseModule = analyzeNoPrelude(`
module PromiseMod exposing (Promise, map)

type Promise a

@external "@vibe/runtime" "mapPromise"
map : (a -> b) -> Promise a -> Promise b
`);

    const source = `
module TaskMod exposing (..)

import PromiseMod exposing (Promise)

type Result x a = Ok a | Err x

identity : a -> a
identity x = x

infixl 1 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x

type Task x a = Task (Promise (Result x a))

taskMap : (a -> b) -> Task x a -> Task x b
taskMap f (Task promise) =
    promise |> PromiseMod.map identity |> Task
`;
    expect(() => {
      return analyze(parse(source), {
        dependencies: new Map([["PromiseMod", promiseModule]]),
        fileContext: { filePath: "TaskMod", srcDir: "" },
      });
    }).not.toThrow();
  });

  test("opaque type with constructor wrapping - single module", () => {
    expect(() =>
      analyzeNoPrelude(`
type Result x a = Ok a | Err x
type Promise a

@external "@vibe/runtime" "mapPromise"
promiseMap : (a -> b) -> Promise a -> Promise b

identity : a -> a
identity x = x

infixl 1 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x

type Task x a = Task (Promise (Result x a))

taskMap : (a -> b) -> Task x a -> Task x b
taskMap f (Task promise) =
    promise |> promiseMap identity |> Task
`),
    ).not.toThrow();
  });
});
