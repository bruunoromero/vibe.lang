import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, type SemanticModule } from "../src/index.ts";

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

describe("cross-module type instantiation", () => {
  // Regression test: external declarations with polymorphic types must be properly
  // instantiated when accessed from another module via qualified names.
  // Previously, external type schemes had empty quantified vars, causing stale
  // type variable IDs to leak across modules and trigger false "Recursive type detected" errors.

  test("qualified access to imported external polymorphic function", () => {
    const depModule = analyzeNoPrelude(`
module Dep exposing (Box, mapBox)

type Box a

@external "@vibe/runtime" "mapBox"
mapBox : (a -> b) -> Box a -> Box b
`);

    const source = `
module Main exposing (..)

import Dep exposing (Box)

identity : a -> a
identity x = x

test1 : Box a -> Box a
test1 box = Dep.mapBox identity box
`;
    expect(() =>
      analyze(parse(source), {
        dependencies: new Map([["Dep", depModule]]),
        fileContext: { filePath: "Main", srcDir: "" },
      }),
    ).not.toThrow();
  });

  test("qualified access with pipe operator (Task.vibe pattern)", () => {
    const depModule = analyzeNoPrelude(`
module Dep exposing (Promise, map)

type Promise a

@external "@vibe/runtime" "mapPromise"
map : (a -> b) -> Promise a -> Promise b
`);

    const source = `
module Main exposing (..)

import Dep exposing (Promise)

type Result x a = Ok a | Err x

identity : a -> a
identity x = x

infixl 1 |>
(|>) : a -> (a -> b) -> b
(|>) x f = f x

type Task x a = Task (Promise (Result x a))

taskMap : (a -> b) -> Task x a -> Task x b
taskMap f (Task promise) =
    promise |> Dep.map identity |> Task
`;
    expect(() =>
      analyze(parse(source), {
        dependencies: new Map([["Dep", depModule]]),
        fileContext: { filePath: "Main", srcDir: "" },
      }),
    ).not.toThrow();
  });

  test("qualified access with alias (import Dep as D)", () => {
    const depModule = analyzeNoPrelude(`
module Dep exposing (Box, mapBox)

type Box a

@external "@vibe/runtime" "mapBox"
mapBox : (a -> b) -> Box a -> Box b
`);

    const source = `
module Main exposing (..)

import Dep as D exposing (Box)

identity : a -> a
identity x = x

test1 : Box a -> Box a
test1 box = D.mapBox identity box
`;
    expect(() =>
      analyze(parse(source), {
        dependencies: new Map([["Dep", depModule]]),
        fileContext: { filePath: "Main", srcDir: "" },
      }),
    ).not.toThrow();
  });

  test("multiple uses of imported external get independent type variables", () => {
    const depModule = analyzeNoPrelude(`
module Dep exposing (convert)

@external "@vibe/runtime" "convert"
convert : a -> b
`);

    // Two uses of Dep.convert should have independent type variables
    // and not interfere with each other
    const source = `
module Main exposing (..)

import Dep

test1 : Int -> String
test1 x = Dep.convert x

test2 : String -> Int
test2 x = Dep.convert x
`;
    expect(() =>
      analyze(parse(source), {
        dependencies: new Map([["Dep", depModule]]),
        fileContext: { filePath: "Main", srcDir: "" },
      }),
    ).not.toThrow();
  });

  test("imported external constructor types are properly instantiated", () => {
    const depModule = analyzeNoPrelude(`
module Dep exposing (Wrapper(..))

type Wrapper a = Wrap a
`);

    // Constructor Wrap should get fresh type variables at each use
    const source = `
module Main exposing (..)

import Dep exposing (Wrapper(..))

test1 : Int -> Wrapper Int
test1 x = Wrap x

test2 : String -> Wrapper String
test2 x = Wrap x
`;
    expect(() =>
      analyze(parse(source), {
        dependencies: new Map([["Dep", depModule]]),
        fileContext: { filePath: "Main", srcDir: "" },
      }),
    ).not.toThrow();
  });

  // Regression test: instances from modules NOT directly imported should still
  // be visible (Haskell's global instance semantics). This prevents errors like
  // "No instance of 'Appendable' found for 'String'" when auto-derived Show
  // uses ++ (Appendable) but the module defining Appendable String isn't imported.
  test("instances from non-imported dependency modules are globally visible", () => {
    // Module that defines a protocol
    const basicsModule = analyzeNoPrelude(`
module Basics exposing (..)

protocol Appendable a where
    (++) : a -> a -> a

protocol Show a where
    toString : a -> String

infixr 5 ++
`);

    // Module that defines an instance (Appendable String)
    const stringModule = analyze(
      parse(`
module StringMod exposing (..)

import Basics exposing (..)

implement Appendable String where
    (++) a b = a
`),
      {
        dependencies: new Map([["Basics", basicsModule]]),
        fileContext: { filePath: "StringMod", srcDir: "" },
      },
    );

    // Module that imports Basics but NOT StringMod.
    // Auto-derived Show would use ++ (Appendable String),
    // so Appendable String must be available via global instance visibility.
    const taskModule = analyze(
      parse(`
module TaskMod exposing (..)

import Basics exposing (..)

type Wrapper a = Wrapper a
`),
      {
        dependencies: new Map([
          ["Basics", basicsModule],
          ["StringMod", stringModule],
        ]),
        fileContext: { filePath: "TaskMod", srcDir: "" },
      },
    );

    // TaskMod should have the Appendable String instance even though
    // it doesn't import StringMod directly
    const hasAppendableString = taskModule.instances.some(
      (inst) =>
        inst.protocolName === "Appendable" &&
        inst.typeArgs.length === 1 &&
        inst.typeArgs[0]!.kind === "con" &&
        inst.typeArgs[0]!.name === "String",
    );
    expect(hasAppendableString).toBe(true);
  });
});

describe("self-instance leak prevention", () => {
  // Defense-in-depth: if a dependency's cached SemanticModule contains
  // instances that originated from the current module (e.g., because a
  // downstream module transitively carried them), those instances must NOT
  // be imported back — otherwise the overlap check falsely fires.

  test("should not import own instances back from dependency cache", () => {
    // 1. Analyze the upstream module that defines a protocol + instance
    const upstreamModule = analyzeNoPrelude(`
module Upstream exposing (..)

protocol MyProto a where
    myMethod : a -> a

implement MyProto Int where
    myMethod x = x
`);

    expect(upstreamModule.instances.length).toBeGreaterThan(0);

    // 2. Build a fake downstream module whose instances array contains
    //    upstream's instances (simulating transitive import propagation)
    const fakeDownstream: SemanticModule = {
      ...upstreamModule,
      module: { ...upstreamModule.module, name: "Downstream" },
    };

    // 3. Re-analyze upstream with the fake downstream as a dependency.
    //    The global-instance-visibility loop sees fakeDownstream's instances
    //    which include "Upstream.MyProto Int". The defense-in-depth filter
    //    must skip them since their moduleName === "Upstream".
    expect(() => {
      analyze(
        parse(`module Upstream exposing (..)

protocol MyProto a where
    myMethod : a -> a

implement MyProto Int where
    myMethod x = x
`),
        {
          dependencies: new Map([["Downstream", fakeDownstream]]),
          fileContext: { filePath: "Upstream", srcDir: "" },
        },
      );
    }).not.toThrow();
  });
});
