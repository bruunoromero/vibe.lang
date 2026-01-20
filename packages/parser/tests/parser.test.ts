import { describe, expect, test } from "bun:test";
import {
  parse,
  collectInfixDeclarations,
  parseWithInfix,
} from "../src/index.ts";
import type { ValueDeclaration } from "@vibe/syntax";

/**
 * Helper to parse test sources.
 * Adds "module Test exposing (..)" if the source doesn't start with a module declaration.
 */
const parseTest = (source: string) => {
  const trimmed = source.trim();
  if (trimmed.startsWith("module ")) {
    return parse(source);
  }
  return parse(`module Test exposing (..)\n${source}`);
};

describe("parser", () => {
  test("parses module headers, imports, and declarations", () => {
    const source = `module Main exposing (..)
import Html exposing (text)
import My.Utils as Utils exposing (..)

main : Html msg
main =
  let
    value = "ok"
  in
    text value

custom x y = x <+> y
`;

    const program = parseTest(source);

    expect(program.module?.name).toBe("Main");
    expect(program.module?.exposing?.kind).toBe("All");

    expect(program.imports.length).toBe(2);
    expect(program.imports[0]?.moduleName).toBe("Html");
    expect(program.imports[1]?.alias).toBe("Utils");
    expect(program.imports[1]?.exposing?.kind).toBe("All");

    const [mainType, mainValue, custom] = program.declarations as [
      any,
      ValueDeclaration,
      ValueDeclaration,
    ];

    expect(mainType.kind).toBe("TypeAnnotationDeclaration");
    expect(mainType.annotation.kind).toBe("TypeRef");
    expect(mainValue.kind).toBe("ValueDeclaration");
    expect(mainValue.body.kind).toBe("LetIn");

    expect(custom.kind).toBe("ValueDeclaration");
    expect(custom.body.kind).toBe("Infix");
    if (custom.body.kind === "Infix") {
      expect(custom.body.operator).toBe("<+>");
    }
  });

  test("parses module header without exposing clause", () => {
    const source = `module Vibe.Unit

import Vibe.Basics exposing (..)

implement Eq Unit where
    (==) _ _ = True
`;
    const program = parse(source);
    expect(program.module.name).toBe("Vibe.Unit");
    expect(program.module.exposing).toBeNull();
    expect(program.declarations.length).toBe(1);
  });

  test("allows custom operators in expressions", () => {
    const program = parseTest("value = a <*> b");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Infix");
    if (decl.body.kind === "Infix") {
      expect(decl.body.operator).toBe("<*>");
    }
  });

  test("parses tuples, records, list ranges, and field access", () => {
    const program = parseTest(`value = (1, 2, 3)
rec = { a = 1, b = 2 }
up = { rec | b = 3 }
xs = [1..10]
y = rec.b`);
    const recDecl = program.declarations[1] as ValueDeclaration;
    const rangeDecl = program.declarations[3] as ValueDeclaration;
    const accessDecl = program.declarations[4] as ValueDeclaration;

    if (recDecl.body.kind === "Record") {
      expect(recDecl.body.fields.length).toBe(2);
    }

    if (rangeDecl.body.kind === "ListRange") {
      expect(rangeDecl.body.start.kind).toBe("Number");
    }

    if (accessDecl.body.kind === "FieldAccess") {
      expect(accessDecl.body.field).toBe("b");
    }
  });

  test("honors operator precedence and associativity", () => {
    // With infix declarations, operators have explicit precedence.
    // Without declarations, all operators use default precedence (5, left-assoc).
    const source = `module Test exposing (..)

infixl 1 |>
infixr 1 <|
infixl 6 +
infixl 7 *
infixr 8 ^
infixr 9 <<

result = 1 + 2 * 3 ^ 2 |> f <| g << h`;
    const { program } = parseWithInfix(source);
    const decl = program.declarations.find(
      (d) => d.kind === "ValueDeclaration" && d.name === "result",
    ) as ValueDeclaration;
    expect(decl).toBeDefined();
    expect(decl.body.kind).toBe("Infix");
    if (decl.body.kind === "Infix") {
      // <| has lowest precedence (1), so it's the outermost operator
      expect(decl.body.operator).toBe("<|");
    }
  });

  test("supports operator declarations", () => {
    const program = parseTest(
      "(<+>) : number -> number -> number\n(<+>) a b = a + b",
    );
    expect(program.declarations.length).toBe(2);
  });

  test("parses external declarations", () => {
    const program = parseTest(`@external "@scope/pkg/sub/path" "makeWidget"
widget : number -> number`);

    expect(program.declarations.length).toBe(1);
    const external = program.declarations[0];

    // Type guard to ensure external is defined and is ExternalDeclaration
    expect(external).toBeDefined();
    expect(external?.kind).toBe("ExternalDeclaration");

    if (external && external.kind === "ExternalDeclaration") {
      expect(external.target.modulePath).toBe("@scope/pkg/sub/path");
      expect(external.target.exportName).toBe("makeWidget");
      expect(external.annotation.kind).toBe("FunctionType");
    }
  });

  test("parses external with empty tuple type annotation", () => {
    const program = parseTest(`@external "@vibe/runtime" "myList2"
myList2 : () -> MyList`);

    expect(program.declarations.length).toBe(1);
    const external = program.declarations[0];

    expect(external?.kind).toBe("ExternalDeclaration");

    if (external && external.kind === "ExternalDeclaration") {
      expect(external.name).toBe("myList2");
      expect(external.target.modulePath).toBe("@vibe/runtime");
      expect(external.target.exportName).toBe("myList2");
      expect(external.annotation.kind).toBe("FunctionType");

      if (external.annotation.kind === "FunctionType") {
        // () should be parsed as Unit
        expect(external.annotation.from.kind).toBe("TypeRef");
        expect((external.annotation.from as any).name).toBe("Unit");
      }
    }
  });

  test("parses multiple externals in a module", () => {
    const program = parseTest(`@external "@vibe/runtime" "myList"
myList : Bool -> Bool -> Bool

type MyList a

@external "@vibe/runtime" "myList2"
myList2 : () -> MyList`);

    // Should have 3 declarations: myList (external), MyList (type), myList2 (external)
    expect(program.declarations.length).toBe(3);

    const myList = program.declarations[0]!;
    expect(myList.kind).toBe("ExternalDeclaration");
    expect((myList as { name: string }).name).toBe("myList");

    const myListType = program.declarations[1]!;
    expect(myListType.kind).toBe("OpaqueTypeDeclaration");
    expect((myListType as { name: string }).name).toBe("MyList");

    const myList2 = program.declarations[2]!;
    expect(myList2.kind).toBe("ExternalDeclaration");
    expect((myList2 as { name: string }).name).toBe("myList2");
  });

  test("parses opaque type followed by function definition", () => {
    // Regression test: opaque types should not consume the next declaration's name
    // as a type parameter (layout-sensitive parsing)
    const program = parseTest(`type Never

never : Never -> a
never nvr = never nvr`);

    expect(program.declarations.length).toBe(3);

    // First declaration: opaque type with no params
    const neverType = program.declarations[0]!;
    expect(neverType.kind).toBe("OpaqueTypeDeclaration");
    if (neverType.kind === "OpaqueTypeDeclaration") {
      expect(neverType.name).toBe("Never");
      expect(neverType.params).toEqual([]);
    }

    // Second declaration: type annotation
    const neverAnnotation = program.declarations[1]!;
    expect(neverAnnotation.kind).toBe("TypeAnnotationDeclaration");
    if (neverAnnotation.kind === "TypeAnnotationDeclaration") {
      expect(neverAnnotation.name).toBe("never");
    }

    // Third declaration: value (function) definition
    const neverValue = program.declarations[2]!;
    expect(neverValue.kind).toBe("ValueDeclaration");
    if (neverValue.kind === "ValueDeclaration") {
      expect(neverValue.name).toBe("never");
    }
  });

  test("parses opaque type with params followed by function definition", () => {
    // Ensure type params on the same line are correctly captured
    const program = parseTest(`type Promise a b

resolve : a -> Promise a b
resolve x = resolve x`);

    expect(program.declarations.length).toBe(3);

    const promiseType = program.declarations[0]!;
    expect(promiseType.kind).toBe("OpaqueTypeDeclaration");
    if (promiseType.kind === "OpaqueTypeDeclaration") {
      expect(promiseType.name).toBe("Promise");
      expect(promiseType.params).toEqual(["a", "b"]);
    }

    const resolveAnnotation = program.declarations[1]!;
    expect(resolveAnnotation.kind).toBe("TypeAnnotationDeclaration");
    if (resolveAnnotation.kind === "TypeAnnotationDeclaration") {
      expect(resolveAnnotation.name).toBe("resolve");
    }
  });

  test("respects indentation for multi-line applications", () => {
    const program = parseTest(`main =
  view model
  |> Html.map msg

next = 1`);

    expect(program.declarations.length).toBe(2);
    const mainDecl = program.declarations[0] as ValueDeclaration;
    expect(mainDecl.body.kind).toBe("Infix");
    const nextDecl = program.declarations[1] as ValueDeclaration;
    expect(nextDecl.name).toBe("next");
  });

  test("respects indentation for record fields and nested applications", () => {
    const program = parseTest(`config =
  { a = 1
  , b = foo
      bar
  }

next = 2`);

    expect(program.declarations.length).toBe(2);
    const config = program.declarations[0] as ValueDeclaration;
    expect(config.body.kind).toBe("Record");
    if (config.body.kind === "Record") {
      expect(config.body.fields.length).toBe(2);
    }
    const nextDecl = program.declarations[1] as ValueDeclaration;
    expect(nextDecl.name).toBe("next");
  });

  test("respects indentation inside tuples", () => {
    const program = parseTest(`value =
  ( one
  , two
  )

next = 3`);

    expect(program.declarations.length).toBe(2);
    const value = program.declarations[0] as ValueDeclaration;
    expect(value.body.kind).toBe("Tuple");
  });

  test("let bindings respect offside rule", () => {
    const program = parseTest(`main =
  let
    a = 1
    b =
      foo
        bar
  in
    a

next = 4`);

    expect(program.declarations.length).toBe(2);
    const mainDecl = program.declarations[0] as ValueDeclaration;
    expect(mainDecl.body.kind).toBe("LetIn");
    const nextDecl = program.declarations[1] as ValueDeclaration;
    expect(nextDecl.name).toBe("next");
  });

  test("case branches respect offside rule", () => {
    const program = parseTest(`main =
  case x of
    Just a ->
      a
    Nothing ->
      0

next = 5`);

    expect(program.declarations.length).toBe(2);
    const mainDecl = program.declarations[0] as ValueDeclaration;
    expect(mainDecl.body.kind).toBe("Case");
    const nextDecl = program.declarations[1] as ValueDeclaration;
    expect(nextDecl.name).toBe("next");
  });

  test("reports case branch misaligned to 'of' column", () => {
    expect(() =>
      parse(`module Test exposing (..)

main =
  case x of
  Just a -> a

next = 5`),
    ).toThrow("Case branches must be indented to at least column");
  });

  test("reports early dedent in let bindings", () => {
    expect(() =>
      parse(`module Test exposing (..)

main =
  let
    a = 1
  b = 2
  in
    a`),
    ).toThrow("Expected 'in' to close let");
  });

  test("requires case branches", () => {
    expect(() =>
      parse(`module Test exposing (..)

main = case x of

next = 6`),
    ).toThrow("Case branches must be indented");
  });

  // ===== Algebraic Data Type (ADT) Declaration Tests =====

  test("parses simple ADT with nullary constructors", () => {
    const program = parseTest(`type Bool = True | False`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");
    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Bool");
      expect(decl.params).toEqual([]);
      expect(decl.constructors?.length).toBe(2);
      expect(decl.constructors?.[0]?.name).toBe("True");
      expect(decl.constructors?.[0]?.args.length).toBe(0);
      expect(decl.constructors?.[1]?.name).toBe("False");
      expect(decl.constructors?.[1]?.args.length).toBe(0);
    }
  });

  test("parses parameterized ADT (Maybe)", () => {
    const program = parseTest(`type Maybe a = Just a | Nothing`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");
    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Maybe");
      expect(decl.params).toEqual(["a"]);
      expect(decl.constructors?.length).toBe(2);

      // Just a
      expect(decl.constructors?.[0]?.name).toBe("Just");
      expect(decl.constructors?.[0]?.args.length).toBe(1);
      expect(decl.constructors?.[0]?.args[0]?.kind).toBe("TypeRef");

      // Nothing
      expect(decl.constructors?.[1]?.name).toBe("Nothing");
      expect(decl.constructors?.[1]?.args.length).toBe(0);
    }
  });

  test("parses ADT with multiple type parameters (Result)", () => {
    const program = parseTest(`type Result e v = Ok v | Err e`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");
    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Result");
      expect(decl.params).toEqual(["e", "v"]);
      expect(decl.constructors?.length).toBe(2);

      // Ok v
      expect(decl.constructors?.[0]?.name).toBe("Ok");
      expect(decl.constructors?.[0]?.args.length).toBe(1);

      // Err e
      expect(decl.constructors?.[1]?.name).toBe("Err");
      expect(decl.constructors?.[1]?.args.length).toBe(1);
    }
  });

  test("parses recursive ADT (List)", () => {
    const program = parseTest(`type List a = Cons a (List a) | Nil`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");
    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("List");
      expect(decl.params).toEqual(["a"]);
      expect(decl.constructors?.length).toBe(2);

      // Cons a (List a)
      expect(decl.constructors?.[0]?.name).toBe("Cons");
      expect(decl.constructors?.[0]?.args.length).toBe(2);

      // Nil
      expect(decl.constructors?.[1]?.name).toBe("Nil");
      expect(decl.constructors?.[1]?.args.length).toBe(0);
    }
  });

  test("parses ADT with function type argument", () => {
    const program = parseTest(`type Handler a = Handler (a -> a)`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");
    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Handler");
      expect(decl.constructors?.[0]?.args.length).toBe(1);
      // The argument should be a function type wrapped in parens
      const arg = decl.constructors?.[0]?.args[0];
      expect(arg?.kind).toBe("FunctionType");
    }
  });

  // ===== Type Alias Tests =====

  test("parses simple type alias", () => {
    const program = parseTest(`type alias UserId = number`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("UserId");
      expect(decl.params).toEqual([]);
      expect(decl.value.kind).toBe("TypeRef");
    }
  });

  test("parses parameterized type alias", () => {
    const program = parseTest(`type alias Pair a b = (a, b)`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Pair");
      expect(decl.params).toEqual(["a", "b"]);
      expect(decl.value.kind).toBe("TupleType");
    }
  });

  test("parses type alias with function type", () => {
    const program = parseTest(`type alias Handler msg = msg -> Model -> Model`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Handler");
      expect(decl.params).toEqual(["msg"]);
      expect(decl.value.kind).toBe("FunctionType");
    }
  });

  test("parses type alias with parameterized type", () => {
    const program = parseTest(`type alias MaybeString = Maybe string`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("MaybeString");
      expect(decl.params).toEqual([]);
      expect(decl.value.kind).toBe("TypeRef");
      if (decl.value.kind === "TypeRef") {
        expect(decl.value.name).toBe("Maybe");
        expect(decl.value.args.length).toBe(1);
      }
    }
  });

  test("parses multiple type declarations", () => {
    const program = parseTest(`type Bool = True | False

type Maybe a = Just a | Nothing

type alias UserId = number

value = 42`);
    expect(program.declarations.length).toBe(4);
    expect(program.declarations[0]?.kind).toBe("TypeDeclaration");
    expect(program.declarations[1]?.kind).toBe("TypeDeclaration");
    expect(program.declarations[2]?.kind).toBe("TypeAliasDeclaration");
    expect(program.declarations[3]?.kind).toBe("ValueDeclaration");
  });

  // ===== Record Type Annotation Tests =====

  test("parses type alias with record type", () => {
    const program = parseTest(`type alias Point = { x : number, y : number }`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Point");
      expect(decl.params).toEqual([]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        // Fields should be sorted alphabetically
        expect(decl.value.fields[0]?.name).toBe("x");
        expect(decl.value.fields[0]?.type.kind).toBe("TypeRef");
        expect(decl.value.fields[1]?.name).toBe("y");
        expect(decl.value.fields[1]?.type.kind).toBe("TypeRef");
      }
    }
  });

  test("parses empty record type", () => {
    const program = parseTest(`type alias Empty = {}`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(0);
      }
    }
  });

  test("parses record type with function fields", () => {
    const program = parseTest(
      `type alias Model = { count : number, increment : number -> number }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        // Fields sorted: count, increment
        expect(decl.value.fields[0]?.name).toBe("count");
        expect(decl.value.fields[1]?.name).toBe("increment");
        expect(decl.value.fields[1]?.type.kind).toBe("FunctionType");
      }
    }
  });

  test("parses multiline record type", () => {
    const program = parseTest(`type alias Person =
  { name : string
  , age : number
  , email : string
  }`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(3);
        // Fields sorted: age, email, name
        expect(decl.value.fields[0]?.name).toBe("age");
        expect(decl.value.fields[1]?.name).toBe("email");
        expect(decl.value.fields[2]?.name).toBe("name");
      }
    }
  });

  test("parses parameterized record type", () => {
    const program = parseTest(
      `type alias Container a = { value : a, count : number }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Container");
      expect(decl.params).toEqual(["a"]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        expect(decl.value.fields[0]?.name).toBe("count");
        expect(decl.value.fields[1]?.name).toBe("value");
        if (decl.value.fields[1]?.type.kind === "TypeRef") {
          expect(decl.value.fields[1].type.name).toBe("a");
        }
      }
    }
  });

  test("parses record type in function signature", () => {
    const program = parseTest(
      `distance : { x : number, y : number } -> number`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAnnotationDeclaration");
    if (decl?.kind === "TypeAnnotationDeclaration") {
      expect(decl.annotation.kind).toBe("FunctionType");
      if (decl.annotation.kind === "FunctionType") {
        expect(decl.annotation.from.kind).toBe("RecordType");
        if (decl.annotation.from.kind === "RecordType") {
          expect(decl.annotation.from.fields.length).toBe(2);
        }
      }
    }
  });

  test("parses nested record types", () => {
    const program = parseTest(
      `type alias Nested = { outer : { inner : number } }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(1);
        expect(decl.value.fields[0]?.name).toBe("outer");
        expect(decl.value.fields[0]?.type.kind).toBe("RecordType");
      }
    }
  });

  test("parses record type alias with multiple type parameters", () => {
    const program = parseTest(
      `type alias Pair a b = { first : a, second : b }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Pair");
      expect(decl.params).toEqual(["a", "b"]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        // Fields sorted: first, second
        expect(decl.value.fields[0]?.name).toBe("first");
        expect(decl.value.fields[0]?.type.kind).toBe("TypeRef");
        if (decl.value.fields[0]?.type.kind === "TypeRef") {
          expect(decl.value.fields[0].type.name).toBe("a");
        }
        expect(decl.value.fields[1]?.name).toBe("second");
        expect(decl.value.fields[1]?.type.kind).toBe("TypeRef");
        if (decl.value.fields[1]?.type.kind === "TypeRef") {
          expect(decl.value.fields[1].type.name).toBe("b");
        }
      }
    }
  });

  test("parses record type with parameterized field types", () => {
    const program = parseTest(
      `type alias ListContainer a = { items : List a, size : number }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("ListContainer");
      expect(decl.params).toEqual(["a"]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        // Fields sorted: items, size
        expect(decl.value.fields[0]?.name).toBe("items");
        expect(decl.value.fields[0]?.type.kind).toBe("TypeRef");
        if (decl.value.fields[0]?.type.kind === "TypeRef") {
          expect(decl.value.fields[0].type.name).toBe("List");
          expect(decl.value.fields[0].type.args.length).toBe(1);
        }
      }
    }
  });

  test("parses record type with nested record type parameter", () => {
    const program = parseTest(
      `type alias Response a = { data : a, metadata : { code : number, message : string } }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Response");
      expect(decl.params).toEqual(["a"]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        // Fields sorted: data, metadata
        expect(decl.value.fields[0]?.name).toBe("data");
        expect(decl.value.fields[1]?.name).toBe("metadata");
        expect(decl.value.fields[1]?.type.kind).toBe("RecordType");
        if (decl.value.fields[1]?.type.kind === "RecordType") {
          expect(decl.value.fields[1].type.fields.length).toBe(2);
        }
      }
    }
  });

  test("parses record type with function type field using parameters", () => {
    const program = parseTest(
      `type alias Handler a = { process : a -> string, callback : string -> a }`,
    );
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("Handler");
      expect(decl.params).toEqual(["a"]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(2);
        // Fields sorted: callback, process
        expect(decl.value.fields[0]?.name).toBe("callback");
        expect(decl.value.fields[0]?.type.kind).toBe("FunctionType");
        expect(decl.value.fields[1]?.name).toBe("process");
        expect(decl.value.fields[1]?.type.kind).toBe("FunctionType");
      }
    }
  });

  test("parses multiline record type with multiple parameters", () => {
    const program = parseTest(`type alias State a b =
  { value : a
  , next : b
  , count : number
  }`);
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeAliasDeclaration");
    if (decl?.kind === "TypeAliasDeclaration") {
      expect(decl.name).toBe("State");
      expect(decl.params).toEqual(["a", "b"]);
      expect(decl.value.kind).toBe("RecordType");
      if (decl.value.kind === "RecordType") {
        expect(decl.value.fields.length).toBe(3);
        // Fields sorted: count, next, value
        expect(decl.value.fields[0]?.name).toBe("count");
        expect(decl.value.fields[1]?.name).toBe("next");
        expect(decl.value.fields[2]?.name).toBe("value");
      }
    }
  });
});

describe("infix declarations", () => {
  test("parses infixl declaration with bare operator", () => {
    const program = parseTest("infixl 6 +");
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("InfixDeclaration");
    if (decl?.kind === "InfixDeclaration") {
      expect(decl.fixity).toBe("infixl");
      expect(decl.precedence).toBe(6);
      expect(decl.operator).toBe("+");
    }
  });

  test("parses infixr declaration with parenthesized operator", () => {
    const program = parseTest("infixr 5 (++)");
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("InfixDeclaration");
    if (decl?.kind === "InfixDeclaration") {
      expect(decl.fixity).toBe("infixr");
      expect(decl.precedence).toBe(5);
      expect(decl.operator).toBe("++");
    }
  });

  test("parses infix (non-associative) declaration", () => {
    const program = parseTest("infix 4 ==");
    expect(program.declarations.length).toBe(1);
    const decl = program.declarations[0];
    expect(decl?.kind).toBe("InfixDeclaration");
    if (decl?.kind === "InfixDeclaration") {
      expect(decl.fixity).toBe("infix");
      expect(decl.precedence).toBe(4);
      expect(decl.operator).toBe("==");
    }
  });

  test("parses multiple infix declarations", () => {
    const source = `infixl 6 +
infixl 6 -
infixl 7 *
infixr 5 ++`;
    const program = parseTest(source);
    expect(program.declarations.length).toBe(4);

    const ops = program.declarations.map((d) =>
      d.kind === "InfixDeclaration" ? d.operator : null,
    );
    expect(ops).toEqual(["+", "-", "*", "++"]);
  });

  test("parses infix declarations mixed with value declarations", () => {
    const source = `infixl 6 +

add : number -> number -> number
add x y = x + y`;
    const program = parseTest(source);
    expect(program.declarations.length).toBe(3);
    expect(program.declarations[0]?.kind).toBe("InfixDeclaration");
    expect(program.declarations[1]?.kind).toBe("TypeAnnotationDeclaration");
    expect(program.declarations[2]?.kind).toBe("ValueDeclaration");
  });

  test("parses custom operator declarations", () => {
    const source = `infixl 1 |>

(|>) : a -> (a -> b) -> b
(|>) x f = f x`;
    const program = parseTest(source);
    expect(program.declarations.length).toBe(3);

    const infixDecl = program.declarations[0];
    expect(infixDecl?.kind).toBe("InfixDeclaration");
    if (infixDecl?.kind === "InfixDeclaration") {
      expect(infixDecl.operator).toBe("|>");
    }

    const typeDecl = program.declarations[1];
    expect(typeDecl?.kind).toBe("TypeAnnotationDeclaration");
    if (typeDecl?.kind === "TypeAnnotationDeclaration") {
      expect(typeDecl.name).toBe("|>");
    }

    const valueDecl = program.declarations[2];
    expect(valueDecl?.kind).toBe("ValueDeclaration");
    if (valueDecl?.kind === "ValueDeclaration") {
      expect(valueDecl.name).toBe("|>");
    }
  });
});

describe("collectInfixDeclarations", () => {
  test("collects infix declarations from source", () => {
    const source = `infixl 6 +
infixr 5 ++
infix 4 ==`;
    const { registry, declarations, errors } = collectInfixDeclarations(source);

    expect(errors.length).toBe(0);
    expect(declarations.length).toBe(3);
    expect(registry.size).toBe(3);

    expect(registry.get("+")).toEqual({ precedence: 6, associativity: "left" });
    expect(registry.get("++")).toEqual({
      precedence: 5,
      associativity: "right",
    });
    expect(registry.get("==")).toEqual({
      precedence: 4,
      associativity: "none",
    });
  });

  test("reports duplicate infix declarations", () => {
    const source = `infixl 6 +
infixl 7 +`;
    const { errors } = collectInfixDeclarations(source);

    expect(errors.length).toBe(1);
    expect(errors[0]?.message).toContain("Duplicate");
  });

  test("ignores non-infix content", () => {
    const source = `infixl 6 +
foo : number -> number
foo x = x + 1`;
    const { registry, errors } = collectInfixDeclarations(source);

    expect(errors.length).toBe(0);
    expect(registry.size).toBe(1);
    expect(registry.has("+")).toBe(true);
  });
});

describe("parseWithInfix", () => {
  test("parses with custom operator precedence", () => {
    const source = `module Test exposing (..)

infixl 7 <*>
infixl 6 <+>

result = a <+> b <*> c`;

    const { program, operatorRegistry, infixErrors } = parseWithInfix(source);

    expect(infixErrors.length).toBe(0);
    expect(operatorRegistry.get("<*>")).toEqual({
      precedence: 7,
      associativity: "left",
    });
    expect(operatorRegistry.get("<+>")).toEqual({
      precedence: 6,
      associativity: "left",
    });

    // With <*> at precedence 7 and <+> at 6, expression parses as: a <+> (b <*> c)
    const valueDecl = program.declarations.find(
      (d) => d.kind === "ValueDeclaration",
    );
    expect(valueDecl?.kind).toBe("ValueDeclaration");
    if (valueDecl?.kind === "ValueDeclaration") {
      const expr = valueDecl.body;
      expect(expr.kind).toBe("Infix");
      if (expr.kind === "Infix") {
        expect(expr.operator).toBe("<+>");
        expect(expr.right.kind).toBe("Infix");
        if (expr.right.kind === "Infix") {
          expect(expr.right.operator).toBe("<*>");
        }
      }
    }
  });
});

describe("operator protocol methods", () => {
  test("parses protocol with operator methods", () => {
    const source = `protocol Eq a where
  (==) : a -> a -> bool
  (/=) : a -> a -> bool`;

    const program = parseTest(source);
    expect(program.declarations.length).toBe(1);

    const protocol = program.declarations[0];
    expect(protocol?.kind).toBe("ProtocolDeclaration");
    if (protocol?.kind === "ProtocolDeclaration") {
      expect(protocol.name).toBe("Eq");
      expect(protocol.params).toEqual(["a"]);
      expect(protocol.methods.length).toBe(2);
      expect(protocol.methods[0]?.name).toBe("==");
      expect(protocol.methods[1]?.name).toBe("/=");
    }
  });

  test("parses protocol with mixed identifier and operator methods", () => {
    const source = `protocol Num a where
  (+) : a -> a -> a
  (-) : a -> a -> a
  negate : a -> a`;

    const program = parseTest(source);
    const protocol = program.declarations[0];
    expect(protocol?.kind).toBe("ProtocolDeclaration");
    if (protocol?.kind === "ProtocolDeclaration") {
      expect(protocol.methods.length).toBe(3);
      expect(protocol.methods[0]?.name).toBe("+");
      expect(protocol.methods[1]?.name).toBe("-");
      expect(protocol.methods[2]?.name).toBe("negate");
    }
  });

  test("parses implementation with operator methods", () => {
    const source = `implement Eq Int where
  (==) = intEq
  (/=) = intNeq`;

    const program = parseTest(source);
    expect(program.declarations.length).toBe(1);

    const impl = program.declarations[0];
    expect(impl?.kind).toBe("ImplementationDeclaration");
    if (impl?.kind === "ImplementationDeclaration") {
      expect(impl.protocolName).toBe("Eq");
      expect(impl.methods.length).toBe(2);
      expect(impl.methods[0]?.name).toBe("==");
      expect(impl.methods[1]?.name).toBe("/=");
    }
  });
});

describe("module export syntax", () => {
  test("parses exposing all (..)", () => {
    const source = `module Main exposing (..)`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("All");
  });

  test("parses simple value exports", () => {
    const source = `module Main exposing (foo, bar, baz)`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      expect(program.module.exposing.exports.length).toBe(3);
      expect(program.module.exposing.exports[0]?.kind).toBe("ExportValue");
      expect(
        program.module.exposing.exports[0]?.kind === "ExportValue" &&
          program.module.exposing.exports[0].name,
      ).toBe("foo");
    }
  });

  test("parses operator exports", () => {
    const source = `module Main exposing ((++), (<$>), (|>))`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      const exports = program.module.exposing.exports;
      expect(exports.length).toBe(3);
      expect(exports[0]?.kind).toBe("ExportOperator");
      if (exports[0]?.kind === "ExportOperator") {
        expect(exports[0].operator).toBe("++");
      }
      if (exports[1]?.kind === "ExportOperator") {
        expect(exports[1].operator).toBe("<$>");
      }
      if (exports[2]?.kind === "ExportOperator") {
        expect(exports[2].operator).toBe("|>");
      }
    }
  });

  test("parses type export with all constructors", () => {
    const source = `module Main exposing (Maybe(..), Result(..))`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      const exports = program.module.exposing.exports;
      expect(exports.length).toBe(2);
      expect(exports[0]?.kind).toBe("ExportTypeAll");
      if (exports[0]?.kind === "ExportTypeAll") {
        expect(exports[0].name).toBe("Maybe");
      }
      if (exports[1]?.kind === "ExportTypeAll") {
        expect(exports[1].name).toBe("Result");
      }
    }
  });

  test("parses type export with specific constructors", () => {
    const source = `module Main exposing (Result(Ok, Err), Bool(True, False))`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      const exports = program.module.exposing.exports;
      expect(exports.length).toBe(2);
      expect(exports[0]?.kind).toBe("ExportTypeSome");
      if (exports[0]?.kind === "ExportTypeSome") {
        expect(exports[0].name).toBe("Result");
        expect(exports[0].members).toEqual(["Ok", "Err"]);
      }
      if (exports[1]?.kind === "ExportTypeSome") {
        expect(exports[1].name).toBe("Bool");
        expect(exports[1].members).toEqual(["True", "False"]);
      }
    }
  });

  test("parses protocol export with all methods", () => {
    const source = `module Main exposing (Num(..), Eq(..))`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      const exports = program.module.exposing.exports;
      expect(exports.length).toBe(2);
      // Note: Parser doesn't distinguish between types and protocols
      // Both use ExportTypeAll syntax - semantics differentiates
      expect(exports[0]?.kind).toBe("ExportTypeAll");
    }
  });

  test("parses protocol export with specific methods", () => {
    const source = `module Main exposing (Num((+), (-)), Eq(eq, neq))`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      const exports = program.module.exposing.exports;
      expect(exports.length).toBe(2);
      expect(exports[0]?.kind).toBe("ExportTypeSome");
      if (exports[0]?.kind === "ExportTypeSome") {
        expect(exports[0].name).toBe("Num");
        expect(exports[0].members).toEqual(["+", "-"]);
      }
      if (exports[1]?.kind === "ExportTypeSome") {
        expect(exports[1].name).toBe("Eq");
        expect(exports[1].members).toEqual(["eq", "neq"]);
      }
    }
  });

  test("parses mixed exports", () => {
    const source = `module Collections exposing 
      ( List(..)
      , Maybe(Just, Nothing)
      , Unit
      , map, filter
      , (++), (<|)
      , Functor(..)
      )`;
    const program = parseTest(source);
    expect(program.module?.exposing?.kind).toBe("Explicit");
    if (program.module?.exposing?.kind === "Explicit") {
      const exports = program.module.exposing.exports;
      expect(exports.length).toBe(8);

      // List(..)
      expect(exports[0]?.kind).toBe("ExportTypeAll");

      // Maybe(Just, Nothing)
      expect(exports[1]?.kind).toBe("ExportTypeSome");
      if (exports[1]?.kind === "ExportTypeSome") {
        expect(exports[1].name).toBe("Maybe");
        expect(exports[1].members).toEqual(["Just", "Nothing"]);
      }

      // Unit (just the type name)
      expect(exports[2]?.kind).toBe("ExportValue");
      if (exports[2]?.kind === "ExportValue") {
        expect(exports[2].name).toBe("Unit");
      }

      // map, filter
      expect(exports[3]?.kind).toBe("ExportValue");
      expect(exports[4]?.kind).toBe("ExportValue");

      // (++), (<|)
      expect(exports[5]?.kind).toBe("ExportOperator");
      expect(exports[6]?.kind).toBe("ExportOperator");

      // Functor(..)
      expect(exports[7]?.kind).toBe("ExportTypeAll");
    }
  });

  test("parses import with explicit export specs", () => {
    const source = `import Html exposing (div, span, text)`;
    const program = parseTest(source);
    expect(program.imports[0]?.exposing?.kind).toBe("Explicit");
    if (program.imports[0]?.exposing?.kind === "Explicit") {
      const exports = program.imports[0].exposing.exports;
      expect(exports.length).toBe(3);
      expect(exports[0]?.kind).toBe("ExportValue");
    }
  });

  test("parses import with type and constructor specs", () => {
    const source = `import Data.Maybe exposing (Maybe(..))`;
    const program = parseTest(source);
    expect(program.imports[0]?.exposing?.kind).toBe("Explicit");
    if (program.imports[0]?.exposing?.kind === "Explicit") {
      const exports = program.imports[0].exposing.exports;
      expect(exports.length).toBe(1);
      expect(exports[0]?.kind).toBe("ExportTypeAll");
      if (exports[0]?.kind === "ExportTypeAll") {
        expect(exports[0].name).toBe("Maybe");
      }
    }
  });

  // ===== Unary Negation Tests =====

  test("parses unary negation of variable", () => {
    const program = parseTest("y = -x");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Unary");
    if (decl.body.kind === "Unary") {
      expect(decl.body.operator).toBe("-");
      expect(decl.body.operand.kind).toBe("Var");
      if (decl.body.operand.kind === "Var") {
        expect(decl.body.operand.name).toBe("x");
      }
    }
  });

  test("parses unary negation of number literal", () => {
    const program = parseTest("y = -10");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Unary");
    if (decl.body.kind === "Unary") {
      expect(decl.body.operator).toBe("-");
      expect(decl.body.operand.kind).toBe("Number");
      if (decl.body.operand.kind === "Number") {
        expect(decl.body.operand.value).toBe("10");
      }
    }
  });

  test("parses unary negation of parenthesized expression", () => {
    const program = parseTest("y = -(x + 1)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Unary");
    if (decl.body.kind === "Unary") {
      expect(decl.body.operator).toBe("-");
      expect(decl.body.operand.kind).toBe("Paren");
      if (decl.body.operand.kind === "Paren") {
        expect(decl.body.operand.expression.kind).toBe("Infix");
      }
    }
  });

  test("parses double negation with grouping", () => {
    const program = parseTest("y = -(-10)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Unary");
    if (decl.body.kind === "Unary") {
      expect(decl.body.operator).toBe("-");
      expect(decl.body.operand.kind).toBe("Paren");
      if (decl.body.operand.kind === "Paren") {
        expect(decl.body.operand.expression.kind).toBe("Unary");
      }
    }
  });

  test("parses unary negation in binary expression", () => {
    const program = parseTest("y = 5 + -x");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Infix");
    if (decl.body.kind === "Infix") {
      expect(decl.body.operator).toBe("+");
      expect(decl.body.right.kind).toBe("Unary");
      if (decl.body.right.kind === "Unary") {
        expect(decl.body.right.operand.kind).toBe("Var");
      }
    }
  });

  test("parses unary negation with higher precedence than binary", () => {
    const program = parseTest("y = -x * 2");
    const decl = program.declarations[0] as ValueDeclaration;
    // -x * 2 should parse as (-x) * 2
    expect(decl.body.kind).toBe("Infix");
    if (decl.body.kind === "Infix") {
      expect(decl.body.operator).toBe("*");
      expect(decl.body.left.kind).toBe("Unary");
    }
  });

  test("binary minus is not parsed as unary", () => {
    const program = parseTest("y = x - 1");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Infix");
    if (decl.body.kind === "Infix") {
      expect(decl.body.operator).toBe("-");
      expect(decl.body.left.kind).toBe("Var");
      expect(decl.body.right.kind).toBe("Number");
    }
  });

  test("parses negation of constructor", () => {
    const program = parseTest("y = -SomeValue");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Unary");
    if (decl.body.kind === "Unary") {
      expect(decl.body.operand.kind).toBe("Var");
      if (decl.body.operand.kind === "Var") {
        expect(decl.body.operand.namespace).toBe("upper");
      }
    }
  });
});

describe("prefix operator syntax", () => {
  test("parses basic operator in parentheses", () => {
    const program = parseTest("add = (+)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Var");
    if (decl.body.kind === "Var") {
      expect(decl.body.name).toBe("+");
      expect(decl.body.namespace).toBe("operator");
    }
  });

  test("parses multi-character operator in parentheses", () => {
    const program = parseTest("eq = (==)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Var");
    if (decl.body.kind === "Var") {
      expect(decl.body.name).toBe("==");
      expect(decl.body.namespace).toBe("operator");
    }
  });

  test("parses custom operator in parentheses", () => {
    const program = parseTest("pipe = (|>)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Var");
    if (decl.body.kind === "Var") {
      expect(decl.body.name).toBe("|>");
      expect(decl.body.namespace).toBe("operator");
    }
  });

  test("parses prefix operator applied to arguments", () => {
    const program = parseTest("result = (+) 1 2");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Apply");
    if (decl.body.kind === "Apply") {
      expect(decl.body.callee.kind).toBe("Var");
      if (decl.body.callee.kind === "Var") {
        expect(decl.body.callee.name).toBe("+");
        expect(decl.body.callee.namespace).toBe("operator");
      }
      expect(decl.body.args.length).toBe(2);
    }
  });

  test("parses prefix operator with variable arguments", () => {
    const program = parseTest("result = (==) x y");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Apply");
    if (decl.body.kind === "Apply") {
      expect(decl.body.callee.kind).toBe("Var");
      if (decl.body.callee.kind === "Var") {
        expect(decl.body.callee.name).toBe("==");
        expect(decl.body.callee.namespace).toBe("operator");
      }
    }
  });

  test("parses prefix operator passed to higher-order function", () => {
    const program = parseTest("result = foldr (::) []");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Apply");
    if (decl.body.kind === "Apply") {
      expect(decl.body.args.length).toBe(2);
      const firstArg = decl.body.args[0];
      expect(firstArg?.kind).toBe("Var");
      if (firstArg?.kind === "Var") {
        expect(firstArg.name).toBe("::");
        expect(firstArg.namespace).toBe("operator");
      }
    }
  });

  test("parses logical operators in parentheses", () => {
    const program = parseTest("and = (&&)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Var");
    if (decl.body.kind === "Var") {
      expect(decl.body.name).toBe("&&");
      expect(decl.body.namespace).toBe("operator");
    }
  });

  test("parses list cons operator in parentheses", () => {
    const program = parseTest("cons = (::)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Var");
    if (decl.body.kind === "Var") {
      expect(decl.body.name).toBe("::");
      expect(decl.body.namespace).toBe("operator");
    }
  });

  test("parenthesized expression is not confused with prefix operator", () => {
    // (x) is a parenthesized variable, not a prefix operator
    const program = parseTest("result = (x)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Paren");
    if (decl.body.kind === "Paren") {
      expect(decl.body.expression.kind).toBe("Var");
      if (decl.body.expression.kind === "Var") {
        expect(decl.body.expression.name).toBe("x");
        expect(decl.body.expression.namespace).toBe("lower");
      }
    }
  });

  test("tuple is not confused with prefix operator", () => {
    // (a, b) is a tuple, not prefix operators
    const program = parseTest("result = (a, b)");
    const decl = program.declarations[0] as ValueDeclaration;
    expect(decl.body.kind).toBe("Tuple");
  });
});
