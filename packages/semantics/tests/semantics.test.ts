import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index.ts";

/**
 * Helper to expect an error during analysis.
 * Adds OPERATOR_PREAMBLE (which includes types and operators) so literals and operators work.
 * For sources starting with 'module', inserts OPERATOR_PREAMBLE after module/import declarations.
 * For sources starting with 'import' (without module), adds module declaration first.
 * For sources without module or import, adds "module Test exposing (..)" at the start.
 */
const expectError = (source: string, message: string) => {
  let fullSource = source;
  const trimmed = source.trim();

  if (trimmed.startsWith("module")) {
    // Has module declaration - insert OPERATOR_PREAMBLE after module/imports
    const lines = source.split("\n");
    let insertIndex = 0;
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      if (line.startsWith("module") || line.startsWith("import")) {
        insertIndex = i + 1;
      } else if (line) {
        // First non-import/module line
        break;
      }
    }
    fullSource =
      lines.slice(0, insertIndex).join("\n") +
      "\n" +
      OPERATOR_PREAMBLE +
      "\n" +
      lines.slice(insertIndex).join("\n");
  } else if (trimmed.startsWith("import")) {
    // Has imports but no module - add module declaration first
    fullSource =
      "module Test exposing (..)\n" + source + "\n" + OPERATOR_PREAMBLE;
  } else {
    // No module or imports - add module declaration
    fullSource =
      "module Test exposing (..)\n" + OPERATOR_PREAMBLE + "\n" + source;
  }
  expect(() => analyze(parse(fullSource))).toThrow(message);
};

/**
 * Helper to analyze code.
 * Adds TYPE_PREAMBLE automatically so literals work.
 * For sources starting with 'module', inserts TYPE_PREAMBLE after module/import declarations.
 * For sources starting with 'import' (without module), adds module declaration first.
 * For sources without module or import, adds "module Test exposing (..)" at the start.
 */
const analyzeNoPrelude = (source: string) => {
  let fullSource = source;
  const trimmed = source.trim();

  if (trimmed.startsWith("module")) {
    // Has module declaration - insert TYPE_PREAMBLE after module/imports
    const lines = source.split("\n");
    let insertIndex = 0;
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      if (line.startsWith("module") || line.startsWith("import")) {
        insertIndex = i + 1;
      } else if (line) {
        // First non-import/module line
        break;
      }
    }
    fullSource =
      lines.slice(0, insertIndex).join("\n") +
      "\n" +
      TYPE_PREAMBLE +
      "\n" +
      lines.slice(insertIndex).join("\n");
  } else if (trimmed.startsWith("import")) {
    // Has imports but no module - add module declaration first
    fullSource = "module Test exposing (..)\n" + source + "\n" + TYPE_PREAMBLE;
  } else {
    // No module or imports - add module declaration
    fullSource = "module Test exposing (..)\n" + TYPE_PREAMBLE + "\n" + source;
  }
  return analyze(parse(fullSource));
};

/**
 * Type definitions preamble for tests.
 * We need to define ADTs that are not builtin (Maybe in this case).
 * Bool, Unit, Int, Float, String, and Char are builtin to the compiler.
 */
const TYPE_PREAMBLE = `
type Maybe a = Just a | Nothing
`;

/**
 * Operator preamble for tests that need arithmetic/comparison operators.
 * This defines the operators locally since we don't have prelude.
 */
const OPERATOR_PREAMBLE = `
${TYPE_PREAMBLE}

infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infixr 5 ++

@external "@vibe/runtime" "intAdd"
(+) : Int -> Int -> Int

@external "@vibe/runtime" "intSub"
(-) : Int -> Int -> Int

@external "@vibe/runtime" "intMul"
(*) : Int -> Int -> Int

@external "@vibe/runtime" "intDiv"
(/) : Int -> Int -> Int

@external "@vibe/runtime" "append"
(++) : String -> String -> String
`;

describe("semantics", () => {
  test("attaches annotations to definitions", () => {
    const result = analyzeNoPrelude(`value : Int
value = 1`);
    expect(result.values.value).toBeDefined();
    expect(result.values.value?.annotation).toBeDefined();
  });

  test("rejects duplicate definitions", () => {
    expectError(
      `a = 1
a = 2`,
      "Duplicate definition",
    );
  });

  test("rejects orphan annotations", () => {
    expectError(`a : Int`, "has no matching definition");
  });

  test("registers external declarations with annotations", () => {
    const result = analyzeNoPrelude(`@external "./lib.js" "compute"
ffiCompute : Int -> Int`);
    expect(result.values.ffiCompute).toBeDefined();
    expect(result.values.ffiCompute?.externalTarget?.modulePath).toBe(
      "./lib.js",
    );
    expect(result.values.ffiCompute?.externalTarget?.exportName).toBe(
      "compute",
    );
    expect(result.values.ffiCompute?.annotation?.kind).toBe("FunctionType");
  });

  test("rejects undefined type in external declaration annotation", () => {
    // UndefinedType is not defined - should error
    expectError(
      `@external "./lib.js" "compute"
ffiCompute : UndefinedType -> Int`,
      "Type 'UndefinedType' is not defined",
    );
  });

  test("rejects undefined type in value annotation", () => {
    // UndefinedType is not defined - should error
    expectError(
      `foo : UndefinedType -> Int
foo x = 1`,
      "Type 'UndefinedType' is not defined",
    );
  });

  test("rejects undefined type argument in external declaration", () => {
    // List a is fine (a is type variable), but List Foo is not if Foo is undefined
    expectError(
      `@external "./lib.js" "compute"
ffiCompute : List Undefined -> Int`,
      "Type 'Undefined' is not defined",
    );
  });

  test("rejects undefined type argument in value annotation", () => {
    expectError(
      `foo : List Undefined -> Int
foo x = 1`,
      "Type 'Undefined' is not defined",
    );
  });

  test("allows type variables in external declaration annotation", () => {
    // Type variables (lowercase like 'a') should be allowed
    const result = analyzeNoPrelude(`@external "./lib.js" "identity"
ffiIdentity : a -> a`);
    expect(result.values.ffiIdentity).toBeDefined();
  });

  test("allows defined types in external declaration annotation", () => {
    // Maybe is defined in TYPE_PREAMBLE, so this should work
    const result = analyzeNoPrelude(`@external "./lib.js" "compute"
ffiCompute : Maybe Int -> Int`);
    expect(result.values.ffiCompute).toBeDefined();
  });

  test("rejects extra annotations for externals", () => {
    expectError(
      `@external "./lib.js" "compute"
ffiCompute : Int -> Int
ffiCompute : Int -> Int`,
      "already includes a type annotation",
    );
  });

  test("rejects module exposing missing value", () => {
    expectError(
      `module Main exposing (foo)
bar = 1`,
      "Module exposes 'foo'",
    );
  });

  test("accepts module exposing existing value", () => {
    const result = analyzeNoPrelude(`module Main exposing (bar)
bar = 1`);
    expect(result.module?.name).toBe("Main");
  });

  test("rejects duplicate imports of the same module", () => {
    expectError(
      `import Foo
import Foo`,
      "Duplicate import of module 'Foo'",
    );
  });

  test("rejects duplicate import aliases", () => {
    expectError(
      `import Foo as A
import Bar as A`,
      "Duplicate import alias 'A'",
    );
  });

  test("rejects undefined variable references", () => {
    expectError(`a = b`, "Undefined name 'b'");
  });

  test("rejects duplicate lambda args", () => {
    expectError(`f x x = x`, "Duplicate pattern variable 'x'");
  });

  test("rejects annotation arity mismatches", () => {
    // When annotation has fewer params than declaration, the type system
    // catches the mismatch during unification (e.g., 't1 -> Int' vs 'Int')
    expectError(
      `f : Int
f x = x`,
      "Type mismatch",
    );
  });

  test("rejects duplicate let bindings", () => {
    expectError(
      `f =
  let
    x = 1
    x = 2
  in
    x`,
      "Duplicate let-binding 'x'",
    );
  });

  test("rejects non-exhaustive case on Bool", () => {
    expectError(
      `f x =
  case x of
    True -> 1`,
      "Non-exhaustive case expression",
    );
  });

  test("rejects constructor arity mismatch", () => {
    expectError(
      `f x =
  case x of
    Just -> 1`,
      "Constructor 'Just' expects 1 argument(s)",
    );
  });

  test("rejects wildcard before later branches", () => {
    expectError(
      `f x =
  case x of
    _ -> 1
    True -> 2`,
      "Wildcard pattern makes following branches unreachable",
    );
  });

  test("rejects record field duplication", () => {
    expectError(`f = { a = 1, a = 2 }`, "Duplicate record field 'a'");
  });

  test("rejects type mismatches in infix usage", () => {
    expectError(`f = 1 + "hi"`, "Type mismatch");
  });

  test("infers simple identity function", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
id x = x`);
    const result = analyze(program);
    expect(result.types.id).toBeDefined();
  });

  // ===== Let-Polymorphism Tests =====
  // These tests verify that the Hindley-Milner type system properly
  // generalizes and instantiates polymorphic types

  test("polymorphic identity function can be used at multiple types", () => {
    // The identity function 'id x = x' should be inferred as polymorphic: forall a. a -> a
    // This allows it to be used at both number and string types
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
id x = x
useAtNumber = id 42
useAtString = id "hello"`);
    const result = analyze(program);

    // All three bindings should infer successfully
    expect(result.types.id).toBeDefined();
    expect(result.types.useAtNumber).toBeDefined();
    expect(result.types.useAtString).toBeDefined();

    // The uses should not interfere with each other
    // (without polymorphism, the first use would fix 'id' to number -> Int)
  });

  test("polymorphic function in let-binding", () => {
    // Let-bound polymorphic functions should be instantiated freshly at each use
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
f =
  let
    id x = x
  in
    (id 1, id "hi")`);
    const result = analyze(program);
    expect(result.types.f).toBeDefined();
  });

  test("nested let-polymorphism", () => {
    // Test that nested let-bindings properly generalize
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
f =
  let
    id x = x
    const y x = y
  in
    let
      a = id 42
      b = id "test"
      c = const 1 "ignored"
    in
      (a, b, c)`);
    const result = analyze(program);
    expect(result.types.f).toBeDefined();
  });

  test("polymorphic const function", () => {
    // const : forall a b. a -> b -> a
    // Returns first argument, ignoring second
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
const x y = x
n = const 1 "ignored"
s = const "hi" 42`);
    const result = analyze(program);
    expect(result.types.const).toBeDefined();
    expect(result.types.n).toBeDefined();
    expect(result.types.s).toBeDefined();
  });

  test("polymorphic compose function", () => {
    // compose : forall a b c. (b -> c) -> (a -> b) -> a -> c
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
compose f g x = f (g x)
addOne n = n + 1
double n = n * 2
addThree = compose addOne double`);
    const result = analyze(program);
    expect(result.types.compose).toBeDefined();
    expect(result.types.addThree).toBeDefined();
  });

  test("polymorphic pair constructor and accessors", () => {
    // Note: Tuple pattern matching works because tuples are built-in syntax
    // This is different from user-defined ADTs which we'll need later
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
pair x y = (x, y)
fst p =
  case p of
    (x, y) -> x
snd p =
  case p of
    (x, y) -> y

p1 = pair 1 "hi"
p2 = pair 2 ()
n = fst p1
s = snd p1`);
    const result = analyze(program);
    expect(result.types.pair).toBeDefined();
    expect(result.types.fst).toBeDefined();
    expect(result.types.snd).toBeDefined();
    expect(result.types.p1).toBeDefined();
    expect(result.types.p2).toBeDefined();
  });

  test("polymorphic list functions", () => {
    // Test polymorphism with list types
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
singleton x = [x]
nums = singleton 42
strs = singleton "hi"`);
    const result = analyze(program);
    expect(result.types.singleton).toBeDefined();
    expect(result.types.nums).toBeDefined();
    expect(result.types.strs).toBeDefined();
  });

  test("list pattern matching - empty and cons patterns", () => {
    // Basic list pattern matching without polymorphism
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
isEmpty xs =
  case xs of
    [] -> True
    (x :: rest) -> False
`);
    const result = analyze(program);
    expect(result.types.isEmpty).toBeDefined();
  });

  test("polymorphic recursion - length function", () => {
    // length : forall a. [a] -> Int
    // Polymorphic recursive functions should work
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
length xs =
  case xs of
    [] -> 0
    (x :: rest) -> 1 + length rest
`);
    const result = analyze(program);
    expect(result.types.length).toBeDefined();
  });

  test("mutual recursion - even/odd", () => {
    // Mutually recursive functions should be typed correctly
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
isEven n =
  if n == 0 then True else isOdd (n - 1)

isOdd n =
  if n == 0 then False else isEven (n - 1)

infixl 4 ==
@external "@vibe/runtime" "intEq"
(==) : Int -> Int -> Bool
`);
    const result = analyze(program);
    expect(result.types.isEven).toBeDefined();
    expect(result.types.isOdd).toBeDefined();
  });

  test("lambda parameters are monomorphic", () => {
    // Lambda-bound variables should NOT be polymorphic
    // This should fail because 'x' is used at both number and string types
    // within the same lambda scope
    expectError(`f = \\x -> (x + 1, x ++ "hi")`, "Type mismatch");
  });

  test("function parameters are monomorphic", () => {
    // Function parameters should NOT be polymorphic within the function body
    expectError(`f x = (x + 1, x ++ "hi")`, "Type mismatch");
  });

  test("polymorphic function with type annotation", () => {
    // Type variables in annotations: lowercase identifiers like 'a', 'b'
    // should be recognized as polymorphic type variables
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
apply : (a -> b) -> a -> b
apply f x = f x

n = apply (\\x -> x + 1) 42
s = apply (\\x -> x ++ "!") "hi"`);
    const result = analyze(program);
    expect(result.types.apply).toBeDefined();
    expect(result.types.n).toBeDefined();
    expect(result.types.s).toBeDefined();
  });

  test("type annotations with single-letter type variables", () => {
    // Single lowercase letters (a, b, c, etc.) are type variables
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
id : a -> a
id x = x

const : a -> b -> a
const x y = x

flip : (a -> b -> c) -> b -> a -> c
flip f y x = f x y`);
    const result = analyze(program);
    expect(result.types.id).toBeDefined();
    expect(result.types.const).toBeDefined();
    expect(result.types.flip).toBeDefined();
  });

  test("type annotations distinguish type variables from concrete types", () => {
    // Single letters = type variables, known words = concrete types
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
toInt : a -> Int
toInt x = 42

toStr : a -> String
toStr x = "hi"`);
    const result = analyze(program);
    expect(result.types.toInt).toBeDefined();
    expect(result.types.toStr).toBeDefined();
  });

  test("type annotations with List type constructor", () => {
    // List is a type constructor that takes a type argument
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
head : List a -> a
head xs = 42

length : List a -> Int
length xs = 0`);
    const result = analyze(program);
    expect(result.types.head).toBeDefined();
    expect(result.types.length).toBeDefined();
  });

  test("List type in various contexts", () => {
    // Test List as builtin ADT in type annotations, type aliases, and literals
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
-- Direct List type annotation
nums : List Int
nums = [1, 2, 3]

-- List in type alias
type alias IntList = List Int

myList : IntList
myList = [42]

-- List with type parameter in type alias
type alias Container a = List a

container : Container String
container = ["hello", "world"]

-- Nested List
nested : List (List Int)
nested = [[1, 2], [3, 4]]`);
    const result = analyze(program);
    expect(result.types.nums).toBeDefined();
    expect(result.types.myList).toBeDefined();
    expect(result.types.container).toBeDefined();
    expect(result.types.nested).toBeDefined();
    expect(result.typeAliases.IntList).toBeDefined();
    expect(result.typeAliases.Container).toBeDefined();
  });

  test("complex nested type annotations", () => {
    // Test deeply nested function types with type variables
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
compose : (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

twice : (a -> a) -> a -> a
twice f x = f (f x)`);
    const result = analyze(program);
    expect(result.types.compose).toBeDefined();
    expect(result.types.twice).toBeDefined();
  });

  test("type annotation must match implementation", () => {
    // If annotation says number -> Int, using it polymorphically should fail
    expectError(
      `f : Int -> Int
f x = x
n = f 42
s = f "hi"`,
      "Type mismatch",
    );
  });

  test("generalization respects scope", () => {
    // Variables free in the outer scope should not be generalized
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
outer =
  let
    x = 42
    f y = x + y
  in
    f 1`);
    const result = analyze(program);
    expect(result.types.outer).toBeDefined();
  });

  // ===== Algebraic Data Type (ADT) Tests =====

  test("registers ADT declaration", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type MyBool = MyTrue | MyFalse`);
    const result = analyze(program);

    // Check ADT is registered
    expect(result.adts.MyBool).toBeDefined();
    expect(result.adts.MyBool?.params).toEqual([]);
    expect(result.adts.MyBool?.constructors).toEqual(["MyTrue", "MyFalse"]);

    // Check constructors are registered
    expect(result.constructors.MyTrue).toBeDefined();
    expect(result.constructors.MyTrue?.arity).toBe(0);
    expect(result.constructors.MyTrue?.parentType).toBe("MyBool");

    expect(result.constructors.MyFalse).toBeDefined();
    expect(result.constructors.MyFalse?.arity).toBe(0);
  });

  test("registers parameterized ADT", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Option a = Some a | None`);
    const result = analyze(program);

    expect(result.adts.Option).toBeDefined();
    expect(result.adts.Option?.params).toEqual(["a"]);
    expect(result.adts.Option?.constructors).toEqual(["Some", "None"]);

    expect(result.constructors.Some?.arity).toBe(1);
    expect(result.constructors.None?.arity).toBe(0);
  });

  test("uses ADT constructors as values", () => {
    // Constructors should be usable as values with proper types
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Option a = Some a | None

wrapped = Some 42
nothing = None`);
    const result = analyze(program);

    expect(result.types.wrapped).toBeDefined();
    expect(result.types.nothing).toBeDefined();
  });

  test("pattern matches on user-defined ADT", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Option a = Some a | None

unwrap opt default =
  case opt of
    Some x -> x
    None -> default`);
    const result = analyze(program);

    expect(result.types.unwrap).toBeDefined();
  });

  test("ADT exhaustiveness checking - complete", () => {
    // Should pass - all constructors covered
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Color = Red | Green | Blue

describe color =
  case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"`);
    const result = analyze(program);
    expect(result.types.describe).toBeDefined();
  });

  test("ADT exhaustiveness checking - incomplete", () => {
    // Should fail - missing Green
    expectError(
      `type Color = Red | Green | Blue

describe color =
  case color of
    Red -> "red"
    Blue -> "blue"`,
      "Non-exhaustive case expression",
    );
  });

  test("ADT exhaustiveness checking - reports missing constructors", () => {
    // The error message should include the missing constructor
    expect(() =>
      analyze(
        parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Status = Pending | Running | Completed | Failed

process status =
  case status of
    Pending -> "waiting"
    Completed -> "done"`),
      ),
    ).toThrow(/missing.*Running.*Failed|Non-exhaustive/);
  });

  test("rejects duplicate type declaration", () => {
    expectError(
      `type MyBool = MyTrue | MyFalse
type MyBool = MyYes | MyNo`,
      "Duplicate type declaration",
    );
  });

  test("rejects duplicate constructor within module", () => {
    expectError(
      `type A = Ctor | Other
type B = Ctor | Another`,
      "Duplicate constructor 'Ctor'",
    );
  });

  test("rejects duplicate type parameter", () => {
    expectError(`type Bad a a = Ctor a`, "Duplicate type parameter 'a'");
  });

  test("validates constructor arity in patterns", () => {
    expectError(
      `type Option a = Some a | None

f opt =
  case opt of
    Some -> 1
    None -> 0`,
      "expects 1 argument",
    );
  });

  test("validates constructor arity - too many args", () => {
    expectError(
      `type Option a = Some a | None

f opt =
  case opt of
    Some x y -> 1
    None -> 0`,
      "expects 1 argument",
    );
  });

  test("recursive ADT - MyList", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type MyList a = Cons a (MyList a) | Nil

empty = Nil
single = Cons 1 Nil
double = Cons 1 (Cons 2 Nil)`);
    const result = analyze(program);

    expect(result.adts.MyList).toBeDefined();
    expect(result.constructors.Cons?.arity).toBe(2);
    expect(result.constructors.Nil?.arity).toBe(0);
    expect(result.types.empty).toBeDefined();
    expect(result.types.single).toBeDefined();
    expect(result.types.double).toBeDefined();
  });

  test("recursive ADT pattern matching", () => {
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
type MyList a = Cons a (MyList a) | Nil

length xs =
  case xs of
    Nil -> 0
    Cons _ tail -> 1 + length tail`);
    const result = analyze(program);
    expect(result.types.length).toBeDefined();
  });

  test("polymorphic ADT map function", () => {
    // This is the test that was previously skipped
    const program = parse(`module Test exposing (..)

${OPERATOR_PREAMBLE}
type Option a = Some a | None

map f opt =
  case opt of
    None -> None
    Some x -> Some (f x)

addOne n = n + 1
result = map addOne (Some 42)`);
    const result = analyze(program);
    expect(result.types.map).toBeDefined();
    expect(result.types.result).toBeDefined();
  });

  test("ADT with multiple type parameters", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Either a b = Left a | Right b

example1 = Left 42
example2 = Right "hello"

getRight e default =
  case e of
    Left _ -> default
    Right x -> x`);
    const result = analyze(program);
    expect(result.adts.Either?.params).toEqual(["a", "b"]);
    expect(result.types.example1).toBeDefined();
    expect(result.types.example2).toBeDefined();
    expect(result.types.getRight).toBeDefined();
  });

  // ===== Type Alias Tests =====

  test("registers type alias", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type alias UserId = Int`);
    const result = analyze(program);

    expect(result.typeAliases.UserId).toBeDefined();
    expect(result.typeAliases.UserId?.params).toEqual([]);
  });

  test("registers parameterized type alias", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type alias Pair a b = (a, b)`);
    const result = analyze(program);

    expect(result.typeAliases.Pair).toBeDefined();
    expect(result.typeAliases.Pair?.params).toEqual(["a", "b"]);
  });

  test("rejects duplicate type alias", () => {
    expectError(
      `type alias UserId = Int
type alias UserId = String`,
      "Duplicate type alias",
    );
  });

  test("rejects duplicate type parameter in alias", () => {
    expectError(`type alias Bad a a = (a, a)`, "Duplicate type parameter 'a'");
  });

  // ===== Record Type Declaration Tests =====

  test("type declaration with record type", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Point = { x : Int, y : Int }`);
    const result = analyze(program);

    expect(result.records.Point).toBeDefined();
    expect(result.records.Point?.params).toEqual([]);
    expect(result.records.Point?.fields.length).toBe(2);
  });

  test("rejects type alias with bare record type", () => {
    expectError(
      `type alias Point = { x : Int, y : Int }`,
      "Type alias 'Point' cannot directly define a record type",
    );
  });

  test("rejects floating record type in function annotation", () => {
    expectError(
      `distance : { x : Int, y : Int } -> Int
distance point = point.x + point.y`,
      "Record types cannot be used directly in type annotations",
    );
  });

  test("record type fields are type-checked correctly", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Point = { x : Int, y : Int }

origin : Point
origin = { x = 0, y = 0 }`);

    // Should not throw - record literal matches type
    const result = analyze(program);
    expect(result.types.origin).toBeDefined();
  });

  test("empty record type", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Empty = {}

empty : Empty
empty = {}`);
    const result = analyze(program);

    expect(result.records.Empty).toBeDefined();
    expect(result.types.empty).toBeDefined();
  });

  test("parameterized record type", () => {
    const program = parse(
      `module Test exposing (..)

${TYPE_PREAMBLE}
type Container a = { value : a, count : Int }

intContainer : Container Int
intContainer = { value = 42, count = 1 }`,
    );
    const result = analyze(program);

    expect(result.records.Container).toBeDefined();
    expect(result.records.Container?.params).toEqual(["a"]);
    expect(result.types.intContainer).toBeDefined();
  });

  test("record type with function fields", () => {
    const program = parse(
      `module Test exposing (..)

${OPERATOR_PREAMBLE}
type Model = { count : Int, increment : Int -> Int }

model : Model
model = { count = 0, increment = \\x -> x + 1 }`,
    );
    const result = analyze(program);

    expect(result.records.Model).toBeDefined();
    expect(result.types.model).toBeDefined();
  });

  test("rejects nested floating record types in type declaration", () => {
    expectError(
      `type Outer = { inner : { value : Int } }`,
      "Record types cannot be used directly in type annotations",
    );
  });

  test("record type with multiple type parameters", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Pair a b = { first : a, second : b }

pair : Pair string number
pair = { first = "hello", second = 42 }`);

    const result = analyze(program);

    expect(result.records.Pair).toBeDefined();
    expect(result.records.Pair?.params).toEqual(["a", "b"]);
    expect(result.types.pair).toBeDefined();
  });

  test("record type with parameterized field type", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type ListBox a = { items : List a, count : Int }

stringBox : ListBox String
stringBox = { items = ["a", "b"], count = 2 }`);

    const result = analyze(program);

    expect(result.records.ListBox).toBeDefined();
    expect(result.records.ListBox?.params).toEqual(["a"]);
    expect(result.types.stringBox).toBeDefined();
  });

  test("rejects nested floating record type in field", () => {
    expectError(
      `type Response a = { data : a, metadata : { code : Int, message : String } }`,
      "Record types cannot be used directly in type annotations",
    );
  });

  test("record type with function field using type parameter", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Handler a = { process : a -> String, callback : String -> a }

handler : Handler Int
handler = { process = \\n -> "result", callback = \\s -> 0 }`);

    const result = analyze(program);

    expect(result.records.Handler).toBeDefined();
    expect(result.records.Handler?.params).toEqual(["a"]);
    expect(result.types.handler).toBeDefined();
  });

  test("multiple parameterized record types coexist", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Container a = { value : a, count : Int }
type Pair a b = { first : a, second : b }
type Wrapper a = { wrapped : a }

c : Container String
c = { value = "data", count = 1 }

p : Pair Int Int
p = { first = 5, second = 3 }

w : Wrapper (List Int)
w = { wrapped = [1, 2, 3] }`);

    const result = analyze(program);

    expect(result.records.Container).toBeDefined();
    expect(result.records.Pair).toBeDefined();
    expect(result.records.Wrapper).toBeDefined();
    expect(result.types.c).toBeDefined();
    expect(result.types.p).toBeDefined();
    expect(result.types.w).toBeDefined();
  });

  test("parameterized record type in record field", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Box a = { contents : a }
type Pair a b = { left : a, right : b }

nested : Pair (Box String) (Box Int)
nested = { left = { contents = "text" }, right = { contents = 42 } }`);

    const result = analyze(program);

    expect(result.records.Box).toBeDefined();
    expect(result.records.Pair).toBeDefined();
    expect(result.types.nested).toBeDefined();
  });

  // ===== Combined ADT and Type Alias Tests =====

  test("type alias and ADT can coexist", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type Option a = Some a | None
type alias MaybeInt = Option Int

wrapped = Some 42`);
    const result = analyze(program);

    expect(result.adts.Option).toBeDefined();
    expect(result.typeAliases.MaybeInt).toBeDefined();
    expect(result.types.wrapped).toBeDefined();
  });

  // ===== Type Validation Tests (negative cases) =====

  test("rejects lowercase type name in type alias (suggests capitalization)", () => {
    expectError(
      `type alias IntPair = (Int, int)`,
      "Type 'int' is not defined. Did you mean 'Int'?",
    );
  });

  test("rejects undefined type in type alias", () => {
    expectError(`type alias MyPair = (Int, Foo)`, "Type 'Foo' is not defined");
  });

  test("rejects undefined type variable in type alias", () => {
    expectError(
      `type alias Container = List a`,
      "Type variable 'a' is not defined in this context",
    );
  });

  test("accepts valid type parameter in type alias", () => {
    const program = parse(`module Test exposing (..)

${TYPE_PREAMBLE}
type alias Container a = List a`);
    const result = analyze(program);

    expect(result.typeAliases.Container).toBeDefined();
    expect(result.typeAliases.Container?.params).toEqual(["a"]);
  });

  test("rejects nested undefined type in type expression", () => {
    expectError(
      `type alias Nested = (Int, (String, foo))`,
      "Type 'foo' is not defined",
    );
  });

  test("rejects undefined type in function type alias", () => {
    expectError(`type alias Handler = foo -> Int`, "Type 'foo' is not defined");
  });

  test("rejects undefined type in tuple type alias", () => {
    expectError(
      `type alias MyTuple = (Int, foo, String)`,
      "Type 'foo' is not defined",
    );
  });

  test("validates type alias stores module name", () => {
    const program = parse(`module TestModule exposing (..)
${TYPE_PREAMBLE}
type alias IntPair = (Int, Int)`);
    const result = analyze(program);

    expect(result.typeAliases.IntPair).toBeDefined();
    expect(result.typeAliases.IntPair?.moduleName).toBe("TestModule");
  });

  test("validates ADT stores module name", () => {
    const program = parse(`module TestModule exposing (..)
${TYPE_PREAMBLE}
type MyOption a = MySome a | MyNone`);
    const result = analyze(program);

    expect(result.adts.MyOption).toBeDefined();
    expect(result.adts.MyOption?.moduleName).toBe("TestModule");
  });
});

describe("module exports", () => {
  test("computes exports for exposing all", () => {
    const program = parse(`module TestModule exposing (..)
${TYPE_PREAMBLE}
type MyOption a = MySome a | MyNone
foo x = x
bar y = y`);
    const result = analyze(program);

    expect(result.exports.exportsAll).toBe(true);
    expect(result.exports.values.has("foo")).toBe(true);
    expect(result.exports.values.has("bar")).toBe(true);
    expect(result.exports.types.has("MyOption")).toBe(true);
    expect(result.exports.types.get("MyOption")?.allConstructors).toBe(true);
  });

  test("computes exports for explicit value exports", () => {
    const program = parse(`module TestModule exposing (foo)
${TYPE_PREAMBLE}
foo x = x
bar y = y`);
    const result = analyze(program);

    expect(result.exports.exportsAll).toBe(false);
    expect(result.exports.values.has("foo")).toBe(true);
    expect(result.exports.values.has("bar")).toBe(false);
  });

  test("computes exports for type with all constructors", () => {
    const program = parse(`module TestModule exposing (MyOption(..))
${TYPE_PREAMBLE}
type MyOption a = MySome a | MyNone`);
    const result = analyze(program);

    expect(result.exports.types.has("MyOption")).toBe(true);
    const typeExport = result.exports.types.get("MyOption");
    expect(typeExport?.allConstructors).toBe(true);
    expect(typeExport?.constructors?.has("MySome")).toBe(true);
    expect(typeExport?.constructors?.has("MyNone")).toBe(true);
  });

  test("computes exports for type with specific constructors", () => {
    const program = parse(`module TestModule exposing (MyOption(MySome))
${TYPE_PREAMBLE}
type MyOption a = MySome a | MyNone`);
    const result = analyze(program);

    expect(result.exports.types.has("MyOption")).toBe(true);
    const typeExport = result.exports.types.get("MyOption");
    expect(typeExport?.allConstructors).toBe(false);
    expect(typeExport?.constructors?.has("MySome")).toBe(true);
    expect(typeExport?.constructors?.has("MyNone")).toBe(false);
  });

  test("computes exports for opaque type", () => {
    const program = parse(`module TestModule exposing (Opaque)
${TYPE_PREAMBLE}
type Opaque a`);
    const result = analyze(program);

    expect(result.exports.types.has("Opaque")).toBe(true);
    expect(result.exports.types.get("Opaque")?.allConstructors).toBe(false);
  });

  test("computes exports for protocol with all methods", () => {
    const program = parse(`module TestModule exposing (MyProtocol(..))
${TYPE_PREAMBLE}
protocol MyProtocol a where
  foo : a -> a
  bar : a -> Int`);
    const result = analyze(program);

    expect(result.exports.protocols.has("MyProtocol")).toBe(true);
    const protocolExport = result.exports.protocols.get("MyProtocol");
    expect(protocolExport?.allMethods).toBe(true);
    expect(protocolExport?.methods?.has("foo")).toBe(true);
    expect(protocolExport?.methods?.has("bar")).toBe(true);
  });

  test("computes exports for protocol with specific methods", () => {
    const program = parse(`module TestModule exposing (MyProtocol(foo))
${TYPE_PREAMBLE}
protocol MyProtocol a where
  foo : a -> a
  bar : a -> Int`);
    const result = analyze(program);

    expect(result.exports.protocols.has("MyProtocol")).toBe(true);
    const protocolExport = result.exports.protocols.get("MyProtocol");
    expect(protocolExport?.allMethods).toBe(false);
    expect(protocolExport?.methods?.has("foo")).toBe(true);
    expect(protocolExport?.methods?.has("bar")).toBe(false);
  });

  test("rejects exporting undefined value", () => {
    expectError(
      `module TestModule exposing (undefined)
foo x = x`,
      "Module exposes 'undefined' which is not defined",
    );
  });

  test("rejects exporting Object.prototype properties as values", () => {
    // Regression test: ensure we use Object.hasOwn to avoid prototype pollution
    // Previously, trying to export 'toString' would succeed because it exists on Object.prototype
    expectError(
      `module TestModule exposing (toString)
foo x = x`,
      "Module exposes 'toString' which is not defined",
    );
  });

  test("rejects exporting constructor property from Object.prototype", () => {
    // Similar test for the 'constructor' property
    expectError(
      `module TestModule exposing (constructor)
foo x = x`,
      "Module exposes 'constructor' which is not defined",
    );
  });

  test("rejects exporting hasOwnProperty from Object.prototype", () => {
    // Test for hasOwnProperty as well
    expectError(
      `module TestModule exposing (hasOwnProperty)
foo x = x`,
      "Module exposes 'hasOwnProperty' which is not defined",
    );
  });

  test("rejects exporting undefined type with constructors", () => {
    expectError(
      `module TestModule exposing (UndefinedType(..))
foo x = x`,
      "Module exposes 'UndefinedType(..)' but 'UndefinedType' is not a type or protocol",
    );
  });

  test("rejects exporting type alias with (..) syntax", () => {
    expectError(
      `module TestModule exposing (MyAlias(..))
type alias MyAlias = Int`,
      "Type alias 'MyAlias' cannot use (..) syntax - type aliases have no constructors",
    );
  });

  test("rejects exporting invalid constructor for type", () => {
    expectError(
      `module TestModule exposing (MyOption(Invalid))
type MyOption a = MySome a | MyNone`,
      "Constructor 'Invalid' is not defined in type 'MyOption'",
    );
  });

  test("rejects exporting invalid method for protocol", () => {
    expectError(
      `module TestModule exposing (MyProtocol(invalid))
protocol MyProtocol a where
  foo : a -> a`,
      "Method 'invalid' is not defined in protocol 'MyProtocol'",
    );
  });

  test("handles mixed exports correctly", () => {
    const program =
      parse(`module TestModule exposing (foo, MyOption(..), bar, MyProtocol(methodA))
${TYPE_PREAMBLE}
type MyOption a = MySome a | MyNone

protocol MyProtocol a where
  methodA : a -> a
  methodB : a -> Int

foo x = x
bar y = y
baz z = z`);
    const result = analyze(program);

    expect(result.exports.exportsAll).toBe(false);
    expect(result.exports.values.has("foo")).toBe(true);
    expect(result.exports.values.has("bar")).toBe(true);
    expect(result.exports.values.has("baz")).toBe(false);
    expect(result.exports.types.has("MyOption")).toBe(true);
    expect(result.exports.types.get("MyOption")?.allConstructors).toBe(true);
    expect(result.exports.protocols.has("MyProtocol")).toBe(true);
    expect(result.exports.protocols.get("MyProtocol")?.allMethods).toBe(false);
    expect(
      result.exports.protocols.get("MyProtocol")?.methods?.has("methodA"),
    ).toBe(true);
    expect(
      result.exports.protocols.get("MyProtocol")?.methods?.has("methodB"),
    ).toBe(false);
  });
});

describe("unary negation", () => {
  test("accepts unary negation of Int literal", () => {
    const result = analyzeNoPrelude("x = -10");
    expect(result.values.x?.type!.kind).toBe("con");
    expect((result.values.x?.type as any).name).toBe("Int");
  });

  test("accepts unary negation of Float literal", () => {
    const result = analyzeNoPrelude("x = -10.5");
    expect(result.values.x?.type!.kind).toBe("con");
    expect((result.values.x?.type as any).name).toBe("Float");
  });

  test("accepts unary negation of Int variable", () => {
    const result = analyzeNoPrelude(`
y : Int
y = 5

x = -y
`);
    expect(result.values.x?.type!.kind).toBe("con");
    expect((result.values.x?.type as any).name).toBe("Int");
  });

  test("accepts unary negation of Float variable", () => {
    const result = analyzeNoPrelude(`
y : Float
y = 5.0

x = -y
`);
    expect(result.values.x?.type!.kind).toBe("con");
    expect((result.values.x?.type as any).name).toBe("Float");
  });

  test("accepts double negation with grouping", () => {
    const result = analyzeNoPrelude("x = -(-10)");
    expect(result.values.x?.type!.kind).toBe("con");
    expect((result.values.x?.type as any).name).toBe("Int");
  });

  test("rejects unary negation of String", () => {
    expectError(
      `
x : String
x = "hello"

y = -x
`,
      "Unary negation is only allowed for Int and Float",
    );
  });

  test("rejects unary negation of Bool", () => {
    expectError(
      `
x = True

y = -x
`,
      "Unary negation is only allowed for Int and Float",
    );
  });

  test("rejects unary negation of unknown type variable", () => {
    expectError(
      `
negate x = -x
`,
      "Unary negation requires a concrete numeric type",
    );
  });
});
describe("module declaration validation", () => {
  test("rejects file without module declaration (parse error)", () => {
    const source = `
${TYPE_PREAMBLE}
main = 1
`;

    // Parser should throw before we even reach semantic analysis
    expect(() => parse(source)).toThrow(
      "Every Vibe file must begin with a module declaration",
    );
  });

  test("rejects module name that doesn't match file path", () => {
    const source = `module WrongName exposing (..)
${TYPE_PREAMBLE}
main = 1
`;
    const program = parse(source);

    expect(() =>
      analyze(program, {
        fileContext: {
          filePath: "/project/src/Main.vibe",
          srcDir: "/project/src",
        },
      }),
    ).toThrow("does not match file path");
  });

  test("accepts matching module declaration for simple module", () => {
    const source = `module Main exposing (..)
${TYPE_PREAMBLE}
main = 1
`;
    const program = parse(source);

    // Should not throw
    const result = analyze(program, {
      fileContext: {
        filePath: "/project/src/Main.vibe",
        srcDir: "/project/src",
      },
    });
    expect(result.module?.name).toBe("Main");
  });

  test("accepts matching module declaration for nested module", () => {
    const source = `module Data.List exposing (..)
${TYPE_PREAMBLE}
empty = True
`;
    const program = parse(source);

    // Should not throw
    const result = analyze(program, {
      fileContext: {
        filePath: "/project/src/Data/List.vibe",
        srcDir: "/project/src",
      },
    });
    expect(result.module?.name).toBe("Data.List");
  });

  test("accepts matching module declaration for deeply nested module", () => {
    const source = `module Vibe.Internal.Utils exposing (..)
${TYPE_PREAMBLE}
helper = True
`;
    const program = parse(source);

    // Should not throw
    const result = analyze(program, {
      fileContext: {
        filePath: "/project/src/Vibe/Internal/Utils.vibe",
        srcDir: "/project/src",
      },
    });
    expect(result.module?.name).toBe("Vibe.Internal.Utils");
  });

  test("error message suggests correct module name", () => {
    const source = `
module Test exposing (..)

${TYPE_PREAMBLE}
main = 1
`;
    const program = parse(source);

    expect(() =>
      analyze(program, {
        fileContext: {
          filePath: "/project/src/Data/List.vibe",
          srcDir: "/project/src",
        },
      }),
    ).toThrow("Expected: module Data.List [exposing (..)]");
  });

  test("error for mismatched module includes expected name", () => {
    const source = `module Wrong.Module exposing (..)
${TYPE_PREAMBLE}
main = 1
`;
    const program = parse(source);

    expect(() =>
      analyze(program, {
        fileContext: {
          filePath: "/project/src/Correct/Module.vibe",
          srcDir: "/project/src",
        },
      }),
    ).toThrow("Expected: module Correct.Module [exposing (..)]");
  });

  test("allows any module name when fileContext is not provided (backward compatibility)", () => {
    // Without fileContext, only path validation is skipped
    // Module declaration itself is still required by the parser
    const source = `module AnyName exposing (..)
${TYPE_PREAMBLE}
main = 1
`;
    const program = parse(source);

    // Should not throw even though module name doesn't match any file path
    const result = analyze(program);
    expect(result.module.name).toBe("AnyName");
  });
});

describe("prefix operator syntax", () => {
  test("resolves built-in logical operator in prefix form", () => {
    // && is a built-in operator that takes Bool -> Bool -> Bool
    const result = analyzeNoPrelude(`test = (&&) True False`);
    expect(result.values.test).toBeDefined();
    expect(result.typeSchemes?.test).toBeDefined();
  });

  test("resolves external operator in prefix form", () => {
    const source = `module Test exposing (..)
${OPERATOR_PREAMBLE}

-- Using (+) as a prefix function
result = (+) 1 2
`;
    const program = parse(source);
    const result = analyze(program);
    expect(result.values.result).toBeDefined();
    expect(result.typeSchemes?.result).toBeDefined();
  });

  test("prefix operator can be passed as argument to higher-order function", () => {
    const source = `module Test exposing (..)
${OPERATOR_PREAMBLE}

-- Define a simple fold-like function
apply : (Int -> Int -> Int) -> Int -> Int -> Int
apply f x y = f x y

-- Pass (+) as an argument
result = apply (+) 3 4
`;
    const program = parse(source);
    const result = analyze(program);
    expect(result.values.result).toBeDefined();
    expect(result.typeSchemes?.result).toBeDefined();
  });

  test("prefix operator works in lambda context", () => {
    const source = `module Test exposing (..)
${OPERATOR_PREAMBLE}

-- Lambda that uses prefix operator
adder = \\x y -> (+) x y
`;
    const program = parse(source);
    const result = analyze(program);
    expect(result.values.adder).toBeDefined();
  });

  test("rejects undefined operator in prefix form", () => {
    const source = `module Test exposing (..)
${TYPE_PREAMBLE}

-- (<>) is not defined
result = (<>) 1 2
`;
    expect(() => analyze(parse(source))).toThrow("Undefined name");
  });

  test("prefix operator can be bound to a name and reused", () => {
    const source = `module Test exposing (..)
${OPERATOR_PREAMBLE}

-- Bind the operator to a name
add = (+)

-- Use the bound name
result = add 1 2
`;
    const program = parse(source);
    const result = analyze(program);
    expect(result.values.add).toBeDefined();
    expect(result.values.result).toBeDefined();
  });

  test("prefix logical operator preserves type signature", () => {
    // (&&) should still be Bool -> Bool -> Bool
    const source = `module Test exposing (..)
${TYPE_PREAMBLE}

-- Bind && to a name with explicit type annotation
myAnd : Bool -> Bool -> Bool
myAnd = (&&)

-- Use it
result = myAnd True False
`;
    const program = parse(source);
    const result = analyze(program);
    expect(result.values.myAnd).toBeDefined();
    expect(result.values.result).toBeDefined();
  });
});

describe("infix declarations", () => {
  test("rejects fixity declaration without corresponding function definition", () => {
    // Declare fixity for an undefined operator (<>) - should fail
    // Note: we use analyzeNoPrelude which doesn't include OPERATOR_PREAMBLE
    expect(() =>
      analyzeNoPrelude(`
infixl 6 <>

value = 1
`),
    ).toThrow(
      "Infix declaration for operator '<>' has no corresponding function definition",
    );
  });

  test("rejects fixity declaration with only parenthesized export", () => {
    // Exporting an operator without defining it should fail
    // Use a unique operator ~> that isn't in OPERATOR_PREAMBLE
    expect(() =>
      analyzeNoPrelude(`
module Test exposing ((~>))

infixl 6 ~>

value = 1
`),
    ).toThrow(
      "Infix declaration for operator '~>' has no corresponding function definition",
    );
  });

  test("allows fixity declaration with external function", () => {
    // Operator is defined via external declaration - should succeed
    const result = analyzeNoPrelude(`
infixl 6 <+>

@external "@vibe/runtime" "intAdd"
(<+>) : Int -> Int -> Int
`);
    expect(result.operators.has("<+>")).toBe(true);
    expect(result.values["<+>"]).toBeDefined();
  });

  test("allows fixity declaration with value declaration", () => {
    // Operator is defined via value declaration - should succeed
    const result = analyzeNoPrelude(`
infixl 6 <#>

(<#>) : Int -> Int -> Int
(<#>) a b = a
`);
    expect(result.operators.has("<#>")).toBe(true);
    expect(result.values["<#>"]).toBeDefined();
  });

  test("allows fixity declaration for protocol method", () => {
    // Protocol method with alphanumeric name - should succeed
    const result = analyzeNoPrelude(`
protocol Add a where
    (<+>) : a -> a -> a

infixl 6 <+>
`);
    expect(result.operators.has("<+>")).toBe(true);
    expect(result.protocols.Add).toBeDefined();
    expect(result.protocols.Add!.methods.has("<+>")).toBe(true);
  });

  test("rejects multiple orphan fixity declarations", () => {
    // Only the first undefined operator should error (semantic analysis stops)
    expect(() =>
      analyzeNoPrelude(`
infixl 6 <>
infixl 7 ><

value = 1
`),
    ).toThrow(
      "Infix declaration for operator '<>' has no corresponding function definition",
    );
  });

  test("allows fixity declaration for operator-style protocol method", () => {
    // Protocol methods with operator names should work
    const result = analyzeNoPrelude(`
protocol Addable a where
    (<>) : a -> a -> a

infixr 5 <>
`);
    expect(result.operators.has("<>")).toBe(true);
    expect(result.protocols.Addable!.methods.has("<>")).toBe(true);
  });

  test("rejects fixity declaration for imported operator", () => {
    // Fixity is intrinsic and cannot be redefined for imported operators
    // We test this via the module resolution path

    // First, create a "dependency" module that exports an operator <+>
    const depSource = `
module DepModule exposing ((<+>))

infixl 6 <+>

@external "@vibe/runtime" "intAdd"
(<+>) : Int -> Int -> Int
`;
    const depResult = analyzeNoPrelude(depSource);

    // Now try to import it and redefine its fixity
    const mainSource = `
module MainModule

import DepModule exposing ((<+>))

infixr 7 <+>
`;
    // This should fail because we're trying to redefine fixity of an imported operator
    expect(() => {
      analyze(parse(mainSource), {
        dependencies: new Map([["DepModule", depResult]]),
      });
    }).toThrow(
      /Cannot declare fixity for imported operator '<\+>' from module 'DepModule'/,
    );
  });
});
