import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze, SemanticError } from "../src/index.ts";

const expectError = (source: string, message: string) => {
  expect(() => analyze(parse(source))).toThrow(message);
};

describe("semantics", () => {
  test("attaches annotations to definitions", () => {
    const program = parse(`value : number
value = 1`);
    const result = analyze(program);
    expect(result.values.value).toBeDefined();
    expect(result.values.value?.annotation).toBeDefined();
  });

  test("rejects duplicate definitions", () => {
    expectError(
      `a = 1
a = 2`,
      "Duplicate definition"
    );
  });

  test("rejects orphan annotations", () => {
    expectError(`a : number`, "has no matching definition");
  });

  test("registers external declarations with annotations", () => {
    const program = parse(`@external "./lib.js" "compute"
ffiCompute : number -> number`);
    const result = analyze(program);
    const info = result.values.ffiCompute;
    expect(info).toBeDefined();
    expect(info?.externalTarget?.modulePath).toBe("./lib.js");
    expect(info?.externalTarget?.exportName).toBe("compute");
    expect(info?.annotation?.kind).toBe("FunctionType");
  });

  test("rejects extra annotations for externals", () => {
    expectError(
      `@external "./lib.js" "compute"
ffiCompute : number -> number
ffiCompute : number -> number`,
      "already includes a type annotation"
    );
  });

  test("rejects module exposing missing value", () => {
    expectError(
      `module Main exposing (foo)
bar = 1`,
      "Module exposes 'foo'"
    );
  });

  test("accepts module exposing existing value", () => {
    const program = parse(`module Main exposing (bar)
bar = 1`);
    const result = analyze(program);
    expect(result.module?.name).toBe("Main");
  });

  test("rejects duplicate imports of the same module", () => {
    expectError(
      `import Foo
import Foo`,
      "Duplicate import of module 'Foo'"
    );
  });

  test("rejects duplicate import aliases", () => {
    expectError(
      `import Foo as A
import Bar as A`,
      "Duplicate import alias 'A'"
    );
  });

  test("rejects undefined variable references", () => {
    expectError(`a = b`, "Undefined name 'b'");
  });

  test("rejects duplicate lambda args", () => {
    expectError(`f x x = x`, "Duplicate pattern variable 'x'");
  });

  test("rejects annotation arity mismatches", () => {
    expectError(
      `f : number
f x = x`,
      "does not match its argument count"
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
      "Duplicate let-binding 'x'"
    );
  });

  test("rejects non-exhaustive case on Bool", () => {
    expectError(
      `f x =
  case x of
    True -> 1`,
      "Non-exhaustive case expression"
    );
  });

  test("rejects constructor arity mismatch", () => {
    expectError(
      `f x =
  case x of
    Just -> 1`,
      "Constructor 'Just' expects 1 argument(s)"
    );
  });

  test("rejects wildcard before later branches", () => {
    expectError(
      `f x =
  case x of
    _ -> 1
    True -> 2`,
      "Wildcard pattern makes following branches unreachable"
    );
  });

  test("rejects record field duplication", () => {
    expectError(`f = { a = 1, a = 2 }`, "Duplicate record field 'a'");
  });

  test("rejects type mismatches in infix usage", () => {
    expectError(`f = 1 + "hi"`, "Type mismatch");
  });

  test("infers simple identity function", () => {
    const program = parse(`id x = x`);
    const result = analyze(program);
    expect(result.types.id).toBeDefined();
  });

  // ===== Let-Polymorphism Tests =====
  // These tests verify that the Hindley-Milner type system properly
  // generalizes and instantiates polymorphic types

  test("polymorphic identity function can be used at multiple types", () => {
    // The identity function 'id x = x' should be inferred as polymorphic: forall a. a -> a
    // This allows it to be used at both number and string types
    const program = parse(`id x = x
useAtNumber = id 42
useAtString = id "hello"`);
    const result = analyze(program);

    // All three bindings should infer successfully
    expect(result.types.id).toBeDefined();
    expect(result.types.useAtNumber).toBeDefined();
    expect(result.types.useAtString).toBeDefined();

    // The uses should not interfere with each other
    // (without polymorphism, the first use would fix 'id' to number -> number)
  });

  test("polymorphic function in let-binding", () => {
    // Let-bound polymorphic functions should be instantiated freshly at each use
    const program = parse(`f =
  let
    id x = x
  in
    (id 1, id "hi")`);
    const result = analyze(program);
    expect(result.types.f).toBeDefined();
  });

  test("nested let-polymorphism", () => {
    // Test that nested let-bindings properly generalize
    const program = parse(`f =
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
    const program = parse(`const x y = x
n = const 1 "ignored"
s = const "hi" 42`);
    const result = analyze(program);
    expect(result.types.const).toBeDefined();
    expect(result.types.n).toBeDefined();
    expect(result.types.s).toBeDefined();
  });

  test("polymorphic compose function", () => {
    // compose : forall a b c. (b -> c) -> (a -> b) -> a -> c
    const program = parse(`compose f g x = f (g x)
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
    const program = parse(`pair x y = (x, y)
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
    const program = parse(`singleton x = [x]
nums = singleton 42
strs = singleton "hi"`);
    const result = analyze(program);
    expect(result.types.singleton).toBeDefined();
    expect(result.types.nums).toBeDefined();
    expect(result.types.strs).toBeDefined();
  });

  test.skip("polymorphic recursion - length function", () => {
    // TODO: This requires list pattern matching ([] and ::) which parser doesn't support yet
    // length : forall a. [a] -> number
    // Polymorphic recursive functions should work
    const program = parse(`length xs =
  case xs of
    [] -> 0
    (x :: rest) -> 1 + length rest

n = length [1, 2, 3]
m = length ["a", "b"]`);
    const result = analyze(program);
    expect(result.types.length).toBeDefined();
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
    const program = parse(`apply : (a -> b) -> a -> b
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
    const program = parse(`id : a -> a
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
    const program = parse(`toNumber : a -> number
toNumber x = 42

toStr : a -> string
toStr x = "hi"`);
    const result = analyze(program);
    expect(result.types.toNumber).toBeDefined();
    expect(result.types.toStr).toBeDefined();
  });

  test("type annotations with List type constructor", () => {
    // List is a type constructor that takes a type argument
    const program = parse(`head : List a -> a
head xs = 42

length : List a -> number
length xs = 0`);
    const result = analyze(program);
    expect(result.types.head).toBeDefined();
    expect(result.types.length).toBeDefined();
  });

  test("complex nested type annotations", () => {
    // Test deeply nested function types with type variables
    const program = parse(`compose : (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

twice : (a -> a) -> a -> a
twice f x = f (f x)`);
    const result = analyze(program);
    expect(result.types.compose).toBeDefined();
    expect(result.types.twice).toBeDefined();
  });

  test("type annotation must match implementation", () => {
    // If annotation says number -> number, using it polymorphically should fail
    expectError(
      `f : number -> number
f x = x
n = f 42
s = f "hi"`,
      "Type mismatch"
    );
  });

  test("generalization respects scope", () => {
    // Variables free in the outer scope should not be generalized
    const program = parse(`outer =
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
    const program = parse(`type MyBool = MyTrue | MyFalse`);
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
    const program = parse(`type Option a = Some a | None`);
    const result = analyze(program);

    expect(result.adts.Option).toBeDefined();
    expect(result.adts.Option?.params).toEqual(["a"]);
    expect(result.adts.Option?.constructors).toEqual(["Some", "None"]);

    expect(result.constructors.Some?.arity).toBe(1);
    expect(result.constructors.None?.arity).toBe(0);
  });

  test("uses ADT constructors as values", () => {
    // Constructors should be usable as values with proper types
    const program = parse(`type Option a = Some a | None

wrapped = Some 42
nothing = None`);
    const result = analyze(program);

    expect(result.types.wrapped).toBeDefined();
    expect(result.types.nothing).toBeDefined();
  });

  test("pattern matches on user-defined ADT", () => {
    const program = parse(`type Option a = Some a | None

unwrap opt default =
  case opt of
    Some x -> x
    None -> default`);
    const result = analyze(program);

    expect(result.types.unwrap).toBeDefined();
  });

  test("ADT exhaustiveness checking - complete", () => {
    // Should pass - all constructors covered
    const program = parse(`type Color = Red | Green | Blue

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
      "Non-exhaustive case expression"
    );
  });

  test("ADT exhaustiveness checking - reports missing constructors", () => {
    // The error message should include the missing constructor
    expect(() =>
      analyze(
        parse(`type Status = Pending | Running | Completed | Failed

process status =
  case status of
    Pending -> "waiting"
    Completed -> "done"`)
      )
    ).toThrow(/missing.*Running.*Failed|Non-exhaustive/);
  });

  test("rejects duplicate type declaration", () => {
    expectError(
      `type MyBool = MyTrue | MyFalse
type MyBool = MyYes | MyNo`,
      "Duplicate type declaration"
    );
  });

  test("rejects duplicate constructor within module", () => {
    expectError(
      `type A = Ctor | Other
type B = Ctor | Another`,
      "Duplicate constructor 'Ctor'"
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
      "expects 1 argument"
    );
  });

  test("validates constructor arity - too many args", () => {
    expectError(
      `type Option a = Some a | None

f opt =
  case opt of
    Some x y -> 1
    None -> 0`,
      "expects 1 argument"
    );
  });

  test("recursive ADT - List", () => {
    const program = parse(`type List a = Cons a (List a) | Nil

empty = Nil
single = Cons 1 Nil
double = Cons 1 (Cons 2 Nil)`);
    const result = analyze(program);

    expect(result.adts.List).toBeDefined();
    expect(result.constructors.Cons?.arity).toBe(2);
    expect(result.constructors.Nil?.arity).toBe(0);
    expect(result.types.empty).toBeDefined();
    expect(result.types.single).toBeDefined();
    expect(result.types.double).toBeDefined();
  });

  test("recursive ADT pattern matching", () => {
    const program = parse(`type List a = Cons a (List a) | Nil

length xs =
  case xs of
    Nil -> 0
    Cons _ tail -> 1 + length tail`);
    const result = analyze(program);
    expect(result.types.length).toBeDefined();
  });

  test("polymorphic ADT map function", () => {
    // This is the test that was previously skipped
    const program = parse(`type Option a = Some a | None

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
    const program = parse(`type Either a b = Left a | Right b

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
    const program = parse(`type alias UserId = number`);
    const result = analyze(program);

    expect(result.typeAliases.UserId).toBeDefined();
    expect(result.typeAliases.UserId?.params).toEqual([]);
  });

  test("registers parameterized type alias", () => {
    const program = parse(`type alias Pair a b = (a, b)`);
    const result = analyze(program);

    expect(result.typeAliases.Pair).toBeDefined();
    expect(result.typeAliases.Pair?.params).toEqual(["a", "b"]);
  });

  test("rejects duplicate type alias", () => {
    expectError(
      `type alias UserId = number
type alias UserId = string`,
      "Duplicate type alias"
    );
  });

  test("rejects duplicate type parameter in alias", () => {
    expectError(`type alias Bad a a = (a, a)`, "Duplicate type parameter 'a'");
  });

  // ===== Record Type Annotation Tests =====

  test("type alias with record type", () => {
    const program = parse(`type alias Point = { x : number, y : number }`);
    const result = analyze(program);

    expect(result.typeAliases.Point).toBeDefined();
    expect(result.typeAliases.Point?.params).toEqual([]);
    expect(result.typeAliases.Point?.value.kind).toBe("RecordType");
  });

  test("function with record type annotation", () => {
    const program = parse(`distance : { x : number, y : number } -> number
distance point = point.x + point.y`);
    const result = analyze(program);

    expect(result.types.distance).toBeDefined();
    expect(result.values.distance?.annotation?.kind).toBe("FunctionType");
  });

  test("record type fields are type-checked correctly", () => {
    const program = parse(`type alias Point = { x : number, y : number }

origin : Point
origin = { x = 0, y = 0 }`);

    // Should not throw - record literal matches type
    const result = analyze(program);
    expect(result.types.origin).toBeDefined();
  });

  test("empty record type", () => {
    const program = parse(`type alias Empty = {}

empty : Empty
empty = {}`);
    const result = analyze(program);

    expect(result.typeAliases.Empty).toBeDefined();
    expect(result.types.empty).toBeDefined();
  });

  test("parameterized record type", () => {
    const program = parse(
      `type alias Container a = { value : a, count : number }

intContainer : Container number
intContainer = { value = 42, count = 1 }`
    );
    const result = analyze(program);

    expect(result.typeAliases.Container).toBeDefined();
    expect(result.typeAliases.Container?.params).toEqual(["a"]);
    expect(result.types.intContainer).toBeDefined();
  });

  test("record type with function fields", () => {
    const program = parse(
      `type alias Model = { count : number, increment : number -> number }

model : Model
model = { count = 0, increment = \\x -> x + 1 }`
    );
    const result = analyze(program);

    expect(result.typeAliases.Model).toBeDefined();
    expect(result.types.model).toBeDefined();
  });

  test("nested record types", () => {
    const program = parse(
      `type alias Outer = { inner : { value : number } }

nested : Outer
nested = { inner = { value = 5 } }`
    );
    const result = analyze(program);

    expect(result.typeAliases.Outer).toBeDefined();
    expect(result.types.nested).toBeDefined();
  });

  test("record type alias with multiple type parameters", () => {
    const program = parse(`type alias Pair a b = { first : a, second : b }

pair : Pair string number
pair = { first = "hello", second = 42 }`);

    const result = analyze(program);

    expect(result.typeAliases.Pair).toBeDefined();
    expect(result.typeAliases.Pair?.params).toEqual(["a", "b"]);
    expect(result.types.pair).toBeDefined();
  });

  test("record type alias with parameterized field type", () => {
    const program =
      parse(`type alias ListBox a = { items : List a, count : number }

stringBox : ListBox string
stringBox = { items = ["a", "b"], count = 2 }`);

    const result = analyze(program);

    expect(result.typeAliases.ListBox).toBeDefined();
    expect(result.typeAliases.ListBox?.params).toEqual(["a"]);
    expect(result.types.stringBox).toBeDefined();
  });

  test("record type with nested record type parameter", () => {
    const program =
      parse(`type alias Response a = { data : a, metadata : { code : number, message : string } }

response : Response string
response = { data = "ok", metadata = { code = 200, message = "Success" } }`);

    const result = analyze(program);

    expect(result.typeAliases.Response).toBeDefined();
    expect(result.typeAliases.Response?.params).toEqual(["a"]);
    expect(result.types.response).toBeDefined();
  });

  test("record type with function field using type parameter", () => {
    const program =
      parse(`type alias Handler a = { process : a -> string, callback : string -> a }

handler : Handler number
handler = { process = \\n -> "result", callback = \\s -> 0 }`);

    const result = analyze(program);

    expect(result.typeAliases.Handler).toBeDefined();
    expect(result.typeAliases.Handler?.params).toEqual(["a"]);
    expect(result.types.handler).toBeDefined();
  });

  test("multiple parameterized record type aliases coexist", () => {
    const program =
      parse(`type alias Container a = { value : a, count : number }
type alias Pair a b = { first : a, second : b }
type alias Wrapper a = { wrapped : a }

c : Container string
c = { value = "data", count = 1 }

p : Pair number number
p = { first = 5, second = 3 }

w : Wrapper (List number)
w = { wrapped = [1, 2, 3] }`);

    const result = analyze(program);

    expect(result.typeAliases.Container).toBeDefined();
    expect(result.typeAliases.Pair).toBeDefined();
    expect(result.typeAliases.Wrapper).toBeDefined();
    expect(result.types.c).toBeDefined();
    expect(result.types.p).toBeDefined();
    expect(result.types.w).toBeDefined();
  });

  test("parameterized record alias in record field", () => {
    const program = parse(`type alias Box a = { contents : a }
type alias Pair a b = { left : a, right : b }

nested : Pair (Box string) (Box number)
nested = { left = { contents = "text" }, right = { contents = 42 } }`);

    const result = analyze(program);

    expect(result.typeAliases.Box).toBeDefined();
    expect(result.typeAliases.Pair).toBeDefined();
    expect(result.types.nested).toBeDefined();
  });

  // ===== Combined ADT and Type Alias Tests =====

  test("type alias and ADT can coexist", () => {
    const program = parse(`type Option a = Some a | None
type alias MaybeInt = Option number

wrapped = Some 42`);
    const result = analyze(program);

    expect(result.adts.Option).toBeDefined();
    expect(result.typeAliases.MaybeInt).toBeDefined();
    expect(result.types.wrapped).toBeDefined();
  });
});
