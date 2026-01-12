import { describe, expect, test } from "bun:test";
import { parse } from "../src/index";

describe("Protocol Declaration Parsing", () => {
  test("parses simple protocol with one method", () => {
    const source = `
protocol Show a where
  show : a -> String
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Show");
      expect(decl.params).toEqual(["a"]);
      expect(decl.methods).toHaveLength(1);
      expect(decl.methods[0]?.name).toBe("show");
    }
  });

  test("parses protocol with multiple methods", () => {
    const source = `
protocol Num a where
  plus : a -> a -> a
  minus : a -> a -> a
  times : a -> a -> a
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Num");
      expect(decl.params).toEqual(["a"]);
      expect(decl.methods).toHaveLength(3);
      expect(decl.methods.map((m) => m.name)).toEqual([
        "plus",
        "minus",
        "times",
      ]);
    }
  });
});

describe("Implementation Declaration Parsing", () => {
  test("parses simple implementation without constraints", () => {
    const source = `
implement Num Int where
  plus = intPlus
  minus = intMinus
  times = intTimes
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ImplementationDeclaration");

    if (decl?.kind === "ImplementationDeclaration") {
      expect(decl.protocolName).toBe("Num");
      expect(decl.constraints).toHaveLength(0);
      expect(decl.typeArgs).toHaveLength(1);
      expect(decl.methods).toHaveLength(3);
    }
  });

  test("parses implementation with single constraint", () => {
    const source = `
implement Show a => Show (List a) where
  show = showList
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ImplementationDeclaration");

    if (decl?.kind === "ImplementationDeclaration") {
      expect(decl.protocolName).toBe("Show");
      expect(decl.constraints).toHaveLength(1);
      expect(decl.constraints[0]?.protocolName).toBe("Show");
    }
  });

  test("parses implementation with multiple constraints", () => {
    const source = `
implement (Num a, Show a) => Show (Pair a a) where
  show = showPair
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ImplementationDeclaration");

    if (decl?.kind === "ImplementationDeclaration") {
      expect(decl.protocolName).toBe("Show");
      expect(decl.constraints).toHaveLength(2);
      expect(decl.constraints[0]?.protocolName).toBe("Num");
      expect(decl.constraints[1]?.protocolName).toBe("Show");
    }
  });
});

describe("Protocol Default Method Parsing", () => {
  test("parses protocol with one default method", () => {
    const source = `
protocol Eq a where
  eq : a -> a -> Bool
  neq : a -> a -> Bool
  neq x y = not (eq x y)
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Eq");
      expect(decl.methods).toHaveLength(2);

      // eq has no default
      expect(decl.methods[0]?.name).toBe("eq");
      expect(decl.methods[0]?.defaultImpl).toBeUndefined();

      // neq has default
      expect(decl.methods[1]?.name).toBe("neq");
      expect(decl.methods[1]?.defaultImpl).toBeDefined();
      expect(decl.methods[1]?.defaultImpl?.args).toHaveLength(2);
      expect(decl.methods[1]?.defaultImpl?.body.kind).toBe("Apply");
    }
  });

  test("parses protocol with all default methods", () => {
    const source = `
protocol Describable a where
  describe : a -> String
  describe _ = "A value"
  longDescription : a -> String
  longDescription x = describe x
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Describable");
      expect(decl.methods).toHaveLength(2);

      // describe has default with wildcard pattern
      expect(decl.methods[0]?.name).toBe("describe");
      expect(decl.methods[0]?.defaultImpl).toBeDefined();
      expect(decl.methods[0]?.defaultImpl?.args[0]?.kind).toBe(
        "WildcardPattern"
      );

      // longDescription has default referencing describe
      expect(decl.methods[1]?.name).toBe("longDescription");
      expect(decl.methods[1]?.defaultImpl).toBeDefined();
    }
  });

  test("parses protocol with operator method and default", () => {
    const source = `
protocol Eq a where
  (==) : a -> a -> Bool
  (/=) : a -> a -> Bool
  (/=) x y = not (x == y)
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Eq");
      expect(decl.methods).toHaveLength(2);

      // (==) has no default
      expect(decl.methods[0]?.name).toBe("==");
      expect(decl.methods[0]?.defaultImpl).toBeUndefined();

      // (/=) has default
      expect(decl.methods[1]?.name).toBe("/=");
      expect(decl.methods[1]?.defaultImpl).toBeDefined();
    }
  });

  test("parses protocol method with default but no type annotation", () => {
    const source = `
protocol Helper a where
  show : a -> String
  debug x = "Debug: " ++ show x
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Helper");
      expect(decl.methods).toHaveLength(2);

      // show has explicit type
      expect(decl.methods[0]?.name).toBe("show");
      expect(decl.methods[0]?.type).toBeDefined();
      expect(decl.methods[0]?.defaultImpl).toBeUndefined();

      // debug has no type annotation but has default implementation
      expect(decl.methods[1]?.name).toBe("debug");
      expect(decl.methods[1]?.type).toBeUndefined();
      expect(decl.methods[1]?.defaultImpl).toBeDefined();
      expect(decl.methods[1]?.defaultImpl?.args).toHaveLength(1);
    }
  });

  test("parses protocol with all methods having defaults and no type annotations", () => {
    const source = `
protocol Defaulted a where
  foo x = x
  bar x y = foo x
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("ProtocolDeclaration");

    if (decl?.kind === "ProtocolDeclaration") {
      expect(decl.name).toBe("Defaulted");
      expect(decl.methods).toHaveLength(2);

      // foo has no type annotation
      expect(decl.methods[0]?.name).toBe("foo");
      expect(decl.methods[0]?.type).toBeUndefined();
      expect(decl.methods[0]?.defaultImpl).toBeDefined();

      // bar has no type annotation
      expect(decl.methods[1]?.name).toBe("bar");
      expect(decl.methods[1]?.type).toBeUndefined();
      expect(decl.methods[1]?.defaultImpl).toBeDefined();
    }
  });

  test("errors on protocol method without type annotation or default", () => {
    const source = `
protocol Invalid a where
  badMethod
`;
    expect(() => parse(source)).toThrow();
  });
});

describe("Type Declaration with implementing Parsing", () => {
  test("parses type with single implementing protocol", () => {
    const source = `
type Color = Red | Green | Blue
  implementing Show
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");

    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Color");
      expect(decl.implementing).toEqual(["Show"]);
    }
  });

  test("parses type with multiple implementing protocols", () => {
    const source = `
type Person = Person String Int
  implementing Show, Eq, Describable
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");

    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Person");
      expect(decl.implementing).toEqual(["Show", "Eq", "Describable"]);
    }
  });

  test("parses type without implementing clause", () => {
    const source = `
type Maybe a = Just a | Nothing
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(1);

    const decl = program.declarations[0];
    expect(decl?.kind).toBe("TypeDeclaration");

    if (decl?.kind === "TypeDeclaration") {
      expect(decl.name).toBe("Maybe");
      expect(decl.implementing).toBeUndefined();
    }
  });
});

describe("Qualified Type Parsing", () => {
  test("parses type annotation with single constraint", () => {
    const source = `
add : Num a => a -> a -> a
add x y = x
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(2);

    const annDecl = program.declarations[0];
    expect(annDecl?.kind).toBe("TypeAnnotationDeclaration");

    if (annDecl?.kind === "TypeAnnotationDeclaration") {
      expect(annDecl.name).toBe("add");
      expect(annDecl.annotation.kind).toBe("QualifiedType");

      if (annDecl.annotation.kind === "QualifiedType") {
        expect(annDecl.annotation.constraints).toHaveLength(1);
        expect(annDecl.annotation.constraints[0]?.protocolName).toBe("Num");
      }
    }
  });

  test("parses type annotation with multiple constraints", () => {
    const source = `
showSum : (Num a, Show a) => a -> a -> String
showSum x y = "result"
`;
    const program = parse(source);
    expect(program.declarations).toHaveLength(2);

    const annDecl = program.declarations[0];
    expect(annDecl?.kind).toBe("TypeAnnotationDeclaration");

    if (annDecl?.kind === "TypeAnnotationDeclaration") {
      expect(annDecl.name).toBe("showSum");
      expect(annDecl.annotation.kind).toBe("QualifiedType");

      if (annDecl.annotation.kind === "QualifiedType") {
        expect(annDecl.annotation.constraints).toHaveLength(2);
        expect(annDecl.annotation.constraints[0]?.protocolName).toBe("Num");
        expect(annDecl.annotation.constraints[1]?.protocolName).toBe("Show");
      }
    }
  });
});
