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
