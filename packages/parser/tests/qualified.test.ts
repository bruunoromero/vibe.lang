import { describe, expect, test } from "bun:test";
import { parse } from "../src/index.ts";

describe("qualified names", () => {
  test("parses qualified type in annotation", () => {
    const source = `module Test exposing (..)
    
import Vibe.Result as R

func : R.Result String Int
func = R.Ok 1
`;
    const program = parse(source);
    expect(program.declarations.length).toBe(2);
    
    
    // Check type annotation
    const typeDecl = program.declarations[0];
    expect(typeDecl).toBeDefined();
    expect(typeDecl!.kind).toBe("TypeAnnotationDeclaration");
    if (typeDecl!.kind === "TypeAnnotationDeclaration") {
      const typeDeclNarrowed = typeDecl as Extract<typeof typeDecl, { kind: "TypeAnnotationDeclaration" }>;
      expect(typeDeclNarrowed.annotation.kind).toBe("TypeRef");
      if (typeDeclNarrowed.annotation.kind === "TypeRef") {
        expect(typeDeclNarrowed.annotation.name).toBe("R.Result");
      }
    }

    // Check value declaration (pattern/expression)
    const valDecl = program.declarations[1];
    expect(valDecl).toBeDefined();
    expect(valDecl!.kind).toBe("ValueDeclaration");
  });

  test("parses qualified patterns", () => {
    const source = `module Test exposing (..)

import Vibe.Result as R

handle : R.Result e a -> a
handle res =
    case res of
        R.Ok val -> val
        R.Err _ -> panic "error"
`;
    const program = parse(source);
    const decl = program.declarations[1];
    expect(decl).toBeDefined();
    expect(decl!.kind).toBe("ValueDeclaration");
    // Verify structure if needed, but successful parse is the main goal here
  });
});
