import { describe, expect, test } from "bun:test";
import { lex } from "../src/index.ts";
import { TokenKind } from "@vibe/syntax";

describe("lex", () => {
  test("lexes a small Elm-style module", () => {
    const source = `module Main exposing (..)
import Html exposing (text)

main : Html msg
main =
  let
    value = "ok"
  in
    text value
`;

    const tokens = lex(source).map((token) => [token.kind, token.lexeme]);

    expect(tokens).toEqual([
      [TokenKind.Keyword, "module"],
      [TokenKind.UpperIdentifier, "Main"],
      [TokenKind.Keyword, "exposing"],
      [TokenKind.LParen, "("],
      [TokenKind.Range, ".."],
      [TokenKind.RParen, ")"],
      [TokenKind.Keyword, "import"],
      [TokenKind.UpperIdentifier, "Html"],
      [TokenKind.Keyword, "exposing"],
      [TokenKind.LParen, "("],
      [TokenKind.LowerIdentifier, "text"],
      [TokenKind.RParen, ")"],
      [TokenKind.LowerIdentifier, "main"],
      [TokenKind.Colon, ":"],
      [TokenKind.UpperIdentifier, "Html"],
      [TokenKind.LowerIdentifier, "msg"],
      [TokenKind.LowerIdentifier, "main"],
      [TokenKind.Equals, "="],
      [TokenKind.Keyword, "let"],
      [TokenKind.LowerIdentifier, "value"],
      [TokenKind.Equals, "="],
      [TokenKind.String, '"ok"'],
      [TokenKind.Keyword, "in"],
      [TokenKind.LowerIdentifier, "text"],
      [TokenKind.LowerIdentifier, "value"],
      [TokenKind.Eof, ""],
    ]);
  });

  test("skips line and nested block comments", () => {
    const source = `value -- inline comment
  {- block comment
     {- nested -}
  -}
  = 42
`;

    const tokens = lex(source).map((token) => [token.kind, token.lexeme]);

    expect(tokens).toEqual([
      [TokenKind.LowerIdentifier, "value"],
      [TokenKind.Equals, "="],
      [TokenKind.Number, "42"],
      [TokenKind.Eof, ""],
    ]);
  });
});
