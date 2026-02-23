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
      [TokenKind.Newline, "\n"],
      [TokenKind.Keyword, "import"],
      [TokenKind.UpperIdentifier, "Html"],
      [TokenKind.Keyword, "exposing"],
      [TokenKind.LParen, "("],
      [TokenKind.LowerIdentifier, "text"],
      [TokenKind.RParen, ")"],
      [TokenKind.Newline, "\n"],
      [TokenKind.LowerIdentifier, "main"],
      [TokenKind.Colon, ":"],
      [TokenKind.UpperIdentifier, "Html"],
      [TokenKind.LowerIdentifier, "msg"],
      [TokenKind.Newline, "\n"],
      [TokenKind.LowerIdentifier, "main"],
      [TokenKind.Equals, "="],
      [TokenKind.Newline, "\n"],
      [TokenKind.Keyword, "let"],
      [TokenKind.Newline, "\n"],
      [TokenKind.LowerIdentifier, "value"],
      [TokenKind.Equals, "="],
      [TokenKind.String, '"ok"'],
      [TokenKind.Newline, "\n"],
      [TokenKind.Keyword, "in"],
      [TokenKind.Newline, "\n"],
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
      [TokenKind.Newline, "\n"],
      [TokenKind.Equals, "="],
      [TokenKind.Number, "42"],
      [TokenKind.Eof, ""],
    ]);
  });
});
