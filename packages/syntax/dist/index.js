// @bun
// src/operators.ts
var OPERATOR_CHARS = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));
var CHAR_TO_IDENTIFIER = {
  ".": "_DOT",
  "+": "_PLUS",
  "-": "_MINUS",
  "*": "_STAR",
  "/": "_SLASH",
  "%": "_PERCENT",
  "^": "_CARET",
  "<": "_LT",
  ">": "_GT",
  "=": "_EQ",
  "|": "_PIPE",
  "&": "_AMP",
  "!": "_BANG",
  ":": "_COLON",
  "~": "_TILDE",
  $: "_DOLLAR",
  "#": "_HASH",
  "@": "_AT",
  "?": "_QUESTION",
  "\\": "_BACKSLASH"
};
function sanitizeOperator(lexeme) {
  if (lexeme.length === 0)
    return lexeme;
  const parts = [];
  for (const char of lexeme) {
    const mapping = CHAR_TO_IDENTIFIER[char];
    if (mapping) {
      parts.push(mapping);
    } else {
      return lexeme;
    }
  }
  return parts.join("");
}
function isOperatorChar(char) {
  return OPERATOR_CHARS.has(char);
}
var BUILTIN_OPERATORS = [
  {
    symbol: "&&",
    fixity: { associativity: "right", precedence: 3 },
    isShortCircuit: true,
    helper: { name: "_AMP_AMP", impl: "(a) => (b) => a && b()" }
  },
  {
    symbol: "||",
    fixity: { associativity: "right", precedence: 2 },
    isShortCircuit: true,
    helper: { name: "_PIPE_PIPE", impl: "(a) => (b) => a || b()" }
  }
];
var SHORT_CIRCUIT_OPERATORS = new Set(BUILTIN_OPERATORS.filter((op) => op.isShortCircuit).map((op) => op.symbol));
var BUILTIN_OPERATOR_FIXITY = Object.fromEntries(BUILTIN_OPERATORS.map((op) => [op.symbol, op.fixity]));
var SHORT_CIRCUIT_HELPERS = Object.fromEntries(BUILTIN_OPERATORS.filter((op) => op.helper).map((op) => [
  op.symbol,
  op.helper
]));

// src/index.ts
var TokenKind;
((TokenKind2) => {
  TokenKind2["Keyword"] = "Keyword";
  TokenKind2["LowerIdentifier"] = "LowerIdentifier";
  TokenKind2["UpperIdentifier"] = "UpperIdentifier";
  TokenKind2["Number"] = "Number";
  TokenKind2["String"] = "String";
  TokenKind2["Char"] = "Char";
  TokenKind2["Operator"] = "Operator";
  TokenKind2["LParen"] = "LParen";
  TokenKind2["RParen"] = "RParen";
  TokenKind2["LBrace"] = "LBrace";
  TokenKind2["RBrace"] = "RBrace";
  TokenKind2["LBracket"] = "LBracket";
  TokenKind2["RBracket"] = "RBracket";
  TokenKind2["Comma"] = "Comma";
  TokenKind2["Dot"] = "Dot";
  TokenKind2["Range"] = "Range";
  TokenKind2["Colon"] = "Colon";
  TokenKind2["Equals"] = "Equals";
  TokenKind2["Pipe"] = "Pipe";
  TokenKind2["Backslash"] = "Backslash";
  TokenKind2["Newline"] = "Newline";
  TokenKind2["BlockStart"] = "BlockStart";
  TokenKind2["BlockSep"] = "BlockSep";
  TokenKind2["BlockEnd"] = "BlockEnd";
  TokenKind2["Eof"] = "Eof";
})(TokenKind ||= {});
var KEYWORDS = [
  "if",
  "then",
  "else",
  "let",
  "in",
  "case",
  "of",
  "type",
  "alias",
  "module",
  "import",
  "exposing",
  "as",
  "port",
  "infix",
  "infixl",
  "infixr",
  "protocol",
  "implement",
  "where"
];
var KEYWORD_SET = new Set(KEYWORDS);
function isKeyword(value) {
  return KEYWORD_SET.has(value);
}
var BUILTIN_MODULE_NAME = "__builtin__";
var BOOL_TYPE_NAME = "Bool";
var UNIT_TYPE_NAME = "Unit";
export {
  sanitizeOperator,
  isOperatorChar,
  isKeyword,
  UNIT_TYPE_NAME,
  TokenKind,
  SHORT_CIRCUIT_OPERATORS,
  SHORT_CIRCUIT_HELPERS,
  OPERATOR_CHARS,
  KEYWORDS,
  CHAR_TO_IDENTIFIER,
  BUILTIN_OPERATOR_FIXITY,
  BUILTIN_OPERATORS,
  BUILTIN_MODULE_NAME,
  BOOL_TYPE_NAME
};
