// @bun
// ../vibe-lexer/dist/Vibe/Vibe/Basics.js
var not = (b) => b ? false : true;
var flip = (f) => (x) => (y) => f(y)(x);

// ../vibe-lexer/dist/Vibe/Vibe/Int.ffi.js
var intAdd = (a, b) => a + b | 0;
var intSub = (a, b) => a - b | 0;
var intMul = (a, b) => a * b | 0;
var intDiv = (a, b) => Math.trunc(a / b);
var intMod = (a, b) => a % b;
var numEq = (a, b) => a === b;
var numLt = (a, b) => a < b;
var numGt = (a, b) => a > b;
var numToString = (n) => n.toString();

// ../vibe-lexer/dist/Vibe/Vibe/Int.js
var _PIPE_PIPE = (a) => (b) => a || b();
var _add = ($a0) => ($a1) => intAdd($a0, $a1);
var _sub = ($a0) => ($a1) => intSub($a0, $a1);
var _mul = ($a0) => ($a1) => intMul($a0, $a1);
var _div = ($a0) => ($a1) => intDiv($a0, $a1);
var _mod = ($a0) => ($a1) => intMod($a0, $a1);
var _eq = ($a0) => ($a1) => numEq($a0, $a1);
var _lt = ($a0) => ($a1) => numLt($a0, $a1);
var _gt = ($a0) => ($a1) => numGt($a0, $a1);
var _toString = ($a0) => numToString($a0);
var _negate = (x) => -x;
var $default_Ord_Int__LT_EQ = (x) => (y) => _PIPE_PIPE(_lt(x)(y))(() => $dict_Eq_Int._EQ_EQ(x)(y));
var $default_Ord_Int__GT_EQ = (x) => (y) => _PIPE_PIPE(_gt(x)(y))(() => $dict_Eq_Int._EQ_EQ(x)(y));
var $dict_Ord_Int = {
  _LT: _lt,
  _GT: _gt,
  _LT_EQ: $default_Ord_Int__LT_EQ,
  _GT_EQ: $default_Ord_Int__GT_EQ
};
var $default_Eq_Int__SLASH_EQ = (x) => (y) => not(_eq(x)(y));
var $dict_Eq_Int = {
  _EQ_EQ: _eq,
  _SLASH_EQ: $default_Eq_Int__SLASH_EQ
};
var $dict_Num_Int = {
  _PLUS: _add,
  _MINUS: _sub,
  _STAR: _mul,
  negate: _negate
};
var $dict_Integral_Int = {
  _SLASH_SLASH: _div,
  _PERCENT: _mod
};
var $dict_Show_Int = {
  toString: _toString
};

// ../vibe-lexer/dist/Vibe/Vibe/Float.ffi.js
var numToString2 = (n) => n.toString();

// ../vibe-lexer/dist/Vibe/Vibe/Float.js
var _toString2 = ($a0) => numToString2($a0);
var $dict_Show_Float = {
  toString: _toString2
};

// ../vibe-lexer/dist/Vibe/Vibe/String.ffi.js
var stringAppend = (a, b) => a + b;
var stringEq = (a, b) => a === b;
var parseInt = (just, nothing, s) => {
  const n = Number.parseInt(s, 10);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var parseFloat = (just, nothing, s) => {
  const n = Number.parseFloat(s);
  if (Number.isNaN(n)) {
    return nothing;
  }
  return just(n);
};
var stringCharAt = (just, nothing, i, s) => {
  if (i >= 0 && i < s.length) {
    return just(s[i]);
  }
  return nothing;
};
var unsafeCharAt = (i, s) => s[i];
var stringToList = (s) => [...s];

// ../vibe-lexer/dist/Vibe/Vibe/Result.js
var Ok = ($0) => ({ $tag: 0, $0 });
var Err = ($0) => ({ $tag: 1, $0 });

// ../vibe-lexer/dist/Vibe/Vibe/Maybe.js
var Just = ($0) => ({ $tag: 0, $0 });
var Nothing = { $tag: 1 };

// ../vibe-lexer/dist/Vibe/Vibe/String.js
var _append = ($a0) => ($a1) => stringAppend($a0, $a1);
var _eq2 = ($a0) => ($a1) => stringEq($a0, $a1);
var _parseInt = ($a0) => ($a1) => ($a2) => parseInt($a0, $a1, $a2);
var _parseFloat = ($a0) => ($a1) => ($a2) => parseFloat($a0, $a1, $a2);
var length = ($recv) => $recv.length;
var _charAt = ($a0) => ($a1) => ($a2) => ($a3) => stringCharAt($a0, $a1, $a2, $a3);
var unsafeCharAt2 = ($a0) => ($a1) => unsafeCharAt($a0, $a1);
var _slice = ($recv) => ($a0) => ($a1) => $recv.slice($a0, $a1);
var slice = (start) => (end) => (s) => _slice(s)(start)(end);
var toList = ($a0) => stringToList($a0);
var charAt = _charAt(Just)(Nothing);
var toInt = _parseInt(Just)(Nothing);
var toFloat = _parseFloat(Just)(Nothing);
var $default_Eq_String__SLASH_EQ = (x) => (y) => not(_eq2(x)(y));
var $dict_Eq_String = {
  _EQ_EQ: _eq2,
  _SLASH_EQ: $default_Eq_String__SLASH_EQ
};
var $dict_Appendable_String = {
  _PLUS_PLUS: _append
};

// ../vibe-lexer/dist/Vibe/Vibe/Char.ffi.js
var charToString = (a) => a;
var charToCode = (c) => c.codePointAt(0);
var charOrd = (a, b) => a < b;
var charOrdGt = (a, b) => a > b;

// ../vibe-lexer/dist/Vibe/Vibe/Char.js
var _PIPE_PIPE2 = (a) => (b) => a || b();
var _AMP_AMP = (a) => (b) => a && b();
var _toString3 = ($a0) => charToString($a0);
var _lt2 = ($a0) => ($a1) => charOrd($a0, $a1);
var _gt2 = ($a0) => ($a1) => charOrdGt($a0, $a1);
var toCode = ($a0) => charToCode($a0);
var $default_Ord_Char__LT_EQ = (x) => (y) => _PIPE_PIPE2(_lt2(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
var $default_Ord_Char__GT_EQ = (x) => (y) => _PIPE_PIPE2(_gt2(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
var $dict_Ord_Char = {
  _LT: _lt2,
  _GT: _gt2,
  _LT_EQ: $default_Ord_Char__LT_EQ,
  _GT_EQ: $default_Ord_Char__GT_EQ
};
var isUpper = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("A"))(() => $dict_Ord_Char._LT_EQ(c)("Z"));
var isLower = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("a"))(() => $dict_Ord_Char._LT_EQ(c)("z"));
var isAlpha = (c) => _PIPE_PIPE2(isUpper(c))(() => isLower(c));
var isDigit = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("0"))(() => $dict_Ord_Char._LT_EQ(c)("9"));
var $impl_Eq_Char__EQ_EQ = (x) => (y) => $dict_Eq_String._EQ_EQ($dict_Show_Char.toString(x))($dict_Show_Char.toString(y));
var $default_Eq_Char__SLASH_EQ = (x) => (y) => not($dict_Eq_Char._EQ_EQ(x)(y));
var $dict_Eq_Char = {
  _EQ_EQ: $impl_Eq_Char__EQ_EQ,
  _SLASH_EQ: $default_Eq_Char__SLASH_EQ
};
var $dict_Show_Char = {
  toString: _toString3
};

// ../vibe-lexer/dist/Vibe/Vibe/List.ffi.js
var listNth = (just, nothing, n, lst) => {
  if (n >= 0 && n < lst.length) {
    return just(lst[n]);
  }
  return nothing;
};

// ../vibe-lexer/dist/Vibe/Vibe/List.js
var _concat = ($recv) => ($a0) => $recv.concat($a0);
var _COLON_COLON = (x) => (xs) => _concat([x])(xs);
var _map = ($recv) => ($a0) => $recv.map($a0);
var map = flip(_map);
var _filter = ($recv) => ($a0) => $recv.filter($a0);
var filter = flip(_filter);
var _nth = ($a0) => ($a1) => ($a2) => ($a3) => listNth($a0, $a1, $a2, $a3);
var nth = _nth(Just)(Nothing);
// ../vibe-lexer/dist/VibeLexer/VibeLexer/Types.js
var LowerIdentifier = { $tag: 0 };
var UpperIdentifier = { $tag: 1 };
var Keyword = { $tag: 2 };
var NumberToken = { $tag: 3 };
var StringToken = { $tag: 4 };
var CharToken = { $tag: 5 };
var Operator = { $tag: 6 };
var Range = { $tag: 7 };
var Backslash = { $tag: 8 };
var LParen = { $tag: 9 };
var RParen = { $tag: 10 };
var LBrace = { $tag: 11 };
var RBrace = { $tag: 12 };
var LBracket = { $tag: 13 };
var RBracket = { $tag: 14 };
var Comma = { $tag: 15 };
var Dot = { $tag: 16 };
var Colon = { $tag: 17 };
var Equals = { $tag: 18 };
var Pipe = { $tag: 19 };
var Newline = { $tag: 20 };
var Eof = { $tag: 24 };

// ../vibe-lexer/dist/Json/Json/Encode.js
var JsonString = ($0) => ({ $tag: 0, $0 });
var JsonInt = ($0) => ({ $tag: 1, $0 });
var JsonBool = ($0) => ({ $tag: 3, $0 });
var JsonArray = ($0) => ({ $tag: 5, $0 });
var JsonObject = ($0) => ({ $tag: 6, $0 });
var string = (s) => JsonString(s);
var int = (n) => JsonInt(n);
var bool = (b) => JsonBool(b);
var hexDigit = (n) => (($match_0) => {
  if ($match_0 === 0) {
    return "0";
  }
  if ($match_0 === 1) {
    return "1";
  }
  if ($match_0 === 2) {
    return "2";
  }
  if ($match_0 === 3) {
    return "3";
  }
  if ($match_0 === 4) {
    return "4";
  }
  if ($match_0 === 5) {
    return "5";
  }
  if ($match_0 === 6) {
    return "6";
  }
  if ($match_0 === 7) {
    return "7";
  }
  if ($match_0 === 8) {
    return "8";
  }
  if ($match_0 === 9) {
    return "9";
  }
  if ($match_0 === 10) {
    return "a";
  }
  if ($match_0 === 11) {
    return "b";
  }
  if ($match_0 === 12) {
    return "c";
  }
  if ($match_0 === 13) {
    return "d";
  }
  if ($match_0 === 14) {
    return "e";
  }
  {
    return "f";
  }
  throw new Error("Pattern match failed");
})(n);
var hexByte = (n) => ((hi) => ((lo) => $dict_Appendable_String._PLUS_PLUS(hexDigit(hi))(hexDigit(lo)))($dict_Integral_Int._PERCENT(n)(16)))($dict_Integral_Int._SLASH_SLASH(n)(16));
var escapeControl = (code) => $dict_Appendable_String._PLUS_PLUS("\\u00")(hexByte(code));
var escapeChar = (c) => (($match_1) => {
  if ($match_1 === '"') {
    return "\\\"";
  }
  if ($match_1 === "\\") {
    return "\\\\";
  }
  if ($match_1 === `
`) {
    return "\\n";
  }
  if ($match_1 === "\r") {
    return "\\r";
  }
  if ($match_1 === "\t") {
    return "\\t";
  }
  {
    return ((code) => $dict_Ord_Int._LT(code)(32) ? escapeControl(code) : $dict_Show_Char.toString(c))(toCode(c));
  }
  throw new Error("Pattern match failed");
})(c);
var escapeChars = (chars) => ((go) => go(chars)(""))((cs) => (acc) => {
  while (true) {
    {
      const $match_2 = cs;
      if (Array.isArray($match_2) && $match_2.length === 0) {
        return acc;
      }
      if (Array.isArray($match_2) && $match_2.length >= 1) {
        const c = $match_2[0];
        const rest = $match_2.slice(1);
        [cs, acc] = [rest, $dict_Appendable_String._PLUS_PLUS(acc)(escapeChar(c))];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
var renderString = (s) => $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS('"')(escapeChars(toList(s))))('"');
var renderFloat = (n) => ((s) => (($match_3) => {
  if ($match_3 === "NaN") {
    return "null";
  }
  if ($match_3 === "Infinity") {
    return "null";
  }
  if ($match_3 === "-Infinity") {
    return "null";
  }
  {
    return s;
  }
  throw new Error("Pattern match failed");
})(s))($dict_Show_Float.toString(n));
var repeatSpaces = (n) => $dict_Ord_Int._LT_EQ(n)(0) ? "" : $dict_Appendable_String._PLUS_PLUS(" ")(repeatSpaces($dict_Num_Int._MINUS(n)(1)));
var makeIndent = (indent) => (depth) => repeatSpaces($dict_Num_Int._STAR(indent)(depth));
var renderValue;
var renderArray;
var renderObject;
var joinValues;
var joinValuesIndented;
var joinPairs;
var joinPairsIndented;
renderValue = (indent) => (depth) => (val) => (($match_4) => {
  if ($match_4.$tag === 0) {
    const s = $match_4.$0;
    return renderString(s);
  }
  if ($match_4.$tag === 1) {
    const n = $match_4.$0;
    return $dict_Show_Int.toString(n);
  }
  if ($match_4.$tag === 2) {
    const n = $match_4.$0;
    return renderFloat(n);
  }
  if ($match_4.$tag === 3) {
    const b = $match_4.$0;
    return b ? "true" : "false";
  }
  if ($match_4.$tag === 4) {
    return "null";
  }
  if ($match_4.$tag === 5) {
    const items = $match_4.$0;
    return renderArray(indent)(depth)(items);
  }
  if ($match_4.$tag === 6) {
    const pairs = $match_4.$0;
    return renderObject(indent)(depth)(pairs);
  }
  throw new Error("Pattern match failed");
})(val);
renderArray = (indent) => (depth) => (items) => (($match_5) => {
  if (Array.isArray($match_5) && $match_5.length === 0) {
    return "[]";
  }
  {
    return $dict_Eq_Int._EQ_EQ(indent)(0) ? $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("[")(joinValues(indent)(depth)(items)))("]") : ((newDepth) => ((pad) => ((innerPad) => $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(`[
`)(pad))(joinValuesIndented(indent)(newDepth)(items)))(`
`))(innerPad))("]"))(makeIndent(indent)(depth)))(makeIndent(indent)(newDepth)))($dict_Num_Int._PLUS(depth)(1));
  }
  throw new Error("Pattern match failed");
})(items);
renderObject = (indent) => (depth) => (pairs) => (($match_6) => {
  if (Array.isArray($match_6) && $match_6.length === 0) {
    return "{}";
  }
  {
    return $dict_Eq_Int._EQ_EQ(indent)(0) ? $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("{")(joinPairs(indent)(depth)(pairs)))("}") : ((newDepth) => ((pad) => ((innerPad) => $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(`{
`)(pad))(joinPairsIndented(indent)(newDepth)(pairs)))(`
`))(innerPad))("}"))(makeIndent(indent)(depth)))(makeIndent(indent)(newDepth)))($dict_Num_Int._PLUS(depth)(1));
  }
  throw new Error("Pattern match failed");
})(pairs);
joinValues = (indent) => (depth) => (items) => ((go) => go(items)(""))((xs) => (acc) => {
  while (true) {
    {
      const $match_7 = xs;
      if (Array.isArray($match_7) && $match_7.length === 0) {
        return acc;
      }
      if (Array.isArray($match_7) && $match_7.length === 1) {
        const x = $match_7[0];
        return $dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x));
      }
      if (Array.isArray($match_7) && $match_7.length >= 1) {
        const x = $match_7[0];
        const rest = $match_7.slice(1);
        [xs, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)))(",")];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
joinValuesIndented = (indent) => (depth) => (items) => ((pad) => ((go) => go(items)(""))((xs) => (acc) => {
  while (true) {
    {
      const $match_8 = xs;
      if (Array.isArray($match_8) && $match_8.length === 0) {
        return acc;
      }
      if (Array.isArray($match_8) && $match_8.length === 1) {
        const x = $match_8[0];
        return $dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x));
      }
      if (Array.isArray($match_8) && $match_8.length >= 1) {
        const x = $match_8[0];
        const rest = $match_8.slice(1);
        [xs, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)))(`,
`))(pad)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
}))(makeIndent(indent)(depth));
joinPairs = (indent) => (depth) => (pairs) => ((go) => go(pairs)(""))((ps) => (acc) => {
  while (true) {
    {
      const $match_9 = ps;
      if (Array.isArray($match_9) && $match_9.length === 0) {
        return acc;
      }
      if (Array.isArray($match_9) && $match_9.length === 1) {
        const k = $match_9[0][0];
        const v = $match_9[0][1];
        return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(":"))(renderValue(indent)(depth)(v));
      }
      if (Array.isArray($match_9) && $match_9.length >= 1) {
        const k = $match_9[0][0];
        const v = $match_9[0][1];
        const rest = $match_9.slice(1);
        [ps, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(":"))(renderValue(indent)(depth)(v)))(",")];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
});
joinPairsIndented = (indent) => (depth) => (pairs) => ((pad) => ((go) => go(pairs)(""))((ps) => (acc) => {
  while (true) {
    {
      const $match_10 = ps;
      if (Array.isArray($match_10) && $match_10.length === 0) {
        return acc;
      }
      if (Array.isArray($match_10) && $match_10.length === 1) {
        const k = $match_10[0][0];
        const v = $match_10[0][1];
        return $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(": "))(renderValue(indent)(depth)(v));
      }
      if (Array.isArray($match_10) && $match_10.length >= 1) {
        const k = $match_10[0][0];
        const v = $match_10[0][1];
        const rest = $match_10.slice(1);
        [ps, acc] = [rest, $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(": "))(renderValue(indent)(depth)(v)))(`,
`))(pad)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
}))(makeIndent(indent)(depth));
var encode = (indent) => (val) => renderValue(indent)(0)(val);
var list = (encode2) => (items) => JsonArray(map(encode2)(items));
var object = (pairs) => JsonObject(pairs);

// ../vibe-lexer/dist/VibeLexer/VibeLexer/Json.js
var tokenKindToString = (kind) => (($match_0) => {
  if ($match_0.$tag === 0) {
    return "LowerIdentifier";
  }
  if ($match_0.$tag === 1) {
    return "UpperIdentifier";
  }
  if ($match_0.$tag === 2) {
    return "Keyword";
  }
  if ($match_0.$tag === 3) {
    return "Number";
  }
  if ($match_0.$tag === 4) {
    return "String";
  }
  if ($match_0.$tag === 5) {
    return "Char";
  }
  if ($match_0.$tag === 6) {
    return "Operator";
  }
  if ($match_0.$tag === 7) {
    return "Range";
  }
  if ($match_0.$tag === 8) {
    return "Backslash";
  }
  if ($match_0.$tag === 9) {
    return "LParen";
  }
  if ($match_0.$tag === 10) {
    return "RParen";
  }
  if ($match_0.$tag === 11) {
    return "LBrace";
  }
  if ($match_0.$tag === 12) {
    return "RBrace";
  }
  if ($match_0.$tag === 13) {
    return "LBracket";
  }
  if ($match_0.$tag === 14) {
    return "RBracket";
  }
  if ($match_0.$tag === 15) {
    return "Comma";
  }
  if ($match_0.$tag === 16) {
    return "Dot";
  }
  if ($match_0.$tag === 17) {
    return "Colon";
  }
  if ($match_0.$tag === 18) {
    return "Equals";
  }
  if ($match_0.$tag === 19) {
    return "Pipe";
  }
  if ($match_0.$tag === 20) {
    return "Newline";
  }
  if ($match_0.$tag === 21) {
    return "BlockStart";
  }
  if ($match_0.$tag === 22) {
    return "BlockSep";
  }
  if ($match_0.$tag === 23) {
    return "BlockEnd";
  }
  if ($match_0.$tag === 24) {
    return "Eof";
  }
  throw new Error("Pattern match failed");
})(kind);
var encodePosition = (pos) => object([["offset", int(pos.offset)], ["line", int(pos.line)], ["column", int(pos.column)]]);
var encodeSpan = (span) => object([["start", encodePosition(span.start)], ["end", encodePosition(span.end)]]);
var encodeToken = (tok) => object([["kind", string(tokenKindToString(tok.kind))], ["lexeme", string(tok.lexeme)], ["span", encodeSpan(tok.span)]]);
var lexToJson = (lexFn) => (source) => (($match_1) => {
  if ($match_1.$tag === 0) {
    const tokens = $match_1.$0;
    return encode(0)(object([["ok", bool(true)], ["tokens", list(encodeToken)(tokens)]]));
  }
  if ($match_1.$tag === 1) {
    const err = $match_1.$0;
    return encode(0)(object([["ok", bool(false)], ["message", string(err.message)], ["span", encodeSpan(err.span)]]));
  }
  throw new Error("Pattern match failed");
})(lexFn(source));

// ../vibe-lexer/dist/VibeLexer/VibeLexer.js
var _PIPE_PIPE3 = (a) => (b) => a || b();
var _AMP_AMP2 = (a) => (b) => a && b();
var initState = (source) => ({ source, index: 0, line: 1, column: 1, sourceLen: length(source) });
var isAtEnd = (state) => $dict_Ord_Int._GT_EQ(state.index)(state.sourceLen);
var peek = (state) => charAt(state.index)(state.source);
var peekAt = (offset) => (state) => charAt($dict_Num_Int._PLUS(state.index)(offset))(state.source);
var position = (state) => ({ offset: state.index, line: state.line, column: state.column });
var advance = (state) => ((ch) => ((newIndex) => ((newState) => [ch, newState])((($match_0) => {
  if ($match_0 === `
`) {
    return { ...state, index: newIndex, line: $dict_Num_Int._PLUS(state.line)(1), column: 1 };
  }
  if ($match_0 === "\t") {
    return { ...state, index: newIndex, column: $dict_Num_Int._PLUS($dict_Num_Int._STAR($dict_Integral_Int._SLASH_SLASH($dict_Num_Int._MINUS(state.column)(1))(8))(8))(9) };
  }
  {
    return { ...state, index: newIndex, column: $dict_Num_Int._PLUS(state.column)(1) };
  }
  throw new Error("Pattern match failed");
})(ch)))($dict_Num_Int._PLUS(state.index)(1)))(unsafeCharAt2(state.index)(state.source));
var skip = (state) => (($match_1) => {
  {
    const s = $match_1[1];
    return s;
  }
  throw new Error("Pattern match failed");
})(advance(state));
var skip2 = (state) => skip(skip(state));
var sliceFrom = (startOffset) => (state) => slice(startOffset)(state.index)(state.source);
var isIdentifierStart = (c) => _PIPE_PIPE3(isAlpha(c))(() => $dict_Eq_Char._EQ_EQ(c)("_"));
var isIdentifierPart = (c) => _PIPE_PIPE3(isIdentifierStart(c))(() => isDigit(c));
var isWhitespace = (c) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(" "))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("\t"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(`
`))(() => $dict_Eq_Char._EQ_EQ(c)("\r"))));
var isOperatorChar = (c) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("!"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("#"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("$"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("%"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("&"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("*"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("+"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("."))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("/"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("<"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("="))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(">"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("?"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("@"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("\\"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("^"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("|"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)("~"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(":"))(() => $dict_Eq_Char._EQ_EQ(c)("-"))))))))))))))))))));
var isKeyword = (word) => (($match_2) => {
  if ($match_2 === "if") {
    return true;
  }
  if ($match_2 === "then") {
    return true;
  }
  if ($match_2 === "else") {
    return true;
  }
  if ($match_2 === "let") {
    return true;
  }
  if ($match_2 === "in") {
    return true;
  }
  if ($match_2 === "case") {
    return true;
  }
  if ($match_2 === "of") {
    return true;
  }
  if ($match_2 === "type") {
    return true;
  }
  if ($match_2 === "alias") {
    return true;
  }
  if ($match_2 === "module") {
    return true;
  }
  if ($match_2 === "import") {
    return true;
  }
  if ($match_2 === "exposing") {
    return true;
  }
  if ($match_2 === "as") {
    return true;
  }
  if ($match_2 === "infix") {
    return true;
  }
  if ($match_2 === "infixl") {
    return true;
  }
  if ($match_2 === "infixr") {
    return true;
  }
  if ($match_2 === "protocol") {
    return true;
  }
  if ($match_2 === "implement") {
    return true;
  }
  if ($match_2 === "where") {
    return true;
  }
  {
    return false;
  }
  throw new Error("Pattern match failed");
})(word);
var skipLineComment = (state) => {
  while (true) {
    if (isAtEnd(state)) {
      return state;
    } else {
      {
        const $match_4 = peek(state);
        if ($match_4.$tag === 1) {
          return state;
        }
        if ($match_4.$tag === 0) {
          const c = $match_4.$0;
          if ($dict_Eq_Char._EQ_EQ(c)(`
`)) {
            return state;
          } else {
            state = skip(state);
            continue;
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var skipBlockComment;
var skipBlockCommentPair;
skipBlockComment = (depth) => (state) => {
  while (true) {
    if ($dict_Eq_Int._EQ_EQ(depth)(0)) {
      return Ok(state);
    } else {
      if (isAtEnd(state)) {
        return Err({ message: "Unterminated block comment", span: { start: position(state), end: position(state) } });
      } else {
        {
          const $match_5 = [peek(state), peekAt(1)(state)];
          if ($match_5[0].$tag === 0 && $match_5[1].$tag === 0) {
            const c1 = $match_5[0].$0;
            const c2 = $match_5[1].$0;
            return skipBlockCommentPair(depth)(state)(c1)(c2);
          }
          {
            [depth, state] = [depth, skip(state)];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
    }
  }
};
skipBlockCommentPair = (depth) => (state) => (c1) => (c2) => (($match_6) => {
  if ($match_6 === "{") {
    return (($match_7) => {
      if ($match_7 === "-") {
        return skipBlockComment($dict_Num_Int._PLUS(depth)(1))(skip2(state));
      }
      {
        return skipBlockComment(depth)(skip(state));
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  if ($match_6 === "-") {
    return (($match_8) => {
      if ($match_8 === "}") {
        return skipBlockComment($dict_Num_Int._MINUS(depth)(1))(skip2(state));
      }
      {
        return skipBlockComment(depth)(skip(state));
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return skipBlockComment(depth)(skip(state));
  }
  throw new Error("Pattern match failed");
})(c1);
var skipWhitespaceAndComments;
var skipWsDispatch;
var skipWsMaybeLine;
var skipWsMaybeBlock;
skipWhitespaceAndComments = (sawNl) => (state) => isAtEnd(state) ? Ok({ state, sawNewline: sawNl }) : (($match_9) => {
  if ($match_9.$tag === 1) {
    return Ok({ state, sawNewline: sawNl });
  }
  if ($match_9.$tag === 0) {
    const c = $match_9.$0;
    return skipWsDispatch(sawNl)(state)(c);
  }
  throw new Error("Pattern match failed");
})(peek(state));
skipWsDispatch = (sawNl) => (state) => (c) => isWhitespace(c) ? ((nl) => skipWhitespaceAndComments(nl)(skip(state)))(_PIPE_PIPE3(sawNl)(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(c)(`
`))(() => $dict_Eq_Char._EQ_EQ(c)("\r")))) : (($match_10) => {
  if ($match_10 === "-") {
    return skipWsMaybeLine(sawNl)(state);
  }
  if ($match_10 === "{") {
    return skipWsMaybeBlock(sawNl)(state);
  }
  {
    return Ok({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(c);
skipWsMaybeLine = (sawNl) => (state) => (($match_11) => {
  if ($match_11.$tag === 0) {
    const c2 = $match_11.$0;
    return (($match_12) => {
      if ($match_12 === "-") {
        return ((s1) => skipWhitespaceAndComments(true)(s1))(skipLineComment(state));
      }
      {
        return Ok({ state, sawNewline: sawNl });
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return Ok({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
skipWsMaybeBlock = (sawNl) => (state) => (($match_13) => {
  if ($match_13.$tag === 0) {
    const c2 = $match_13.$0;
    return (($match_14) => {
      if ($match_14 === "-") {
        return ((lineBefore) => ((s2) => (($match_15) => {
          if ($match_15.$tag === 1) {
            const e = $match_15.$0;
            return Err(e);
          }
          if ($match_15.$tag === 0) {
            const s3 = $match_15.$0;
            return ((nl) => skipWhitespaceAndComments(nl)(s3))(_PIPE_PIPE3(sawNl)(() => $dict_Ord_Int._GT(s3.line)(lineBefore)));
          }
          throw new Error("Pattern match failed");
        })(skipBlockComment(1)(s2)))(skip2(state)))(state.line);
      }
      {
        return Ok({ state, sawNewline: sawNl });
      }
      throw new Error("Pattern match failed");
    })(c2);
  }
  {
    return Ok({ state, sawNewline: sawNl });
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var identKind = (text) => (isUpper2) => isKeyword(text) ? Keyword : isUpper2 ? UpperIdentifier : LowerIdentifier;
var consumeIdentifierChars = (state) => {
  while (true) {
    {
      const $match_16 = peek(state);
      if ($match_16.$tag === 1) {
        return state;
      }
      if ($match_16.$tag === 0) {
        const c = $match_16.$0;
        if (_PIPE_PIPE3(isIdentifierPart(c))(() => $dict_Eq_Char._EQ_EQ(c)("'"))) {
          state = skip(state);
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var readIdentifierOrKeyword = (state) => (startPos) => ((startIdx) => (($match_17) => {
  {
    const firstChar = $match_17[0];
    const s1 = $match_17[1];
    return ((isUpper2) => ((s2) => ((text) => ((endPos) => ((span) => ((kind) => [{ kind, lexeme: text, span }, s2])(identKind(text)(isUpper2)))({ start: startPos, end: endPos }))(position(s2)))(sliceFrom(startIdx)(s2)))(consumeIdentifierChars(s1)))(isUpper(firstChar));
  }
  throw new Error("Pattern match failed");
})(advance(state)))(state.index);
var makeNumberToken = (state) => (startIdx) => (startPos) => ((text) => ((endPos) => [{ kind: NumberToken, lexeme: text, span: { start: startPos, end: endPos } }, state])(position(state)))(sliceFrom(startIdx)(state));
var consumeDigits = (state) => {
  while (true) {
    {
      const $match_18 = peek(state);
      if ($match_18.$tag === 1) {
        return state;
      }
      if ($match_18.$tag === 0) {
        const c = $match_18.$0;
        if (isDigit(c)) {
          state = skip(state);
          continue;
        } else {
          return state;
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
var readNumberAfterDot = (s1) => (startIdx) => (startPos) => (($match_19) => {
  if ($match_19.$tag === 0) {
    const d = $match_19.$0;
    return isDigit(d) ? ((s2) => ((s3) => makeNumberToken(s3)(startIdx)(startPos))(consumeDigits(s2)))(skip(s1)) : makeNumberToken(s1)(startIdx)(startPos);
  }
  {
    return makeNumberToken(s1)(startIdx)(startPos);
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(s1));
var readNumberAfterInt = (s1) => (startIdx) => (startPos) => (($match_20) => {
  if ($match_20.$tag === 0) {
    const c = $match_20.$0;
    return (($match_21) => {
      if ($match_21 === ".") {
        return readNumberAfterDot(s1)(startIdx)(startPos);
      }
      {
        return makeNumberToken(s1)(startIdx)(startPos);
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return makeNumberToken(s1)(startIdx)(startPos);
  }
  throw new Error("Pattern match failed");
})(peek(s1));
var readNumber = (state) => (startPos) => ((startIdx) => ((s1) => readNumberAfterInt(s1)(startIdx)(startPos))(consumeDigits(state)))(state.index);
var isValidStringEscape = (esc) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("n"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("r"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("t"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)('"'))(() => $dict_Eq_Char._EQ_EQ(esc)("\\")))));
var readStringBody;
var readStringEscape;
readStringBody = (state) => (startIdx) => (startPos) => {
  while (true) {
    {
      const $match_22 = peek(state);
      if ($match_22.$tag === 1) {
        return Err({ message: "Unterminated string literal", span: { start: startPos, end: position(state) } });
      }
      if ($match_22.$tag === 0) {
        const c = $match_22.$0;
        {
          const $match_23 = c;
          if ($match_23 === '"') {
            {
              const s1 = skip(state);
              {
                const text = sliceFrom(startIdx)(s1);
                {
                  const endPos = position(s1);
                  return Ok([{ kind: StringToken, lexeme: text, span: { start: startPos, end: endPos } }, s1]);
                }
              }
            }
          }
          if ($match_23 === `
`) {
            return Err({ message: "Unterminated string literal", span: { start: startPos, end: position(state) } });
          }
          if ($match_23 === "\\") {
            return readStringEscape(state)(startIdx)(startPos);
          }
          {
            [state, startIdx, startPos] = [skip(state), startIdx, startPos];
            continue;
          }
          throw new Error("Pattern match failed");
        }
      }
      throw new Error("Pattern match failed");
    }
  }
};
readStringEscape = (state) => (startIdx) => (startPos) => ((s1) => (($match_24) => {
  if ($match_24.$tag === 1) {
    return Err({ message: "Unterminated string literal", span: { start: startPos, end: position(s1) } });
  }
  if ($match_24.$tag === 0) {
    const esc = $match_24.$0;
    return isValidStringEscape(esc) ? readStringBody(skip(s1))(startIdx)(startPos) : Err({ message: "Invalid escape in string", span: { start: startPos, end: position(s1) } });
  }
  throw new Error("Pattern match failed");
})(peek(s1)))(skip(state));
var readString = (state) => (startPos) => ((startIdx) => ((s1) => readStringBody(s1)(startIdx)(startPos))(skip(state)))(state.index);
var isValidCharEscape = (esc) => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("n"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("r"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("t"))(() => _PIPE_PIPE3($dict_Eq_Char._EQ_EQ(esc)("'"))(() => $dict_Eq_Char._EQ_EQ(esc)("\\")))));
var expectClosingQuote = (state) => (startIdx) => (startPos) => (($match_25) => {
  if ($match_25.$tag === 0) {
    const c = $match_25.$0;
    return (($match_26) => {
      if ($match_26 === "'") {
        return ((s1) => ((text) => ((endPos) => Ok([{ kind: CharToken, lexeme: text, span: { start: startPos, end: endPos } }, s1]))(position(s1)))(sliceFrom(startIdx)(s1)))(skip(state));
      }
      {
        return Err({ message: "Char literal must contain exactly one character", span: { start: startPos, end: position(state) } });
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Err({ message: "Char literal must contain exactly one character", span: { start: startPos, end: position(state) } });
  }
  throw new Error("Pattern match failed");
})(peek(state));
var readCharEscape = (s1) => (startIdx) => (startPos) => ((s2) => (($match_27) => {
  if ($match_27.$tag === 1) {
    return Err({ message: "Unterminated char literal", span: { start: startPos, end: position(s2) } });
  }
  if ($match_27.$tag === 0) {
    const esc = $match_27.$0;
    return isValidCharEscape(esc) ? ((s3) => expectClosingQuote(s3)(startIdx)(startPos))(skip(s2)) : Err({ message: "Invalid escape in char literal", span: { start: startPos, end: position(s2) } });
  }
  throw new Error("Pattern match failed");
})(peek(s2)))(skip(s1));
var readChar = (state) => (startPos) => ((startIdx) => ((s1) => (($match_28) => {
  if ($match_28.$tag === 1) {
    return Err({ message: "Unterminated char literal", span: { start: startPos, end: position(s1) } });
  }
  if ($match_28.$tag === 0) {
    const c = $match_28.$0;
    return (($match_29) => {
      if ($match_29 === `
`) {
        return Err({ message: "Unterminated char literal", span: { start: startPos, end: position(s1) } });
      }
      if ($match_29 === "\\") {
        return readCharEscape(s1)(startIdx)(startPos);
      }
      {
        return ((s2) => expectClosingQuote(s2)(startIdx)(startPos))(skip(s1));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  throw new Error("Pattern match failed");
})(peek(s1)))(skip(state)))(state.index);
var makeSimpleToken = (state) => (startPos) => (kind) => ((startIdx) => ((s1) => ((text) => ((endPos) => [{ kind, lexeme: text, span: { start: startPos, end: endPos } }, s1])(position(s1)))(sliceFrom(startIdx)(s1)))(skip(state)))(state.index);
var makeTwoCharToken = (state) => (startPos) => (startIdx) => (kind) => ((s2) => ((text) => ((endPos) => [{ kind, lexeme: text, span: { start: startPos, end: endPos } }, s2])(position(s2)))(sliceFrom(startIdx)(s2)))(skip2(state));
var readDot = (state) => (startPos) => (startIdx) => (($match_30) => {
  if ($match_30.$tag === 0) {
    const c = $match_30.$0;
    return (($match_31) => {
      if ($match_31 === ".") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Range)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Dot)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Dot)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readColon = (state) => (startPos) => (startIdx) => (($match_32) => {
  if ($match_32.$tag === 0) {
    const c = $match_32.$0;
    return (($match_33) => {
      if ($match_33 === ":") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Colon)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Colon)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readEquals = (state) => (startPos) => (startIdx) => (($match_34) => {
  if ($match_34.$tag === 0) {
    const c = $match_34.$0;
    return (($match_35) => {
      if ($match_35 === ">") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_35 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Equals)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Equals)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readPipe = (state) => (startPos) => (startIdx) => (($match_36) => {
  if ($match_36.$tag === 0) {
    const c = $match_36.$0;
    return (($match_37) => {
      if ($match_37 === ">") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_37 === "|") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return Ok(Just(makeSimpleToken(state)(startPos)(Pipe)));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return Ok(Just(makeSimpleToken(state)(startPos)(Pipe)));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var consumeOperator;
var consumeOperatorCheck;
consumeOperator = (state) => (($match_38) => {
  if ($match_38.$tag === 1) {
    return state;
  }
  if ($match_38.$tag === 0) {
    const c = $match_38.$0;
    return isOperatorChar(c) ? consumeOperatorCheck(state)(c) : state;
  }
  throw new Error("Pattern match failed");
})(peek(state));
consumeOperatorCheck = (state) => (c) => (($match_39) => {
  if ($match_39 === "-") {
    return (($match_40) => {
      if ($match_40.$tag === 0) {
        const c2 = $match_40.$0;
        return (($match_41) => {
          if ($match_41 === ">") {
            return state;
          }
          {
            return consumeOperator(skip(state));
          }
          throw new Error("Pattern match failed");
        })(c2);
      }
      {
        return consumeOperator(skip(state));
      }
      throw new Error("Pattern match failed");
    })(peekAt(1)(state));
  }
  {
    return consumeOperator(skip(state));
  }
  throw new Error("Pattern match failed");
})(c);
var readLAngle = (state) => (startPos) => (startIdx) => (($match_42) => {
  if ($match_42.$tag === 0) {
    const c = $match_42.$0;
    return (($match_43) => {
      if ($match_43 === "|") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_43 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readDash = (state) => (startPos) => (startIdx) => (($match_44) => {
  if ($match_44.$tag === 0) {
    const c = $match_44.$0;
    return (($match_45) => {
      if ($match_45 === ">") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      if ($match_45 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readGt = (state) => (startPos) => (startIdx) => (($match_46) => {
  if ($match_46.$tag === 0) {
    const c = $match_46.$0;
    return (($match_47) => {
      if ($match_47 === "=") {
        return Ok(Just(makeTwoCharToken(state)(startPos)(startIdx)(Operator)));
      }
      {
        return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  {
    return ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state));
  }
  throw new Error("Pattern match failed");
})(peekAt(1)(state));
var readGenericOperator = (state) => (startPos) => (startIdx) => (c) => isOperatorChar(c) ? ((s1) => ((text) => ((endPos) => Ok(Just([{ kind: Operator, lexeme: text, span: { start: startPos, end: endPos } }, s1])))(position(s1)))(sliceFrom(startIdx)(s1)))(consumeOperator(state)) : Ok(Nothing);
var readPunctuationOrOperator = (state) => (startPos) => ((startIdx) => (($match_48) => {
  if ($match_48.$tag === 1) {
    return Ok(Nothing);
  }
  if ($match_48.$tag === 0) {
    const c = $match_48.$0;
    return (($match_49) => {
      if ($match_49 === "(") {
        return Ok(Just(makeSimpleToken(state)(startPos)(LParen)));
      }
      if ($match_49 === ")") {
        return Ok(Just(makeSimpleToken(state)(startPos)(RParen)));
      }
      if ($match_49 === "{") {
        return Ok(Just(makeSimpleToken(state)(startPos)(LBrace)));
      }
      if ($match_49 === "}") {
        return Ok(Just(makeSimpleToken(state)(startPos)(RBrace)));
      }
      if ($match_49 === "[") {
        return Ok(Just(makeSimpleToken(state)(startPos)(LBracket)));
      }
      if ($match_49 === "]") {
        return Ok(Just(makeSimpleToken(state)(startPos)(RBracket)));
      }
      if ($match_49 === ",") {
        return Ok(Just(makeSimpleToken(state)(startPos)(Comma)));
      }
      if ($match_49 === ".") {
        return readDot(state)(startPos)(startIdx);
      }
      if ($match_49 === ":") {
        return readColon(state)(startPos)(startIdx);
      }
      if ($match_49 === "=") {
        return readEquals(state)(startPos)(startIdx);
      }
      if ($match_49 === "|") {
        return readPipe(state)(startPos)(startIdx);
      }
      if ($match_49 === "<") {
        return readLAngle(state)(startPos)(startIdx);
      }
      if ($match_49 === "-") {
        return readDash(state)(startPos)(startIdx);
      }
      if ($match_49 === ">") {
        return readGt(state)(startPos)(startIdx);
      }
      if ($match_49 === "\\") {
        return Ok(Just(makeSimpleToken(state)(startPos)(Backslash)));
      }
      {
        return readGenericOperator(state)(startPos)(startIdx)(c);
      }
      throw new Error("Pattern match failed");
    })(c);
  }
  throw new Error("Pattern match failed");
})(peek(state)))(state.index);
var readTokenDispatch = (state) => (startPos) => (c) => isIdentifierStart(c) ? Ok(readIdentifierOrKeyword(state)(startPos)) : isDigit(c) ? Ok(readNumber(state)(startPos)) : (($match_50) => {
  if ($match_50 === '"') {
    return readString(state)(startPos);
  }
  if ($match_50 === "'") {
    return readChar(state)(startPos);
  }
  {
    return (($match_51) => {
      if ($match_51.$tag === 1) {
        const e = $match_51.$0;
        return Err(e);
      }
      if ($match_51.$tag === 0 && $match_51.$0.$tag === 1) {
        return Err({ message: $dict_Appendable_String._PLUS_PLUS($dict_Appendable_String._PLUS_PLUS("Unexpected character '")($dict_Show_Char.toString(c)))("'"), span: { start: startPos, end: startPos } });
      }
      if ($match_51.$tag === 0 && $match_51.$0.$tag === 0) {
        const result = $match_51.$0.$0;
        return Ok(result);
      }
      throw new Error("Pattern match failed");
    })(readPunctuationOrOperator(state)(startPos));
  }
  throw new Error("Pattern match failed");
})(c);
var readToken = (state) => (startPos) => (($match_52) => {
  if ($match_52.$tag === 1) {
    return Err({ message: "Unexpected end of input", span: { start: startPos, end: startPos } });
  }
  if ($match_52.$tag === 0) {
    const c = $match_52.$0;
    return readTokenDispatch(state)(startPos)(c);
  }
  throw new Error("Pattern match failed");
})(peek(state));
var maybeInsertNewline = (tokens) => (sawNl) => (hasEmitted) => (state) => _AMP_AMP2(sawNl)(() => hasEmitted) ? ((nlPos) => ((nlToken) => _COLON_COLON(nlToken)(tokens))({ kind: Newline, lexeme: `
`, span: { start: nlPos, end: nlPos } }))(position(state)) : tokens;
var lexLoop = (loop) => {
  while (true) {
    if (isAtEnd(loop.state)) {
      return Ok(loop);
    } else {
      {
        const $match_53 = skipWhitespaceAndComments(loop.sawNewline)(loop.state);
        if ($match_53.$tag === 1) {
          const e = $match_53.$0;
          return Err(e);
        }
        if ($match_53.$tag === 0) {
          const skipResult = $match_53.$0;
          if (isAtEnd(skipResult.state)) {
            return Ok({ ...loop, state: skipResult.state, sawNewline: skipResult.sawNewline });
          } else {
            {
              const tokens1 = maybeInsertNewline(loop.tokens)(skipResult.sawNewline)(loop.hasEmittedToken)(skipResult.state);
              {
                const startPos = position(skipResult.state);
                {
                  const $match_54 = readToken(skipResult.state)(startPos);
                  if ($match_54.$tag === 1) {
                    const e = $match_54.$0;
                    return Err(e);
                  }
                  if ($match_54.$tag === 0) {
                    const tok = $match_54.$0[0];
                    const newState = $match_54.$0[1];
                    loop = { tokens: _COLON_COLON(tok)(tokens1), state: newState, sawNewline: false, hasEmittedToken: true };
                    continue;
                  }
                  throw new Error("Pattern match failed");
                }
              }
            }
          }
        }
        throw new Error("Pattern match failed");
      }
    }
  }
};
var reverseHelper = (lst) => (acc) => {
  while (true) {
    {
      const $match_55 = lst;
      if (Array.isArray($match_55) && $match_55.length === 0) {
        return acc;
      }
      if (Array.isArray($match_55) && $match_55.length >= 1) {
        const x = $match_55[0];
        const xs = $match_55.slice(1);
        [lst, acc] = [xs, _COLON_COLON(x)(acc)];
        continue;
      }
      throw new Error("Pattern match failed");
    }
  }
};
var reverse = (lst) => reverseHelper(lst)([]);
var lex = (source) => ((initial) => (($match_56) => {
  if ($match_56.$tag === 1) {
    const e = $match_56.$0;
    return Err(e);
  }
  if ($match_56.$tag === 0) {
    const loop = $match_56.$0;
    return ((endPos) => ((eofToken) => Ok(reverse(_COLON_COLON(eofToken)(loop.tokens))))({ kind: Eof, lexeme: "", span: { start: endPos, end: endPos } }))(position(loop.state));
  }
  throw new Error("Pattern match failed");
})(lexLoop(initial)))({ tokens: [], state: initState(source), sawNewline: false, hasEmittedToken: false });
var lexToJson2 = (source) => lexToJson(lex)(source);

// src/index.ts
class LexError extends Error {
  span;
  constructor(message, span) {
    super(message);
    this.span = span;
  }
}
function lex2(source) {
  const json = lexToJson2(source);
  const result = JSON.parse(json);
  if (!result.ok) {
    throw new LexError(result.message, result.span);
  }
  return result.tokens.map((t) => ({ kind: t.kind, lexeme: t.lexeme, span: t.span }));
}
export {
  lex2 as lex,
  LexError
};
