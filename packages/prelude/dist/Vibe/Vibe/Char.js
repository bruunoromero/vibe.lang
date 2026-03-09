import { charOrdGt as $$_gt, charOrd as $$_lt, charToString as $$_toString, charFromCode as $$fromCode, charToCode as $$toCode } from "./Char.ffi.js";

import * as String from "../Vibe/String.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import { not } from "../Vibe/Basics.js";

// Short-Circuit Operator Helpers
const _PIPE_PIPE = (a) => (b) => a || b();
const _AMP_AMP = (a) => (b) => a && b();

// Values
const _toString = ($a0) => $$_toString($a0);
const _lt = ($a0) => ($a1) => $$_lt($a0, $a1);
const _gt = ($a0) => ($a1) => $$_gt($a0, $a1);
const toCode = ($a0) => $$toCode($a0);
const fromCode = ($a0) => $$fromCode($a0);
const $default_Ord_Char__LT_EQ = (x) => (y) => _PIPE_PIPE(_lt(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
const $default_Ord_Char__GT_EQ = (x) => (y) => _PIPE_PIPE(_gt(x)(y))(() => $dict_Eq_Char._EQ_EQ(x)(y));
const $dict_Ord_Char = {
  _LT: _lt,
  _GT: _gt,
  _LT_EQ: $default_Ord_Char__LT_EQ,
  _GT_EQ: $default_Ord_Char__GT_EQ
};
const isUpper = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("A"))(() => $dict_Ord_Char._LT_EQ(c)("Z"));
const isLower = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("a"))(() => $dict_Ord_Char._LT_EQ(c)("z"));
const isAlpha = (c) => _PIPE_PIPE(isUpper(c))(() => isLower(c));
const isDigit = (c) => _AMP_AMP($dict_Ord_Char._GT_EQ(c)("0"))(() => $dict_Ord_Char._LT_EQ(c)("9"));
const isAlphaNum = (c) => _PIPE_PIPE(isAlpha(c))(() => isDigit(c));
const $impl_Eq_Char__EQ_EQ = (x) => (y) => String.$dict_Eq_String._EQ_EQ($dict_Show_Char.toString(x))($dict_Show_Char.toString(y));
const $default_Eq_Char__SLASH_EQ = (x) => (y) => not($dict_Eq_Char._EQ_EQ(x)(y));
const $dict_Eq_Char = {
  _EQ_EQ: $impl_Eq_Char__EQ_EQ,
  _SLASH_EQ: $default_Eq_Char__SLASH_EQ
};
const isWhitespace = (c) => _PIPE_PIPE($dict_Eq_Char._EQ_EQ(c)(" "))(() => _PIPE_PIPE($dict_Eq_Char._EQ_EQ(c)("\n"))(() => _PIPE_PIPE($dict_Eq_Char._EQ_EQ(c)("\r"))(() => $dict_Eq_Char._EQ_EQ(c)("\t"))));
const $dict_Show_Char = {
  toString: _toString
};

export { $dict_Eq_Char, $dict_Ord_Char, $dict_Show_Char, fromCode, isAlpha, isAlphaNum, isDigit, isLower, isUpper, isWhitespace, toCode };