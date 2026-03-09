import { stringAppend as $$_append, stringCharAt as $$_charAt, stringEq as $$_eq, stringFromChar as $$_fromChar, stringGt as $$_gt, stringLt as $$_lt, parseFloat as $$_parseFloat, parseInt as $$_parseInt, stringToList as $$toList, unsafeCharAt as $$unsafeCharAt } from "./String.ffi.js";

import { identity, not } from "../Vibe/Basics.js";
import { Just, Nothing } from "../Vibe/Maybe.js";
import * as Int from "../Vibe/Int.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Float from "../Vibe/Float.js";

// Short-Circuit Operator Helpers
const _PIPE_PIPE = (a) => (b) => a || b();

// Values
const _append = ($a0) => ($a1) => $$_append($a0, $a1);
const _eq = ($a0) => ($a1) => $$_eq($a0, $a1);
const _lt = ($a0) => ($a1) => $$_lt($a0, $a1);
const _gt = ($a0) => ($a1) => $$_gt($a0, $a1);
const _parseInt = ($a0) => ($a1) => ($a2) => $$_parseInt($a0, $a1, $a2);
const _parseFloat = ($a0) => ($a1) => ($a2) => $$_parseFloat($a0, $a1, $a2);
const length = ($recv) => $recv.length;
const _charAt = ($a0) => ($a1) => ($a2) => ($a3) => $$_charAt($a0, $a1, $a2, $a3);
const unsafeCharAt = ($a0) => ($a1) => $$unsafeCharAt($a0, $a1);
const _slice = ($recv) => ($a0) => ($a1) => $recv.slice($a0, $a1);
const slice = (start) => (end) => (s) => _slice(s)(start)(end);
const _startsWith = ($recv) => ($a0) => $recv.startsWith($a0);
const startsWith = (prefix) => (s) => _startsWith(s)(prefix);
const _endsWith = ($recv) => ($a0) => $recv.endsWith($a0);
const endsWith = (suffix) => (s) => _endsWith(s)(suffix);
const _includes = ($recv) => ($a0) => $recv.includes($a0);
const contains = (sub) => (s) => _includes(s)(sub);
const toList = ($a0) => $$toList($a0);
const _join = ($recv) => ($a0) => $recv.join($a0);
const fromList = (cs) => _join(cs)("");
const trim = ($recv) => $recv.trim();
const _split = ($recv) => ($a0) => $recv.split($a0);
const split = (sep) => (s) => _split(s)(sep);
const _joinStrings = ($recv) => ($a0) => $recv.join($a0);
const join = (sep) => (lst) => _joinStrings(lst)(sep);
const charAt = _charAt(Just)(Nothing);
const toInt = _parseInt(Just)(Nothing);
const toFloat = _parseFloat(Just)(Nothing);
const isEmpty = (s) => Int.$dict_Eq_Int._EQ_EQ(length(s))(0);
const lines = (s) => split("\n")(s);
const _fromChar = ($a0) => $$_fromChar($a0);
const fromChar = _fromChar;
const $default_Eq_String__SLASH_EQ = (x) => (y) => not(_eq(x)(y));
const $default_Ord_String__LT_EQ = (x) => (y) => _PIPE_PIPE(_lt(x)(y))(() => $dict_Eq_String._EQ_EQ(x)(y));
const $default_Ord_String__GT_EQ = (x) => (y) => _PIPE_PIPE(_gt(x)(y))(() => $dict_Eq_String._EQ_EQ(x)(y));
const $dict_Eq_String = {
  _EQ_EQ: _eq,
  _SLASH_EQ: $default_Eq_String__SLASH_EQ
};
const $dict_Ord_String = {
  _LT: _lt,
  _GT: _gt,
  _LT_EQ: $default_Ord_String__LT_EQ,
  _GT_EQ: $default_Ord_String__GT_EQ
};
const $dict_Show_String = {
  toString: identity
};
const $dict_Appendable_String = {
  _PLUS_PLUS: _append
};

export { $dict_Appendable_String, $dict_Eq_String, $dict_Ord_String, $dict_Show_String, charAt, contains, endsWith, fromChar, fromList, isEmpty, join, length, lines, slice, split, startsWith, toFloat, toInt, toList, trim, unsafeCharAt };