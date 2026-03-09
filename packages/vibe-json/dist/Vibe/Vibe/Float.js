import { floatAdd as $$_add, floatDiv as $$_div, numEq as $$_eq, numGt as $$_gt, numLt as $$_lt, floatMul as $$_mul, floatSub as $$_sub, numToString as $$_toString, floatCeiling as $$ceiling, floatFloor as $$floor, floatRound as $$round, floatTruncate as $$truncate } from "./Float.ffi.js";

import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import { not } from "../Vibe/Basics.js";

// Short-Circuit Operator Helpers
const _PIPE_PIPE = (a) => (b) => a || b();

// Values
const _add = ($a0) => ($a1) => $$_add($a0, $a1);
const _sub = ($a0) => ($a1) => $$_sub($a0, $a1);
const _mul = ($a0) => ($a1) => $$_mul($a0, $a1);
const _div = ($a0) => ($a1) => $$_div($a0, $a1);
const _toString = ($a0) => $$_toString($a0);
const _eq = ($a0) => ($a1) => $$_eq($a0, $a1);
const _lt = ($a0) => ($a1) => $$_lt($a0, $a1);
const _gt = ($a0) => ($a1) => $$_gt($a0, $a1);
const _negate = (x) => -x;
const floor = ($a0) => $$floor($a0);
const ceiling = ($a0) => $$ceiling($a0);
const round = ($a0) => $$round($a0);
const truncate = ($a0) => $$truncate($a0);
const $default_Eq_Float__SLASH_EQ = (x) => (y) => not(_eq(x)(y));
const $default_Ord_Float__LT_EQ = (x) => (y) => _PIPE_PIPE(_lt(x)(y))(() => $dict_Eq_Float._EQ_EQ(x)(y));
const $default_Ord_Float__GT_EQ = (x) => (y) => _PIPE_PIPE(_gt(x)(y))(() => $dict_Eq_Float._EQ_EQ(x)(y));
const $dict_Eq_Float = {
  _EQ_EQ: _eq,
  _SLASH_EQ: $default_Eq_Float__SLASH_EQ
};
const $dict_Ord_Float = {
  _LT: _lt,
  _GT: _gt,
  _LT_EQ: $default_Ord_Float__LT_EQ,
  _GT_EQ: $default_Ord_Float__GT_EQ
};
const $dict_Num_Float = {
  _PLUS: _add,
  _MINUS: _sub,
  _STAR: _mul,
  negate: _negate
};
const $dict_Fractional_Float = {
  _SLASH: _div
};
const $dict_Show_Float = {
  toString: _toString
};

export { $dict_Eq_Float, $dict_Fractional_Float, $dict_Num_Float, $dict_Ord_Float, $dict_Show_Float, ceiling, floor, round, truncate };