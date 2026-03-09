import { intAdd as $$_add, intDiv as $$_div, numEq as $$_eq, numGt as $$_gt, numLt as $$_lt, intMod as $$_mod, intMul as $$_mul, intSub as $$_sub, numToString as $$_toString, intToFloat as $$toFloat } from "./Int.ffi.js";

import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import { not } from "../Vibe/Basics.js";

// Short-Circuit Operator Helpers
const _PIPE_PIPE = (a) => (b) => a || b();

// Values
const _add = ($a0) => ($a1) => $$_add($a0, $a1);
const _sub = ($a0) => ($a1) => $$_sub($a0, $a1);
const _mul = ($a0) => ($a1) => $$_mul($a0, $a1);
const _div = ($a0) => ($a1) => $$_div($a0, $a1);
const _mod = ($a0) => ($a1) => $$_mod($a0, $a1);
const _eq = ($a0) => ($a1) => $$_eq($a0, $a1);
const _lt = ($a0) => ($a1) => $$_lt($a0, $a1);
const _gt = ($a0) => ($a1) => $$_gt($a0, $a1);
const _toString = ($a0) => $$_toString($a0);
const _negate = (x) => -x;
const $default_Ord_Int__LT_EQ = (x) => (y) => _PIPE_PIPE(_lt(x)(y))(() => $dict_Eq_Int._EQ_EQ(x)(y));
const $default_Ord_Int__GT_EQ = (x) => (y) => _PIPE_PIPE(_gt(x)(y))(() => $dict_Eq_Int._EQ_EQ(x)(y));
const $dict_Ord_Int = {
  _LT: _lt,
  _GT: _gt,
  _LT_EQ: $default_Ord_Int__LT_EQ,
  _GT_EQ: $default_Ord_Int__GT_EQ
};
const max = (a) => (b) => ($dict_Ord_Int._GT(a)(b) ? a : b);
const min = (a) => (b) => ($dict_Ord_Int._LT(a)(b) ? a : b);
const abs = (x) => ($dict_Ord_Int._LT(x)(0) ? -x : x);
const toFloat = ($a0) => $$toFloat($a0);
const $default_Eq_Int__SLASH_EQ = (x) => (y) => not(_eq(x)(y));
const $dict_Eq_Int = {
  _EQ_EQ: _eq,
  _SLASH_EQ: $default_Eq_Int__SLASH_EQ
};
const $dict_Num_Int = {
  _PLUS: _add,
  _MINUS: _sub,
  _STAR: _mul,
  negate: _negate
};
const $dict_Integral_Int = {
  _SLASH_SLASH: _div,
  _PERCENT: _mod
};
const $dict_Show_Int = {
  toString: _toString
};

export { $dict_Eq_Int, $dict_Integral_Int, $dict_Num_Int, $dict_Ord_Int, $dict_Show_Int, abs, max, min, toFloat };