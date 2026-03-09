import * as $inst_Unit from "../Vibe/Unit.js";
import { not } from "../Vibe/Basics.js";

// Values
const $impl_Eq_Bool__EQ_EQ = (x) => (y) => (($match_0) => { if ($match_0[0] === true && $match_0[1] === true) { return true; } if ($match_0[0] === false && $match_0[1] === false) { return true; } { return false; } throw new Error("Pattern match failed"); })([x, y]);
const $default_Eq_Bool__SLASH_EQ = (x) => (y) => not($dict_Eq_Bool._EQ_EQ(x)(y));
const $impl_Show_Bool_toString = (x) => (x ? "True" : "False");
const $dict_Eq_Bool = {
  _EQ_EQ: $impl_Eq_Bool__EQ_EQ,
  _SLASH_EQ: $default_Eq_Bool__SLASH_EQ
};
const $dict_Show_Bool = {
  toString: $impl_Show_Bool_toString
};

export { $dict_Eq_Bool, $dict_Show_Bool };