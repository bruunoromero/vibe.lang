import { not } from "../Vibe/Basics.js";

// Values
const $impl_Eq_Unit__EQ_EQ = (_) => (_) => true;
const $default_Eq_Unit__SLASH_EQ = (x) => (y) => not($dict_Eq_Unit._EQ_EQ(x)(y));
const $impl_Show_Unit_toString = (_) => "()";
const $dict_Eq_Unit = {
  _EQ_EQ: $impl_Eq_Unit__EQ_EQ,
  _SLASH_EQ: $default_Eq_Unit__SLASH_EQ
};
const $dict_Show_Unit = {
  toString: $impl_Show_Unit_toString
};

export { $dict_Eq_Unit, $dict_Show_Unit };