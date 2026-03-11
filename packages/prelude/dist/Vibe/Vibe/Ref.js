import { refEq as $$_eq, createRef as $$create, getRef as $$get, setRef as $$set } from "./Ref.ffi.js";

import * as String from "../Vibe/String.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_List from "../Vibe/List.js";
import * as $inst_Dict from "../Vibe/Dict.js";
import { not } from "../Vibe/Basics.js";

// Values
const create = ($a0) => $$create($a0);
const get = ($a0) => $$get($a0);
const set = ($a0) => ($a1) => $$set($a0, $a1);
const _eq = ($a0) => ($a1) => $$_eq($a0, $a1);
const update = (f) => (ref) => ((current) => ((updated) => set(ref)(updated))(f(current)))(get(ref));
const $impl_Show_Ref_v1974_toString = ($dict_Show) => (ref) => String.$dict_Appendable_String._PLUS_PLUS("Ref(")(String.$dict_Appendable_String._PLUS_PLUS($dict_Show.toString(get(ref)))(")"));
const $impl_Eq_Ref_v1975__EQ_EQ = (x) => (y) => _eq(x)(y);
const $default_Eq_Ref_v1975__SLASH_EQ = (x) => (y) => not($dict_Eq_Ref_v1975._EQ_EQ(x)(y));
const $dict_Show_Ref_v1974 = ($dict_Show) => ({
  toString: $impl_Show_Ref_v1974_toString($dict_Show)
});
const $dict_Eq_Ref_v1975 = {
  _EQ_EQ: $impl_Eq_Ref_v1975__EQ_EQ,
  _SLASH_EQ: $default_Eq_Ref_v1975__SLASH_EQ
};

export { $dict_Eq_Ref_v1975, $dict_Show_Ref_v1974, create, get, set };