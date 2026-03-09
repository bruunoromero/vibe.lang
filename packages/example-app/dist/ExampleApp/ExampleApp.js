import { readFile as $$readFile } from "./ExampleApp.ffi.js";

import * as Vibe from "../Vibe/Vibe.js";
import * as Ref from "../Vibe/Vibe/Ref.js";
import * as Promise from "../Vibe/Vibe/Promise.js";
import * as Task from "../Vibe/Vibe/Task.js";
import * as $inst_Unit from "../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Vibe/Int.js";
import * as $inst_Float from "../Vibe/Vibe/Float.js";
import * as $inst_String from "../Vibe/Vibe/String.js";
import * as $inst_Char from "../Vibe/Vibe/Char.js";
import * as $inst_List from "../Vibe/Vibe/List.js";

// Short-Circuit Operator Helpers
const _AMP_AMP = (a) => (b) => a && b();

// Values
const readFile = ($a0) => $$readFile($a0);
const main = (true ? "It's TRUE" : "It's FALSE");
const $impl_Eq_Person__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP($inst_String.$dict_Eq_String._EQ_EQ(x_impl.name)(y_impl.name))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.age)(y_impl.age));
const $impl_Show_Person_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("Person { ")("name = "))($inst_String.$dict_Show_String.toString(x_impl.name)))(", "))("age = "))($inst_Int.$dict_Show_Int.toString(x_impl.age)))(" }");
const $dict_Eq_Person = {
  _EQ_EQ: $impl_Eq_Person__EQ_EQ
};
const $dict_Show_Person = {
  toString: $impl_Show_Person_toString
};

export { $dict_Eq_Person, $dict_Show_Person, main, readFile };