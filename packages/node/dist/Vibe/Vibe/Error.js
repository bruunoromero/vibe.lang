import { create as $$create } from "./Error.ffi.js";

import * as Vibe from "../Vibe.js";
import * as Maybe from "../Vibe/Maybe.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_List from "../Vibe/List.js";

// Values
const create = ($a0) => $$create($a0);
const _stack = ($recv) => $recv.stack;
const stack = (error) => Vibe._PIPE_GT(_stack(error))(Maybe.fromNullable);
const message = ($recv) => $recv.message;
const name = ($recv) => $recv.name;

export { create };