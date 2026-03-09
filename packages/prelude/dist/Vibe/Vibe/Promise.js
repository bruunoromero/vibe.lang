import { catchPromise as $$$catch, createPromise as $$create, flatMapPromise as $$flatMap, mapPromise as $$map } from "./Promise.ffi.js";

import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_List from "../Vibe/List.js";

// Values
const create = ($a0) => $$create($a0);
const flatMap = ($a0) => ($a1) => $$flatMap($a0, $a1);
const map = ($a0) => ($a1) => $$map($a0, $a1);
const $catch = ($a0) => ($a1) => $$$catch($a0, $a1);

export { $catch, create, flatMap, map };