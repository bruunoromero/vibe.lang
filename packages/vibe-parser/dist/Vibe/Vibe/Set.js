import { setDiff as $$diff, setEmpty as empty, setFilter as $$filter, setFoldl as $$foldl, setFromList as $$fromList, setInsert as $$insert, setIntersect as $$intersect, setMap as $$map, setRemove as $$remove, setToList as $$toList, setUnion as $$union } from "./Set.ffi.js";

import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_List from "../Vibe/List.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_Types from "../../VibeLexer/VibeLexer/Types.js";
import * as $inst_Parser from "../../VibeParser/VibeParser/Parser.js";
import * as $inst_Layout from "../../VibeParser/VibeParser/Layout.js";
import * as $inst_Encode from "../../Json/Json/Encode.js";
import * as $inst_Decode from "../../Json/Json/Decode.js";

// Values
const insert = ($a0) => ($a1) => $$insert($a0, $a1);
const singleton = (x) => insert(x)(empty);
const remove = ($a0) => ($a1) => $$remove($a0, $a1);
const _has = ($recv) => ($a0) => $recv.has($a0);
const member = (x) => (s) => _has(s)(x);
const size = ($recv) => $recv.size;
const isEmpty = (s) => $inst_Int.$dict_Eq_Int._EQ_EQ(size(s))(0);
const fromList = ($a0) => $$fromList($a0);
const toList = ($a0) => $$toList($a0);
const foldl = ($a0) => ($a1) => ($a2) => $$foldl($a0, $a1, $a2);
const map = ($a0) => ($a1) => $$map($a0, $a1);
const filter = ($a0) => ($a1) => $$filter($a0, $a1);
const union = ($a0) => ($a1) => $$union($a0, $a1);
const intersect = ($a0) => ($a1) => $$intersect($a0, $a1);
const diff = ($a0) => ($a1) => $$diff($a0, $a1);

export { diff, empty, filter, foldl, fromList, insert, intersect, isEmpty, map, member, remove, singleton, size, toList, union };