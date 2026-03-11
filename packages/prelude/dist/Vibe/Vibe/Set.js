import { setDiff as $$diff, setEmpty as empty, setFilter as $$filter, setFoldl as $$foldl, setFromList as $$fromList, setInsert as $$insert, setIntersect as $$intersect, setMap as $$map, setMember as $$member, setRemove as $$remove, setToList as $$toList, setUnion as $$union } from "./Set.ffi.js";

import { not } from "../Vibe/Basics.js";
import { Just, Nothing } from "../Vibe/Maybe.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_List from "../Vibe/List.js";

// Values
const insert = ($a0) => ($a1) => $$insert($a0, $a1);
const singleton = (v) => insert(v)(empty);
const fromList = ($a0) => $$fromList($a0);
const remove = ($a0) => ($a1) => $$remove($a0, $a1);
const member = ($a0) => ($a1) => $$member($a0, $a1);
const size = ($recv) => $recv.size;
const isEmpty = (s) => $inst_Int.$dict_Eq_Int._EQ_EQ(size(s))(0);
const toList = ($a0) => $$toList($a0);
const foldl = ($a0) => ($a1) => ($a2) => $$foldl($a0, $a1, $a2);
const map = ($a0) => ($a1) => $$map($a0, $a1);
const filter = ($a0) => ($a1) => $$filter($a0, $a1);
const union = ($a0) => ($a1) => $$union($a0, $a1);
const intersect = ($a0) => ($a1) => $$intersect($a0, $a1);
const diff = ($a0) => ($a1) => $$diff($a0, $a1);

export { diff, empty, filter, foldl, fromList, insert, intersect, isEmpty, map, member, remove, singleton, size, toList, union };