import { dictFromList as $$_fromList, dictToList as $$_toList, dictEmpty as empty, dictFilter as $$filter, dictFoldl as $$foldl, dictInsert as $$insert, dictKeys as $$keys, dictMap as $$map, dictRemove as $$remove, dictUnion as $$union, dictValues as $$values } from "./Dict.ffi.js";

import { Just, Nothing } from "../Vibe/Maybe.js";
import { identity } from "../Vibe/Basics.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_List from "../Vibe/List.js";

// Values
const insert = ($a0) => ($a1) => ($a2) => $$insert($a0, $a1, $a2);
const singleton = (k) => (v) => insert(k)(v)(empty);
const remove = ($a0) => ($a1) => $$remove($a0, $a1);
const _has = ($recv) => ($a0) => $recv.has($a0);
const _unsafeGet = ($recv) => ($a0) => $recv.get($a0);
const get = (k) => (d) => (_has(d)(k) ? Just(_unsafeGet(d)(k)) : Nothing);
const member = (k) => (d) => _has(d)(k);
const size = ($recv) => $recv.size;
const isEmpty = (d) => $inst_Int.$dict_Eq_Int._EQ_EQ(size(d))(0);
const foldl = ($a0) => ($a1) => ($a2) => $$foldl($a0, $a1, $a2);
const keys = ($a0) => $$keys($a0);
const values = ($a0) => $$values($a0);
const _toList = ($a0) => ($a1) => $$_toList($a0, $a1);
const toList = _toList((k) => (v) => [k, v]);
const _fromList = ($a0) => ($a1) => ($a2) => $$_fromList($a0, $a1, $a2);
const fromList = _fromList((pair) => (($match_0) => { { const k = $match_0[0]; return k; } throw new Error("Pattern match failed"); })(pair))((pair) => (($match_1) => { { const v = $match_1[1]; return v; } throw new Error("Pattern match failed"); })(pair));
const map = ($a0) => ($a1) => $$map($a0, $a1);
const filter = ($a0) => ($a1) => $$filter($a0, $a1);
const union = ($a0) => ($a1) => $$union($a0, $a1);
const update = (key) => (fn) => (dict) => (($match_2) => { if ($match_2.$tag === 0) { const value = $match_2.$0; return insert(key)(value)(dict); } if ($match_2.$tag === 1) { return remove(key)(dict); } throw new Error("Pattern match failed"); })(fn(get(key)(dict)));

export { empty, filter, foldl, fromList, get, insert, isEmpty, keys, map, member, remove, singleton, size, toList, union, update, values };