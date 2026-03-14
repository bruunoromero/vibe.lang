import * as Dict from "../Vibe/Dict.js";
import { _COLON_COLON } from "../Vibe/List.js";
import { not } from "../Vibe/Basics.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_Ref from "../Vibe/Ref.js";

// ADT Constructors
const Set = ($0) => ({ $tag: 0, $0 });

// Values
const empty = Set(Dict.empty);
const insert = ($dict_Ord) => (v) => ({ $0: dict }) => Set(Dict.insert($dict_Ord)(v)(undefined)(dict));
const singleton = ($dict_Ord) => (v) => insert($dict_Ord)(v)(empty);
const _fromListHelp = ($dict_Ord) => (items) => (acc) => (($match_0) => { if (Array.isArray($match_0) && $match_0.length === 0) { return acc; } if (Array.isArray($match_0) && $match_0.length >= 1) { const x = $match_0[0]; const rest = $match_0.slice(1); return _fromListHelp($dict_Ord)(rest)(insert($dict_Ord)(x)(acc)); } throw new Error("Pattern match failed"); })(items);
const fromList = ($dict_Ord) => (items) => _fromListHelp($dict_Ord)(items)(empty);
const remove = ($dict_Ord) => (v) => ({ $0: dict }) => Set(Dict.remove($dict_Eq)($dict_Ord)(v)(dict));
const member = ($dict_Ord) => (v) => ({ $0: dict }) => Dict.member($dict_Ord)(v)(dict);
const size = ({ $0: dict }) => Dict.size(dict);
const isEmpty = ({ $0: dict }) => Dict.isEmpty(dict);
const toList = ({ $0: dict }) => Dict.keys(dict);
const foldl = (fn) => (acc) => ({ $0: dict }) => Dict.foldl((k) => (_) => (b) => fn(k)(b))(acc)(dict);
const map = ($dict_Ord) => (fn) => (s) => foldl((v) => (acc) => insert($dict_Ord)(fn(v))(acc))(empty)(s);
const filter = ($dict_Ord) => (pred) => ({ $0: dict }) => Set(Dict.filter($dict_Ord)((k) => (_) => pred(k))(dict));
const union = ($dict_Ord) => ({ $0: d1 }) => ({ $0: d2 }) => Set(Dict.union($dict_Ord)(d1)(d2));
const intersect = ($dict_Ord) => (s1) => (s2) => filter($dict_Ord)((v) => member($dict_Ord)(v)(s2))(s1);
const diff = ($dict_Ord) => (s1) => (s2) => filter($dict_Ord)((v) => not(member($dict_Ord)(v)(s2)))(s1);

export { diff, empty, filter, foldl, fromList, insert, intersect, isEmpty, map, member, remove, singleton, size, toList, union };