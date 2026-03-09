import { listNth as $$_nth, listSort as $$_sort, listReverse as $$reverse } from "./List.ffi.js";

import * as String from "../Vibe/String.js";
import { Just, Nothing } from "../Vibe/Maybe.js";
import { flip, not } from "../Vibe/Basics.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_Char from "../Vibe/Char.js";

// Short-Circuit Operator Helpers
const _AMP_AMP = (a) => (b) => a && b();

// Values
const _concat = ($recv) => ($a0) => $recv.concat($a0);
const _COLON_COLON = (x) => (xs) => _concat([x])(xs);
const _reduce = ($recv) => ($a0) => ($a1) => $recv.reduce($a0, $a1);
const foldl = (fn) => (initial) => (lst) => _reduce(lst)(fn)(initial);
const _map = ($recv) => ($a0) => $recv.map($a0);
const map = flip(_map);
const _filter = ($recv) => ($a0) => $recv.filter($a0);
const filter = flip(_filter);
const head = (xs) => (($match_0) => { if (Array.isArray($match_0) && $match_0.length >= 1) { const x = $match_0[0]; return Just(x); } if (Array.isArray($match_0) && $match_0.length === 0) { return Nothing; } throw new Error("Pattern match failed"); })(xs);
const tail = (xs) => (($match_1) => { if (Array.isArray($match_1) && $match_1.length >= 1) { const xtail = $match_1.slice(1); return Just(xtail); } if (Array.isArray($match_1) && $match_1.length === 0) { return Nothing; } throw new Error("Pattern match failed"); })(xs);
const last = (xs) => { while (true) { { const $match_2 = xs; if (Array.isArray($match_2) && $match_2.length === 0) { return Nothing; } if (Array.isArray($match_2) && $match_2.length === 1) { const x = $match_2[0]; return Just(x); } if (Array.isArray($match_2) && $match_2.length >= 1) { const rest = $match_2.slice(1); xs = rest; continue; } throw new Error("Pattern match failed"); } } };
const isEmpty = (xs) => (($match_3) => { if (Array.isArray($match_3) && $match_3.length === 0) { return true; } { return false; } throw new Error("Pattern match failed"); })(xs);
const singleton = (x) => [x];
const length = ($recv) => $recv.length;
const reverse = ($a0) => $$reverse($a0);
const _nth = ($a0) => ($a1) => ($a2) => ($a3) => $$_nth($a0, $a1, $a2, $a3);
const nth = _nth(Just)(Nothing);
const foldr = (fn) => (acc) => (xs) => foldl((a) => (x) => fn(x)(a))(acc)(reverse(xs));
const take = (n) => (xs) => ($inst_Int.$dict_Ord_Int._LT_EQ(n)(0) ? [] : (($match_4) => { if (Array.isArray($match_4) && $match_4.length === 0) { return []; } if (Array.isArray($match_4) && $match_4.length >= 1) { const x = $match_4[0]; const rest = $match_4.slice(1); return _COLON_COLON(x)(take($inst_Int.$dict_Num_Int._MINUS(n)(1))(rest)); } throw new Error("Pattern match failed"); })(xs));
const drop = (n) => (xs) => { while (true) { if ($inst_Int.$dict_Ord_Int._LT_EQ(n)(0)) { return xs; } else { { const $match_5 = xs; if (Array.isArray($match_5) && $match_5.length === 0) { return []; } if (Array.isArray($match_5) && $match_5.length >= 1) { const rest = $match_5.slice(1); [n, xs] = [$inst_Int.$dict_Num_Int._MINUS(n)(1), rest]; continue; } throw new Error("Pattern match failed"); } } } };
const takeWhile = (pred) => (xs) => (($match_6) => { if (Array.isArray($match_6) && $match_6.length === 0) { return []; } if (Array.isArray($match_6) && $match_6.length >= 1) { const x = $match_6[0]; const rest = $match_6.slice(1); return (pred(x) ? _COLON_COLON(x)(takeWhile(pred)(rest)) : []); } throw new Error("Pattern match failed"); })(xs);
const dropWhile = (pred) => (xs) => { while (true) { { const $match_7 = xs; if (Array.isArray($match_7) && $match_7.length === 0) { return []; } if (Array.isArray($match_7) && $match_7.length >= 1) { const x = $match_7[0]; const rest = $match_7.slice(1); if (pred(x)) { [pred, xs] = [pred, rest]; continue; } else { return xs; } } throw new Error("Pattern match failed"); } } };
const any = (pred) => (xs) => { while (true) { { const $match_8 = xs; if (Array.isArray($match_8) && $match_8.length === 0) { return false; } if (Array.isArray($match_8) && $match_8.length >= 1) { const x = $match_8[0]; const rest = $match_8.slice(1); if (pred(x)) { return true; } else { [pred, xs] = [pred, rest]; continue; } } throw new Error("Pattern match failed"); } } };
const all = (pred) => (xs) => { while (true) { { const $match_9 = xs; if (Array.isArray($match_9) && $match_9.length === 0) { return true; } if (Array.isArray($match_9) && $match_9.length >= 1) { const x = $match_9[0]; const rest = $match_9.slice(1); if (pred(x)) { [pred, xs] = [pred, rest]; continue; } else { return false; } } throw new Error("Pattern match failed"); } } };
const member = ($dict_Eq) => (target) => (xs) => any((x) => $dict_Eq._EQ_EQ(x)(target))(xs);
const $dict_Appendable_List_v358 = {
  _PLUS_PLUS: _concat
};
const concat = (lists) => foldr((xs) => (acc) => $dict_Appendable_List_v358._PLUS_PLUS(xs)(acc))([])(lists);
const concatMap = (fn) => (xs) => concat($dict_Appendable_List_v358)(map(fn)(xs));
const filterMap = (fn) => (xs) => ((step) => foldr(step)([])(xs))((x) => (acc) => (($match_10) => { if ($match_10.$tag === 0) { const val = $match_10.$0; return _COLON_COLON(val)(acc); } if ($match_10.$tag === 1) { return acc; } throw new Error("Pattern match failed"); })(fn(x)));
const indexedMap = (fn) => (xs) => ((helper) => helper(0)(xs))((i) => (remaining) => (($match_11) => { if (Array.isArray($match_11) && $match_11.length === 0) { return []; } if (Array.isArray($match_11) && $match_11.length >= 1) { const x = $match_11[0]; const rest = $match_11.slice(1); return _COLON_COLON(fn(i)(x))(helper($inst_Int.$dict_Num_Int._PLUS(i)(1))(rest)); } throw new Error("Pattern match failed"); })(remaining));
const zip = (xs) => (ys) => (($match_12) => { if (Array.isArray($match_12[0]) && $match_12[0].length >= 1 && Array.isArray($match_12[1]) && $match_12[1].length >= 1) { const x = $match_12[0][0]; const xrest = $match_12[0].slice(1); const y = $match_12[1][0]; const yrest = $match_12[1].slice(1); return _COLON_COLON([x, y])(zip(xrest)(yrest)); } { return []; } throw new Error("Pattern match failed"); })([xs, ys]);
const unzip = (pairs) => ((step) => foldr(step)([[], []])(pairs))((pair) => (acc) => (($match_13) => { { const a = $match_13[0][0]; const b = $match_13[0][1]; const as_ = $match_13[1][0]; const bs = $match_13[1][1]; return [_COLON_COLON(a)(as_), _COLON_COLON(b)(bs)]; } throw new Error("Pattern match failed"); })([pair, acc]));
const range = (lo) => (hi) => ($inst_Int.$dict_Ord_Int._GT(lo)(hi) ? [] : _COLON_COLON(lo)(range($inst_Int.$dict_Num_Int._PLUS(lo)(1))(hi)));
const repeat = (n) => (val) => ($inst_Int.$dict_Ord_Int._LT_EQ(n)(0) ? [] : _COLON_COLON(val)(repeat($inst_Int.$dict_Num_Int._MINUS(n)(1))(val)));
const intersperse = (sep) => (xs) => (($match_14) => { if (Array.isArray($match_14) && $match_14.length === 0) { return []; } if (Array.isArray($match_14) && $match_14.length === 1) { const x = $match_14[0]; return [x]; } if (Array.isArray($match_14) && $match_14.length >= 1) { const x = $match_14[0]; const rest = $match_14.slice(1); return _COLON_COLON(x)(_COLON_COLON(sep)(intersperse(sep)(rest))); } throw new Error("Pattern match failed"); })(xs);
const _sort = ($a0) => ($a1) => $$_sort($a0, $a1);
const sort = ($dict_Ord) => (xs) => _sort($dict_Ord._LT)(xs);
const sortBy = (cmp) => (xs) => _sort(cmp)(xs);
const $impl_Eq_List_v356__EQ_EQ = ($dict_Eq) => (xs) => (ys) => (($match_15) => { if (Array.isArray($match_15[0]) && $match_15[0].length === 0 && Array.isArray($match_15[1]) && $match_15[1].length === 0) { return true; } if (Array.isArray($match_15[0]) && $match_15[0].length >= 1 && Array.isArray($match_15[1]) && $match_15[1].length >= 1) { const x = $match_15[0][0]; const xtail = $match_15[0].slice(1); const y = $match_15[1][0]; const ytail = $match_15[1].slice(1); return _AMP_AMP($dict_Eq._EQ_EQ(x)(y))(() => $dict_Eq._EQ_EQ(xtail)(ytail)); } { return false; } throw new Error("Pattern match failed"); })([xs, ys]);
const $default_Eq_List_v356__SLASH_EQ = ($dict_Eq) => (x) => (y) => not($dict_Eq._EQ_EQ(x)(y));
const $impl_Show_List_v357_toString = ($dict_Show) => (lst) => ((elementStrings) => String.$dict_Appendable_String._PLUS_PLUS("[")(String.$dict_Appendable_String._PLUS_PLUS(String.join(", ")(elementStrings))("]")))(map($dict_Show.toString)(lst));
const $dict_Eq_List_v356 = ($dict_Eq) => ({
  _EQ_EQ: $impl_Eq_List_v356__EQ_EQ($dict_Eq),
  _SLASH_EQ: $default_Eq_List_v356__SLASH_EQ($dict_Eq)
});
const $dict_Show_List_v357 = ($dict_Show) => ({
  toString: $impl_Show_List_v357_toString($dict_Show)
});

export { $dict_Appendable_List_v358, $dict_Eq_List_v356, $dict_Show_List_v357, _COLON_COLON, all, any, concat, concatMap, drop, dropWhile, filter, filterMap, foldl, foldr, head, indexedMap, intersperse, isEmpty, last, length, map, member, nth, range, repeat, reverse, singleton, sort, sortBy, tail, take, takeWhile, unzip, zip };