import { fromNullable as $$_fromNullable } from "./Maybe.ffi.js";

import { not } from "../Vibe/Basics.js";
import { Ok, Err } from "../Vibe/Result.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";

// ADT Constructors
const Just = ($0) => ({ $tag: 0, $0 });
const Nothing = { $tag: 1 };

// Values
const isJust = (maybeValue) => (($match_0) => { if ($match_0.$tag === 0) { return true; } if ($match_0.$tag === 1) { return false; } throw new Error("Pattern match failed"); })(maybeValue);
const isNothing = (maybeValue) => not(isJust(maybeValue));
const withDefault = (defaultValue) => (maybeValue) => (($match_1) => { if ($match_1.$tag === 0) { const value = $match_1.$0; return value; } if ($match_1.$tag === 1) { return defaultValue; } throw new Error("Pattern match failed"); })(maybeValue);
const fromResult = (result) => (($match_2) => { if ($match_2.$tag === 0) { const value = $match_2.$0; return Just(value); } if ($match_2.$tag === 1) { return { $tag: 1 }; } throw new Error("Pattern match failed"); })(result);
const _fromNullable = ($a0) => ($a1) => ($a2) => $$_fromNullable($a0, $a1, $a2);
const fromNullable = (nullableValue) => _fromNullable(Just)({ $tag: 1 })(nullableValue);
const map = (fn) => (maybeValue) => (($match_3) => { if ($match_3.$tag === 0) { const value = $match_3.$0; return Just(fn(value)); } if ($match_3.$tag === 1) { return { $tag: 1 }; } throw new Error("Pattern match failed"); })(maybeValue);
const andThen = (fn) => (maybeValue) => (($match_4) => { if ($match_4.$tag === 0) { const value = $match_4.$0; return fn(value); } if ($match_4.$tag === 1) { return { $tag: 1 }; } throw new Error("Pattern match failed"); })(maybeValue);
const or = (fallback) => (primary) => (($match_5) => { if ($match_5.$tag === 0) { return primary; } if ($match_5.$tag === 1) { return fallback; } throw new Error("Pattern match failed"); })(primary);
const toList = (maybeValue) => (($match_6) => { if ($match_6.$tag === 0) { const value = $match_6.$0; return [value]; } if ($match_6.$tag === 1) { return []; } throw new Error("Pattern match failed"); })(maybeValue);

export { Just, Nothing, andThen, fromNullable, fromResult, isJust, isNothing, map, or, toList, withDefault };