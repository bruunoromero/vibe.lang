import { not } from "../Vibe/Basics.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";

// ADT Constructors
const Ok = ($0) => ({ $tag: 0, $0 });
const Err = ($0) => ({ $tag: 1, $0 });

// Values
const isOk = (result) => (($match_0) => { if ($match_0.$tag === 0) { return true; } if ($match_0.$tag === 1) { return false; } throw new Error("Pattern match failed"); })(result);
const isErr = (result) => not(isOk(result));
const map = (f) => (result) => (($match_1) => { if ($match_1.$tag === 0) { const value = $match_1.$0; return Ok(f(value)); } if ($match_1.$tag === 1) { const error = $match_1.$0; return Err(error); } throw new Error("Pattern match failed"); })(result);
const andThen = (fn) => (result) => (($match_2) => { if ($match_2.$tag === 0) { const value = $match_2.$0; return fn(value); } if ($match_2.$tag === 1) { const error = $match_2.$0; return Err(error); } throw new Error("Pattern match failed"); })(result);
const mapError = (fn) => (result) => (($match_3) => { if ($match_3.$tag === 0) { const value = $match_3.$0; return Ok(value); } if ($match_3.$tag === 1) { const error = $match_3.$0; return Err(fn(error)); } throw new Error("Pattern match failed"); })(result);
const withDefault = (fallback) => (result) => (($match_4) => { if ($match_4.$tag === 0) { const value = $match_4.$0; return value; } if ($match_4.$tag === 1) { return fallback; } throw new Error("Pattern match failed"); })(result);

export { Err, Ok, andThen, isErr, isOk, map, mapError, withDefault };