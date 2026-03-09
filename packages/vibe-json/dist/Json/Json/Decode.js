import { jsonArrayGet as $$_arrayGet, jsonArrayLength as $$_arrayLength, jsonIsArray as $$_isArray, jsonIsBool as $$_isBool, jsonIsInt as $$_isInt, jsonIsNull as $$_isNull, jsonIsNumber as $$_isNumber, jsonIsObject as $$_isObject, jsonIsString as $$_isString, jsonParse as $$_jsonParse, jsonToString as $$_jsonToString, jsonObjectGet as $$_objectGet, jsonObjectKeys as $$_objectKeys, jsonToBool as $$_toBool, jsonToFloat as $$_toFloat, jsonToInt as $$_toInt } from "./Decode.ffi.js";

import * as Vibe from "../../Vibe/Vibe.js";
import * as String from "../../Vibe/Vibe/String.js";
import * as List from "../../Vibe/Vibe/List.js";
import * as Encode from "../Json/Encode.js";
import * as $inst_Unit from "../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../Vibe/Vibe/Float.js";
import * as $inst_Char from "../../Vibe/Vibe/Char.js";

// Short-Circuit Operator Helpers
const _AMP_AMP = (a) => (b) => a && b();

// ADT Constructors
const Decoder = ($0) => ({ $tag: 0, $0 });
const Field = ($0) => ($1) => ({ $tag: 0, $0, $1 });
const Index = ($0) => ($1) => ({ $tag: 1, $0, $1 });
const OneOf = ($0) => ({ $tag: 2, $0 });
const Failure = ($0) => ({ $tag: 3, $0 });

// Values
const _jsonParse = ($a0) => ($a1) => ($a2) => $$_jsonParse($a0, $a1, $a2);
const _isNull = ($a0) => $$_isNull($a0);
const _isBool = ($a0) => $$_isBool($a0);
const _isInt = ($a0) => $$_isInt($a0);
const _isNumber = ($a0) => $$_isNumber($a0);
const _isString = ($a0) => $$_isString($a0);
const _isArray = ($a0) => $$_isArray($a0);
const _isObject = ($a0) => $$_isObject($a0);
const _toBool = ($a0) => $$_toBool($a0);
const _toInt = ($a0) => $$_toInt($a0);
const _toFloat = ($a0) => $$_toFloat($a0);
const _jsonToString = ($a0) => $$_jsonToString($a0);
const _arrayLength = ($a0) => $$_arrayLength($a0);
const _arrayGet = ($a0) => ($a1) => ($a2) => ($a3) => $$_arrayGet($a0, $a1, $a2, $a3);
const _objectKeys = ($a0) => $$_objectKeys($a0);
const _objectGet = ($a0) => ($a1) => ($a2) => ($a3) => $$_objectGet($a0, $a1, $a2, $a3);
const runDecoder = (decoder) => (json) => (($match_0) => { if ($match_0.$tag === 0) { const f = $match_0.$0; return f(json); } throw new Error("Pattern match failed"); })(decoder);
const arrayGet = _arrayGet(Vibe.Just)(Vibe.Nothing);
const objectGet = _objectGet(Vibe.Just)(Vibe.Nothing);
const jsonParse = _jsonParse(Vibe.Ok)(Vibe.Err);
const decodeString_ = (json) => (_isString(json) ? Vibe.Ok(_jsonToString(json)) : Vibe.Err(Failure("Expecting a STRING")));
const string = Decoder(decodeString_);
const decodeInt_ = (json) => (_isInt(json) ? Vibe.Ok(_toInt(json)) : Vibe.Err(Failure("Expecting an INT")));
const int = Decoder(decodeInt_);
const decodeFloat_ = (json) => (_isNumber(json) ? Vibe.Ok(_toFloat(json)) : Vibe.Err(Failure("Expecting a FLOAT")));
const float = Decoder(decodeFloat_);
const decodeBool_ = (json) => (_isBool(json) ? Vibe.Ok(_toBool(json)) : Vibe.Err(Failure("Expecting a BOOL")));
const bool = Decoder(decodeBool_);
const decodeNull_ = (fallback) => (json) => (_isNull(json) ? Vibe.Ok(fallback) : Vibe.Err(Failure("Expecting null")));
const $null = (fallback) => Decoder(decodeNull_(fallback));
const tryDecoders = (decoders) => (json) => (errors) => { while (true) { { const $match_1 = decoders; if (Array.isArray($match_1) && $match_1.length === 0) { return Vibe.Err(OneOf(errors)); } if (Array.isArray($match_1) && $match_1.length >= 1) { const d = $match_1[0]; const rest = $match_1.slice(1); { const $match_2 = runDecoder(d)(json); if ($match_2.$tag === 0) { const val = $match_2.$0; return Vibe.Ok(val); } if ($match_2.$tag === 1) { const e = $match_2.$0; [decoders, json, errors] = [rest, json, List.$dict_Appendable_List_v358._PLUS_PLUS(errors)([e])]; continue; } throw new Error("Pattern match failed"); } } throw new Error("Pattern match failed"); } } };
const decodeOneOf_ = (decoders) => (json) => tryDecoders(decoders)(json)([]);
const oneOf = (decoders) => Decoder(decodeOneOf_(decoders));
const decodeMap_ = (f) => (decoder) => (json) => (($match_3) => { if ($match_3.$tag === 0) { const a = $match_3.$0; return Vibe.Ok(f(a)); } if ($match_3.$tag === 1) { const e = $match_3.$0; return Vibe.Err(e); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(json));
const map = (f) => (decoder) => Decoder(decodeMap_(f)(decoder));
const nullable = (decoder) => oneOf([map(Vibe.Just)(decoder), $null(Vibe.Nothing)]);
const decodeMaybe_ = (decoder) => (json) => (($match_4) => { if ($match_4.$tag === 0) { const a = $match_4.$0; return Vibe.Ok(Vibe.Just(a)); } if ($match_4.$tag === 1) { return Vibe.Ok(Vibe.Nothing); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(json));
const maybe = (decoder) => Decoder(decodeMaybe_(decoder));
const decodeListItems = (decoder) => (json) => (idx) => (len) => ($inst_Int.$dict_Ord_Int._GT_EQ(idx)(len) ? Vibe.Ok([]) : (($match_5) => { if ($match_5.$tag === 1) { return Vibe.Err(Index(idx)(Failure("Index out of bounds"))); } if ($match_5.$tag === 0) { const item = $match_5.$0; return (($match_6) => { if ($match_6.$tag === 1) { const e = $match_6.$0; return Vibe.Err(Index(idx)(e)); } if ($match_6.$tag === 0) { const val = $match_6.$0; return (($match_7) => { if ($match_7.$tag === 1) { const e = $match_7.$0; return Vibe.Err(e); } if ($match_7.$tag === 0) { const rest = $match_7.$0; return Vibe.Ok(Vibe._COLON_COLON(val)(rest)); } throw new Error("Pattern match failed"); })(decodeListItems(decoder)(json)($inst_Int.$dict_Num_Int._PLUS(idx)(1))(len)); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(item)); } throw new Error("Pattern match failed"); })(arrayGet(idx)(json)));
const decodeList_ = (decoder) => (json) => (_isArray(json) ? decodeListItems(decoder)(json)(0)(_arrayLength(json)) : Vibe.Err(Failure("Expecting an ARRAY")));
const list = (decoder) => Decoder(decodeList_(decoder));
const decodeKeyValues = (decoder) => (json) => (keys) => (($match_8) => { if (Array.isArray($match_8) && $match_8.length === 0) { return Vibe.Ok([]); } if (Array.isArray($match_8) && $match_8.length >= 1) { const k = $match_8[0]; const rest = $match_8.slice(1); return (($match_9) => { if ($match_9.$tag === 1) { return Vibe.Err(Field(k)(Failure("Key missing"))); } if ($match_9.$tag === 0) { const item = $match_9.$0; return (($match_10) => { if ($match_10.$tag === 1) { const e = $match_10.$0; return Vibe.Err(Field(k)(e)); } if ($match_10.$tag === 0) { const val = $match_10.$0; return (($match_11) => { if ($match_11.$tag === 1) { const e = $match_11.$0; return Vibe.Err(e); } if ($match_11.$tag === 0) { const pairs = $match_11.$0; return Vibe.Ok(Vibe._COLON_COLON([k, val])(pairs)); } throw new Error("Pattern match failed"); })(decodeKeyValues(decoder)(json)(rest)); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(item)); } throw new Error("Pattern match failed"); })(objectGet(k)(json)); } throw new Error("Pattern match failed"); })(keys);
const decodeKeyValuePairs_ = (decoder) => (json) => (_isObject(json) ? decodeKeyValues(decoder)(json)(_objectKeys(json)) : Vibe.Err(Failure("Expecting an OBJECT")));
const keyValuePairs = (decoder) => Decoder(decodeKeyValuePairs_(decoder));
const decodeField_ = (key) => (decoder) => (json) => (_isObject(json) ? (($match_12) => { if ($match_12.$tag === 1) { return Vibe.Err(Field(key)(Failure("Missing field"))); } if ($match_12.$tag === 0) { const sub = $match_12.$0; return (($match_13) => { if ($match_13.$tag === 1) { const e = $match_13.$0; return Vibe.Err(Field(key)(e)); } if ($match_13.$tag === 0) { const val = $match_13.$0; return Vibe.Ok(val); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(sub)); } throw new Error("Pattern match failed"); })(objectGet(key)(json)) : Vibe.Err(Failure("Expecting an OBJECT")));
const field = (key) => (decoder) => Decoder(decodeField_(key)(decoder));
const at = (path) => (decoder) => (($match_14) => { if (Array.isArray($match_14) && $match_14.length === 0) { return decoder; } if (Array.isArray($match_14) && $match_14.length >= 1) { const k = $match_14[0]; const rest = $match_14.slice(1); return field(k)(at(rest)(decoder)); } throw new Error("Pattern match failed"); })(path);
const decodeIndex_ = (idx) => (decoder) => (json) => (_isArray(json) ? (($match_15) => { if ($match_15.$tag === 1) { return Vibe.Err(Index(idx)(Failure("Index out of bounds"))); } if ($match_15.$tag === 0) { const sub = $match_15.$0; return (($match_16) => { if ($match_16.$tag === 1) { const e = $match_16.$0; return Vibe.Err(Index(idx)(e)); } if ($match_16.$tag === 0) { const val = $match_16.$0; return Vibe.Ok(val); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(sub)); } throw new Error("Pattern match failed"); })(arrayGet(idx)(json)) : Vibe.Err(Failure("Expecting an ARRAY")));
const index = (idx) => (decoder) => Decoder(decodeIndex_(idx)(decoder));
const decodeMap2_ = (f) => (da) => (db) => (json) => (($match_17) => { if ($match_17.$tag === 1) { const e = $match_17.$0; return Vibe.Err(e); } if ($match_17.$tag === 0) { const a = $match_17.$0; return (($match_18) => { if ($match_18.$tag === 1) { const e = $match_18.$0; return Vibe.Err(e); } if ($match_18.$tag === 0) { const b = $match_18.$0; return Vibe.Ok(f(a)(b)); } throw new Error("Pattern match failed"); })(runDecoder(db)(json)); } throw new Error("Pattern match failed"); })(runDecoder(da)(json));
const map2 = (f) => (da) => (db) => Decoder(decodeMap2_(f)(da)(db));
const decodeMap3_ = (f) => (da) => (db) => (dc) => (json) => (($match_19) => { if ($match_19.$tag === 1) { const e = $match_19.$0; return Vibe.Err(e); } if ($match_19.$tag === 0) { const a = $match_19.$0; return (($match_20) => { if ($match_20.$tag === 1) { const e = $match_20.$0; return Vibe.Err(e); } if ($match_20.$tag === 0) { const b = $match_20.$0; return (($match_21) => { if ($match_21.$tag === 1) { const e = $match_21.$0; return Vibe.Err(e); } if ($match_21.$tag === 0) { const c = $match_21.$0; return Vibe.Ok(f(a)(b)(c)); } throw new Error("Pattern match failed"); })(runDecoder(dc)(json)); } throw new Error("Pattern match failed"); })(runDecoder(db)(json)); } throw new Error("Pattern match failed"); })(runDecoder(da)(json));
const map3 = (f) => (da) => (db) => (dc) => Decoder(decodeMap3_(f)(da)(db)(dc));
const decodeMap4_ = (f) => (da) => (db) => (dc) => (dd) => (json) => (($match_22) => { if ($match_22.$tag === 1) { const e = $match_22.$0; return Vibe.Err(e); } if ($match_22.$tag === 0) { const a = $match_22.$0; return (($match_23) => { if ($match_23.$tag === 1) { const e = $match_23.$0; return Vibe.Err(e); } if ($match_23.$tag === 0) { const b = $match_23.$0; return (($match_24) => { if ($match_24.$tag === 1) { const e = $match_24.$0; return Vibe.Err(e); } if ($match_24.$tag === 0) { const c = $match_24.$0; return (($match_25) => { if ($match_25.$tag === 1) { const e = $match_25.$0; return Vibe.Err(e); } if ($match_25.$tag === 0) { const d = $match_25.$0; return Vibe.Ok(f(a)(b)(c)(d)); } throw new Error("Pattern match failed"); })(runDecoder(dd)(json)); } throw new Error("Pattern match failed"); })(runDecoder(dc)(json)); } throw new Error("Pattern match failed"); })(runDecoder(db)(json)); } throw new Error("Pattern match failed"); })(runDecoder(da)(json));
const map4 = (f) => (da) => (db) => (dc) => (dd) => Decoder(decodeMap4_(f)(da)(db)(dc)(dd));
const decodeMap5_ = (fn) => (da) => (db) => (dc) => (dd) => (de) => (json) => (($match_26) => { if ($match_26.$tag === 1) { const e = $match_26.$0; return Vibe.Err(e); } if ($match_26.$tag === 0) { const a = $match_26.$0; return (($match_27) => { if ($match_27.$tag === 1) { const e = $match_27.$0; return Vibe.Err(e); } if ($match_27.$tag === 0) { const b = $match_27.$0; return (($match_28) => { if ($match_28.$tag === 1) { const e = $match_28.$0; return Vibe.Err(e); } if ($match_28.$tag === 0) { const c = $match_28.$0; return (($match_29) => { if ($match_29.$tag === 1) { const e = $match_29.$0; return Vibe.Err(e); } if ($match_29.$tag === 0) { const d = $match_29.$0; return (($match_30) => { if ($match_30.$tag === 1) { const e = $match_30.$0; return Vibe.Err(e); } if ($match_30.$tag === 0) { const ee = $match_30.$0; return Vibe.Ok(fn(a)(b)(c)(d)(ee)); } throw new Error("Pattern match failed"); })(runDecoder(de)(json)); } throw new Error("Pattern match failed"); })(runDecoder(dd)(json)); } throw new Error("Pattern match failed"); })(runDecoder(dc)(json)); } throw new Error("Pattern match failed"); })(runDecoder(db)(json)); } throw new Error("Pattern match failed"); })(runDecoder(da)(json));
const map5 = (fn) => (da) => (db) => (dc) => (dd) => (de) => Decoder(decodeMap5_(fn)(da)(db)(dc)(dd)(de));
const decodeSucceed_ = (val) => (_) => Vibe.Ok(val);
const succeed = (val) => Decoder(decodeSucceed_(val));
const decodeFail_ = (msg) => (_) => Vibe.Err(Failure(msg));
const fail = (msg) => Decoder(decodeFail_(msg));
const decodeAndThen_ = (callback) => (decoder) => (json) => (($match_31) => { if ($match_31.$tag === 1) { const e = $match_31.$0; return Vibe.Err(e); } if ($match_31.$tag === 0) { const a = $match_31.$0; return runDecoder(callback(a))(json); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(json));
const andThen = (callback) => (decoder) => Decoder(decodeAndThen_(callback)(decoder));
const decodeLazy_ = (thunk) => (json) => runDecoder(thunk(undefined))(json);
const lazy = (thunk) => Decoder(decodeLazy_(thunk));
let jsonToValue;
let convertArray;
let convertObject;
jsonToValue = (json) => (_isNull(json) ? Encode.JsonNull : (_isBool(json) ? Encode.JsonBool(_toBool(json)) : (_isInt(json) ? Encode.JsonInt(_toInt(json)) : (_isNumber(json) ? Encode.JsonFloat(_toFloat(json)) : (_isString(json) ? Encode.JsonString(_jsonToString(json)) : (_isArray(json) ? Encode.JsonArray(convertArray(json)(0)(_arrayLength(json))) : (_isObject(json) ? Encode.JsonObject(convertObject(json)(_objectKeys(json))) : Encode.JsonNull)))))));
convertArray = (json) => (idx) => (len) => ($inst_Int.$dict_Ord_Int._GT_EQ(idx)(len) ? [] : (($match_32) => { if ($match_32.$tag === 1) { return []; } if ($match_32.$tag === 0) { const item = $match_32.$0; return Vibe._COLON_COLON(jsonToValue(item))(convertArray(json)($inst_Int.$dict_Num_Int._PLUS(idx)(1))(len)); } throw new Error("Pattern match failed"); })(arrayGet(idx)(json)));
convertObject = (json) => (keys) => { while (true) { { const $match_33 = keys; if (Array.isArray($match_33) && $match_33.length === 0) { return []; } if (Array.isArray($match_33) && $match_33.length >= 1) { const k = $match_33[0]; const rest = $match_33.slice(1); { const $match_34 = objectGet(k)(json); if ($match_34.$tag === 1) { [json, keys] = [json, rest]; continue; } if ($match_34.$tag === 0) { const item = $match_34.$0; return Vibe._COLON_COLON([k, jsonToValue(item)])(convertObject(json)(rest)); } throw new Error("Pattern match failed"); } } throw new Error("Pattern match failed"); } } };
const decodeValue_ = (json) => Vibe.Ok(jsonToValue(json));
const value = Decoder(decodeValue_);
let errorToString;
let errorToStringHelp;
let formatOneOf;
errorToString = (err) => errorToStringHelp("")(err);
errorToStringHelp = (context) => (err) => { while (true) { { const $match_35 = err; if ($match_35.$tag === 3) { const msg = $match_35.$0; if (String.isEmpty(context)) { return msg; } else { return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("At ")(context))(": "))(msg); } } if ($match_35.$tag === 0) { const key = $match_35.$0; const inner = $match_35.$1; { const newContext = (String.isEmpty(context) ? String.$dict_Appendable_String._PLUS_PLUS(".")(key) : String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(context)("."))(key)); [context, err] = [newContext, inner]; continue; } } if ($match_35.$tag === 1) { const idx = $match_35.$0; const inner = $match_35.$1; { const newContext = (String.isEmpty(context) ? String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("[")($inst_Int.$dict_Show_Int.toString(idx)))("]") : String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(context)("["))($inst_Int.$dict_Show_Int.toString(idx)))("]")); [context, err] = [newContext, inner]; continue; } } if ($match_35.$tag === 2) { const errors = $match_35.$0; { const prefix = (String.isEmpty(context) ? "One of the following failed:\n" : String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("At ")(context))(", one of the following failed:\n")); return String.$dict_Appendable_String._PLUS_PLUS(prefix)(formatOneOf(errors)(1)); } } throw new Error("Pattern match failed"); } } };
formatOneOf = (errors) => (n) => (($match_36) => { if (Array.isArray($match_36) && $match_36.length === 0) { return ""; } if (Array.isArray($match_36) && $match_36.length >= 1) { const e = $match_36[0]; const rest = $match_36.slice(1); return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("  (")($inst_Int.$dict_Show_Int.toString(n)))(") "))(errorToString(e)))("\n"))(formatOneOf(rest)($inst_Int.$dict_Num_Int._PLUS(n)(1))); } throw new Error("Pattern match failed"); })(errors);
const decodeString = (decoder) => (str) => (($match_37) => { if ($match_37.$tag === 1) { const msg = $match_37.$0; return Vibe.Err(String.$dict_Appendable_String._PLUS_PLUS("Invalid JSON: ")(msg)); } if ($match_37.$tag === 0) { const json = $match_37.$0; return (($match_38) => { if ($match_38.$tag === 0) { const val = $match_38.$0; return Vibe.Ok(val); } if ($match_38.$tag === 1) { const err = $match_38.$0; return Vibe.Err(errorToString(err)); } throw new Error("Pattern match failed"); })(runDecoder(decoder)(json)); } throw new Error("Pattern match failed"); })(jsonParse(str));
let $impl_Eq_Error__EQ_EQ;
let $dict_Eq_Error;
$impl_Eq_Error__EQ_EQ = (x_impl) => (y_impl) => (($match_39) => { if ($match_39[0].$tag === 0 && $match_39[1].$tag === 0) { const a_0 = $match_39[0].$0; const a_1 = $match_39[0].$1; const b_0 = $match_39[1].$0; const b_1 = $match_39[1].$1; return _AMP_AMP(String.$dict_Eq_String._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1)); } if ($match_39[0].$tag === 1 && $match_39[1].$tag === 1) { const a_0 = $match_39[0].$0; const a_1 = $match_39[0].$1; const b_0 = $match_39[1].$0; const b_1 = $match_39[1].$1; return _AMP_AMP($inst_Int.$dict_Eq_Int._EQ_EQ(a_0)(b_0))(() => $dict_Eq_Error._EQ_EQ(a_1)(b_1)); } if ($match_39[0].$tag === 2 && $match_39[1].$tag === 2) { const a_0 = $match_39[0].$0; const b_0 = $match_39[1].$0; return List.$dict_Eq_List_v356($dict_Eq_Error)._EQ_EQ(a_0)(b_0); } if ($match_39[0].$tag === 3 && $match_39[1].$tag === 3) { const a_0 = $match_39[0].$0; const b_0 = $match_39[1].$0; return String.$dict_Eq_String._EQ_EQ(a_0)(b_0); } { return false; } throw new Error("Pattern match failed"); })([x_impl, y_impl]);
$dict_Eq_Error = {
  _EQ_EQ: $impl_Eq_Error__EQ_EQ
};
let $impl_Show_Error_toString;
let $dict_Show_Error;
$impl_Show_Error_toString = (x_impl) => (($match_40) => { if ($match_40.$tag === 0) { const a0 = $match_40.$0; const a1 = $match_40.$1; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("Field(")(String.$dict_Show_String.toString(a0)))(", "))($dict_Show_Error.toString(a1)))(")"); } if ($match_40.$tag === 1) { const a0 = $match_40.$0; const a1 = $match_40.$1; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("Index(")($inst_Int.$dict_Show_Int.toString(a0)))(", "))($dict_Show_Error.toString(a1)))(")"); } if ($match_40.$tag === 2) { const a0 = $match_40.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("OneOf(")(List.$dict_Show_List_v357($dict_Show_Error).toString(a0)))(")"); } if ($match_40.$tag === 3) { const a0 = $match_40.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("Failure(")(String.$dict_Show_String.toString(a0)))(")"); } throw new Error("Pattern match failed"); })(x_impl);
$dict_Show_Error = {
  toString: $impl_Show_Error_toString
};

export { $dict_Eq_Error, $dict_Show_Error, $null, Failure, Field, Index, OneOf, andThen, at, bool, decodeString, errorToString, fail, field, float, index, int, keyValuePairs, lazy, list, map, map2, map3, map4, map5, maybe, nullable, oneOf, string, succeed, value };