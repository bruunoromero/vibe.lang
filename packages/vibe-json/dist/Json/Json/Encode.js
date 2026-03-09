import * as Vibe from "../../Vibe/Vibe.js";
import * as String from "../../Vibe/Vibe/String.js";
import * as Char from "../../Vibe/Vibe/Char.js";
import * as List from "../../Vibe/Vibe/List.js";
import * as $inst_Unit from "../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../Vibe/Vibe/Float.js";

// Short-Circuit Operator Helpers
const _AMP_AMP = (a) => (b) => a && b();

// ADT Constructors
const JsonString = ($0) => ({ $tag: 0, $0 });
const JsonInt = ($0) => ({ $tag: 1, $0 });
const JsonFloat = ($0) => ({ $tag: 2, $0 });
const JsonBool = ($0) => ({ $tag: 3, $0 });
const JsonNull = { $tag: 4 };
const JsonArray = ($0) => ({ $tag: 5, $0 });
const JsonObject = ($0) => ({ $tag: 6, $0 });

// Values
const string = (s) => JsonString(s);
const int = (n) => JsonInt(n);
const float = (n) => JsonFloat(n);
const bool = (b) => JsonBool(b);
const $null = { $tag: 4 };
const hexDigit = (n) => (($match_0) => { if ($match_0 === 0) { return "0"; } if ($match_0 === 1) { return "1"; } if ($match_0 === 2) { return "2"; } if ($match_0 === 3) { return "3"; } if ($match_0 === 4) { return "4"; } if ($match_0 === 5) { return "5"; } if ($match_0 === 6) { return "6"; } if ($match_0 === 7) { return "7"; } if ($match_0 === 8) { return "8"; } if ($match_0 === 9) { return "9"; } if ($match_0 === 10) { return "a"; } if ($match_0 === 11) { return "b"; } if ($match_0 === 12) { return "c"; } if ($match_0 === 13) { return "d"; } if ($match_0 === 14) { return "e"; } { return "f"; } throw new Error("Pattern match failed"); })(n);
const hexByte = (n) => ((hi) => ((lo) => String.$dict_Appendable_String._PLUS_PLUS(hexDigit(hi))(hexDigit(lo)))($inst_Int.$dict_Integral_Int._PERCENT(n)(16)))($inst_Int.$dict_Integral_Int._SLASH_SLASH(n)(16));
const escapeControl = (code) => String.$dict_Appendable_String._PLUS_PLUS("\\u00")(hexByte(code));
const escapeChar = (c) => (($match_1) => { if ($match_1 === "\"") { return "\\\""; } if ($match_1 === "\\") { return "\\\\"; } if ($match_1 === "\n") { return "\\n"; } if ($match_1 === "\r") { return "\\r"; } if ($match_1 === "\t") { return "\\t"; } { return ((code) => ($inst_Int.$dict_Ord_Int._LT(code)(32) ? escapeControl(code) : Char.$dict_Show_Char.toString(c)))(Char.toCode(c)); } throw new Error("Pattern match failed"); })(c);
const escapeChars = (chars) => ((go) => go(chars)(""))((cs) => (acc) => { while (true) { { const $match_2 = cs; if (Array.isArray($match_2) && $match_2.length === 0) { return acc; } if (Array.isArray($match_2) && $match_2.length >= 1) { const c = $match_2[0]; const rest = $match_2.slice(1); [cs, acc] = [rest, String.$dict_Appendable_String._PLUS_PLUS(acc)(escapeChar(c))]; continue; } throw new Error("Pattern match failed"); } } });
const renderString = (s) => String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("\"")(escapeChars(String.toList(s))))("\"");
const renderFloat = (n) => ((s) => (($match_3) => { if ($match_3 === "NaN") { return "null"; } if ($match_3 === "Infinity") { return "null"; } if ($match_3 === "-Infinity") { return "null"; } { return s; } throw new Error("Pattern match failed"); })(s))($inst_Float.$dict_Show_Float.toString(n));
const repeatSpaces = (n) => ($inst_Int.$dict_Ord_Int._LT_EQ(n)(0) ? "" : String.$dict_Appendable_String._PLUS_PLUS(" ")(repeatSpaces($inst_Int.$dict_Num_Int._MINUS(n)(1))));
const makeIndent = (indent) => (depth) => repeatSpaces($inst_Int.$dict_Num_Int._STAR(indent)(depth));
let renderValue;
let renderArray;
let renderObject;
let joinValues;
let joinValuesIndented;
let joinPairs;
let joinPairsIndented;
renderValue = (indent) => (depth) => (val) => (($match_4) => { if ($match_4.$tag === 0) { const s = $match_4.$0; return renderString(s); } if ($match_4.$tag === 1) { const n = $match_4.$0; return $inst_Int.$dict_Show_Int.toString(n); } if ($match_4.$tag === 2) { const n = $match_4.$0; return renderFloat(n); } if ($match_4.$tag === 3) { const b = $match_4.$0; return (b ? "true" : "false"); } if ($match_4.$tag === 4) { return "null"; } if ($match_4.$tag === 5) { const items = $match_4.$0; return renderArray(indent)(depth)(items); } if ($match_4.$tag === 6) { const pairs = $match_4.$0; return renderObject(indent)(depth)(pairs); } throw new Error("Pattern match failed"); })(val);
renderArray = (indent) => (depth) => (items) => (($match_5) => { if (Array.isArray($match_5) && $match_5.length === 0) { return "[]"; } { return ($inst_Int.$dict_Eq_Int._EQ_EQ(indent)(0) ? String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("[")(joinValues(indent)(depth)(items)))("]") : ((newDepth) => ((pad) => ((innerPad) => String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("[\n")(pad))(joinValuesIndented(indent)(newDepth)(items)))("\n"))(innerPad))("]"))(makeIndent(indent)(depth)))(makeIndent(indent)(newDepth)))($inst_Int.$dict_Num_Int._PLUS(depth)(1))); } throw new Error("Pattern match failed"); })(items);
renderObject = (indent) => (depth) => (pairs) => (($match_6) => { if (Array.isArray($match_6) && $match_6.length === 0) { return "{}"; } { return ($inst_Int.$dict_Eq_Int._EQ_EQ(indent)(0) ? String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("{")(joinPairs(indent)(depth)(pairs)))("}") : ((newDepth) => ((pad) => ((innerPad) => String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("{\n")(pad))(joinPairsIndented(indent)(newDepth)(pairs)))("\n"))(innerPad))("}"))(makeIndent(indent)(depth)))(makeIndent(indent)(newDepth)))($inst_Int.$dict_Num_Int._PLUS(depth)(1))); } throw new Error("Pattern match failed"); })(pairs);
joinValues = (indent) => (depth) => (items) => ((go) => go(items)(""))((xs) => (acc) => { while (true) { { const $match_7 = xs; if (Array.isArray($match_7) && $match_7.length === 0) { return acc; } if (Array.isArray($match_7) && $match_7.length === 1) { const x = $match_7[0]; return String.$dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)); } if (Array.isArray($match_7) && $match_7.length >= 1) { const x = $match_7[0]; const rest = $match_7.slice(1); [xs, acc] = [rest, String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)))(",")]; continue; } throw new Error("Pattern match failed"); } } });
joinValuesIndented = (indent) => (depth) => (items) => ((pad) => ((go) => go(items)(""))((xs) => (acc) => { while (true) { { const $match_8 = xs; if (Array.isArray($match_8) && $match_8.length === 0) { return acc; } if (Array.isArray($match_8) && $match_8.length === 1) { const x = $match_8[0]; return String.$dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)); } if (Array.isArray($match_8) && $match_8.length >= 1) { const x = $match_8[0]; const rest = $match_8.slice(1); [xs, acc] = [rest, String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(acc)(renderValue(indent)(depth)(x)))(",\n"))(pad)]; continue; } throw new Error("Pattern match failed"); } } }))(makeIndent(indent)(depth));
joinPairs = (indent) => (depth) => (pairs) => ((go) => go(pairs)(""))((ps) => (acc) => { while (true) { { const $match_9 = ps; if (Array.isArray($match_9) && $match_9.length === 0) { return acc; } if (Array.isArray($match_9) && $match_9.length === 1) { const k = $match_9[0][0]; const v = $match_9[0][1]; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(":"))(renderValue(indent)(depth)(v)); } if (Array.isArray($match_9) && $match_9.length >= 1) { const k = $match_9[0][0]; const v = $match_9[0][1]; const rest = $match_9.slice(1); [ps, acc] = [rest, String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(":"))(renderValue(indent)(depth)(v)))(",")]; continue; } throw new Error("Pattern match failed"); } } });
joinPairsIndented = (indent) => (depth) => (pairs) => ((pad) => ((go) => go(pairs)(""))((ps) => (acc) => { while (true) { { const $match_10 = ps; if (Array.isArray($match_10) && $match_10.length === 0) { return acc; } if (Array.isArray($match_10) && $match_10.length === 1) { const k = $match_10[0][0]; const v = $match_10[0][1]; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(": "))(renderValue(indent)(depth)(v)); } if (Array.isArray($match_10) && $match_10.length >= 1) { const k = $match_10[0][0]; const v = $match_10[0][1]; const rest = $match_10.slice(1); [ps, acc] = [rest, String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(acc)(renderString(k)))(": "))(renderValue(indent)(depth)(v)))(",\n"))(pad)]; continue; } throw new Error("Pattern match failed"); } } }))(makeIndent(indent)(depth));
const encode = (indent) => (val) => renderValue(indent)(0)(val);
const list = (encode) => (items) => JsonArray(List.map(encode)(items));
const object = (pairs) => JsonObject(pairs);
const $impl_Eq_Value__EQ_EQ = (x_impl) => (y_impl) => (($match_11) => { if ($match_11[0].$tag === 0 && $match_11[1].$tag === 0) { const a_0 = $match_11[0].$0; const b_0 = $match_11[1].$0; return String.$dict_Eq_String._EQ_EQ(a_0)(b_0); } if ($match_11[0].$tag === 1 && $match_11[1].$tag === 1) { const a_0 = $match_11[0].$0; const b_0 = $match_11[1].$0; return $inst_Int.$dict_Eq_Int._EQ_EQ(a_0)(b_0); } if ($match_11[0].$tag === 2 && $match_11[1].$tag === 2) { const a_0 = $match_11[0].$0; const b_0 = $match_11[1].$0; return $inst_Float.$dict_Eq_Float._EQ_EQ(a_0)(b_0); } if ($match_11[0].$tag === 3 && $match_11[1].$tag === 3) { const a_0 = $match_11[0].$0; const b_0 = $match_11[1].$0; return $inst_Bool.$dict_Eq_Bool._EQ_EQ(a_0)(b_0); } if ($match_11[0].$tag === 4 && $match_11[1].$tag === 4) { return true; } if ($match_11[0].$tag === 5 && $match_11[1].$tag === 5) { const a_0 = $match_11[0].$0; const b_0 = $match_11[1].$0; return List.$dict_Eq_List_v356($dict_Eq_Value)._EQ_EQ(a_0)(b_0); } if ($match_11[0].$tag === 6 && $match_11[1].$tag === 6) { const a_0 = $match_11[0].$0; const b_0 = $match_11[1].$0; return List.$dict_Eq_List_v356($dict_Eq_tuple_String_Value)._EQ_EQ(a_0)(b_0); } { return false; } throw new Error("Pattern match failed"); })([x_impl, y_impl]);
const $dict_Eq_Value = {
  _EQ_EQ: $impl_Eq_Value__EQ_EQ
};
const $impl_Eq_tuple_String_Value__EQ_EQ = (x_impl) => (y_impl) => (($match_12) => { { const x0 = $match_12[0][0]; const x1 = $match_12[0][1]; const y0 = $match_12[1][0]; const y1 = $match_12[1][1]; return _AMP_AMP(String.$dict_Eq_String._EQ_EQ(x0)(y0))(() => $dict_Eq_Value._EQ_EQ(x1)(y1)); } throw new Error("Pattern match failed"); })([x_impl, y_impl]);
const $impl_Show_Value_toString = (x_impl) => (($match_13) => { if ($match_13.$tag === 0) { const a0 = $match_13.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("JsonString(")(String.$dict_Show_String.toString(a0)))(")"); } if ($match_13.$tag === 1) { const a0 = $match_13.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("JsonInt(")($inst_Int.$dict_Show_Int.toString(a0)))(")"); } if ($match_13.$tag === 2) { const a0 = $match_13.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("JsonFloat(")($inst_Float.$dict_Show_Float.toString(a0)))(")"); } if ($match_13.$tag === 3) { const a0 = $match_13.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("JsonBool(")($inst_Bool.$dict_Show_Bool.toString(a0)))(")"); } if ($match_13.$tag === 4) { return "JsonNull"; } if ($match_13.$tag === 5) { const a0 = $match_13.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("JsonArray(")(List.$dict_Show_List_v357($dict_Show_Value).toString(a0)))(")"); } if ($match_13.$tag === 6) { const a0 = $match_13.$0; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("JsonObject(")(List.$dict_Show_List_v357($dict_Show_tuple_String_Value).toString(a0)))(")"); } throw new Error("Pattern match failed"); })(x_impl);
const $dict_Show_Value = {
  toString: $impl_Show_Value_toString
};
const $impl_Show_tuple_String_Value_toString = (x_impl) => (($match_14) => { { const x0 = $match_14[0]; const x1 = $match_14[1]; return String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS(String.$dict_Appendable_String._PLUS_PLUS("(")(String.$dict_Show_String.toString(x0)))(", "))($dict_Show_Value.toString(x1)))(")"); } throw new Error("Pattern match failed"); })(x_impl);
const $dict_Eq_tuple_String_Value = {
  _EQ_EQ: $impl_Eq_tuple_String_Value__EQ_EQ
};
const $dict_Show_tuple_String_Value = {
  toString: $impl_Show_tuple_String_Value_toString
};

export { $dict_Eq_Value, $dict_Eq_tuple_String_Value, $dict_Show_Value, $dict_Show_tuple_String_Value, $null, JsonArray, JsonBool, JsonFloat, JsonInt, JsonNull, JsonObject, JsonString, bool, encode, float, int, list, object, string };