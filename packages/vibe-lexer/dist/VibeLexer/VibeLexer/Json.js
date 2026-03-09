import * as Vibe from "../../Vibe/Vibe.js";
import * as List from "../../Vibe/Vibe/List.js";
import * as E from "../../Json/Json/Encode.js";
import * as Types from "../VibeLexer/Types.js";
import * as $inst_Unit from "../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../Vibe/Vibe/Float.js";
import * as $inst_String from "../../Vibe/Vibe/String.js";
import * as $inst_Char from "../../Vibe/Vibe/Char.js";

// Values
const tokenKindToString = (kind) => (($match_0) => { if ($match_0.$tag === 0) { return "LowerIdentifier"; } if ($match_0.$tag === 1) { return "UpperIdentifier"; } if ($match_0.$tag === 2) { return "Keyword"; } if ($match_0.$tag === 3) { return "Number"; } if ($match_0.$tag === 4) { return "String"; } if ($match_0.$tag === 5) { return "Char"; } if ($match_0.$tag === 6) { return "Operator"; } if ($match_0.$tag === 7) { return "Range"; } if ($match_0.$tag === 8) { return "Backslash"; } if ($match_0.$tag === 9) { return "LParen"; } if ($match_0.$tag === 10) { return "RParen"; } if ($match_0.$tag === 11) { return "LBrace"; } if ($match_0.$tag === 12) { return "RBrace"; } if ($match_0.$tag === 13) { return "LBracket"; } if ($match_0.$tag === 14) { return "RBracket"; } if ($match_0.$tag === 15) { return "Comma"; } if ($match_0.$tag === 16) { return "Dot"; } if ($match_0.$tag === 17) { return "Colon"; } if ($match_0.$tag === 18) { return "Equals"; } if ($match_0.$tag === 19) { return "Pipe"; } if ($match_0.$tag === 20) { return "Newline"; } if ($match_0.$tag === 21) { return "BlockStart"; } if ($match_0.$tag === 22) { return "BlockSep"; } if ($match_0.$tag === 23) { return "BlockEnd"; } if ($match_0.$tag === 24) { return "Eof"; } throw new Error("Pattern match failed"); })(kind);
const encodePosition = (pos) => E.object([["offset", E.int(pos.offset)], ["line", E.int(pos.line)], ["column", E.int(pos.column)]]);
const encodeSpan = (span) => E.object([["start", encodePosition(span.start)], ["end", encodePosition(span.end)]]);
const encodeToken = (tok) => E.object([["kind", E.string(tokenKindToString(tok.kind))], ["lexeme", E.string(tok.lexeme)], ["span", encodeSpan(tok.span)]]);
const lexToJson = (lexFn) => (source) => (($match_1) => { if ($match_1.$tag === 0) { const tokens = $match_1.$0; return E.encode(0)(E.object([["ok", E.bool(true)], ["tokens", E.list(encodeToken)(tokens)]])); } if ($match_1.$tag === 1) { const err = $match_1.$0; return E.encode(0)(E.object([["ok", E.bool(false)], ["message", E.string(err.message)], ["span", encodeSpan(err.span)]])); } throw new Error("Pattern match failed"); })(lexFn(source));

export { lexToJson };