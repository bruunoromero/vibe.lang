import * as Vibe from "../../Vibe/Vibe.js";
import * as $inst_Unit from "../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../Vibe/Vibe/Float.js";
import * as $inst_String from "../../Vibe/Vibe/String.js";
import * as $inst_Char from "../../Vibe/Vibe/Char.js";
import * as $inst_List from "../../Vibe/Vibe/List.js";

// Short-Circuit Operator Helpers
const _AMP_AMP = (a) => (b) => a && b();

// ADT Constructors
const LowerIdentifier = { $tag: 0 };
const UpperIdentifier = { $tag: 1 };
const Keyword = { $tag: 2 };
const NumberToken = { $tag: 3 };
const StringToken = { $tag: 4 };
const CharToken = { $tag: 5 };
const Operator = { $tag: 6 };
const Range = { $tag: 7 };
const Backslash = { $tag: 8 };
const LParen = { $tag: 9 };
const RParen = { $tag: 10 };
const LBrace = { $tag: 11 };
const RBrace = { $tag: 12 };
const LBracket = { $tag: 13 };
const RBracket = { $tag: 14 };
const Comma = { $tag: 15 };
const Dot = { $tag: 16 };
const Colon = { $tag: 17 };
const Equals = { $tag: 18 };
const Pipe = { $tag: 19 };
const Newline = { $tag: 20 };
const BlockStart = { $tag: 21 };
const BlockSep = { $tag: 22 };
const BlockEnd = { $tag: 23 };
const Eof = { $tag: 24 };

// Values
const $impl_Eq_TokenKind__EQ_EQ = (x_impl) => (y_impl) => (($match_0) => { if ($match_0[0].$tag === 0 && $match_0[1].$tag === 0) { return true; } if ($match_0[0].$tag === 1 && $match_0[1].$tag === 1) { return true; } if ($match_0[0].$tag === 2 && $match_0[1].$tag === 2) { return true; } if ($match_0[0].$tag === 3 && $match_0[1].$tag === 3) { return true; } if ($match_0[0].$tag === 4 && $match_0[1].$tag === 4) { return true; } if ($match_0[0].$tag === 5 && $match_0[1].$tag === 5) { return true; } if ($match_0[0].$tag === 6 && $match_0[1].$tag === 6) { return true; } if ($match_0[0].$tag === 7 && $match_0[1].$tag === 7) { return true; } if ($match_0[0].$tag === 8 && $match_0[1].$tag === 8) { return true; } if ($match_0[0].$tag === 9 && $match_0[1].$tag === 9) { return true; } if ($match_0[0].$tag === 10 && $match_0[1].$tag === 10) { return true; } if ($match_0[0].$tag === 11 && $match_0[1].$tag === 11) { return true; } if ($match_0[0].$tag === 12 && $match_0[1].$tag === 12) { return true; } if ($match_0[0].$tag === 13 && $match_0[1].$tag === 13) { return true; } if ($match_0[0].$tag === 14 && $match_0[1].$tag === 14) { return true; } if ($match_0[0].$tag === 15 && $match_0[1].$tag === 15) { return true; } if ($match_0[0].$tag === 16 && $match_0[1].$tag === 16) { return true; } if ($match_0[0].$tag === 17 && $match_0[1].$tag === 17) { return true; } if ($match_0[0].$tag === 18 && $match_0[1].$tag === 18) { return true; } if ($match_0[0].$tag === 19 && $match_0[1].$tag === 19) { return true; } if ($match_0[0].$tag === 20 && $match_0[1].$tag === 20) { return true; } if ($match_0[0].$tag === 21 && $match_0[1].$tag === 21) { return true; } if ($match_0[0].$tag === 22 && $match_0[1].$tag === 22) { return true; } if ($match_0[0].$tag === 23 && $match_0[1].$tag === 23) { return true; } if ($match_0[0].$tag === 24 && $match_0[1].$tag === 24) { return true; } { return false; } throw new Error("Pattern match failed"); })([x_impl, y_impl]);
const $impl_Show_TokenKind_toString = (x_impl) => (($match_1) => { if ($match_1.$tag === 0) { return "LowerIdentifier"; } if ($match_1.$tag === 1) { return "UpperIdentifier"; } if ($match_1.$tag === 2) { return "Keyword"; } if ($match_1.$tag === 3) { return "NumberToken"; } if ($match_1.$tag === 4) { return "StringToken"; } if ($match_1.$tag === 5) { return "CharToken"; } if ($match_1.$tag === 6) { return "Operator"; } if ($match_1.$tag === 7) { return "Range"; } if ($match_1.$tag === 8) { return "Backslash"; } if ($match_1.$tag === 9) { return "LParen"; } if ($match_1.$tag === 10) { return "RParen"; } if ($match_1.$tag === 11) { return "LBrace"; } if ($match_1.$tag === 12) { return "RBrace"; } if ($match_1.$tag === 13) { return "LBracket"; } if ($match_1.$tag === 14) { return "RBracket"; } if ($match_1.$tag === 15) { return "Comma"; } if ($match_1.$tag === 16) { return "Dot"; } if ($match_1.$tag === 17) { return "Colon"; } if ($match_1.$tag === 18) { return "Equals"; } if ($match_1.$tag === 19) { return "Pipe"; } if ($match_1.$tag === 20) { return "Newline"; } if ($match_1.$tag === 21) { return "BlockStart"; } if ($match_1.$tag === 22) { return "BlockSep"; } if ($match_1.$tag === 23) { return "BlockEnd"; } if ($match_1.$tag === 24) { return "Eof"; } throw new Error("Pattern match failed"); })(x_impl);
const $impl_Eq_Position__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP(_AMP_AMP($inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.offset)(y_impl.offset))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.line)(y_impl.line)))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.column)(y_impl.column));
const $impl_Show_Position_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("Position { ")("offset = "))($inst_Int.$dict_Show_Int.toString(x_impl.offset)))(", "))("line = "))($inst_Int.$dict_Show_Int.toString(x_impl.line)))(", "))("column = "))($inst_Int.$dict_Show_Int.toString(x_impl.column)))(" }");
const $dict_Eq_Position = {
  _EQ_EQ: $impl_Eq_Position__EQ_EQ
};
const $impl_Eq_Span__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP($dict_Eq_Position._EQ_EQ(x_impl.start)(y_impl.start))(() => $dict_Eq_Position._EQ_EQ(x_impl.end)(y_impl.end));
const $dict_Show_Position = {
  toString: $impl_Show_Position_toString
};
const $impl_Show_Span_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("Span { ")("start = "))($dict_Show_Position.toString(x_impl.start)))(", "))("end = "))($dict_Show_Position.toString(x_impl.end)))(" }");
const $dict_Eq_TokenKind = {
  _EQ_EQ: $impl_Eq_TokenKind__EQ_EQ
};
const $dict_Eq_Span = {
  _EQ_EQ: $impl_Eq_Span__EQ_EQ
};
const $impl_Eq_Token__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP(_AMP_AMP($dict_Eq_TokenKind._EQ_EQ(x_impl.kind)(y_impl.kind))(() => $inst_String.$dict_Eq_String._EQ_EQ(x_impl.lexeme)(y_impl.lexeme)))(() => $dict_Eq_Span._EQ_EQ(x_impl.span)(y_impl.span));
const $dict_Show_TokenKind = {
  toString: $impl_Show_TokenKind_toString
};
const $dict_Show_Span = {
  toString: $impl_Show_Span_toString
};
const $impl_Show_Token_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("Token { ")("kind = "))($dict_Show_TokenKind.toString(x_impl.kind)))(", "))("lexeme = "))($inst_String.$dict_Show_String.toString(x_impl.lexeme)))(", "))("span = "))($dict_Show_Span.toString(x_impl.span)))(" }");
const $impl_Eq_LexerState__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP(_AMP_AMP(_AMP_AMP(_AMP_AMP($inst_String.$dict_Eq_String._EQ_EQ(x_impl.source)(y_impl.source))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.index)(y_impl.index)))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.line)(y_impl.line)))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.column)(y_impl.column)))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.sourceLen)(y_impl.sourceLen));
const $impl_Show_LexerState_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("LexerState { ")("source = "))($inst_String.$dict_Show_String.toString(x_impl.source)))(", "))("index = "))($inst_Int.$dict_Show_Int.toString(x_impl.index)))(", "))("line = "))($inst_Int.$dict_Show_Int.toString(x_impl.line)))(", "))("column = "))($inst_Int.$dict_Show_Int.toString(x_impl.column)))(", "))("sourceLen = "))($inst_Int.$dict_Show_Int.toString(x_impl.sourceLen)))(" }");
const $impl_Eq_LexError__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP($inst_String.$dict_Eq_String._EQ_EQ(x_impl.message)(y_impl.message))(() => $dict_Eq_Span._EQ_EQ(x_impl.span)(y_impl.span));
const $impl_Show_LexError_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("LexError { ")("message = "))($inst_String.$dict_Show_String.toString(x_impl.message)))(", "))("span = "))($dict_Show_Span.toString(x_impl.span)))(" }");
const $dict_Eq_Token = {
  _EQ_EQ: $impl_Eq_Token__EQ_EQ
};
const $dict_Show_Token = {
  toString: $impl_Show_Token_toString
};
const $dict_Eq_LexerState = {
  _EQ_EQ: $impl_Eq_LexerState__EQ_EQ
};
const $dict_Show_LexerState = {
  toString: $impl_Show_LexerState_toString
};
const $dict_Eq_LexError = {
  _EQ_EQ: $impl_Eq_LexError__EQ_EQ
};
const $dict_Show_LexError = {
  toString: $impl_Show_LexError_toString
};

export { $dict_Eq_LexError, $dict_Eq_LexerState, $dict_Eq_Position, $dict_Eq_Span, $dict_Eq_Token, $dict_Eq_TokenKind, $dict_Show_LexError, $dict_Show_LexerState, $dict_Show_Position, $dict_Show_Span, $dict_Show_Token, $dict_Show_TokenKind, Backslash, BlockEnd, BlockSep, BlockStart, CharToken, Colon, Comma, Dot, Eof, Equals, Keyword, LBrace, LBracket, LParen, LowerIdentifier, Newline, NumberToken, Operator, Pipe, RBrace, RBracket, RParen, Range, StringToken, UpperIdentifier };