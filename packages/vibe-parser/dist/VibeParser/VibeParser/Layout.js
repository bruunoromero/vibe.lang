import * as Vibe from "../../Vibe/Vibe.js";
import * as List from "../../Vibe/Vibe/List.js";
import * as T from "../../VibeLexer/VibeLexer/Types.js";
import * as $inst_Unit from "../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../Vibe/Vibe/Float.js";
import * as $inst_String from "../../Vibe/Vibe/String.js";
import * as $inst_Char from "../../Vibe/Vibe/Char.js";
import * as $inst_Parser from "../VibeParser/Parser.js";

// Short-Circuit Operator Helpers
const _AMP_AMP = (a) => (b) => a && b();

// Values
const initState = ({ output: [], stack: [], bracketDepth: 0 });
const makeVirtual = (kind) => (span) => ({ kind: kind, lexeme: "", span: span });
const isLayoutKeyword = (lexeme) => (($match_0) => { if ($match_0 === "let") { return true; } if ($match_0 === "of") { return true; } if ($match_0 === "where") { return true; } { return false; } throw new Error("Pattern match failed"); })(lexeme);
const stackTop = (stack) => List.head(stack);
const stackPop = (stack) => (($match_1) => { if (Array.isArray($match_1) && $match_1.length >= 1) { const rest = $match_1.slice(1); return rest; } if (Array.isArray($match_1) && $match_1.length === 0) { return []; } throw new Error("Pattern match failed"); })(stack);
const isLetContext = (ctx) => $inst_String.$dict_Eq_String._EQ_EQ(ctx.keyword)("let");
const hasLetContext = (stack) => List.any(isLetContext)(stack);
const skipNewlines = (tokens) => { while (true) { { const $match_2 = tokens; if (Array.isArray($match_2) && $match_2.length === 0) { return []; } if (Array.isArray($match_2) && $match_2.length >= 1) { const tok = $match_2[0]; const rest = $match_2.slice(1); { const $match_3 = tok.kind; if ($match_3.$tag === 20) { tokens = rest; continue; } { return tokens; } throw new Error("Pattern match failed"); } } throw new Error("Pattern match failed"); } } };
const nextReal = (tokens) => { while (true) { { const $match_4 = tokens; if (Array.isArray($match_4) && $match_4.length === 0) { return Vibe.Nothing; } if (Array.isArray($match_4) && $match_4.length >= 1) { const tok = $match_4[0]; const rest = $match_4.slice(1); { const $match_5 = tok.kind; if ($match_5.$tag === 20) { tokens = rest; continue; } { return Vibe.Just(tok); } throw new Error("Pattern match failed"); } } throw new Error("Pattern match failed"); } } };
const closeContextsBelow = (col) => (span) => (state) => { while (true) { { const $match_6 = stackTop(state.stack); if ($match_6.$tag === 1) { return state; } if ($match_6.$tag === 0) { const ctx = $match_6.$0; if ($inst_Int.$dict_Ord_Int._LT(col)(ctx.column)) { [col, span, state] = [col, span, ({ ...state, output: Vibe._COLON_COLON(makeVirtual(T.BlockEnd)(span))(state.output), stack: stackPop(state.stack) })]; continue; } else { return state; } } throw new Error("Pattern match failed"); } } };
const closeContextsForBracket = (state) => (span) => { while (true) { { const $match_7 = stackTop(state.stack); if ($match_7.$tag === 1) { return state; } if ($match_7.$tag === 0) { const ctx = $match_7.$0; if ($inst_Int.$dict_Ord_Int._GT_EQ(ctx.bracketDepth)(state.bracketDepth)) { [state, span] = [({ ...state, output: Vibe._COLON_COLON(makeVirtual(T.BlockEnd)(span))(state.output), stack: stackPop(state.stack) }), span]; continue; } else { return state; } } throw new Error("Pattern match failed"); } } };
const closeForIn = (state) => (span) => { while (true) { { const $match_8 = stackTop(state.stack); if ($match_8.$tag === 1) { return state; } if ($match_8.$tag === 0) { const ctx = $match_8.$0; { const newState = ({ ...state, output: Vibe._COLON_COLON(makeVirtual(T.BlockEnd)(span))(state.output), stack: stackPop(state.stack) }); if ($inst_String.$dict_Eq_String._EQ_EQ(ctx.keyword)("let")) { return newState; } else { [state, span] = [newState, span]; continue; } } } throw new Error("Pattern match failed"); } } };
const closeAllContexts = (span) => (state) => { while (true) { { const $match_9 = stackTop(state.stack); if ($match_9.$tag === 1) { return state; } if ($match_9.$tag === 0) { [span, state] = [span, ({ ...state, output: Vibe._COLON_COLON(makeVirtual(T.BlockEnd)(span))(state.output), stack: stackPop(state.stack) })]; continue; } throw new Error("Pattern match failed"); } } };
const handleEof = (tok) => (state) => ((closed) => ({ ...closed, output: Vibe._COLON_COLON(tok)(closed.output) }))(closeAllContexts(tok.span)(state));
let processTokens;
let handleNewline;
let handleOpenBracket;
let handleCloseBracket;
let handleKeyword;
let handleIn;
let handleLayoutKeyword;
processTokens = (tokens) => (state) => { while (true) { { const $match_10 = tokens; if (Array.isArray($match_10) && $match_10.length === 0) { return state; } if (Array.isArray($match_10) && $match_10.length >= 1) { const tok = $match_10[0]; const rest = $match_10.slice(1); { const $match_11 = tok.kind; if ($match_11.$tag === 24) { return handleEof(tok)(state); } if ($match_11.$tag === 20) { return handleNewline(rest)(state); } if ($match_11.$tag === 9) { return handleOpenBracket(tok)(rest)(state); } if ($match_11.$tag === 13) { return handleOpenBracket(tok)(rest)(state); } if ($match_11.$tag === 11) { return handleOpenBracket(tok)(rest)(state); } if ($match_11.$tag === 10) { return handleCloseBracket(tok)(rest)(state); } if ($match_11.$tag === 14) { return handleCloseBracket(tok)(rest)(state); } if ($match_11.$tag === 12) { return handleCloseBracket(tok)(rest)(state); } if ($match_11.$tag === 2) { return handleKeyword(tok)(rest)(state); } { [tokens, state] = [rest, ({ ...state, output: Vibe._COLON_COLON(tok)(state.output) })]; continue; } throw new Error("Pattern match failed"); } } throw new Error("Pattern match failed"); } } };
handleNewline = (rest) => (state) => (($match_12) => { if ($match_12.$tag === 1) { return processTokens(rest)(state); } if ($match_12.$tag === 0) { const nextTok = $match_12.$0; return (($match_13) => { if ($match_13.$tag === 24) { return processTokens(rest)(state); } { return ((col) => ((closed) => ((withSep) => processTokens(rest)(withSep))((($match_14) => { if ($match_14.$tag === 1) { return closed; } if ($match_14.$tag === 0) { const ctx = $match_14.$0; return ($inst_Int.$dict_Eq_Int._EQ_EQ(col)(ctx.column) ? ({ ...closed, output: Vibe._COLON_COLON(makeVirtual(T.BlockSep)(nextTok.span))(closed.output) }) : closed); } throw new Error("Pattern match failed"); })(stackTop(closed.stack))))(closeContextsBelow(col)(nextTok.span)(state)))(nextTok.span.start.column); } throw new Error("Pattern match failed"); })(nextTok.kind); } throw new Error("Pattern match failed"); })(nextReal(rest));
handleOpenBracket = (tok) => (rest) => (state) => processTokens(rest)(({ ...state, output: Vibe._COLON_COLON(tok)(state.output), bracketDepth: $inst_Int.$dict_Num_Int._PLUS(state.bracketDepth)(1) }));
handleCloseBracket = (tok) => (rest) => (state) => ((closed) => ((newDepth) => processTokens(rest)(({ ...closed, output: Vibe._COLON_COLON(tok)(closed.output), bracketDepth: newDepth })))(($inst_Int.$dict_Ord_Int._GT(closed.bracketDepth)(0) ? $inst_Int.$dict_Num_Int._MINUS(closed.bracketDepth)(1) : 0)))(closeContextsForBracket(state)(tok.span));
handleKeyword = (tok) => (rest) => (state) => ($inst_String.$dict_Eq_String._EQ_EQ(tok.lexeme)("in") ? handleIn(tok)(rest)(state) : (isLayoutKeyword(tok.lexeme) ? handleLayoutKeyword(tok)(rest)(state) : processTokens(rest)(({ ...state, output: Vibe._COLON_COLON(tok)(state.output) }))));
handleIn = (tok) => (rest) => (state) => ((closed) => processTokens(rest)(({ ...closed, output: Vibe._COLON_COLON(tok)(closed.output) })))((hasLetContext(state.stack) ? closeForIn(state)(tok.span) : state));
handleLayoutKeyword = (tok) => (rest) => (state) => ((stateWithTok) => ((remaining) => (($match_15) => { if (Array.isArray($match_15) && $match_15.length === 0) { return processTokens(remaining)(stateWithTok); } if (Array.isArray($match_15) && $match_15.length >= 1) { const nextTok = $match_15[0]; return (($match_16) => { if ($match_16.$tag === 24) { return processTokens(remaining)(stateWithTok); } { return ((ctx) => ((newState) => processTokens(remaining)(newState))(({ ...stateWithTok, stack: Vibe._COLON_COLON(ctx)(stateWithTok.stack), output: Vibe._COLON_COLON(makeVirtual(T.BlockStart)(nextTok.span))(stateWithTok.output) })))(({ column: nextTok.span.start.column, keyword: tok.lexeme, bracketDepth: stateWithTok.bracketDepth })); } throw new Error("Pattern match failed"); })(nextTok.kind); } throw new Error("Pattern match failed"); })(remaining))(skipNewlines(rest)))(({ ...state, output: Vibe._COLON_COLON(tok)(state.output) }));
const insertLayoutTokens = (tokens) => ((finalState) => List.reverse(finalState.output))(processTokens(tokens)(initState));
const $impl_Eq_LayoutContext__EQ_EQ = (x_impl) => (y_impl) => _AMP_AMP(_AMP_AMP($inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.column)(y_impl.column))(() => $inst_String.$dict_Eq_String._EQ_EQ(x_impl.keyword)(y_impl.keyword)))(() => $inst_Int.$dict_Eq_Int._EQ_EQ(x_impl.bracketDepth)(y_impl.bracketDepth));
const $impl_Show_LayoutContext_toString = (x_impl) => $inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS($inst_String.$dict_Appendable_String._PLUS_PLUS("LayoutContext { ")("column = "))($inst_Int.$dict_Show_Int.toString(x_impl.column)))(", "))("keyword = "))($inst_String.$dict_Show_String.toString(x_impl.keyword)))(", "))("bracketDepth = "))($inst_Int.$dict_Show_Int.toString(x_impl.bracketDepth)))(" }");
const $dict_Eq_LayoutContext = {
  _EQ_EQ: $impl_Eq_LayoutContext__EQ_EQ
};
const $dict_Show_LayoutContext = {
  toString: $impl_Show_LayoutContext_toString
};

export { $dict_Eq_LayoutContext, $dict_Show_LayoutContext, insertLayoutTokens };