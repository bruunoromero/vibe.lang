import * as Vibe from "../../Vibe/Vibe.js";
import * as Dict from "../../Vibe/Vibe/Dict.js";
import * as Maybe from "../../Vibe/Vibe/Maybe.js";
import * as Types from "../VibeParser/Types.js";
import * as $inst_Unit from "../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../Vibe/Vibe/Float.js";
import * as $inst_String from "../../Vibe/Vibe/String.js";
import * as $inst_Char from "../../Vibe/Vibe/Char.js";
import * as $inst_List from "../../Vibe/Vibe/List.js";
import * as $inst_Types from "../../VibeLexer/VibeLexer/Types.js";

// Values
const emptyRegistry = Dict.empty;
const defaultOperatorInfo = ({ precedence: 9, associativity: Types.AssocLeft });
const builtinRegistry = Vibe._PIPE_GT(Vibe._PIPE_GT(Dict.empty)(Dict.insert($inst_String.$dict_Ord_String)("&&")(({ precedence: 3, associativity: Types.AssocRight }))))(Dict.insert($inst_String.$dict_Ord_String)("||")(({ precedence: 2, associativity: Types.AssocRight })));
const getOperatorInfo = (op) => (registry) => (($match_0) => { if ($match_0.$tag === 0) { const info = $match_0.$0; return info; } if ($match_0.$tag === 1) { return defaultOperatorInfo; } throw new Error("Pattern match failed"); })(Dict.get($inst_String.$dict_Ord_String)(op)(registry));
const insertOperator = Dict.insert($inst_String.$dict_Ord_String);
const mergeRegistries = (base) => (override) => Dict.union($inst_String.$dict_Ord_String)(override)(base);

export { builtinRegistry, defaultOperatorInfo, emptyRegistry, getOperatorInfo, insertOperator, mergeRegistries };