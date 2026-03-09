import { readFile as $$_readFileWithEncoding } from "node:fs/promises";

import * as Vibe from "../../../Vibe/Vibe.js";
import * as Task from "../../../Vibe/Vibe/Task.js";
import * as $inst_Unit from "../../../Vibe/Vibe/Unit.js";
import * as $inst_Bool from "../../../Vibe/Vibe/Bool.js";
import * as $inst_Int from "../../../Vibe/Vibe/Int.js";
import * as $inst_Float from "../../../Vibe/Vibe/Float.js";
import * as $inst_String from "../../../Vibe/Vibe/String.js";
import * as $inst_Char from "../../../Vibe/Vibe/Char.js";
import * as $inst_List from "../../../Vibe/Vibe/List.js";

// Values
const _readFileWithEncoding = ($a0) => ($a1) => $$_readFileWithEncoding($a0, $a1);
const readFileWithEncoding = (path) => (encoding) => Vibe._PIPE_GT(_readFileWithEncoding(encoding)(path))(Task.fromPromise);
const readFile = readFileWithEncoding("utf-8");

export { readFile, readFileWithEncoding };