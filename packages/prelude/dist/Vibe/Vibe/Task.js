import * as Basics from "../Vibe/Basics.js";
import * as Result from "../Vibe/Result.js";
import { Ok, Err } from "../Vibe/Result.js";
import * as Promise from "../Vibe/Promise.js";
import * as Error from "../Vibe/Error.js";
import * as $inst_Unit from "../Vibe/Unit.js";
import * as $inst_Bool from "../Vibe/Bool.js";
import * as $inst_Int from "../Vibe/Int.js";
import * as $inst_Float from "../Vibe/Float.js";
import * as $inst_String from "../Vibe/String.js";
import * as $inst_Char from "../Vibe/Char.js";
import * as $inst_List from "../Vibe/List.js";

// ADT Constructors
const Task = ($0) => ({ $tag: 0, $0 });

// Values
const fromPromise = (promise) => Basics._PIPE_GT(Basics._PIPE_GT(Basics._PIPE_GT(promise)(Promise.map(Ok)))(Promise.$catch(Basics._GT_GT(Error.create)(Err))))(Task);
const map = (f) => ({ $0: promise }) => Basics._PIPE_GT(Basics._PIPE_GT(promise)(Promise.map(Result.map(f))))(Task);

export { fromPromise };