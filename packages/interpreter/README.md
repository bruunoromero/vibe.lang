# @vibe/interpreter

Full-featured interpreter for compile-time macro expansion and direct code execution.

## Purpose

Provides a complete evaluation engine that executes vibe AST nodes directly without compiling to JavaScript. This enables:

- **Rich macro expansion**: Full Clojure-style computation during macro expansion with arbitrary functions, data manipulation, and control flow
- **REPL execution**: Direct interpretation mode for faster iteration without compilation overhead
- **Compile-time computation**: Evaluate expressions at build time for configuration, code generation, and validation

## Architecture

The interpreter operates on AST nodes from `@vibe/syntax` and produces runtime values. It supports:

- **Complete expression evaluation**: All node kinds including literals, collections, function calls, special forms
- **Lexical scoping**: Environment chain with closure capture for `fn` forms
- **Builtin operations**: Comprehensive standard library mirroring `@vibe/runtime`
- **Function closures**: First-class functions with captured lexical environments
- **Error handling**: Evaluation errors with source spans for diagnostics

## Usage

```typescript
import { evaluate, createEnvironment, nodeToValue } from "@vibe/interpreter";
import { parseSource } from "@vibe/parser";

// Parse source code
const result = await parseSource("(+ 1 2 3)");

// Evaluate with builtins
const env = createEnvironment();
const evalResult = evaluate(result.program.body[0], env);

if (evalResult.ok) {
  console.log(evalResult.value); // { kind: "number", value: 6 }
}
```

## Integration

- **Semantics**: Used by `@vibe/semantics` analyzer to evaluate unquote expressions during macro expansion
- **CLI REPL**: Powers direct interpretation mode for interactive development
- **Runtime**: Shares implementation with `@vibe/runtime` for consistency between compile-time and runtime behavior

## Limitations

- **Call stack depth**: Limited to 1000 frames to prevent infinite recursion
- **No I/O operations**: Intentionally isolated from file system and network for deterministic builds
- **No mutable state**: All operations are pure; no `set!` or mutation primitives
