# Plan: Eliminate `BUN_JSC_maxPerThreadStackUsage` Workaround

## Background

Compiling `packages/vibe-parser` requires `BUN_JSC_maxPerThreadStackUsage=8388608` (8 MB)
because recursive functions in the generated Vibe runtime overflow the default JSC stack
(~1–2 MB) when processing large source files.

Binary search results:

| Stack size | Result           |
| ---------- | ---------------- |
| 4 MB       | Stack overflow   |
| 5 MB       | Stack overflow   |
| 6 MB       | Passes           |
| 8 MB       | Passes (current) |

---

## What has been fixed

### TypeScript compiler walkers (complete)

The original overflow was in the TypeScript compiler. These have been made iterative:

- **`collectFreeVars`** (`semantics/src/index.ts`): Rewritten from recursive `visit()` to
  an explicit `worklist: Array<[Expr, Set<string>]>` loop.
- **`analyzeExpr`** (`semantics/src/index.ts`): Wrapped in `while (true)` with mutable
  `expr`/`scope`/`substitution` loop variables. `LetIn` and `Paren` cases use `continue`
  instead of recursion.
- **`lowerLetIn`** (`ir/src/lowering/expressions.ts`): Pre-flattens all nested `LetIn`
  levels into an array iteratively, then wraps from inside out.
- **`lowerExpr`** (`ir/src/lowering/expressions.ts`): `Paren` case reassigns `expr` and
  continues instead of recurring.

After these fixes the overflow moved out of the TypeScript compiler entirely and into
the generated Vibe runtime.

---

## Remaining bottleneck: Vibe runtime recursion

The current overflow is in `vibe-lexer/dist/Json/Json/Encode.js`, generated from
`packages/vibe-json/src/Json/Encode.vibe`:

```
at <anonymous> (Json/Encode.js:33:292)   -- escapeChars recursive call
at renderString (Json/Encode.js:34:119)
at <anonymous> (Json/Encode.js:50:...)   -- joinValues/joinPairs recursion
```

`escapeChars` recurses once per character of the source string (≥2500 chars for
Parser.vibe). The emitted JS is a direct recursive call, so each character costs one
JS stack frame.

```vibe
-- Current: NOT a tail call — (++) happens after the recursive return
escapeChars chars =
  case chars of
    [] -> ""
    c :: rest -> escapeChar c ++ escapeChars rest
```

`joinValues` and `joinPairs` have the same pattern over the token list.

---

## Fix: Codegen TCO + accumulator rewrites (do next)

Two pieces are required because TCO alone cannot help a function whose recursive call
is not in tail position.

### Step 1 — Codegen TCO for self tail calls

Add tail-call detection to `packages/codegen/`. When a function's last action is a
call to itself (i.e., the recursive call is in tail position in the IR), emit a
`while (true)` loop with variable rebinding instead of a recursive JS call.

```js
// Before (current codegen output)
const go = ($dict) => (cs) => (acc) =>
  cs.length === 0 ? acc : go($dict)(cs.slice(1))(acc + escapeChar(cs[0]));

// After (TCO codegen output)
const go = ($dict) => (cs) => (acc) => {
  while (true) {
    if (cs.length === 0) return acc;
    acc = acc + escapeChar(cs[0]);
    cs = cs.slice(1);
  }
};
```

Self tail calls are the common case and require no trampoline. Mutual tail calls (two
functions calling each other in tail position) are rarer and can be handled with a
trampoline in a follow-up.

### Step 2 — Rewrite `escapeChars` / `joinValues` / `joinPairs` with an accumulator

The recursive call in `escapeChars` is not currently in tail position — `++` runs
after the return. Rewrite with an accumulator so the call becomes a tail call, at
which point Step 1 eliminates its stack growth:

```vibe
-- After: tail call, codegen emits while(true)
escapeChars chars =
  let go cs acc =
    case cs of
      [] -> acc
      c :: rest -> go rest (acc ++ escapeChar c)
  in go chars ""
```

Apply the same accumulator pattern to `joinValues` and `joinPairs`.

### Steps

1. Implement tail-call detection in codegen: identify `IRCall` nodes where the callee
   resolves to the enclosing function and the call is in tail position.
2. Emit `while (true)` with parameter rebinding for detected self tail calls.
3. Run `bun test` to verify no regressions.
4. Rewrite `escapeChars`, `joinValues`, and `joinPairs` in
   `packages/vibe-json/src/Json/Encode.vibe` with the accumulator pattern.
5. Rebuild `packages/vibe-lexer` (which depends on vibe-json) and `packages/vibe-parser`.
6. Test without any `BUN_JSC_maxPerThreadStackUsage` override.
7. Remove `BUN_JSC_maxPerThreadStackUsage` from `packages/vibe-parser/package.json`.

### Success criterion

`bun run build` in `packages/vibe-parser` passes with **no env var override**.
`bun test` passes across all packages.
`bunx turbo run build` succeeds end-to-end.
