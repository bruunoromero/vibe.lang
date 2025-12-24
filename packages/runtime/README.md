# @vibe/runtime

Minimal runtime helpers for the `lang` compiler.

## Symbols vs. Strings

Runtime symbols are plain JavaScript objects tagged with `__vibeType: "symbol"`:

```
import { symbol, symbol_QMARK } from "@vibe/runtime";

const foo = symbol("foo");
symbol_QMARK(foo); // true
symbol_QMARK("foo"); // false
```

- `eq*` compares symbols by name so `symbol("a")` is distinct from `symbol("b")` and regular strings.
- `str`, `get`, `assoc`, and `dissoc` now treat these tagged objects as first-class values. `get` also accepts namespace objects (the ones emitted for `(require alias ...)`) and plain `Map` instances, returning `null` or an explicit default when a member is missing.
- `type` returns keyword-style symbols such as `:number`, `:symbol`, etc., making it easy to pattern-match inside macros.

Use `(external runtime "@vibe/runtime")` plus `runtime/symbol` or `runtime/symbol?` to work with tagged symbols inside user code without needing a new builtin form.
