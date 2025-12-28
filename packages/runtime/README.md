# @vibe/runtime

Minimal runtime helpers for the `lang` compiler.

## Symbols and Keywords

Runtime symbols are plain JavaScript objects tagged with `__vibeType: "symbol"`:

```
import { symbol, symbol_QMARK } from "@vibe/runtime";

const foo = symbol("foo");
symbol_QMARK(foo); // true
symbol_QMARK("foo"); // false
```

- `eq*` compares symbols by name so `symbol("a")` is distinct from `symbol("b")` and regular strings.
- `str`, `get`, `assoc`, and `dissoc` treat tagged symbols and keywords as first-class values. `get` also accepts namespace objects (the ones emitted for `(require alias ...)`) and plain `Map` instances, returning `null` or an explicit default when a member is missing.
- `type` returns tagged keywords such as `:number`, `:symbol`, or `:keyword`, making it easy to pattern-match inside macros.

Keywords share the same tagged-object approach and are automatically interned so repeated calls reuse the same reference:

```
import { keyword, keyword_QMARK } from "@vibe/runtime";

const tag = keyword("demo");
keyword_QMARK(tag); // true
keyword("demo") === tag; // true
```

Use `(external runtime "@vibe/runtime")` plus `runtime/symbol`, `runtime/keyword`, and the related predicates inside user code without needing new builtin forms.
