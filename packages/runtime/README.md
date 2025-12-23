# @vibe/runtime

Minimal runtime helpers for the `lang` compiler.

Exports:

- `println(...args)` — prints to console and returns last arg or `null`.
- `isTruthy(value)` — lightweight truthiness check.
- `__env()` — placeholder environment accessor (empty by default).

This package is intentionally minimal. `@vibe/codegen` will import `println` from here
so generated modules avoid inlining the helper.
