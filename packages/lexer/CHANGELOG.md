# @vibe/lexer Changelog

## 2025-12-27

- Removed the `@` deref reader macro now that deref syntax is no longer supported. Stray `@` characters emit the existing "Unexpected character '@'" diagnostic instead of producing reader tokens.
