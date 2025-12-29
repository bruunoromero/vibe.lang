# @vibe/lexer Changelog

## 2025-12-27

- Removed the `@` deref reader macro now that deref syntax is no longer supported. Stray `@` characters emit the existing "Unexpected character '@'" diagnostic instead of producing reader tokens.

## 2025-12-29

- Removed `{`/`}` map literal token kinds so brace characters are no longer parsed as map delimiters. Map literal support was removed from the language on 2025-12-29; the lexer treats braces as delimiters only where necessary.
