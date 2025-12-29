# @vibe/syntax Changelog

## 2025-12-29

- Removed map literal syntax (`{ key value ... }`) and associated `MapNode`/token kinds across the syntax package. The language no longer supports `{}` map literals as of this date; documentation (`docs/syntax-spec.md`) was updated to reflect the removal.
- Removed the reader-dispatch syntax (`#`) and the `Dispatch` AST node. Set-literals and other `#`-prefixed reader forms have been retired; authors should use explicit helper forms or ordinary collections instead. The lexer, parser, and `@vibe/syntax` package were updated accordingly.

## 2025-12-28

- Added `try`, `throw` to `BUILTIN_SYMBOLS` to designate them as reserved special forms for error handling. These symbols are now recognized by downstream analyzers and must not be redefined in user code.
- Updated `docs/syntax-spec.md` to document the canonical forms: `(try body... (catch symbol handler...) (finally cleanup...))` and `(throw expr)`.

## 2025-12-27

- Removed the `TokenType.Deref` and `NodeKind.Deref` definitions now that the `@form` reader macro has been retired across the toolchain. The syntax package no longer exports deref as part of `ReaderMacroKind`, keeping downstream packages in sync with the reduced surface.

## 2025-12-26

- Replaced the `defmacro` builtin head with the new `macro` literal. `BUILTIN_SYMBOLS` now includes `macro` (so analyzers/repls treat it as a reserved special form) and documentation highlights `(def name (macro ...))` as the canonical declaration style.
- Parser snapshots were refreshed to showcase the new `def` + `macro` structure so downstream tooling can reference up-to-date AST shapes.
- Added the `defp` special form to `BUILTIN_SYMBOLS` so analyzers, the REPL, and codegen treat private definitions as reserved heads alongside `def`.

## 2025-12-23

- Trimmed `BUILTIN_SYMBOLS` down to the language's special forms so runtime helpers now flow through explicit `(require)` calls (e.g., `@vibe/prelude`) instead of being treated as implicit globals.
- Updated `docs/syntax-spec.md` to document the restricted special form list and point users at the prelude for arithmetic/collection helpers.
- Documented and surfaced the alias-less `(import "./module.lang")` form by marking it as a built-in special form alongside `require`/`external` so downstream packages can detect and lower flattened imports deterministically.

## 2025-12-22

- **Feature: Clojure-style identifier suffixes** — Symbols may now end with special characters (`?`, `!`, `*`, `+`, `-`, `=`, `<`, `>`, `/`) to support Clojure naming conventions (e.g., `is-prime?`, `set-value!`, `map*`). This aligns with Lisp tradition and improves code clarity.
- Updated syntax specification to document the new identifier format.

## 2025-12-21

- Promoted `(require ...)` and `(external ...)` forms into a dedicated `NamespaceImport` AST node so downstream stages can access the import kind, alias, and specifier without re-parsing generic list structures.
- Exported the shared `NamespaceImportKind`/`NamespaceImportNode` types to unblock parser, semantics, and tooling consumers that need to reason about module metadata.

## 2025-12-20

- Added foundational AST node kinds for programs, collections, reader macros, and atoms.
- Documented the semantic analyzer's metadata expectations (ADR-002) while keeping AST shapes immutable.
- Captured macro declaration/usage shapes via new parser snapshots so tooling consumers have concrete AST samples for `defmacro` forms.
- Introduced optional `scopeId` metadata on every AST node so the parser can tag lexical scope boundaries deterministically.
