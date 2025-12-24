# @vibe/syntax Changelog

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

- Added foundational AST node kinds for programs, collections, reader macros, dispatch forms, and atoms.
- Documented the semantic analyzer's metadata expectations (ADR-002) while keeping AST shapes immutable.
- Captured macro declaration/usage shapes via new parser snapshots so tooling consumers have concrete AST samples for `defmacro` forms.
- Introduced optional `scopeId` metadata on every AST node so the parser can tag lexical scope boundaries deterministically.
