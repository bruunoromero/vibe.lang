# @vibe/parser Changelog

## 2025-12-21

- Promoted `(require ...)`/`(external ...)` forms into dedicated `NamespaceImport` nodes during parsing so downstream stages can access alias + specifier metadata without re-pattern-matching raw lists.
- Annotated namespace import nodes with scope metadata and added parser tests covering the new structure.

## 2025-12-20

- Introduced the initial parser that converts lexer tokens into AST nodes and reports structural diagnostics.
- Added a post-parse scope annotator that assigns deterministic `scopeId` metadata to every AST node, along with regression tests covering `let`/`fn` scope threading.
