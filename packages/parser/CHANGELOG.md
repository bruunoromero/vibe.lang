# @vibe/parser Changelog

## 2025-12-25

- Syntax-quoted forms now preserve trailing `#` symbols as standalone `SymbolNode`s so auto-gensym placeholders survive parsing. Added regression coverage for `` `(let [foo# 1] foo#) `` to ensure scope metadata keeps these placeholders inside the template until macro expansion replaces them.
- Scope annotator now understands multi-arity `fn` forms, allocating deterministic clause-specific scope IDs so the analyzer can reuse parser hints when validating each parameter vector.

## 2025-12-23

- Promoted `(import "./module.lang")` into the existing `NamespaceImport` AST node (with `importKind = "import"`) so downstream stages can distinguish alias-less imports without re-parsing list literals.
- Added regression tests covering the new syntax to lock the node shape and ensure the parser enforces the "string-literal only" argument contract.

## 2025-12-22

- **Feature: Clojure-style identifier parsing** — Added test coverage for symbols with special-character suffixes (`?`, `!`, `*`, etc.). Parser now correctly handles function definitions and usage sites with Clojure-style names like `is-valid?` and `set-value!`.

## 2025-12-21

- Promoted `(require ...)`/`(external ...)` forms into dedicated `NamespaceImport` nodes during parsing so downstream stages can access alias + specifier metadata without re-pattern-matching raw lists.
- Annotated namespace import nodes with scope metadata and added parser tests covering the new structure.

## 2025-12-20

- Introduced the initial parser that converts lexer tokens into AST nodes and reports structural diagnostics.
- Added a post-parse scope annotator that assigns deterministic `scopeId` metadata to every AST node, along with regression tests covering `let`/`fn` scope threading.
