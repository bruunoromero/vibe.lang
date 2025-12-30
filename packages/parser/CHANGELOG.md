# @vibe/parser Changelog

## 2025-12-30

- Removed the auto-gensym placeholder reader behavior. The parser once again treats `#` as a delimiter so `foo#` no longer lexes as a special symbol, and the associated regression tests were dropped alongside the feature.

## 2025-12-28

- **Feature: try/catch/finally scoping** — The parser's `ScopeAnnotator` now recognizes try forms and allocates a dedicated child scope for catch clauses so caught error bindings are hygienically isolated from the try body. Finally clauses execute in the parent scope.
- Parser tests in `packages/parser/tests/parse.test.ts` validate nested control structures.

## 2025-12-27

- Removed the deref (`@form`) reader macro. The parser no longer emits `NodeKind.Deref` nodes, treats stray `@` characters as lexical errors, and the scope annotator no longer reserves metadata for deref forms. Snapshot and unit tests now cover the remaining reader macros to ensure diagnostics stay consistent.

## 2025-12-29

- Removed parsing of `{ ... }` map literals and the associated `MapNode` AST shape. Map-literal handling and related snapshot cases were removed; binding-pattern parsing no longer supports map destructuring patterns.

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
