# ADR-001: Lisp Parser Foundation

- **Status**: Accepted
- **Date**: 2025-12-20

## Context

We have a working lexer that produces rich token streams, but there is no parser or AST schema to move toward semantic analysis or code generation. The language targets a Lisp-like syntax similar to Clojure, meaning the parser must understand lists, vectors, maps, sets, reader macros (quote, syntax-quote, unquote, unquote-splicing, deref), and generic dispatch forms. Downstream stages will require immutable AST nodes with precise source spans, all centralized within `@vibe/syntax`.

## Decision

1. Extend `@vibe/syntax` to define immutable AST node types (`Program`, `List`, `Vector`, `Map`, `Set`, reader macros, atoms, and dispatch) plus lightweight helpers for working with spans.
2. Introduce a new workspace package, `@vibe/parser`, that consumes lexer streams and produces ASTs. The parser will:
   - Gather lexical diagnostics and preserve them in the final result.
   - Emit additional diagnostics for unmatched delimiters, unexpected tokens, uneven map literals, and missing macro operands.
   - Represent structural forms using the new AST nodes while keeping arrays readonly.
3. Add a `vibe parse` CLI command in `@vibe/cli` that mirrors `vibe tokenize` inputs but prints the parsed AST JSON and diagnostics.

### Update – Module Imports (2025-12-21)

- `require`/`external` forms now promote to a dedicated `NamespaceImport` AST node during parsing. The node preserves the original list elements but annotates the import kind, alias expression, and string literal specifier so downstream stages no longer have to pattern-match generic lists.
- Namespace-qualified tokens (e.g., `alias/member`) continue to lex as single `SymbolNode`s. The parser does not introduce bespoke AST nodes for them; instead, later stages desugar them into `(get alias member)` semantics when needed.
- The semantic analyzer is expected to consult a workspace-provided module resolver when it encounters `require` imports (allowing diagnostics such as `SEM_REQUIRE_RESOLVE_FAILED`), while `external` imports continue to pass through verbatim for JavaScript modules.
- All top-level `def` forms are auto-exported at codegen time as named ES exports; the language surface itself does not expose explicit module/export declarations.

### Update – Import Flattening (2025-12-23)

- Added a dedicated surface form `(import "./module.lang")` that promotes to the same `NamespaceImport` AST node as `require`/`external`, but omits the alias operand.
- The parser stores the string literal specifier in the node's `source` slot so downstream stages can reuse the machinery that already handles namespace nodes without backtracking through generic lists.
- This keeps AST immutability intact while enabling semantics/codegen to detect import-vs-alias behavior purely via `importKind` metadata.

### Implementation Status (2025-12-22)

- **Completed**: We implemented implicit file-level modules where every top-level `def` is considered exported. Changes were made across parser, semantics, codegen, runtime, and CLI to support namespace imports, module export recording, and named ES exports only. Tests and example builds (`packages/example-app`) were updated and the full test suite passes.

## Consequences

- Parser clients have a single entry point that accepts any lexer source (`LexSource`) and returns `{ ok, program, diagnostics }`.
- AST definitions now live in `@vibe/syntax`, so future stages (`@vibe/semantics`, `@vibe/codegen`) can consume consistent node shapes without re-defining them.
- CLI users can experiment with parsing immediately, easing test coverage for future grammar work.
- Any future reader macro extensions need only update the syntax package and parser without affecting the CLI surface.
