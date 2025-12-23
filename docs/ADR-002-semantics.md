# ADR-002: Semantic Analyzer Skeleton

- **Status**: Accepted
- **Date**: 2025-12-20

## Context

We have a working lexer and parser that can surface syntax diagnostics and emit immutable AST nodes. Downstream compilation stages (macro expansion, symbol resolution, code generation) still lack a shared semantic graph, so the CLI and future tooling cannot reason about scopes or hygienic bindings. We need a dedicated `@vibe/semantics` package that consumes parser output, records lexical scopes, and begins plumbing metadata required for macro hygiene without mutating the AST supplied by `@vibe/syntax`.

## Decision

1. Introduce `@vibe/semantics` as a new workspace package that exports `analyzeProgram(program, options)` and returns `{ ok, diagnostics, graph }`. The semantic graph contains:
   - Deterministic scope identifiers, parent relationships, and hygiene tags per lexical boundary.
   - Symbol bindings (variables, parameters, macros, built-ins) with stable IDs that reference their defining scope and node.
   - Node metadata records (one per AST node) that describe which scope a node belongs to plus optional symbol resolution info (definition vs. usage, resolved symbol ID, hygiene tag).
2. Maintain AST immutability by keeping metadata out-of-band. `@vibe/semantics` assigns opaque `nodeId` values while traversing, and maps additional information via plain records in the returned graph.
3. Recognize the minimal set of special forms needed to bootstrap symbol resolution: `def`, `defmacro`, `let`, and `fn`. Additional forms can be layered on via feature flags without changing existing consumers.
4. Export a default list of builtin symbols (e.g., arithmetic ops, `println`) so unresolved symbol diagnostics stay meaningful even before a stdlib is available.
5. Extend the CLI with a new `vibe analyze` command that parses source, invokes the semantic analyzer, prints the AST alongside semantic metadata, and surfaces combined diagnostics.

### Update – Namespace Imports (2025-12-21)

- Analyzer now validates `(require alias "./path.lang")` and `(external alias "pkg")` forms, ensuring they appear as standalone top-level statements with a symbol alias plus a single string literal argument.
- These import forms are restricted to the program root scope; attempts to emit `(require ...)` or `(external ...)` inside nested scopes raise dedicated diagnostics so module loading stays deterministic.
- Namespace-qualified identifiers such as `alias/member` resolve by binding the alias portion; unresolved aliases raise `SEM_UNRESOLVED_NAMESPACE_ALIAS`. The analyzer also exposes `get` as a builtin head so `(get alias member)` can act as canonical access.
- These rules keep ASTs immutable while still threading module metadata into the semantic graph for codegen.

## Semantic Graph Contract

`analyzeProgram(program, options)` is the stable entry point for consumers. It returns an `AnalyzeResult` object shaped as `{ ok, diagnostics, graph }`, where `diagnostics` mirrors the parser's diagnostic objects and `graph` is the semantic IR described below.

### Graph Topology

- `SemanticGraph.scopes: ScopeRecord[]`
  - `id: ScopeId` — monotonic identifier (`scope_0` = root) that never collides across runs for the same AST.
  - `parentId: ScopeId | null` — `null` only for the root scope.
  - `hygieneTag: string` — deterministic token derived from the scope lineage (e.g., `h0`, `h0_1`).
  - `symbols: SymbolId[]` — ordered list of symbol identifiers declared in the scope.
- `SemanticGraph.symbols: SymbolRecord[]`
  - `id: SymbolId` — stable identifier (`symbol_0`, `symbol_1`, ...).
  - `name: string` — user-provided lexeme.
  - `scopeId: ScopeId` — scope owning the declaration.
  - `kind: "var" | "macro" | "parameter" | "builtin"` — informs codegen and tooling.
  - `nodeId: NodeId | null` — identifier of the defining AST node (builtins yield `null`).
  - `hygieneTag: string` — copied from the defining scope so macro expansion can flag injected bindings.
  - `alias: string` — sanitized identifier reserved for this symbol; downstream passes reuse it to avoid re-sanitizing names and to keep diagnostics deterministic.
- `SemanticGraph.nodes: SemanticNodeRecord[]`
  - `nodeId: NodeId` — assigned per AST node during traversal.
  - `kind: NodeKind` — mirrors the parser's enum to avoid RTTI checks.
  - `span: SourceSpan` — passthrough span for diagnostics.
  - `scopeId: ScopeId` — lexical scope active when the node was visited.
  - `hygieneTag: string` — same derivation as scopes, allowing consumers to compare hygiene domains quickly.
  - `symbol?: NodeSymbolInfo` — `{ name, role, symbolId? }` when the node corresponds to a symbol occurrence.

### Contract Invariants

1. Every `ScopeRecord` except the root has a parent that appears earlier in the `scopes` array.
2. `symbol.scopeId` must point to a scope that lists the symbol inside `ScopeRecord.symbols`.
3. `SemanticNodeRecord.scopeId` must refer to a valid scope; `symbol.symbolId` must reference a `SymbolRecord` when present.
4. Hygiene tags are pure functions of scope lineage so identical sources reproduce identical IDs/tags.
5. Diagnostics reference `span`s that are also present on at least one `SemanticNodeRecord` for easier correlation.

### Example Shape

```
{
   "ok": true,
   "diagnostics": [],
   "graph": {
      "scopes": [
         { "id": "scope_0", "parentId": null, "hygieneTag": "h0", "symbols": ["symbol_0"] }
      ],
      "symbols": [
         { "id": "symbol_0", "name": "answer", "scopeId": "scope_0", "kind": "var", "nodeId": "node_2", "hygieneTag": "h0", "alias": "answer" }
      ],
      "nodes": [
         { "nodeId": "node_0", "kind": "Program", "span": { "start": 0, "end": 17 }, "scopeId": "scope_0", "hygieneTag": "h0" },
         { "nodeId": "node_2", "kind": "Symbol", "span": { "start": 5, "end": 11 }, "scopeId": "scope_0", "hygieneTag": "h0", "symbol": { "name": "answer", "role": "definition", "symbolId": "symbol_0" } }
      ]
   }
}
```

This structure is the IR contract that downstream stages (codegen, tooling, macro debuggers) consume. Any additive fields must be reflected here before implementation to keep packages loosely coupled.

## AST Metadata Requirements

The analyzer depends on the following data points from `@vibe/syntax` nodes while keeping the schema stable:

- `SymbolNode.lexeme` and `SymbolNode.value` must remain immutable strings so bindings can key lookups consistently.
- Every AST node needs a precise `span` so scope assignments and diagnostics can be traced back to source locations.
- Reader macro nodes must continue to expose their `target` expressions so hygiene tags propagate into quoted forms.
- The parser now annotates each AST node with an optional `scopeId` hint (`scope_0`, `scope_1`, …). When present, the analyzer reuses these deterministic IDs so downstream consumers can correlate scopes across parser, semantics, and IR dumps. Macro-expanded nodes strip `scopeId` metadata before analysis to prevent leaks from the definition site.

Rather than embedding IDs inside the nodes, the analyzer emits side tables with these fields:

| Field                 | Description                                                                                |
| --------------------- | ------------------------------------------------------------------------------------------ |
| `nodeId`              | Stable identifier assigned per AST node during traversal.                                  |
| `scopeId`             | Identifier of the lexical scope that owns the node.                                        |
| `hygieneTag`          | Token derived from the scope lineage to keep macro-expansion deterministic.                |
| `symbolId` (optional) | Reference to a resolved binding; omitted for literals or unresolved references.            |
| `role`                | Indicates whether a symbol occurrence is a `definition`, `usage`, `parameter`, or `macro`. |

Consumers must treat these artifacts as derived metadata. No AST interfaces gain new fields, ensuring compatibility with existing parser clients.

## Consequences

- `@vibe/cli` can now demo the full front-end pipeline (`tokenize -> parse -> analyze`) via `vibe analyze`, making regressions easier to spot.
- Future macro-expansion work can reuse hygiene tags emitted in the semantic graph without re-traversing the AST.
- Scope-aware diagnostics (duplicate bindings, unresolved symbols) are centralized in one package, simplifying downstream tooling.
- Maintaining AST immutability means tooling in other languages (e.g., Rust prototypes) can still consume the same JSON the parser emits while optionally pairing it with analyzer output.

## Macro Expansion Pipeline (Update)

- `defmacro` now registers macro symbols alongside their parameter lists and syntax-quoted bodies. The analyzer stores these templates out-of-band and only expands them on demand.
- During analysis, list forms whose head resolves to a macro symbol are expanded before any further traversal. Arguments are bound as raw AST nodes (no evaluation) and injected via `~` and `~@` inside the stored syntax quote.
- The expander detects recursion (`SEM_MACRO_RECURSION`), arity mismatches, missing operands, and unsupported unquote expressions, surfacing diagnostics with spans at the callsite.
- `gensym` is recognized inside macro bodies (via `~(gensym "hint")`) to generate hygienic synthetic symbols suffixed with a monotonically increasing counter.
- Expanded forms are analyzed in the caller's scope but inherit the analyzer's hygiene tags so downstream passes can distinguish introduced identifiers from user bindings.
