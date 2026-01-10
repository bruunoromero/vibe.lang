# ADR: Parser Generator Evaluation for Vibe

**Date**: 2026-01-06  
**Status**: Decided - Keep Hand-Written Parser  
**Decision**: After evaluating Peggy as a parser generator, we've decided to retain the hand-written recursive descent parser while formalizing the grammar documentation.

## Context

The Vibe language has layout-sensitive (indentation-based) syntax similar to Elm, Haskell, and Python. This includes:

- Let bindings requiring consistent indentation
- Case branches indented relative to the `of` keyword
- Multi-line applications, tuples, lists, and records respecting layout rules
- Function application via juxtaposition (whitespace-sensitive)

We evaluated using Peggy (a PEG parser generator) to:

1. Formalize the grammar as source of truth
2. Reduce maintenance burden of hand-written parser code
3. Clarify language syntax through declarative grammar

## Decision

**Keep the hand-written recursive descent parser** for the following reasons:

###1. **Layout Sensitivity is Core to Vibe**

PEG parsers (including Peggy) don't natively support indentation-based syntax. While it's technically possible to encode layout rules in PEG actions (tracking indentation state manually), this:

- Defeats the purpose of using a generator (you end up writing complex manual code anyway)
- Makes the grammar harder to read and maintain than imperative parsing code
- Loses the declarative benefits of formal grammars

### 2. **Test Results**

When we implemented a Peggy grammar for Vibe:

- ✅ Successfully parsed simple expressions and declarations
- ❌ Failed on layout-sensitive constructs (multi-line let/case/applications)
- ❌ Couldn't distinguish between same-line continuation and new declarations
- ❌ Operator precedence required complex manual implementation in actions
- ❌ Error messages were less specific than hand-written parser

Example failures:

```vibe
-- This should parse as one declaration:
render model =
  view model
  |> Html.map msg

-- But Peggy parsed it as two declarations (broke at the newline)
```

### 3. **Hand-Written Parser Strengths**

Our current parser has:

- **Explicit layout stack**: Tracks indentation contextually (let blocks, case branches, etc.)
- **Custom error messages**: Tailored to common Vibe syntax mistakes
- **Operator precedence table**: Clean, declarative mapping of operators
- **Fine-grained control**: Can handle edge cases and provide helpful recovery

### 4. **Alternative Considered: tree-sitter**

tree-sitter is designed for layout-sensitive languages and incremental parsing, but:

- Requires C compilation (additional build complexity)
- Overkill for a compiler (more suited for IDE tooling)
- Would require porting existing parser logic to a new DSL

## Implementation

Instead of using a parser generator, we're taking a hybrid approach:

1. **Formalize grammar as EBNF documentation** (`docs/grammar.ebnf`)

   - Serves as specification and reference
   - Documents layout rules in prose (not executable)
   - Source of truth for language syntax

2. **Keep hand-written parser** (`packages/parser/src/index.ts`)

   - Implement grammar rules from EBNF spec
   - Maintain existing layout stack mechanism
   - Continue providing custom error messages

3. **Add inline grammar comments** to parser methods

   - Link parser code back to EBNF rules
   - Example: `// Grammar: Expr ::= BinaryExpr`

4. **Test coverage ensures correctness**
   - Parser tests validate behavior against grammar spec
   - Layout-specific tests prevent regressions

## Benefits of This Approach

- ✅ **Clear specification**: EBNF grammar documents intended language design
- ✅ **Working implementation**: Hand-written parser handles layout correctly
- ✅ **Better errors**: Custom messages guide users to fixes
- ✅ **Maintainability**: Code structure mirrors grammar rules
- ✅ **No compromise**: Full control over parsing behavior

## Consequences

- **Manual updates required**: When grammar changes, both EBNF and parser code must be updated
- **Vigilance needed**: Tests must catch divergence between spec and implementation
- **Higher initial complexity**: Newcomers must understand both grammar and parser code

## Future Considerations

If Vibe's syntax stabilizes and we need:

- **IDE support** (syntax highlighting, completion): Consider tree-sitter grammar
- **Multiple language targets**: ANTLR might justify the complexity
- **Simplified syntax** (removing layout sensitivity): Peggy could work

For now, the hand-written parser remains the best tool for the job.

## References

- [grammar.ebnf](./grammar.ebnf) - Formal Vibe grammar specification
- [Peggy documentation](https://peggyjs.org/)
- [tree-sitter](https://tree-sitter.github.io/tree-sitter/)
- Original parser: `packages/parser/src/index.ts`
