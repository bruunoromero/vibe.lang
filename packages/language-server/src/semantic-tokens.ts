/**
 * Semantic Tokens Provider for Vibe.
 *
 * Maps Vibe AST nodes and tokens to LSP semantic token types for rich syntax highlighting.
 */
import {
  SemanticTokensBuilder,
  SemanticTokenTypes,
  SemanticTokenModifiers,
} from "vscode-languageserver";
import type { SemanticTokensLegend } from "vscode-languageserver";
import type {
  Token,
  TokenKind,
  Program,
  Expr,
  Pattern,
  Declaration,
  TypeExpr,
  Span,
} from "@vibe/syntax";
import type { SemanticModule, TypeScheme } from "@vibe/semantics";
import type { DocumentCache } from "./types";

/**
 * Semantic token types supported by the Vibe language server.
 */
export const TOKEN_TYPES = [
  SemanticTokenTypes.namespace, // 0: Module names
  SemanticTokenTypes.type, // 1: Type names, ADTs
  SemanticTokenTypes.class, // 2: Protocols
  SemanticTokenTypes.enum, // 3: ADT variants (constructors)
  SemanticTokenTypes.interface, // 4: Type aliases
  SemanticTokenTypes.typeParameter, // 5: Type variables
  SemanticTokenTypes.parameter, // 6: Function parameters
  SemanticTokenTypes.variable, // 7: Local variables
  SemanticTokenTypes.property, // 8: Record fields
  SemanticTokenTypes.function, // 9: Functions
  SemanticTokenTypes.keyword, // 10: Keywords
  SemanticTokenTypes.string, // 11: String literals
  SemanticTokenTypes.number, // 12: Number literals
  SemanticTokenTypes.operator, // 13: Operators
  SemanticTokenTypes.comment, // 14: Comments
  SemanticTokenTypes.macro, // 15: External declarations
];

/**
 * Semantic token modifiers supported.
 */
export const TOKEN_MODIFIERS = [
  SemanticTokenModifiers.declaration, // 0: Symbol declaration
  SemanticTokenModifiers.definition, // 1: Symbol definition
  SemanticTokenModifiers.readonly, // 2: Immutable values
  SemanticTokenModifiers.static, // 3: Top-level definitions
  SemanticTokenModifiers.defaultLibrary, // 4: Prelude/built-in
];

/**
 * Semantic tokens legend for client registration.
 */
export const SEMANTIC_TOKENS_LEGEND: SemanticTokensLegend = {
  tokenTypes: TOKEN_TYPES,
  tokenModifiers: TOKEN_MODIFIERS,
};

/**
 * Token type indices for convenience.
 */
const TokenTypeIndex = {
  Namespace: 0,
  Type: 1,
  Protocol: 2,
  Constructor: 3,
  TypeAlias: 4,
  TypeParameter: 5,
  Parameter: 6,
  Variable: 7,
  Property: 8,
  Function: 9,
  Keyword: 10,
  String: 11,
  Number: 12,
  Operator: 13,
  Comment: 14,
  External: 15,
};

/**
 * Token modifier bit flags.
 */
const TokenModifierFlags = {
  Declaration: 1 << 0,
  Definition: 1 << 1,
  Readonly: 1 << 2,
  Static: 1 << 3,
  DefaultLibrary: 1 << 4,
};

/**
 * Semantic token entry before encoding.
 */
interface SemanticToken {
  line: number; // 0-based line
  char: number; // 0-based character
  length: number;
  type: number; // Token type index
  modifiers: number; // Modifier bit flags
}

/**
 * Provide semantic tokens for a document.
 */
export function provideSemanticTokens(cache: DocumentCache): number[] {
  const tokens: SemanticToken[] = [];

  // Use AST-based token extraction for semantic accuracy
  if (cache.parseResult?.ast) {
    extractASTTokens(
      cache.parseResult.ast,
      cache.semanticResult?.module,
      cache.content,
      tokens
    );
  }

  // Sort tokens by position (required by LSP spec)
  tokens.sort((a, b) => {
    if (a.line !== b.line) return a.line - b.line;
    return a.char - b.char;
  });

  // Encode as delta format
  return encodeTokens(tokens);
}

/**
 * Extract semantic tokens from AST with semantic information.
 */
function extractASTTokens(
  ast: Program,
  semantics: SemanticModule | undefined,
  content: string,
  tokens: SemanticToken[]
): void {
  // Module declaration
  if (ast.module) {
    addToken(
      tokens,
      ast.module.span,
      TokenTypeIndex.Namespace,
      TokenModifierFlags.Declaration
    );
  }

  // Imports
  if (ast.imports) {
    for (const imp of ast.imports) {
      if (!imp) continue;
      // Module name in import
      const modSpan = getModuleNameSpan(imp.span, imp.moduleName, content);
      addToken(tokens, modSpan, TokenTypeIndex.Namespace, 0);
    }
  }

  // Declarations
  if (ast.declarations) {
    for (const decl of ast.declarations) {
      if (!decl) continue;
      extractDeclarationTokens(decl, semantics, content, tokens);
    }
  }
}

/**
 * Extract tokens from a declaration.
 */
function extractDeclarationTokens(
  decl: Declaration,
  semantics: SemanticModule | undefined,
  content: string,
  tokens: SemanticToken[]
): void {
  if (!decl) return;

  switch (decl.kind) {
    case "ValueDeclaration": {
      // Function/value name
      const isFunction =
        (decl.args && decl.args.length > 0) ||
        semantics?.typeSchemes[decl.name]?.type.kind === "fun";
      const typeIdx = isFunction
        ? TokenTypeIndex.Function
        : TokenTypeIndex.Variable;
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(
        tokens,
        nameSpan,
        typeIdx,
        TokenModifierFlags.Declaration |
          TokenModifierFlags.Definition |
          TokenModifierFlags.Readonly
      );

      // Parameters
      if (decl.args) {
        for (const arg of decl.args) {
          if (arg) {
            extractPatternTokens(arg, content, tokens, true);
          }
        }
      }

      // Body
      if (decl.body) {
        extractExprTokens(decl.body, semantics, content, tokens);
      }
      break;
    }

    case "TypeAnnotationDeclaration": {
      // Name in type annotation
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      const isFunction =
        decl.annotation?.kind === "FunctionType" ||
        decl.annotation?.kind === "QualifiedType";
      addToken(
        tokens,
        nameSpan,
        isFunction ? TokenTypeIndex.Function : TokenTypeIndex.Variable,
        TokenModifierFlags.Declaration
      );

      // Type expression
      if (decl.annotation) {
        extractTypeExprTokens(decl.annotation, content, tokens);
      }
      break;
    }

    case "TypeDeclaration": {
      // Type name
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(
        tokens,
        nameSpan,
        TokenTypeIndex.Type,
        TokenModifierFlags.Declaration | TokenModifierFlags.Definition
      );

      // Type parameters
      // Note: spans not directly available, would need position tracking

      // Constructors
      for (const ctor of decl.constructors) {
        addToken(
          tokens,
          ctor.span,
          TokenTypeIndex.Constructor,
          TokenModifierFlags.Declaration
        );

        // Constructor argument types
        for (const arg of ctor.args) {
          extractTypeExprTokens(arg, content, tokens);
        }
      }
      break;
    }

    case "TypeAliasDeclaration": {
      // Alias name
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(
        tokens,
        nameSpan,
        TokenTypeIndex.TypeAlias,
        TokenModifierFlags.Declaration | TokenModifierFlags.Definition
      );

      // Value type
      extractTypeExprTokens(decl.value, content, tokens);
      break;
    }

    case "OpaqueTypeDeclaration": {
      // Opaque type name
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(
        tokens,
        nameSpan,
        TokenTypeIndex.Type,
        TokenModifierFlags.Declaration | TokenModifierFlags.Definition
      );
      break;
    }

    case "ProtocolDeclaration": {
      // Protocol name
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(
        tokens,
        nameSpan,
        TokenTypeIndex.Protocol,
        TokenModifierFlags.Declaration | TokenModifierFlags.Definition
      );

      // Methods
      for (const method of decl.methods) {
        // Method name
        const methodSpan = getNameSpan(method.span, method.name, content);
        addToken(
          tokens,
          methodSpan,
          TokenTypeIndex.Function,
          TokenModifierFlags.Declaration
        );

        // Method type
        if (method.type) {
          extractTypeExprTokens(method.type, content, tokens);
        }

        // Default implementation
        if (method.defaultImpl) {
          for (const arg of method.defaultImpl.args) {
            extractPatternTokens(arg, content, tokens, true);
          }
          extractExprTokens(
            method.defaultImpl.body,
            semantics,
            content,
            tokens
          );
        }
      }
      break;
    }

    case "ImplementationDeclaration": {
      // Protocol name
      const protocolSpan = getNameSpan(decl.span, decl.protocolName, content);
      addToken(tokens, protocolSpan, TokenTypeIndex.Protocol, 0);

      // Type arguments
      for (const typeArg of decl.typeArgs) {
        extractTypeExprTokens(typeArg, content, tokens);
      }

      // Method implementations
      for (const method of decl.methods) {
        const methodSpan = getNameSpan(method.span, method.name, content);
        addToken(
          tokens,
          methodSpan,
          TokenTypeIndex.Function,
          TokenModifierFlags.Definition
        );
        extractExprTokens(method.implementation, semantics, content, tokens);
      }
      break;
    }

    case "ExternalDeclaration": {
      // External binding name
      const nameSpan = getNameSpan(decl.span, decl.name, content);
      addToken(
        tokens,
        nameSpan,
        TokenTypeIndex.External,
        TokenModifierFlags.Declaration | TokenModifierFlags.Definition
      );

      // Type annotation
      extractTypeExprTokens(decl.annotation, content, tokens);
      break;
    }

    case "InfixDeclaration": {
      // Operator
      const opSpan = getNameSpan(decl.span, decl.operator, content);
      addToken(
        tokens,
        opSpan,
        TokenTypeIndex.Operator,
        TokenModifierFlags.Declaration
      );
      break;
    }
  }
}

/**
 * Extract tokens from an expression.
 */
function extractExprTokens(
  expr: Expr,
  semantics: SemanticModule | undefined,
  content: string,
  tokens: SemanticToken[]
): void {
  if (!expr || !expr.kind) return;

  switch (expr.kind) {
    case "Var": {
      // Determine if it's a constructor, function, or variable
      let typeIdx: number;
      if (expr.namespace === "upper") {
        typeIdx = TokenTypeIndex.Constructor;
      } else if (semantics?.typeSchemes[expr.name]?.type?.kind === "fun") {
        typeIdx = TokenTypeIndex.Function;
      } else {
        typeIdx = TokenTypeIndex.Variable;
      }
      addToken(tokens, expr.span, typeIdx, TokenModifierFlags.Readonly);
      break;
    }

    case "Number":
      addToken(tokens, expr.span, TokenTypeIndex.Number, 0);
      break;

    case "String":
      addToken(tokens, expr.span, TokenTypeIndex.String, 0);
      break;

    case "Char":
      addToken(tokens, expr.span, TokenTypeIndex.String, 0);
      break;

    case "Lambda":
      if (expr.args) {
        for (const arg of expr.args) {
          if (arg) {
            extractPatternTokens(arg, content, tokens, true);
          }
        }
      }
      if (expr.body) {
        extractExprTokens(expr.body, semantics, content, tokens);
      }
      break;

    case "Apply":
      if (expr.callee) {
        extractExprTokens(expr.callee, semantics, content, tokens);
      }
      if (expr.args) {
        for (const arg of expr.args) {
          if (arg) {
            extractExprTokens(arg, semantics, content, tokens);
          }
        }
      }
      break;

    case "If":
      if (expr.condition) {
        extractExprTokens(expr.condition, semantics, content, tokens);
      }
      if (expr.thenBranch) {
        extractExprTokens(expr.thenBranch, semantics, content, tokens);
      }
      if (expr.elseBranch) {
        extractExprTokens(expr.elseBranch, semantics, content, tokens);
      }
      break;

    case "LetIn":
      for (const binding of expr.bindings) {
        // Binding name
        const nameSpan = getNameSpan(binding.span, binding.name, content);
        const isFunction = binding.args.length > 0;
        addToken(
          tokens,
          nameSpan,
          isFunction ? TokenTypeIndex.Function : TokenTypeIndex.Variable,
          TokenModifierFlags.Declaration | TokenModifierFlags.Readonly
        );

        // Parameters
        for (const arg of binding.args) {
          extractPatternTokens(arg, content, tokens, true);
        }

        // Binding body
        extractExprTokens(binding.body, semantics, content, tokens);
      }
      extractExprTokens(expr.body, semantics, content, tokens);
      break;

    case "Case":
      extractExprTokens(expr.discriminant, semantics, content, tokens);
      for (const branch of expr.branches) {
        extractPatternTokens(branch.pattern, content, tokens, false);
        extractExprTokens(branch.body, semantics, content, tokens);
      }
      break;

    case "Infix":
      extractExprTokens(expr.left, semantics, content, tokens);
      // Operator - estimate span
      addToken(tokens, estimateOperatorSpan(expr), TokenTypeIndex.Operator, 0);
      extractExprTokens(expr.right, semantics, content, tokens);
      break;

    case "Paren":
      extractExprTokens(expr.expression, semantics, content, tokens);
      break;

    case "Tuple":
      for (const el of expr.elements) {
        extractExprTokens(el, semantics, content, tokens);
      }
      break;

    case "Unit":
      // No specific tokens
      break;

    case "List":
      for (const el of expr.elements) {
        extractExprTokens(el, semantics, content, tokens);
      }
      break;

    case "ListRange":
      extractExprTokens(expr.start, semantics, content, tokens);
      extractExprTokens(expr.end, semantics, content, tokens);
      break;

    case "Record":
      for (const field of expr.fields) {
        // Field name
        const fieldSpan = getNameSpan(field.span, field.name, content);
        addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
        extractExprTokens(field.value, semantics, content, tokens);
      }
      break;

    case "RecordUpdate": {
      // Base variable
      const baseSpan = getNameSpan(expr.span, expr.base, content);
      addToken(tokens, baseSpan, TokenTypeIndex.Variable, 0);
      for (const field of expr.fields) {
        const fieldSpan = getNameSpan(field.span, field.name, content);
        addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
        extractExprTokens(field.value, semantics, content, tokens);
      }
      break;
    }

    case "FieldAccess":
      extractExprTokens(expr.target, semantics, content, tokens);
      // Field name - estimate span
      const fieldSpan = estimateFieldSpan(expr);
      addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
      break;
  }
}

/**
 * Extract tokens from a pattern.
 */
function extractPatternTokens(
  pattern: Pattern,
  content: string,
  tokens: SemanticToken[],
  isParameter: boolean
): void {
  if (!pattern || !pattern.kind) return;

  switch (pattern.kind) {
    case "VarPattern":
      addToken(
        tokens,
        pattern.span,
        isParameter ? TokenTypeIndex.Parameter : TokenTypeIndex.Variable,
        TokenModifierFlags.Declaration | TokenModifierFlags.Readonly
      );
      break;

    case "WildcardPattern":
      // No token for wildcards
      break;

    case "ConstructorPattern":
      // Constructor name
      const ctorSpan = getNameSpan(pattern.span, pattern.name, content);
      addToken(tokens, ctorSpan, TokenTypeIndex.Constructor, 0);
      for (const arg of pattern.args) {
        extractPatternTokens(arg, content, tokens, false);
      }
      break;

    case "TuplePattern":
      for (const el of pattern.elements) {
        extractPatternTokens(el, content, tokens, false);
      }
      break;

    case "ListPattern":
      for (const el of pattern.elements) {
        extractPatternTokens(el, content, tokens, false);
      }
      break;

    case "ConsPattern":
      extractPatternTokens(pattern.head, content, tokens, false);
      extractPatternTokens(pattern.tail, content, tokens, false);
      break;
  }
}

/**
 * Extract tokens from a type expression.
 */
function extractTypeExprTokens(
  typeExpr: TypeExpr,
  content: string,
  tokens: SemanticToken[]
): void {
  if (!typeExpr || !typeExpr.kind) return;

  switch (typeExpr.kind) {
    case "TypeRef": {
      // Check if it's a type variable (lowercase) or type name (uppercase)
      const firstChar = typeExpr.name[0];
      const isTypeVar = firstChar && firstChar === firstChar.toLowerCase();
      const typeIdx = isTypeVar
        ? TokenTypeIndex.TypeParameter
        : TokenTypeIndex.Type;
      const nameSpan = getNameSpan(typeExpr.span, typeExpr.name, content);
      addToken(tokens, nameSpan, typeIdx, 0);

      for (const arg of typeExpr.args) {
        extractTypeExprTokens(arg, content, tokens);
      }
      break;
    }

    case "FunctionType":
      extractTypeExprTokens(typeExpr.from, content, tokens);
      extractTypeExprTokens(typeExpr.to, content, tokens);
      break;

    case "TupleType":
      for (const el of typeExpr.elements) {
        extractTypeExprTokens(el, content, tokens);
      }
      break;

    case "RecordType":
      for (const field of typeExpr.fields) {
        // Field name
        const fieldSpan = estimateRecordFieldSpan(typeExpr.span, field.name);
        addToken(tokens, fieldSpan, TokenTypeIndex.Property, 0);
        extractTypeExprTokens(field.type, content, tokens);
      }
      break;

    case "QualifiedType":
      // Constraints
      for (const constraint of typeExpr.constraints) {
        const protocolSpan = getNameSpan(
          constraint.span,
          constraint.protocolName,
          content
        );
        addToken(tokens, protocolSpan, TokenTypeIndex.Protocol, 0);
        for (const arg of constraint.typeArgs) {
          extractTypeExprTokens(arg, content, tokens);
        }
      }
      // Inner type
      extractTypeExprTokens(typeExpr.type, content, tokens);
      break;
  }
}

/**
 * Add a semantic token.
 */
function addToken(
  tokens: SemanticToken[],
  span: Span,
  type: number,
  modifiers: number
): void {
  const length = span.end.offset - span.start.offset;
  if (length <= 0) return;

  tokens.push({
    line: Math.max(0, span.start.line - 1), // Convert to 0-based
    char: Math.max(0, span.start.column - 1),
    length,
    type,
    modifiers,
  });
}

/**
 * Encode tokens as delta format.
 */
function encodeTokens(tokens: SemanticToken[]): number[] {
  const data: number[] = [];
  let prevLine = 0;
  let prevChar = 0;

  for (const token of tokens) {
    const deltaLine = token.line - prevLine;
    const deltaChar = deltaLine === 0 ? token.char - prevChar : token.char;

    data.push(deltaLine, deltaChar, token.length, token.type, token.modifiers);

    prevLine = token.line;
    prevChar = token.char;
  }

  return data;
}

/**
 * Helper to find the actual span for just the name part of a declaration.
 * Searches for the name in the source text to get accurate positioning.
 */
function getNameSpan(fullSpan: Span, name: string, content: string): Span {
  // Extract the text from the full span
  const spanText = content.substring(
    fullSpan.start.offset,
    fullSpan.end.offset
  );

  // Find the name within this text
  const nameIndex = spanText.indexOf(name);

  if (nameIndex === -1) {
    // Fallback to old behavior if name not found
    return {
      start: fullSpan.start,
      end: {
        offset: fullSpan.start.offset + name.length,
        line: fullSpan.start.line,
        column: fullSpan.start.column + name.length,
      },
    };
  }

  // Calculate the actual position of the name
  const nameStart = fullSpan.start.offset + nameIndex;
  const textBeforeName = spanText.substring(0, nameIndex);
  const newlinesBefore = (textBeforeName.match(/\n/g) || []).length;

  let startColumn: number;
  if (newlinesBefore > 0) {
    // Find column after last newline
    const lastNewlineIndex = textBeforeName.lastIndexOf("\n");
    startColumn = textBeforeName.length - lastNewlineIndex;
  } else {
    // Same line, add to start column
    startColumn = fullSpan.start.column + nameIndex;
  }

  return {
    start: {
      offset: nameStart,
      line: fullSpan.start.line + newlinesBefore,
      column: startColumn,
    },
    end: {
      offset: nameStart + name.length,
      line: fullSpan.start.line + newlinesBefore,
      column: startColumn + name.length,
    },
  };
}

/**
 * Helper to estimate span for module name.
 */
function getModuleNameSpan(
  fullSpan: Span,
  moduleName: string,
  content: string
): Span {
  return getNameSpan(fullSpan, moduleName, content);
}

/**
 * Estimate operator span in infix expression.
 */
function estimateOperatorSpan(expr: {
  kind: "Infix";
  left: Expr;
  operator: string;
  right: Expr;
  span: Span;
}): Span {
  // Place between left and right expressions
  const leftEnd = expr.left.span.end;
  return {
    start: {
      offset: leftEnd.offset + 1,
      line: leftEnd.line,
      column: leftEnd.column + 1,
    },
    end: {
      offset: leftEnd.offset + 1 + expr.operator.length,
      line: leftEnd.line,
      column: leftEnd.column + 1 + expr.operator.length,
    },
  };
}

/**
 * Estimate field span in field access.
 */
function estimateFieldSpan(expr: {
  kind: "FieldAccess";
  target: Expr;
  field: string;
  span: Span;
}): Span {
  const targetEnd = expr.target.span.end;
  return {
    start: {
      offset: targetEnd.offset + 1, // +1 for the dot
      line: targetEnd.line,
      column: targetEnd.column + 1,
    },
    end: {
      offset: targetEnd.offset + 1 + expr.field.length,
      line: targetEnd.line,
      column: targetEnd.column + 1 + expr.field.length,
    },
  };
}

/**
 * Estimate record field name span.
 */
function estimateRecordFieldSpan(recordSpan: Span, fieldName: string): Span {
  // This is a rough approximation
  return {
    start: recordSpan.start,
    end: {
      offset: recordSpan.start.offset + fieldName.length,
      line: recordSpan.start.line,
      column: recordSpan.start.column + fieldName.length,
    },
  };
}
