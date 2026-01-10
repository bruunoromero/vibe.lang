/**
 * Vibe Parser - Generates Abstract Syntax Tree (AST) from source code
 *
 * This parser implements a recursive descent parser for the Vibe language,
 * as specified in docs/grammar.ebnf. It transforms a token stream into an AST
 * that preserves all syntactic structure for later semantic analysis.
 *
 * Key features:
 * - Layout-sensitive parsing (indentation matters)
 * - Operator precedence climbing for binary expressions
 * - Full span tracking for error reporting
 * - Defers semantic validation to the semantic analyzer
 */

import { lex } from "@vibe/lexer";
import {
  TokenKind,
  type Token,
  type Program,
  type ModuleDeclaration,
  type ImportDeclaration,
  type Exposing,
  type Declaration,
  type TypeAnnotationDeclaration,
  type TypeExpr,
  type ValueDeclaration,
  type Pattern,
  type Expr,
  type RecordField,
  type Span,
  type TypeDeclaration,
  type TypeAliasDeclaration,
  type ConstructorVariant,
} from "@vibe/syntax";

/**
 * Error thrown during parsing when unexpected syntax is encountered
 */
export class ParseError extends Error {
  constructor(message: string, public readonly span: Span) {
    super(message);
  }
}

/**
 * Main entry point: Parse Vibe source code into an AST
 *
 * Grammar: Program = [ModuleDeclaration], {ImportDeclaration}, {Declaration}
 *
 * @param source - Vibe source code as a string
 * @returns Abstract Syntax Tree representing the program
 */
export function parse(source: string): Program {
  // Step 1: Tokenize the source code
  const tokens = lex(source);

  // Step 2: Create parser instance with token stream
  const parser = new Parser(tokens);

  // Step 3: Parse tokens into AST
  return parser.parseProgram();
}

/**
 * Recursive descent parser for Vibe language
 * Implements grammar rules from docs/grammar.ebnf
 */
class Parser {
  /** Current position in token stream */
  private index = 0;

  /** Stack tracking indentation levels for layout-sensitive constructs */
  private layoutStack: number[] = [];

  constructor(private readonly tokens: Token[]) {}

  /**
   * Parse complete program: optional module header, imports, and declarations
   *
   * Grammar: Program = [ModuleDeclaration], {ImportDeclaration}, {Declaration}
   */
  parseProgram(): Program {
    // Parse optional module declaration (e.g., "module Main exposing (..)")
    const module = this.peekKeyword("module")
      ? this.parseModuleDeclaration()
      : undefined;

    // Parse all import declarations
    const imports: ImportDeclaration[] = [];
    while (this.peekKeyword("import")) {
      imports.push(this.parseImport());
    }

    // Parse all top-level declarations (functions, type annotations, externals)
    const declarations: Declaration[] = [];
    while (!this.isAtEnd()) {
      declarations.push(this.parseDeclaration());
    }

    return { module, imports, declarations };
  }

  /**
   * Parse module declaration
   *
   * Grammar: ModuleDeclaration = "module", ModuleName, "exposing", Exposing
   * Example: module Main exposing (..)
   */
  private parseModuleDeclaration(): ModuleDeclaration {
    // Consume "module" keyword
    const moduleToken = this.expectKeyword("module");

    // Parse module name (e.g., "Main" or "Data.List")
    const { name, end } = this.parseModuleName();

    // Consume "exposing" keyword
    this.expectKeyword("exposing");

    // Parse exposing clause ((..) or explicit list)
    const exposing = this.parseExposing();

    // Build AST node with full span
    const span: Span = { start: moduleToken.span.start, end };
    return { name, exposing, span };
  }

  /**
   * Parse import declaration
   *
   * Grammar: ImportDeclaration = "import", ModuleName, ["as", UpperIdentifier], ["exposing", Exposing]
   * Examples:
   *   import Html
   *   import Html as H
   *   import Html exposing (div, text)
   */
  private parseImport(): ImportDeclaration {
    // Consume "import" keyword
    const importToken = this.expectKeyword("import");

    // Parse module name being imported
    const { name: moduleName, end: moduleEnd } = this.parseModuleName();

    // Parse optional "as" alias
    let alias: string | undefined;
    if (this.peekKeyword("as")) {
      this.expectKeyword("as");
      const aliasToken = this.expect(TokenKind.UpperIdentifier, "import alias");
      alias = aliasToken.lexeme;
    }

    // Parse optional "exposing" clause
    let exposing: Exposing | undefined;
    if (this.peekKeyword("exposing")) {
      this.expectKeyword("exposing");
      exposing = this.parseExposing();
    }

    // Calculate span from start to end of import
    const last = exposing
      ? exposing.span.end
      : alias
      ? this.previousSpan().end
      : moduleEnd;
    return {
      moduleName,
      alias,
      exposing,
      span: { start: importToken.span.start, end: last },
    };
  }

  /**
   * Parse exposing clause
   *
   * Grammar: Exposing = "(", "..", ")" | "(", IdentifierList, ")"
   * Examples:
   *   (..)              - expose all
   *   (foo, bar, Baz)   - expose specific names
   */
  private parseExposing(): Exposing {
    // Consume opening parenthesis
    this.expect(TokenKind.LParen, "exposing list start");

    // Check if exposing all (..)
    if (this.match(TokenKind.Range)) {
      const end = this.expect(TokenKind.RParen, "close exposing (..)").span.end;
      return { kind: "All", span: { start: this.previousSpan().start, end } };
    }

    // Parse explicit list of names
    const names: string[] = [];
    const start = this.currentSpan().start;

    // Parse comma-separated identifier list
    while (true) {
      const nameToken = this.expectAnyIdentifier("exposed value");
      names.push(nameToken.lexeme);

      // Continue if comma found, otherwise break
      if (this.match(TokenKind.Comma)) {
        continue;
      }
      break;
    }

    // Consume closing parenthesis
    const end = this.expect(TokenKind.RParen, "close exposing list").span.end;
    return { kind: "Explicit", names, span: { start, end } };
  }

  /**
   * Parse top-level declaration
   *
   * Grammar: Declaration = TypeDeclaration | TypeAliasDeclaration | ExternalDeclaration | TypeAnnotationDeclaration | ValueDeclaration
   *
   * Examples:
   *   type Maybe a = Just a | Nothing        (Type declaration - ADT)
   *   type alias UserId = number             (Type alias)
   *   @external "module" "func" foo : Int -> Int     (External FFI)
   *   foo : Int -> Int                               (Type annotation)
   *   foo x = x + 1                                  (Value declaration)
   *   (+) a b = add a b                              (Operator declaration)
   */
  private parseDeclaration(): Declaration {
    // Check for external declaration (@external ...)
    if (this.peekExternalAttribute()) {
      return this.parseExternalDeclaration();
    }

    // Check for type declaration or type alias (type ...)
    if (this.peekKeyword("type")) {
      return this.parseTypeOrAliasDeclaration();
    }

    // Parse declaration name (identifier or operator in parens)
    const { name, span: nameSpan } = this.parseDeclarationName();

    // Check if this is a type annotation (name : Type)
    if (this.match(TokenKind.Colon)) {
      const annotation = this.parseTypeExpression();
      const span: Span = { start: nameSpan.start, end: annotation.span.end };
      return {
        kind: "TypeAnnotationDeclaration",
        name,
        annotation,
        span,
      } satisfies TypeAnnotationDeclaration;
    }

    // Otherwise, parse value declaration (name args = body)
    // Parse pattern arguments (if any)
    const args: Pattern[] = [];
    while (this.isPatternStart(this.current())) {
      args.push(this.parsePattern());
    }

    // Consume equals sign
    this.expect(TokenKind.Equals, "declaration body");

    // Parse expression body
    const body = this.parseExpression();
    const span: Span = { start: nameSpan.start, end: body.span.end };

    return {
      kind: "ValueDeclaration",
      name,
      args,
      body,
      span,
    } satisfies ValueDeclaration;
  }

  /**
   * Parse type declaration or type alias
   *
   * Grammar:
   *   TypeDeclaration = "type", UpperIdentifier, {LowerIdentifier}, "=", ConstructorDef, {"|", ConstructorDef}
   *   TypeAliasDeclaration = "type", "alias", UpperIdentifier, {LowerIdentifier}, "=", TypeExpr
   *
   * Examples:
   *   type Bool = True | False
   *   type Maybe a = Just a | Nothing
   *   type Result e v = Ok v | Err e
   *   type alias UserId = number
   *   type alias Pair a b = (a, b)
   */
  private parseTypeOrAliasDeclaration():
    | TypeDeclaration
    | TypeAliasDeclaration {
    // Consume "type" keyword
    const typeToken = this.expectKeyword("type");

    // Check if this is a type alias (type alias ...)
    if (this.peekKeyword("alias")) {
      return this.parseTypeAliasDeclaration(typeToken);
    }

    // Parse type name (must be uppercase)
    const nameToken = this.expect(TokenKind.UpperIdentifier, "type name");
    const name = nameToken.lexeme;

    // Parse type parameters (zero or more lowercase identifiers)
    const params: string[] = [];
    while (this.peek(TokenKind.LowerIdentifier)) {
      const param = this.expect(TokenKind.LowerIdentifier, "type parameter");
      params.push(param.lexeme);
    }

    // Consume equals sign
    this.expect(TokenKind.Equals, "type definition");

    // Parse constructor variants separated by pipe (|)
    const constructors: ConstructorVariant[] = [];
    constructors.push(this.parseConstructorVariant());

    // Parse additional variants separated by |
    while (this.match(TokenKind.Pipe)) {
      constructors.push(this.parseConstructorVariant());
    }

    // Calculate span from "type" to end of last constructor
    const lastConstructor = constructors[constructors.length - 1]!;
    const span: Span = {
      start: typeToken.span.start,
      end: lastConstructor.span.end,
    };

    return {
      kind: "TypeDeclaration",
      name,
      params,
      constructors,
      span,
    } satisfies TypeDeclaration;
  }

  /**
   * Parse a single constructor variant in an ADT definition
   *
   * Grammar: ConstructorDef = UpperIdentifier, {TypeTerm}
   *
   * Examples:
   *   True                    (nullary constructor)
   *   Just a                  (unary constructor with type parameter)
   *   Node a (Tree a) (Tree a) (constructor with multiple arguments)
   */
  private parseConstructorVariant(): ConstructorVariant {
    // Parse constructor name (must be uppercase)
    const nameToken = this.expect(
      TokenKind.UpperIdentifier,
      "constructor name"
    );
    const name = nameToken.lexeme;

    // Parse constructor arguments (zero or more type terms)
    // Arguments continue as long as we see valid type starts on the same line
    const args: TypeExpr[] = [];
    let lastSpan = nameToken.span;

    while (
      this.isTypeStart(this.current()) &&
      this.onSameLine(lastSpan, this.current()) &&
      !this.peek(TokenKind.Pipe) // Stop before next variant
    ) {
      const arg = this.parseTypeTerm();
      args.push(arg);
      lastSpan = arg.span;
    }

    // Calculate span from constructor name to end of last argument (or just name if no args)
    const endSpan =
      args.length > 0 ? args[args.length - 1]!.span.end : nameToken.span.end;
    const span: Span = { start: nameToken.span.start, end: endSpan };

    return { name, args, span };
  }

  /**
   * Parse type alias declaration
   *
   * Grammar: TypeAliasDeclaration = "type", "alias", UpperIdentifier, {LowerIdentifier}, "=", TypeExpr
   *
   * Examples:
   *   type alias UserId = number
   *   type alias Pair a b = (a, b)
   *   type alias Handler msg = msg -> Model -> Model
   *
   * @param typeToken - The already consumed "type" keyword token
   */
  private parseTypeAliasDeclaration(typeToken: Token): TypeAliasDeclaration {
    // Consume "alias" keyword
    this.expectKeyword("alias");

    // Parse alias name (must be uppercase)
    const nameToken = this.expect(TokenKind.UpperIdentifier, "type alias name");
    const name = nameToken.lexeme;

    // Parse type parameters (zero or more lowercase identifiers)
    const params: string[] = [];
    while (this.peek(TokenKind.LowerIdentifier)) {
      const param = this.expect(TokenKind.LowerIdentifier, "type parameter");
      params.push(param.lexeme);
    }

    // Consume equals sign
    this.expect(TokenKind.Equals, "type alias definition");

    // Parse the type expression
    const value = this.parseTypeExpression();

    // Calculate span from "type" to end of type expression
    const span: Span = { start: typeToken.span.start, end: value.span.end };

    return {
      kind: "TypeAliasDeclaration",
      name,
      params,
      value,
      span,
    } satisfies TypeAliasDeclaration;
  }

  /**
   * Parse type expression (entry point)
   *
   * Grammar: TypeExpr = TypeTerm, ["->", TypeExpr]
   */
  private parseTypeExpression(): TypeExpr {
    return this.parseTypeArrow();
  }

  /**
   * Parse function type with right-associative arrow
   *
   * Grammar: TypeExpr = TypeTerm, ["->", TypeExpr]
   * Example: Int -> String -> Bool  (parses as Int -> (String -> Bool))
   * Example: (a -> b) -> a -> b  (parses as (a -> b) -> (a -> b))
   */
  private parseTypeArrow(): TypeExpr {
    // Parse left side of arrow
    const left = this.parseTypeTerm();

    // Check for arrow operator
    if (this.matchOperator("->")) {
      // RIGHT-ASSOCIATIVE: recursively parse the entire right side
      // This ensures "a -> b -> c" becomes "a -> (b -> c)"
      const right = this.parseTypeArrow(); // Recursive call for right-associativity
      return {
        kind: "FunctionType",
        from: left,
        to: right,
        span: { start: left.span.start, end: right.span.end },
      };
    }

    return left;
  }

  private parseTypeTerm(): TypeExpr {
    // Parse record type { field1 : Type1, field2 : Type2, ... }
    if (this.match(TokenKind.LBrace)) {
      const start = this.previousSpan().start;
      const fields: Array<{ name: string; type: TypeExpr }> = [];

      // Check for empty record {}
      if (this.match(TokenKind.RBrace)) {
        return {
          kind: "RecordType",
          fields: [],
          span: { start, end: this.previousSpan().end },
        };
      }

      // Parse first field
      const firstName = this.expect(
        TokenKind.LowerIdentifier,
        "record field name"
      );
      this.expect(TokenKind.Colon, "':' after field name");
      const firstType = this.parseTypeExpression();
      fields.push({ name: firstName.lexeme, type: firstType });

      const baseIndent = firstName.span.start.column;
      let lastEnd = firstType.span.end;

      // Parse subsequent fields
      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
        this.advance(); // consume comma

        const fieldName = this.expect(
          TokenKind.LowerIdentifier,
          "record field name"
        );
        this.expect(TokenKind.Colon, "':' after field name");
        const fieldType = this.parseTypeExpression();
        fields.push({ name: fieldName.lexeme, type: fieldType });
        lastEnd = fieldType.span.end;
      }

      this.expect(TokenKind.RBrace, "close record type");

      // Sort fields alphabetically for consistency
      fields.sort((a, b) => a.name.localeCompare(b.name));

      return {
        kind: "RecordType",
        fields,
        span: { start, end: this.previousSpan().end },
      };
    }

    if (this.match(TokenKind.LParen)) {
      const start = this.previousSpan().start;
      const first = this.parseTypeExpression();
      const elements: TypeExpr[] = [first];
      const baseIndent = first.span.start.column;
      let lastEnd = first.span.end;

      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
        this.advance();
        const expr = this.parseTypeExpression();
        elements.push(expr);
        lastEnd = expr.span.end;
      }

      this.expect(TokenKind.RParen, "close type group");

      if (elements.length === 1) {
        return { ...first, span: { start, end: this.previousSpan().end } };
      }

      return {
        kind: "TupleType",
        elements,
        span: { start, end: this.previousSpan().end },
      };
    }

    const ident = this.expectIdentifier("type reference");

    // Only uppercase type constructors can have arguments
    // Lowercase identifiers are type variables and don't take arguments
    // This ensures "a (List a)" parses as two separate types, not "a" applied to "(List a)"
    const isTypeConstructor = ident.kind === TokenKind.UpperIdentifier;

    const args: TypeExpr[] = [];
    let lastSpan = ident.span;
    while (
      isTypeConstructor &&
      this.isTypeStart(this.current()) &&
      this.onSameLine(lastSpan, this.current())
    ) {
      const arg = this.parseTypeTerm();
      args.push(arg);
      lastSpan = arg.span;
    }

    return {
      kind: "TypeRef",
      name: ident.lexeme,
      args,
      span: {
        start: ident.span.start,
        end: args.at(-1)?.span.end ?? ident.span.end,
      },
    };
  }

  /**
   * Parse pattern (used in function arguments, case branches, let bindings)
   *
   * Grammar: Pattern = VarPattern | WildcardPattern | ConstructorPattern | TuplePattern | "(", Pattern, ")"
   *
   * Examples:
   *   x                    (VarPattern)
   *   _                    (WildcardPattern)
   *   Just x               (ConstructorPattern with arg)
   *   (x, y)               (TuplePattern)
   */
  private parsePattern(): Pattern {
    // Check for lower identifier (variable or wildcard)
    if (this.match(TokenKind.LowerIdentifier)) {
      const tok = this.previous();

      // Underscore is wildcard pattern
      if (tok.lexeme === "_") {
        return { kind: "WildcardPattern", span: tok.span };
      }

      // Otherwise it's a variable pattern
      return { kind: "VarPattern", name: tok.lexeme, span: tok.span };
    }

    // Check for constructor pattern (uppercase identifier)
    if (this.match(TokenKind.UpperIdentifier)) {
      const ctor = this.previous();

      // Parse constructor arguments (zero or more patterns)
      const args: Pattern[] = [];
      while (this.isPatternStart(this.current())) {
        args.push(this.parsePattern());
      }

      const end = args.at(-1)?.span.end ?? ctor.span.end;
      return {
        kind: "ConstructorPattern",
        name: ctor.lexeme,
        args,
        span: { start: ctor.span.start, end },
      };
    }

    // Check for parenthesized pattern or tuple
    if (this.match(TokenKind.LParen)) {
      const start = this.previousSpan().start;

      // Parse first pattern
      const first = this.parsePattern();
      const elements: Pattern[] = [first];
      const baseIndent = first.span.start.column;
      let lastEnd = first.span.end;

      // Check for additional comma-separated patterns (tuple)
      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);

        // Respect layout rules for multi-line tuples
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;

        this.advance(); // consume comma
        const pat = this.parsePattern();
        elements.push(pat);
        lastEnd = pat.span.end;
      }

      this.expect(TokenKind.RParen, "close pattern group");

      // Single element = just a grouped pattern
      if (elements.length === 1) {
        return { ...first, span: { start, end: this.previousSpan().end } };
      }

      // Multiple elements = tuple pattern
      return {
        kind: "TuplePattern",
        elements,
        span: { start, end: this.previousSpan().end },
      };
    }

    throw this.error("pattern", this.current());
  }

  /**
   * Parse expression (entry point)
   *
   * Grammar: Expr = BinaryExpr
   *
   * @param baseIndentFloor - Minimum indentation for layout-sensitive contexts
   */
  private parseExpression(baseIndentFloor?: number): Expr {
    return this.parseBinaryExpression(0, baseIndentFloor);
  }

  /**
   * Parse binary expression with operator precedence climbing
   *
   * Grammar: BinaryExpr = Application, {Operator, Application}
   *
   * This implements precedence climbing algorithm to handle operator precedence
   * and associativity correctly (e.g., 1 + 2 * 3 parses as 1 + (2 * 3))
   *
   * @param minPrecedence - Minimum precedence level to consider
   * @param baseIndentFloor - Minimum indentation for layout
   */
  private parseBinaryExpression(
    minPrecedence: number,
    baseIndentFloor?: number
  ): Expr {
    // Parse left operand (an application expression)
    let left = this.parseApplication(baseIndentFloor);

    // Parse operators and right operands
    while (true) {
      const operatorToken = this.current();

      // Stop if not an operator
      if (operatorToken.kind !== TokenKind.Operator) break;

      // Get precedence and associativity for this operator
      const { precedence, associativity } = getOperatorInfo(
        operatorToken.lexeme
      );

      // Stop if precedence too low
      if (precedence < minPrecedence) break;

      // Consume operator
      this.advance();

      // Calculate minimum precedence for right side
      // Left-associative: increase precedence (left binds tighter)
      // Right-associative: keep same precedence (right binds tighter)
      const nextMin = associativity === "left" ? precedence + 1 : precedence;

      // Parse right operand
      const right = this.parseBinaryExpression(nextMin, baseIndentFloor);

      // Build infix expression node
      left = {
        kind: "Infix",
        operator: operatorToken.lexeme,
        left,
        right,
        span: { start: left.span.start, end: right.span.end },
      };
    }

    return left;
  }

  /**
   * Parse function application (juxtaposition)
   *
   * Grammar: Application = PrimaryWithAccess, {PrimaryWithAccess}
   *
   * Example: f x y  (parses as (f x) y - left-associative)
   *
   * @param baseIndentFloor - Minimum indentation for layout
   */
  private parseApplication(baseIndentFloor?: number): Expr {
    // Parse first expression (callee)
    let expr = this.parsePrimaryWithAccess();
    const args: Expr[] = [];

    // Calculate effective indentation for arguments
    const baseIndent = expr.span.start.column;
    const effectiveIndent = Math.min(baseIndent, baseIndentFloor ?? baseIndent);
    let lastEnd = expr.span.end;

    // Parse arguments while respecting layout
    while (this.isExpressionStart(this.current())) {
      const nextToken = this.current();

      // Stop if argument doesn't continue the layout
      if (!this.continuesLayout(effectiveIndent, lastEnd, nextToken)) break;

      args.push(this.parsePrimaryWithAccess());
      lastEnd = args.at(-1)!.span.end;
    }

    // No arguments = not an application
    if (args.length === 0) return expr;

    // Build application node
    return {
      kind: "Apply",
      callee: expr,
      args,
      span: { start: expr.span.start, end: args.at(-1)!.span.end },
    };
  }

  /**
   * Parse primary expression with field access
   *
   * Grammar: PrimaryWithAccess = Primary, {".", LowerIdentifier}
   *
   * Example: record.field.nested
   */
  private parsePrimaryWithAccess(): Expr {
    // Parse base expression
    let expr = this.parsePrimary();

    // Parse field accesses (chained with dots)
    while (this.match(TokenKind.Dot)) {
      const field = this.expect(TokenKind.LowerIdentifier, "record field");
      expr = {
        kind: "FieldAccess",
        target: expr,
        field: field.lexeme,
        span: { start: expr.span.start, end: field.span.end },
      };
    }

    return expr;
  }

  private parsePrimary(): Expr {
    const token = this.current();

    if (this.peekKeyword("if")) return this.parseIf();
    if (this.peekKeyword("let")) return this.parseLetIn();
    if (this.peekKeyword("case")) return this.parseCase();

    if (this.match(TokenKind.Backslash)) {
      const start = this.previousSpan().start;
      const args: Pattern[] = [];
      while (!this.matchOperator("->")) {
        if (!this.isPatternStart(this.current())) {
          throw this.error("lambda argument", this.current());
        }
        args.push(this.parsePattern());
      }
      const body = this.parseExpression();
      return {
        kind: "Lambda",
        args,
        body,
        span: { start, end: body.span.end },
      };
    }

    if (this.match(TokenKind.LowerIdentifier)) {
      const tok = this.previous();
      return {
        kind: "Var",
        name: tok.lexeme,
        namespace: "lower",
        span: tok.span,
      };
    }

    if (this.match(TokenKind.UpperIdentifier)) {
      const tok = this.previous();
      return {
        kind: "Var",
        name: tok.lexeme,
        namespace: "upper",
        span: tok.span,
      };
    }

    if (this.match(TokenKind.Number)) {
      const tok = this.previous();
      return { kind: "Number", value: tok.lexeme, span: tok.span };
    }

    if (this.match(TokenKind.String)) {
      const tok = this.previous();
      return { kind: "String", value: tok.lexeme, span: tok.span };
    }

    if (this.match(TokenKind.Char)) {
      const tok = this.previous();
      return { kind: "Char", value: tok.lexeme, span: tok.span };
    }

    if (this.match(TokenKind.LParen)) {
      const start = this.previousSpan().start;
      if (this.match(TokenKind.RParen)) {
        return { kind: "Unit", span: { start, end: this.previousSpan().end } };
      }
      const first = this.parseExpression();
      const elements: Expr[] = [first];
      const baseIndent = first.span.start.column;
      let lastEnd = first.span.end;

      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
        this.advance();
        const expr = this.parseExpression();
        elements.push(expr);
        lastEnd = expr.span.end;
      }

      this.expect(TokenKind.RParen, "close group");

      if (elements.length === 1) {
        return {
          kind: "Paren",
          expression: first,
          span: { start, end: this.previousSpan().end },
        };
      }

      return {
        kind: "Tuple",
        elements,
        span: { start, end: this.previousSpan().end },
      };
    }

    if (this.match(TokenKind.LBracket)) {
      const start = this.previousSpan().start;
      if (this.match(TokenKind.RBracket)) {
        return {
          kind: "List",
          elements: [],
          span: { start, end: this.previousSpan().end },
        };
      }

      const first = this.parseExpression();
      if (this.match(TokenKind.Range)) {
        const endExpr = this.parseExpression();
        this.expect(TokenKind.RBracket, "close list range");
        return {
          kind: "ListRange",
          start: first,
          end: endExpr,
          span: { start, end: this.previousSpan().end },
        };
      }

      const elements: Expr[] = [first];
      const baseIndent = first.span.start.column;
      let lastEnd = first.span.end;
      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
        this.advance();
        const expr = this.parseExpression();
        elements.push(expr);
        lastEnd = expr.span.end;
      }
      this.expect(TokenKind.RBracket, "close list");
      return {
        kind: "List",
        elements,
        span: { start, end: this.previousSpan().end },
      };
    }

    if (this.match(TokenKind.LBrace)) {
      const start = this.previousSpan().start;
      if (this.match(TokenKind.RBrace)) {
        return {
          kind: "Record",
          fields: [],
          span: { start, end: this.previousSpan().end },
        };
      }

      const head = this.expect(
        TokenKind.LowerIdentifier,
        "record field or base"
      );
      if (this.match(TokenKind.Pipe)) {
        const fields = this.parseRecordFields(
          undefined,
          head.span.start.column
        );
        this.expect(TokenKind.RBrace, "close record update");
        return {
          kind: "RecordUpdate",
          base: head.lexeme,
          fields,
          span: { start, end: this.previousSpan().end },
        };
      }

      const fields = this.parseRecordFields(head, head.span.start.column);
      this.expect(TokenKind.RBrace, "close record");
      return {
        kind: "Record",
        fields,
        span: { start, end: this.previousSpan().end },
      };
    }

    throw this.error("expression", token);
  }

  private parseRecordFields(
    firstFieldToken?: Token,
    providedBaseIndent?: number
  ): RecordField[] {
    const fields: RecordField[] = [];

    let currentFieldToken =
      firstFieldToken ??
      this.expect(TokenKind.LowerIdentifier, "record field name");

    const baseIndent =
      providedBaseIndent ?? currentFieldToken.span.start.column;
    let lastEnd = currentFieldToken.span.end;

    while (true) {
      this.expect(TokenKind.Equals, "record field assignment");
      const value = this.parseExpression(baseIndent);
      lastEnd = value.span.end;
      fields.push({
        name: currentFieldToken.lexeme,
        value,
        span: { start: currentFieldToken.span.start, end: value.span.end },
      });

      if (!this.peek(TokenKind.Comma)) break;
      const next = this.peekAhead(1);
      if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
      this.advance();
      currentFieldToken = this.expect(
        TokenKind.LowerIdentifier,
        "record field name"
      );
    }

    return fields;
  }

  private parseIf(): Expr {
    const ifToken = this.expectKeyword("if");
    const condition = this.parseExpression();
    this.expectKeyword("then");
    const thenBranch = this.parseExpression();
    this.expectKeyword("else");
    const elseBranch = this.parseExpression();
    return {
      kind: "If",
      condition,
      thenBranch,
      elseBranch,
      span: { start: ifToken.span.start, end: elseBranch.span.end },
    };
  }

  /**
   * Parse let-in expression
   *
   * Grammar: LetInExpr = "let", {LetBinding}, "in", Expr
   * Example:
   *   let
   *     x = 10
   *     y = 20
   *   in
   *     x + y
   *
   * Note: Indentation-sensitive - bindings must be indented past "let"
   */
  private parseLetIn(): Expr {
    // Consume "let" keyword
    const letToken = this.expectKeyword("let");
    const bindings: ValueDeclaration[] = [];

    // Establish layout context: bindings must be indented past "let"
    const layoutIndent = letToken.span.start.column + 1;
    let exitedLayoutEarly = false;

    this.withLayout(layoutIndent, () => {
      // Parse declarations until we see "in" or exit layout
      while (!this.peekKeyword("in") && this.inCurrentLayout(this.current())) {
        if (this.isAtEnd()) {
          throw this.error("'in' to close let expression", this.current());
        }

        // Parse declaration (can be value or type annotation)
        // NOTE: We defer validation of whether type annotations are allowed
        // to the semantic analyzer, and just parse the structure here
        const decl = this.parseDeclaration();

        // Store as ValueDeclaration in AST (semantic analyzer will validate)
        if (decl.kind === "ValueDeclaration") {
          bindings.push(decl);
        } else {
          // For type annotations, store them anyway - semantics will check later
          bindings.push(decl as any);
        }
      }

      // Check if we exited layout without seeing "in"
      if (!this.peekKeyword("in") && !this.isAtEnd()) {
        exitedLayoutEarly = true;
      }
    });

    // Report error if layout was exited early
    if (exitedLayoutEarly) {
      throw new ParseError(
        `Expected 'in' to close let at column ${layoutIndent} or beyond`,
        this.current().span
      );
    }

    // Consume "in" keyword
    this.expectKeyword("in");

    // Parse body expression
    const body = this.parseExpression();
    return {
      kind: "LetIn",
      bindings,
      body,
      span: { start: letToken.span.start, end: body.span.end },
    };
  }

  /**
   * Parse case expression
   *
   * Grammar: CaseExpr = "case", Expr, "of", {CaseBranch}
   * Grammar: CaseBranch = Pattern, "->", Expr
   *
   * Example:
   *   case value of
   *     Just x -> x
   *     Nothing -> 0
   *
   * Note: Indentation-sensitive - branches must be indented past "of"
   */
  private parseCase(): Expr {
    // Consume "case" keyword
    const caseToken = this.expectKeyword("case");

    // Parse discriminant expression
    const discriminant = this.parseExpression();

    // Consume "of" keyword
    const ofToken = this.expectKeyword("of");
    const ofColumn = ofToken.span.start.column;

    // Parse case branches
    const branches: Array<{ pattern: Pattern; body: Expr; span: Span }> = [];

    // Establish layout context: branches must be indented past "case"
    const layoutIndent = caseToken.span.start.column + 1;

    this.withLayout(layoutIndent, () => {
      // Check if first token after "of" is properly indented
      const firstAfterOf = this.current();
      if (
        !this.inCurrentLayout(firstAfterOf) &&
        !this.peekKeyword("in") &&
        !this.peekKeyword("else") &&
        !this.peek(TokenKind.Eof)
      ) {
        throw new ParseError(
          `Case branches must be indented to at least column ${layoutIndent} (or column ${
            ofColumn + 1
          } relative to 'of'), but found column ${
            firstAfterOf.span.start.column
          }`,
          firstAfterOf.span
        );
      }

      // Parse branches while in layout context
      while (
        !this.isAtEnd() &&
        this.inCurrentLayout(this.current()) &&
        !this.peekKeyword("in") &&
        !this.peekKeyword("else")
      ) {
        const next = this.current();

        // Verify proper indentation
        if (next.span.start.column < layoutIndent) {
          throw new ParseError(
            `Case branch must be indented to at least column ${layoutIndent} (or column ${
              ofColumn + 1
            } relative to 'of'), but found column ${next.span.start.column}`,
            next.span
          );
        }

        // Parse pattern
        const pattern = this.parsePattern();

        // Consume "->" arrow
        this.expectOperator("->");

        // Parse branch body
        const branchIndent = pattern.span.start.column + 1;
        const body = this.parseExpression(branchIndent);

        // Add branch to list
        branches.push({
          pattern,
          body,
          span: { start: pattern.span.start, end: body.span.end },
        });

        if (this.peek(TokenKind.Eof)) break;
      }
    });

    // NOTE: We defer validation of whether case has branches to semantic analyzer
    // The grammar allows empty case, but it's semantically invalid
    // We still report it here for better error messages

    const end = branches.at(-1)?.span.end ?? discriminant.span.end;
    return {
      kind: "Case",
      discriminant,
      branches,
      span: { start: caseToken.span.start, end },
    };
  }

  private parseModuleName(): { name: string; end: Span["end"] } {
    const first = this.expect(TokenKind.UpperIdentifier, "module name");
    const parts = [first.lexeme];
    let end = first.span.end;

    while (this.match(TokenKind.Dot)) {
      const next = this.expect(
        TokenKind.UpperIdentifier,
        "module name segment"
      );
      parts.push(next.lexeme);
      end = next.span.end;
    }

    return { name: parts.join("."), end };
  }

  private parseDeclarationName(): { name: string; span: Span } {
    if (this.match(TokenKind.LowerIdentifier)) {
      const tok = this.previous();
      return { name: tok.lexeme, span: tok.span };
    }

    if (this.match(TokenKind.LParen)) {
      const start = this.previousSpan().start;
      const op = this.expect(TokenKind.Operator, "operator name");
      const end = this.expect(TokenKind.RParen, "close operator name").span.end;
      return { name: op.lexeme, span: { start, end } };
    }

    throw this.error("declaration name", this.current());
  }

  private parseExternalDeclaration(): Declaration {
    const atToken = this.expectOperator("@");
    const externalToken = this.expect(
      TokenKind.LowerIdentifier,
      "external tag"
    );
    if (externalToken.lexeme !== "external") {
      throw this.error("external tag", externalToken);
    }

    const modulePathTok = this.expect(TokenKind.String, "external module path");
    const exportTok = this.expect(TokenKind.String, "external export name");

    const { name, span: nameSpan } = this.parseDeclarationName();
    this.expect(TokenKind.Colon, "external type annotation");
    const annotation = this.parseTypeExpression();

    const targetSpan: Span = {
      start: modulePathTok.span.start,
      end: exportTok.span.end,
    };

    const span: Span = { start: atToken.span.start, end: annotation.span.end };

    return {
      kind: "ExternalDeclaration",
      name,
      target: {
        modulePath: this.unquote(modulePathTok.lexeme),
        exportName: this.unquote(exportTok.lexeme),
        span: targetSpan,
      },
      annotation,
      span,
    };
  }

  private peekExternalAttribute(): boolean {
    const current = this.current();
    const next = this.peekAhead(1);
    return (
      current.kind === TokenKind.Operator &&
      current.lexeme === "@" &&
      next?.kind === TokenKind.LowerIdentifier &&
      next.lexeme === "external"
    );
  }

  private unquote(lexeme: string): string {
    if (lexeme.length >= 2 && lexeme.startsWith('"') && lexeme.endsWith('"')) {
      return lexeme.slice(1, -1);
    }
    return lexeme;
  }

  private expect(kind: TokenKind, label: string): Token {
    const token = this.current();
    if (token.kind === kind) {
      this.advance();
      return token;
    }
    throw this.error(label, token);
  }

  private expectKeyword(keyword: string): Token {
    const token = this.current();
    if (token.kind === TokenKind.Keyword && token.lexeme === keyword) {
      this.advance();
      return token;
    }
    throw this.error(`keyword '${keyword}'`, token);
  }

  private expectOperator(op: string): Token {
    const token = this.current();
    if (token.kind === TokenKind.Operator && token.lexeme === op) {
      this.advance();
      return token;
    }
    throw this.error(`operator '${op}'`, token);
  }

  private expectAnyIdentifier(label: string): Token {
    const token = this.current();
    if (
      token.kind === TokenKind.LowerIdentifier ||
      token.kind === TokenKind.UpperIdentifier
    ) {
      this.advance();
      return token;
    }
    throw this.error(label, token);
  }

  private expectIdentifier(label: string): Token {
    return this.expectAnyIdentifier(label);
  }

  private match(kind: TokenKind): boolean {
    if (this.current().kind === kind) {
      this.advance();
      return true;
    }
    return false;
  }

  private matchOperator(op: string): boolean {
    const token = this.current();
    if (token.kind === TokenKind.Operator && token.lexeme === op) {
      this.advance();
      return true;
    }
    return false;
  }

  private peek(kind: TokenKind): boolean {
    return this.current().kind === kind;
  }

  private peekKeyword(keyword: string): boolean {
    const token = this.current();
    return token.kind === TokenKind.Keyword && token.lexeme === keyword;
  }

  private isPatternStart(token: Token): boolean {
    return (
      token.kind === TokenKind.LowerIdentifier ||
      token.kind === TokenKind.UpperIdentifier ||
      token.kind === TokenKind.LParen
    );
  }

  private isExpressionStart(token: Token): boolean {
    if (token.kind === TokenKind.Keyword) {
      return (
        token.lexeme === "if" ||
        token.lexeme === "let" ||
        token.lexeme === "case"
      );
    }
    return (
      token.kind === TokenKind.LowerIdentifier ||
      token.kind === TokenKind.UpperIdentifier ||
      token.kind === TokenKind.Number ||
      token.kind === TokenKind.String ||
      token.kind === TokenKind.Char ||
      token.kind === TokenKind.LParen ||
      token.kind === TokenKind.LBracket ||
      token.kind === TokenKind.LBrace ||
      token.kind === TokenKind.Backslash
    );
  }

  private isTypeStart(token: Token): boolean {
    return (
      token.kind === TokenKind.LowerIdentifier ||
      token.kind === TokenKind.UpperIdentifier ||
      token.kind === TokenKind.LParen ||
      token.kind === TokenKind.LBrace
    );
  }

  // ===== Token Stream Helpers =====
  // These methods manage the token stream and current position

  /**
   * Get current token without consuming it
   */
  private current(): Token {
    const token =
      this.tokens[this.index] ?? this.tokens[this.tokens.length - 1];
    if (!token) {
      throw new ParseError("No tokens available", {
        start: { offset: 0, line: 0, column: 0 },
        end: { offset: 0, line: 0, column: 0 },
      });
    }
    return token;
  }

  /**
   * Get span of current token
   */
  private currentSpan(): Span {
    return this.current().span;
  }

  /**
   * Get previous token (last consumed token)
   */
  private previous(): Token {
    const token = this.tokens[this.index - 1] ?? this.tokens[0];
    if (!token) {
      throw new ParseError("No previous token", {
        start: { offset: 0, line: 0, column: 0 },
        end: { offset: 0, line: 0, column: 0 },
      });
    }
    return token;
  }

  /**
   * Get span of previous token
   */
  private previousSpan(): Span {
    return this.previous().span;
  }

  /**
   * Consume and return current token, advancing position
   */
  private advance(): Token {
    const token = this.tokens[this.index];
    if (!token) {
      const fallback = this.tokens[this.tokens.length - 1];
      if (!fallback) {
        throw new ParseError("Advanced past end of token stream", {
          start: { offset: 0, line: 0, column: 0 },
          end: { offset: 0, line: 0, column: 0 },
        });
      }
      return fallback;
    }
    this.index += 1;
    return token;
  }

  /**
   * Check if we've reached end of file
   */
  private isAtEnd(): boolean {
    return this.peek(TokenKind.Eof);
  }

  /**
   * Check if token is on same line as span end
   */
  private onSameLine(span: Span, token: Token): boolean {
    return span.end.line === token.span.start.line;
  }

  /**
   * Look ahead in token stream without consuming
   */
  private peekAhead(offset: number): Token | undefined {
    return this.tokens[this.index + offset];
  }

  // ===== Layout Management Helpers =====
  // These manage the indentation stack for layout-sensitive parsing

  /**
   * Get current layout indentation level
   */
  private currentLayout(): number | undefined {
    return this.layoutStack[this.layoutStack.length - 1];
  }

  /**
   * Check if token is within current layout (properly indented)
   */
  private inCurrentLayout(token: Token): boolean {
    const layout = this.currentLayout();
    if (layout === undefined) return true;
    return token.span.start.column >= layout;
  }

  /**
   * Execute function within a new layout context
   * Layout is automatically popped when function returns
   */
  private withLayout<T>(indent: number, fn: () => T): T {
    this.layoutStack.push(indent);
    try {
      return fn();
    } finally {
      this.layoutStack.pop();
    }
  }

  /**
   * Check if next token continues current layout
   * A token continues layout if it's on the same line OR
   * on a new line with column >= baseIndent
   */
  private continuesLayout(
    baseIndent: number,
    lastEnd: Span["end"],
    next: Token
  ): boolean {
    // Same line always continues
    if (next.span.start.line === lastEnd.line) return true;

    // New line must be properly indented
    return next.span.start.column >= baseIndent;
  }

  /**
   * Create parse error with context
   */
  private error(label: string, token: Token): ParseError {
    return new ParseError(
      `Expected ${label} but found ${token.kind} '${token.lexeme}'`,
      token.span
    );
  }
  /**
   * Operator precedence and associativity information
   * Matches the precedence table in docs/grammar.ebnf
   */
}

type Associativity = "left" | "right";

/**
 * Operator precedence table
 * Higher precedence binds tighter
 *
 * Precedence levels (as documented in grammar.ebnf):
 * 1: |>, <|              (application operators)
 * 2: ||                  (logical or)
 * 3: &&                  (logical and)
 * 4: ==, /=, <, <=, >, >= (comparison)
 * 5: ::, ++              (cons, append) - right associative
 * 6: +, -                (addition, subtraction)
 * 7: *, /, //, %         (multiplication, division, modulo)
 * 8: ^                   (exponentiation) - right associative
 * 9: <<, >>              (composition) - right associative
 */
const OP_INFO: Record<
  string,
  { precedence: number; associativity: Associativity }
> = {
  "||": { precedence: 2, associativity: "left" },
  "&&": { precedence: 3, associativity: "left" },
  "==": { precedence: 4, associativity: "left" },
  "/=": { precedence: 4, associativity: "left" },
  "<": { precedence: 4, associativity: "left" },
  "<=": { precedence: 4, associativity: "left" },
  ">": { precedence: 4, associativity: "left" },
  ">=": { precedence: 4, associativity: "left" },
  "::": { precedence: 5, associativity: "right" },
  "++": { precedence: 5, associativity: "right" },
  "|>": { precedence: 1, associativity: "left" },
  "<|": { precedence: 1, associativity: "right" },
  "<<": { precedence: 9, associativity: "right" },
  ">>": { precedence: 9, associativity: "right" },
  "+": { precedence: 6, associativity: "left" },
  "-": { precedence: 6, associativity: "left" },
  "*": { precedence: 7, associativity: "left" },
  "/": { precedence: 7, associativity: "left" },
  "//": { precedence: 7, associativity: "left" },
  "%": { precedence: 7, associativity: "left" },
  "^": { precedence: 8, associativity: "right" },
};

// Default precedence for custom operators: 5, left-associative
const DEFAULT_OP_INFO = { precedence: 5, associativity: "left" as const };

/**
 * Get operator precedence and associativity
 * Returns default for custom operators not in table
 */
function getOperatorInfo(op: string): {
  precedence: number;
  associativity: Associativity;
} {
  return OP_INFO[op] ?? DEFAULT_OP_INFO;
}
