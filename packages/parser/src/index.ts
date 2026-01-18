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
  type ExportSpec,
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
  type OpaqueTypeDeclaration,
  type ConstructorVariant,
  type ProtocolDeclaration,
  type ProtocolMethod,
  type ImplementationDeclaration,
  type Constraint,
  type MethodImplementation,
  type InfixDeclaration,
  type OperatorRegistry,
  type RecordFieldType,
} from "@vibe/syntax";
import {
  buildRegistryFromTokens,
  getOperatorInfo as getOperatorInfoFromRegistry,
  InfixParseError,
  BUILTIN_OPERATOR_REGISTRY,
  mergeRegistries,
} from "./operator-registry";

// Re-export BUILTIN_OPERATOR_REGISTRY for use by other packages
export { BUILTIN_OPERATOR_REGISTRY } from "./operator-registry";

/**
 * Error thrown during parsing when unexpected syntax is encountered
 */
export class ParseError extends Error {
  constructor(
    message: string,
    public readonly span: Span,
  ) {
    super(message);
  }
}

/**
 * Main entry point: Parse Vibe source code into an AST
 *
 * Grammar: Program = [ModuleDeclaration], {ImportDeclaration}, {Declaration}
 *
 * @param source - Vibe source code as a string
 * @param operatorRegistry - Optional registry of custom operator declarations
 * @returns Abstract Syntax Tree representing the program
 */
export function parse(
  source: string,
  operatorRegistry?: OperatorRegistry,
): Program {
  // Step 1: Tokenize the source code
  const tokens = lex(source);

  // Step 2: Create parser instance with token stream and operator registry
  const parser = new Parser(tokens, operatorRegistry);

  // Step 3: Parse tokens into AST
  return parser.parseProgram();
}

/**
 * Pre-process source to extract infix declarations.
 * This is used to build an operator registry before the main parsing pass,
 * enabling correct precedence and associativity for custom operators.
 *
 * @param source - Vibe source code as a string
 * @returns Registry of operator precedence/associativity declarations
 */
export function collectInfixDeclarations(source: string): {
  registry: OperatorRegistry;
  declarations: InfixDeclaration[];
  errors: ParseError[];
} {
  const tokens = lex(source);
  const result = buildRegistryFromTokens(tokens);

  // Convert InfixParseError to ParseError for backward compatibility
  const errors = result.errors.map((e) => new ParseError(e.message, e.span));

  return {
    registry: result.registry,
    declarations: result.declarations,
    errors,
  };
}

/**
 * Parse source with automatic infix declaration pre-processing.
 * This is a convenience function that combines collectInfixDeclarations and parse.
 *
 * @param source - Vibe source code as a string
 * @returns Program AST with infix declarations properly handled
 */
export function parseWithInfix(source: string): {
  program: Program;
  operatorRegistry: OperatorRegistry;
  infixErrors: ParseError[];
} {
  const { registry, errors } = collectInfixDeclarations(source);
  // Merge with builtin operators to ensure && and || have correct precedence
  const mergedRegistry = mergeRegistries(BUILTIN_OPERATOR_REGISTRY, registry);
  const program = parse(source, mergedRegistry);
  return { program, operatorRegistry: mergedRegistry, infixErrors: errors };
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

  /** Custom operator registry for user-defined precedence/associativity */
  private operatorRegistry: OperatorRegistry;

  constructor(
    private readonly tokens: Token[],
    operatorRegistry?: OperatorRegistry,
  ) {
    // Start with builtin operators (&&, ||) and merge any provided registry on top
    this.operatorRegistry = operatorRegistry
      ? mergeRegistries(BUILTIN_OPERATOR_REGISTRY, operatorRegistry)
      : BUILTIN_OPERATOR_REGISTRY;
  }

  /**
   * Get operator precedence and associativity.
   * Looks up the operator in the provided registry, falling back to defaults.
   *
   * All standard operators should be declared via infix declarations in Vibe.vibe.
   * For unknown operators without declarations, we use a default of precedence 9
   * and left associativity - but this should only occur for operators in modules
   * that don't properly import their dependencies.
   */
  private getOperatorInfo(op: string): {
    precedence: number;
    associativity: "left" | "right" | "none";
  } {
    return getOperatorInfoFromRegistry(this.operatorRegistry, op);
  }

  /**
   * Parse complete program: module header (required), imports, and declarations
   *
   * Grammar: Program = ModuleDeclaration, {ImportDeclaration}, {Declaration}
   */
  parseProgram(): Program {
    // Parse module declaration (required - every Vibe file must start with one)
    if (!this.peekKeyword("module")) {
      const currentToken = this.current();
      throw new ParseError(
        `Every Vibe file must begin with a module declaration.\n` +
          `Expected: module <ModuleName> exposing (..)`,
        currentToken.span,
      );
    }
    const module = this.parseModuleDeclaration();

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
   * Grammar:
   *   Exposing = "(", "..", ")" | "(", ExportSpecList, ")"
   *   ExportSpec = LowerIdentifier                        -- value/function
   *              | "(", Operator, ")"                     -- operator
   *              | UpperIdentifier                        -- opaque type/protocol
   *              | UpperIdentifier, "(", "..", ")"        -- type/protocol with all constructors/methods
   *              | UpperIdentifier, "(", IdentList, ")"   -- type/protocol with specific members
   *
   * Examples:
   *   (..)                              - expose all
   *   (foo, bar)                        - expose values foo and bar
   *   (Maybe(..), Result(Ok, Err))      - expose Maybe with all ctors, Result with Ok/Err
   *   (Num(..), Eq(==))                 - expose Num with all methods, Eq with == only
   *   ((++), (<$>))                     - expose operators
   */
  private parseExposing(): Exposing {
    // Consume opening parenthesis
    const openParen = this.expect(TokenKind.LParen, "exposing list start");
    const start = openParen.span.start;

    // Check if exposing all (..)
    if (this.match(TokenKind.Range)) {
      const end = this.expect(TokenKind.RParen, "close exposing (..)").span.end;
      return { kind: "All", span: { start, end } };
    }

    // Parse explicit list of export specs
    const exports: ExportSpec[] = [];

    // Parse comma-separated export spec list
    while (true) {
      const spec = this.parseExportSpec();
      exports.push(spec);

      // Continue if comma found, otherwise break
      if (this.match(TokenKind.Comma)) {
        continue;
      }
      break;
    }

    // Consume closing parenthesis
    const end = this.expect(TokenKind.RParen, "close exposing list").span.end;
    return { kind: "Explicit", exports, span: { start, end } };
  }

  /**
   * Parse a single export specification
   *
   * Examples:
   *   foo                 -> ExportValue "foo"
   *   (++)                -> ExportOperator "++"
   *   Maybe               -> ExportValue "Maybe"
   *   Maybe(..)           -> ExportTypeAll "Maybe"
   *   Maybe(Just,Nothing) -> ExportTypeSome "Maybe" ["Just", "Nothing"]
   *   Num(..)             -> ExportTypeAll "Num"
   *   Num(+, -)           -> ExportTypeSome "Num" ["+", "-"]
   */
  private parseExportSpec(): ExportSpec {
    const current = this.current();

    // Check for operator export: (++)
    if (current.kind === TokenKind.LParen) {
      const start = current.span.start;
      this.advance(); // consume (

      // Expect an operator inside
      const opToken = this.expect(TokenKind.Operator, "operator in export");
      const operator = opToken.lexeme;

      const end = this.expect(
        TokenKind.RParen,
        "closing paren for operator export",
      ).span.end;

      return { kind: "ExportOperator", operator, span: { start, end } };
    }

    // Check for lower identifier (simple value export)
    if (current.kind === TokenKind.LowerIdentifier) {
      this.advance();
      return {
        kind: "ExportValue",
        name: current.lexeme,
        span: current.span,
      };
    }

    // Check for upper identifier (type, protocol, or constructor)
    if (current.kind === TokenKind.UpperIdentifier) {
      const nameToken = this.advance();
      const name = nameToken.lexeme;
      const start = nameToken.span.start;

      // Check if followed by (..) or (Con1, Con2)
      if (this.peek(TokenKind.LParen)) {
        this.advance(); // consume (

        // Check for (..)
        if (this.match(TokenKind.Range)) {
          const end = this.expect(
            TokenKind.RParen,
            "closing paren for type/protocol export",
          ).span.end;
          return { kind: "ExportTypeAll", name, span: { start, end } };
        }

        // Parse list of constructor/method names
        const members: string[] = [];
        while (true) {
          const memberCurrent = this.current();

          // Members can be:
          // - Upper identifiers (constructors like Just, Nothing)
          // - Lower identifiers (methods like map, filter)
          // - Operators in parens (methods like (+), (==))
          if (memberCurrent.kind === TokenKind.LParen) {
            this.advance(); // consume (
            const opToken = this.expect(
              TokenKind.Operator,
              "operator in member list",
            );
            members.push(opToken.lexeme);
            this.expect(TokenKind.RParen, "closing paren for operator member");
          } else if (
            memberCurrent.kind === TokenKind.UpperIdentifier ||
            memberCurrent.kind === TokenKind.LowerIdentifier
          ) {
            members.push(memberCurrent.lexeme);
            this.advance();
          } else {
            throw new ParseError(
              `Expected constructor name, method name, or operator in export list, got ${memberCurrent.kind}`,
              memberCurrent.span,
            );
          }

          // Continue if comma found
          if (this.match(TokenKind.Comma)) {
            continue;
          }
          break;
        }

        const end = this.expect(
          TokenKind.RParen,
          "closing paren for type/protocol export",
        ).span.end;
        return { kind: "ExportTypeSome", name, members, span: { start, end } };
      }

      // Just the type/protocol name without members - treated as ExportValue
      // (opaque type export or protocol without methods)
      return { kind: "ExportValue", name, span: nameToken.span };
    }

    throw new ParseError(
      `Expected export specification, got ${current.kind}`,
      current.span,
    );
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

    // Check for protocol declaration (protocol ...)
    if (this.peekKeyword("protocol")) {
      return this.parseProtocolDeclaration();
    }

    // Check for instance declaration (implement ...)
    if (this.peekKeyword("implement")) {
      return this.parseImplementationDeclaration();
    }

    // Check for infix declaration (infix, infixl, infixr)
    if (
      this.peekKeyword("infix") ||
      this.peekKeyword("infixl") ||
      this.peekKeyword("infixr")
    ) {
      return this.parseInfixDeclaration();
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
    // Uses shared parseMethodBody for consistency with protocol/implementation methods
    const { args, body } = this.parseMethodBody();
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
   * Parse type declaration, type alias, or opaque type
   *
   * Grammar:
   *   TypeDeclaration = "type", UpperIdentifier, {LowerIdentifier}, "=", ConstructorDef, {"|", ConstructorDef}
   *   OpaqueTypeDeclaration = "type", UpperIdentifier, {LowerIdentifier}  (no "=")
   *   TypeAliasDeclaration = "type", "alias", UpperIdentifier, {LowerIdentifier}, "=", TypeExpr
   *
   * Examples:
   *   type Bool = True | False            (ADT)
   *   type Maybe a = Just a | Nothing     (ADT)
   *   type Promise a                      (Opaque - for JS interop)
   *   type alias Point = { x : Int, y : Int } (Record - requires 'alias')
   *   type alias UserId = Int             (Type alias)
   */
  private parseTypeOrAliasDeclaration():
    | TypeDeclaration
    | TypeAliasDeclaration
    | OpaqueTypeDeclaration {
    // Consume "type" keyword
    const typeToken = this.expectKeyword("type");

    // Check if this is a type alias (type alias ...)
    if (this.peekKeyword("alias")) {
      return this.parseTypeAliasDeclaration(typeToken);
    }

    // Try to parse constraints (optional context)
    const constraints: Constraint[] = [];

    // Check if we have constraints (look ahead for =>)
    if (this.peekConstraintContextInTypeDecl()) {
      // Parse constraint(s) before =>
      if (this.match(TokenKind.LParen)) {
        // Multiple constraints: (Eq a, Show a) =>
        do {
          constraints.push(this.parseConstraint());
        } while (this.match(TokenKind.Comma));

        this.expect(TokenKind.RParen, "close constraint list");
      } else {
        // Single constraint: Eq a =>
        constraints.push(this.parseConstraint());
      }

      // Consume =>
      this.expectOperator("=>");
    }

    // Parse type name (must be uppercase)
    const nameToken = this.expect(TokenKind.UpperIdentifier, "type name");
    const name = nameToken.lexeme;

    // Track the base column for layout-sensitive parsing
    const typeColumn = typeToken.span.start.column;
    const typeLine = typeToken.span.start.line;

    // Parse type parameters (zero or more lowercase identifiers)
    // Type parameters must be on the same line as the type declaration,
    // or on subsequent lines indented more than the "type" keyword.
    // This ensures we don't accidentally consume the next declaration.
    const params: string[] = [];
    while (this.peek(TokenKind.LowerIdentifier)) {
      const nextToken = this.current();
      const nextLine = nextToken.span.start.line;
      const nextColumn = nextToken.span.start.column;

      // If on the same line as "type", it's a type parameter
      // If on a different line, it must be indented more than "type"
      if (nextLine !== typeLine && nextColumn <= typeColumn) {
        // This identifier is at a new declaration boundary, stop parsing params
        break;
      }

      const param = this.expect(TokenKind.LowerIdentifier, "type parameter");
      params.push(param.lexeme);
    }

    // Check for Opaque type (no equals sign)
    // Opaque types end when we see something that's not part of the type params
    if (!this.peek(TokenKind.Equals)) {
      if (constraints.length > 0) {
        throw new ParseError(
          "Opaque types cannot have constraints",
          { start: typeToken.span.start, end: this.previousSpan().end }
        );
      }
      // This is an opaque type: type Name params
      const lastToken =
        params.length > 0 ? this.tokens[this.index - 1]! : nameToken;
      const span: Span = {
        start: typeToken.span.start,
        end: lastToken.span.end,
      };

      return {
        kind: "OpaqueTypeDeclaration",
        name,
        params,
        span,
      } satisfies OpaqueTypeDeclaration;
    }

    // Consume equals sign
    this.expect(TokenKind.Equals, "type definition");

    // Check if this is a record type (= { ... })
    if (this.peek(TokenKind.LBrace)) {
      const recordFields = this.parseRecordTypeFields();
      return {
        kind: "TypeDeclaration",
        name,
        params,
        constraints,
        recordFields,
        span: { start: typeToken.span.start, end: this.previousSpan().end },
      };
    }

    // Parse ADT - constructor variants separated by pipe (|)
    const constructors: ConstructorVariant[] = [];
    constructors.push(this.parseConstructorVariant());

    // Parse additional variants separated by |
    while (this.match(TokenKind.Pipe)) {
      constructors.push(this.parseConstructorVariant());
    }

    // Check for optional 'implementing' clause
    // Syntax: implementing Protocol1, Protocol2, ...
    let implementing: string[] | undefined = undefined;
    let endSpan = constructors[constructors.length - 1]!.span.end;

    if (this.peekKeyword("implementing")) {
      this.advance(); // consume 'implementing'
      implementing = [];

      // Parse comma-separated list of protocol names
      const protocolName = this.expect(
        TokenKind.UpperIdentifier,
        "protocol name after 'implementing'",
      );
      implementing.push(protocolName.lexeme);
      endSpan = protocolName.span.end;

      while (this.match(TokenKind.Comma)) {
        const nextProtocol = this.expect(
          TokenKind.UpperIdentifier,
          "protocol name",
        );
        implementing.push(nextProtocol.lexeme);
        endSpan = nextProtocol.span.end;
      }
    }

    // Calculate span from "type" to end of last constructor or implementing clause
    const span: Span = {
      start: typeToken.span.start,
      end: endSpan,
    };

    return {
      kind: "TypeDeclaration",
      name,
      params,
      constraints,
      constructors,
      implementing,
      span,
    } satisfies TypeDeclaration;
  }

  /**
   * Check if we have a constraint context in a type declaration (looks ahead for =>)
   * The => must appear before the equals sign or end of declaration (for opaque types).
   */
  private peekConstraintContextInTypeDecl(): boolean {
    // Look ahead to find => operator
    let i = 0;
    let parenDepth = 0;

    while (this.peekAhead(i)) {
      const tok = this.peekAhead(i);
      if (!tok) break;

      // Track parentheses depth
      if (tok.kind === TokenKind.LParen) parenDepth++;
      if (tok.kind === TokenKind.RParen) parenDepth--;

      // Found => at top level - we have constraints
      if (
        parenDepth === 0 &&
        tok.kind === TokenKind.Operator &&
        tok.lexeme === "=>"
      ) {
        return true;
      }

      // Stop looking if we hit '=' - valid boundary for type def
      if (tok.kind === TokenKind.Equals && parenDepth === 0) {
        return false;
      }

      // Stop looking if we hit newline at top level (end of opaque type)
      // Actually parser doesn't see newlines easily in token stream usually, 
      // but we can check if we hit another keyword that starts a decl?
      // For now, looking for => within reasonable distance is enough.
      
      i++;

      // Don't look too far ahead
      if (i > 50) break;
    }

    return false;
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
      "constructor name",
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
   * Note: Record types now require the 'type alias' syntax.
   *
   * Examples:
   *   type alias UserId = Int
   *   type alias Pair a b = (a, b)
   *   type alias Handler msg = msg -> Model -> Model
   *   type alias Point = { x : Int, y : Int }
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
   * Parse protocol declaration
   *
   * Grammar: protocol UpperIdentifier {LowerIdentifier} where {MethodSignature}
   *
   * MethodSignature:
   *   - Required method: name : Type
   *   - Method with default: name : Type, followed by name args = expr
   *
   * Example:
   *   protocol Num a where
   *     plus : a -> a -> a
   *     minus : a -> a -> a
   *     times : a -> a -> a
   *
   *   protocol Eq a where
   *     eq : a -> a -> Bool
   *     neq : a -> a -> Bool
   *     neq x y = not (eq x y)
   */
  private parseProtocolDeclaration(): ProtocolDeclaration {
    // Consume "protocol" keyword
    const protocolToken = this.expectKeyword("protocol");

    // Try to parse constraints (optional superclass context)
    const constraints: Constraint[] = [];

    // Check if we have constraints (look ahead for =>)
    const hasConstraints = this.peekConstraintContextInProtocol();

    if (hasConstraints) {
      // Parse constraint(s) before =>
      if (this.match(TokenKind.LParen)) {
        // Multiple constraints: (Eq a, Show a) =>
        do {
          constraints.push(this.parseConstraint());
        } while (this.match(TokenKind.Comma));

        this.expect(TokenKind.RParen, "close constraint list");
      } else {
        // Single constraint: Eq a =>
        constraints.push(this.parseConstraint());
      }

      // Consume =>
      this.expectOperator("=>");
    }

    // Parse protocol name (must be uppercase)
    const nameToken = this.expect(TokenKind.UpperIdentifier, "protocol name");
    const name = nameToken.lexeme;

    // Parse type parameters (typically one, but support multiple)
    const params: string[] = [];
    while (this.peek(TokenKind.LowerIdentifier)) {
      const param = this.expect(TokenKind.LowerIdentifier, "type parameter");
      params.push(param.lexeme);
    }

    // Consume "where" keyword
    this.expectKeyword("where");

    // Parse method signatures with optional default implementations (indented block)
    const methods: ProtocolMethod[] = [];

    // Get base indentation from first method
    // Methods can be either LowerIdentifier or (Operator)
    if (!this.peek(TokenKind.LowerIdentifier) && !this.peek(TokenKind.LParen)) {
      throw new ParseError(
        "Expected at least one method signature in protocol",
        this.currentSpan(),
      );
    }

    const firstMethodStart = this.current().span.start;
    const baseIndent = firstMethodStart.column;

    // Parse all method signatures (and optionally their default implementations)
    while (
      this.peek(TokenKind.LowerIdentifier) ||
      this.peek(TokenKind.LParen)
    ) {
      const methodToken = this.current();

      // Check indentation
      if (methodToken.span.start.column < baseIndent) {
        break;
      }

      // Parse method name - either identifier or operator in parens
      let methodName: string;
      const methodStart = methodToken.span.start;

      if (this.match(TokenKind.LParen)) {
        // Operator method: (==), (+), etc.
        const opToken = this.expect(
          TokenKind.Operator,
          "operator in protocol method",
        );
        methodName = opToken.lexeme;
        this.expect(TokenKind.RParen, "close paren after operator");
      } else {
        // Regular method name
        this.advance();
        methodName = methodToken.lexeme;
      }

      // Check if there's a type annotation (: Type) or just a default implementation
      let methodType: TypeExpr | undefined = undefined;
      let defaultImpl: ProtocolMethod["defaultImpl"] = undefined;

      if (this.peek(TokenKind.Colon)) {
        // Has explicit type annotation
        this.advance(); // consume colon

        // Parse method type
        methodType = this.parseTypeExpression();

        // Check if this method has a default implementation on the next line
        // The default implementation has the form: methodName args = expr
        // and must be at the same indentation level as the method signature
        if (
          (this.peek(TokenKind.LowerIdentifier) ||
            this.peek(TokenKind.LParen)) &&
          this.current().span.start.column === baseIndent
        ) {
          // Save position to potentially backtrack
          const savedIndex = this.index;

          // Try to parse a default implementation
          const nextToken = this.current();
          let nextMethodName: string | null = null;

          if (this.peek(TokenKind.LParen)) {
            // Operator method: (==) x y = ...
            this.advance(); // consume (
            if (this.peek(TokenKind.Operator)) {
              nextMethodName = this.current().lexeme;
              this.advance(); // consume operator
              if (this.peek(TokenKind.RParen)) {
                this.advance(); // consume )
              } else {
                // Not a valid default impl start, backtrack
                this.index = savedIndex;
                nextMethodName = null;
              }
            } else {
              this.index = savedIndex;
            }
          } else if (this.peek(TokenKind.LowerIdentifier)) {
            // Check if it's the same method name
            if (nextToken.lexeme === methodName) {
              nextMethodName = nextToken.lexeme;
              this.advance(); // consume method name
            }
          }

          // If we matched the method name, check for arguments and =
          if (nextMethodName === methodName) {
            // Parse pattern arguments (if any)
            // Use isFunctionParamPatternStart for consistency with standard functions
            const args: Pattern[] = [];
            while (this.isFunctionParamPatternStart(this.current())) {
              args.push(this.parseFunctionParamPattern());
            }

            // Check for equals sign
            if (this.peek(TokenKind.Equals)) {
              this.advance(); // consume =
              // Parse the default implementation expression
              const body = this.parseExpression();
              defaultImpl = { args, body };
            } else {
              // Not a default implementation, backtrack
              this.index = savedIndex;
            }
          } else if (nextMethodName !== null) {
            // Different method name, backtrack
            this.index = savedIndex;
          }
        }
      } else if (
        this.isFunctionParamPatternStart(this.current()) ||
        this.peek(TokenKind.Equals)
      ) {
        // No type annotation, must have default implementation
        // Uses shared parseMethodBody for consistency
        const { args, body } = this.parseMethodBody();
        defaultImpl = { args, body };
      } else {
        throw new ParseError(
          `Protocol method '${methodName}' must have either a type annotation or a default implementation`,
          this.currentSpan(),
        );
      }

      const methodSpan: Span = {
        start: methodStart,
        end:
          defaultImpl?.body.span.end ??
          methodType?.span.end ??
          this.current().span.end,
      };

      methods.push({
        name: methodName,
        type: methodType,
        defaultImpl,
        span: methodSpan,
      });
    }

    const lastMethod = methods[methods.length - 1]!;
    const span: Span = {
      start: protocolToken.span.start,
      end: lastMethod.span.end,
    };

    return {
      kind: "ProtocolDeclaration",
      name,
      params,
      constraints,
      methods,
      span,
    } satisfies ProtocolDeclaration;
  }

  /**
   * Parse instance declaration
   *
   * Grammar: instance [Constraints =>] UpperIdentifier TypeExpr {TypeExpr} where {LowerIdentifier = Expr}
   *
   * Examples:
   *   instance Num Int where
   *     plus = intPlusImpl
   *     minus = intMinusImpl
   *
   *   implement Show a => Show (List a) where
   *     show = showListImpl
   */
  private parseImplementationDeclaration(): ImplementationDeclaration {
    // Consume "implement" keyword
    const implementToken = this.expectKeyword("implement");

    // Try to parse constraints (optional context)
    const constraints: Constraint[] = [];

    // Check if we have constraints (look ahead for =>)
    const hasConstraints = this.peekConstraintContext();

    if (hasConstraints) {
      // Parse constraint(s) before =>
      if (this.match(TokenKind.LParen)) {
        // Multiple constraints: (Num a, Show a) =>
        do {
          constraints.push(this.parseConstraint());
        } while (this.match(TokenKind.Comma));

        this.expect(TokenKind.RParen, "close constraint list");
      } else {
        // Single constraint: Num a =>
        constraints.push(this.parseConstraint());
      }

      // Consume =>
      this.expectOperator("=>");
    }

    // Parse protocol name
    const protocolNameToken = this.expect(
      TokenKind.UpperIdentifier,
      "protocol name in implementation",
    );
    const protocolName = protocolNameToken.lexeme;

    // Parse type arguments (the concrete type(s) for this implementation)
    // Type arguments are optional - zero type args means a nullary protocol implementation
    // We use parseTypeAtom for each argument so that "Convertible Float Int" parses as
    // [Float, Int] rather than [Float Int] (which would be Float applied to Int).
    const typeArgs: TypeExpr[] = [];
    while (this.isTypeStart(this.current())) {
      typeArgs.push(this.parseTypeAtom());
    }

    // Consume "where" keyword
    this.expectKeyword("where");

    // Parse method implementations (indented block)
    const methods: MethodImplementation[] = [];

    // Methods can be either LowerIdentifier or (Operator)
    if (!this.peek(TokenKind.LowerIdentifier) && !this.peek(TokenKind.LParen)) {
      throw new ParseError(
        "Expected at least one method implementation in implementation",
        this.currentSpan(),
      );
    }

    const firstMethodStart = this.current().span.start;
    const baseIndent = firstMethodStart.column;

    // Parse all method implementations
    while (
      this.peek(TokenKind.LowerIdentifier) ||
      this.peek(TokenKind.LParen)
    ) {
      const methodToken = this.current();

      // Check indentation
      if (methodToken.span.start.column < baseIndent) {
        break;
      }

      // Parse method name using shared parseDeclarationName
      // This handles both regular identifiers and operators in parens
      const { name: methodName, span: nameSpan } = this.parseDeclarationName();

      // Parse method body using shared parseMethodBody
      // This ensures consistent pattern parsing with standard functions
      const { args, body: implementation } = this.parseMethodBody();

      methods.push({
        name: methodName,
        args: args.length > 0 ? args : undefined,
        implementation,
        span: { start: nameSpan.start, end: implementation.span.end },
      });
    }

    const lastMethod = methods[methods.length - 1]!;
    const span: Span = {
      start: implementToken.span.start,
      end: lastMethod.span.end,
    };

    return {
      kind: "ImplementationDeclaration",
      constraints,
      protocolName,
      typeArgs,
      methods,
      span,
    } satisfies ImplementationDeclaration;
  }

  /**
   * Parse infix declaration (operator precedence and associativity)
   *
   * Grammar: InfixDeclaration = ("infix" | "infixl" | "infixr"), Number, Operator
   *
   * Examples:
   *   infixl 6 +     -- left-associative, precedence 6
   *   infixr 5 ++    -- right-associative, precedence 5
   *   infix 4 ==     -- non-associative, precedence 4
   */
  private parseInfixDeclaration(): InfixDeclaration {
    const start = this.currentSpan().start;

    // Parse fixity keyword (infix, infixl, or infixr)
    const fixityToken = this.current();
    if (
      fixityToken.kind !== TokenKind.Keyword ||
      (fixityToken.lexeme !== "infix" &&
        fixityToken.lexeme !== "infixl" &&
        fixityToken.lexeme !== "infixr")
    ) {
      throw new ParseError(
        "Expected 'infix', 'infixl', or 'infixr'",
        this.currentSpan(),
      );
    }
    const fixity = fixityToken.lexeme as "infix" | "infixl" | "infixr";
    this.advance();

    // Parse precedence number (1-9)
    const precedenceToken = this.expect(TokenKind.Number, "precedence number");
    const precedence = parseInt(precedenceToken.lexeme, 10);

    if (precedence < 0 || precedence > 9) {
      throw new ParseError(
        `Precedence must be between 0 and 9, got ${precedence}`,
        precedenceToken.span,
      );
    }

    // Parse operator - either as bare operator or in parentheses
    let operator: string;
    let end: Span["end"];

    if (this.match(TokenKind.LParen)) {
      // Operator in parens: infixl 6 (+)
      const opToken = this.expect(TokenKind.Operator, "operator");
      operator = opToken.lexeme;
      end = this.expect(TokenKind.RParen, "close paren after operator").span
        .end;
    } else if (this.peek(TokenKind.Operator)) {
      // Bare operator: infixl 6 +
      const opToken = this.expect(TokenKind.Operator, "operator");
      operator = opToken.lexeme;
      end = opToken.span.end;
    } else {
      throw new ParseError(
        "Expected operator after precedence in infix declaration",
        this.currentSpan(),
      );
    }

    return {
      kind: "InfixDeclaration",
      fixity,
      precedence,
      operator,
      span: { start, end },
    } satisfies InfixDeclaration;
  }

  /**
   * Parse a single constraint (e.g., "Num a" or "Show (List a)")
   */
  private parseConstraint(): Constraint {
    const start = this.currentSpan().start;

    // Parse protocol name
    const protocolToken = this.expect(
      TokenKind.UpperIdentifier,
      "protocol name in constraint",
    );
    const protocolName = protocolToken.lexeme;

    // Parse type arguments
    const typeArgs: TypeExpr[] = [];
    while (this.isTypeStart(this.current())) {
      typeArgs.push(this.parseTypeTerm());
    }

    if (typeArgs.length === 0) {
      throw new ParseError(
        "Expected at least one type argument in constraint",
        this.currentSpan(),
      );
    }

    const end = typeArgs[typeArgs.length - 1]!.span.end;

    return {
      protocolName,
      typeArgs,
      span: { start, end },
    };
  }

  /**
   * Check if we have a constraint context (looks ahead for =>)
   */
  private peekConstraintContext(): boolean {
    // Look ahead to find => operator
    let i = 0;
    while (this.peekAhead(i)) {
      const tok = this.peekAhead(i);
      if (!tok) break;

      // Found => - we have constraints
      if (tok.kind === TokenKind.Operator && tok.lexeme === "=>") {
        return true;
      }

      // Stop looking if we hit 'where' - no constraints
      if (tok.kind === TokenKind.Keyword && tok.lexeme === "where") {
        return false;
      }

      i++;

      // Don't look too far ahead
      if (i > 20) break;
    }

    return false;
  }

  /**
   * Check if we have a constraint context in a protocol declaration (looks ahead for =>)
   * Similar to peekConstraintContext but specifically for protocol declarations.
   * The => must appear before the protocol name (UpperIdentifier followed by 'where').
   */
  private peekConstraintContextInProtocol(): boolean {
    // Look ahead to find => operator before we hit the protocol name + 'where'
    let i = 0;
    let parenDepth = 0;

    while (this.peekAhead(i)) {
      const tok = this.peekAhead(i);
      if (!tok) break;

      // Track parentheses depth for nested constraints like (Eq a, Show a)
      if (tok.kind === TokenKind.LParen) parenDepth++;
      if (tok.kind === TokenKind.RParen) parenDepth--;

      // Found => at top level - we have constraints
      if (
        parenDepth === 0 &&
        tok.kind === TokenKind.Operator &&
        tok.lexeme === "=>"
      ) {
        return true;
      }

      // Stop looking if we hit 'where' - no constraints before protocol name
      if (tok.kind === TokenKind.Keyword && tok.lexeme === "where") {
        return false;
      }

      i++;

      // Don't look too far ahead
      if (i > 30) break;
    }

    return false;
  }

  /**
   * Parse type expression (entry point)
   *
   * Grammar: TypeExpr = [Constraints =>] TypeArrow
   *
   * Supports qualified types like: Num a => a -> a -> a
   */
  private parseTypeExpression(): TypeExpr {
    const start = this.currentSpan().start;

    // Check if we have constraints (look ahead for =>)
    const hasConstraints = this.peekConstraintContextInType();

    if (hasConstraints) {
      const constraints: Constraint[] = [];

      // Parse constraint(s) before =>
      if (this.match(TokenKind.LParen)) {
        // Multiple constraints: (Num a, Show a) => Type
        do {
          constraints.push(this.parseConstraint());
        } while (this.match(TokenKind.Comma));

        this.expect(TokenKind.RParen, "close constraint list");
      } else {
        // Single constraint: Num a => Type
        constraints.push(this.parseConstraint());
      }

      // Consume =>
      this.expectOperator("=>");

      // Parse the underlying type
      const type = this.parseTypeArrow();

      return {
        kind: "QualifiedType",
        constraints,
        type,
        span: { start, end: type.span.end },
      };
    }

    return this.parseTypeArrow();
  }

  /**
   * Check if we have a constraint context in a type signature (looks ahead for =>)
   * The => must be on the same line as the start of the type expression.
   */
  private peekConstraintContextInType(): boolean {
    // Look ahead to find => operator
    let i = 0;
    let parenDepth = 0;

    // Get the current line to ensure we don't cross line boundaries
    const startLine = this.current().span.start.line;

    while (this.peekAhead(i)) {
      const tok = this.peekAhead(i);
      if (!tok) break;

      // Stop looking if we've moved to a different line (constraint must be on same line)
      if (tok.span.start.line !== startLine) {
        return false;
      }

      // Track parentheses depth
      if (tok.kind === TokenKind.LParen) parenDepth++;
      if (tok.kind === TokenKind.RParen) parenDepth--;

      // Found => at top level - we have constraints
      if (
        parenDepth === 0 &&
        tok.kind === TokenKind.Operator &&
        tok.lexeme === "=>"
      ) {
        return true;
      }

      // Stop looking if we hit certain tokens that indicate no constraints
      if (parenDepth === 0) {
        if (tok.kind === TokenKind.Equals) return false;
        if (tok.kind === TokenKind.Keyword) return false;
      }

      i++;

      // Don't look too far ahead
      if (i > 30) break;
    }

    return false;
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
            const fields: Array<{ name: string; type: TypeExpr; span: Span }> = [];

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
        "record field name",
      );
      this.expect(TokenKind.Colon, "':' after field name");
      const firstType = this.parseTypeExpression();
      fields.push({
        name: firstName.lexeme,
        type: firstType,
        span: { start: firstName.span.start, end: firstType.span.end },
      });

      const baseIndent = firstName.span.start.column;
      let lastEnd = firstType.span.end;

      // Parse subsequent fields
      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
        this.advance(); // consume comma

        const fieldName = this.expect(
          TokenKind.LowerIdentifier,
          "record field name",
        );
        this.expect(TokenKind.Colon, "':' after field name");
        const fieldType = this.parseTypeExpression();
        fields.push({
          name: fieldName.lexeme,
          type: fieldType,
          span: { start: fieldName.span.start, end: fieldType.span.end },
        });
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

      // Check for empty tuple () which is syntactic sugar for Unit
      if (this.match(TokenKind.RParen)) {
        const end = this.previousSpan().end;
        return {
          kind: "TypeRef",
          name: "Unit",
          args: [],
          span: { start, end },
        };
      }

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
      const arg = this.parseTypeAtom();
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
   * Parse atomic type expression (identifier, parenthesized type, record, or tuple)
   * Used for type application arguments to avoid requiring parentheses.
   *
   * This allows writing: Pair Int Int
   * Instead of requiring: Pair (Int) (Int)
   */
  private parseTypeAtom(): TypeExpr {
    // Parse record type { field1 : Type1, field2 : Type2, ... }
    if (this.match(TokenKind.LBrace)) {
      const start = this.previousSpan().start;
      const fields: Array<{ name: string; type: TypeExpr; span: Span }> = [];

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
        "record field name",
      );
      this.expect(TokenKind.Colon, "':' after field name");
      const firstType = this.parseTypeExpression();
      fields.push({
        name: firstName.lexeme,
        type: firstType,
        span: { start: firstName.span.start, end: firstType.span.end },
      });

      const baseIndent = firstName.span.start.column;
      let lastEnd = firstType.span.end;

      // Parse subsequent fields
      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
        this.advance(); // consume comma

        const fieldName = this.expect(
          TokenKind.LowerIdentifier,
          "record field name",
        );
        this.expect(TokenKind.Colon, "':' after field name");
        const fieldType = this.parseTypeExpression();
        fields.push({
          name: fieldName.lexeme,
          type: fieldType,
          span: { start: fieldName.span.start, end: fieldType.span.end },
        });
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

      // Check for empty tuple () which is syntactic sugar for Unit
      if (this.match(TokenKind.RParen)) {
        const end = this.previousSpan().end;
        return {
          kind: "TypeRef",
          name: "Unit",
          args: [],
          span: { start, end },
        };
      }

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

    // Parse simple type reference (no type application at this level)
    const ident = this.expectIdentifier("type reference");

    return {
      kind: "TypeRef",
      name: ident.lexeme,
      args: [],
      span: ident.span,
    };
  }

  /**
   * Parse record type fields: { field1 : Type1, field2 : Type2, ... }
   * Returns an array of record field types.
   */
  private parseRecordTypeFields(): RecordFieldType[] {
    this.expect(TokenKind.LBrace, "record type start '{'");
    const fields: RecordFieldType[] = [];

    // Check for empty record {}
    if (this.match(TokenKind.RBrace)) {
      return [];
    }

    // Parse first field
    const firstNameToken = this.expect(
      TokenKind.LowerIdentifier,
      "record field name",
    );
    this.expect(TokenKind.Colon, "':' after field name");
    const firstType = this.parseTypeExpression();
    fields.push({
      name: firstNameToken.lexeme,
      type: firstType,
      span: { start: firstNameToken.span.start, end: firstType.span.end },
    });

    const baseIndent = firstNameToken.span.start.column;
    let lastEnd = firstType.span.end;

    // Parse subsequent fields
    while (this.peek(TokenKind.Comma)) {
      const next = this.peekAhead(1);
      if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;
      this.advance(); // consume comma

      const fieldNameToken = this.expect(
        TokenKind.LowerIdentifier,
        "record field name",
      );
      this.expect(TokenKind.Colon, "':' after field name");
      const fieldType = this.parseTypeExpression();
      fields.push({
        name: fieldNameToken.lexeme,
        type: fieldType,
        span: { start: fieldNameToken.span.start, end: fieldType.span.end },
      });
      lastEnd = fieldType.span.end;
    }

    this.expect(TokenKind.RBrace, "close record type '}'");
    return fields;
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
   *   []                   (Empty ListPattern)
   *   [x, y]               (ListPattern with elements)
   *   x :: xs              (ConsPattern)
   */
  private parsePattern(): Pattern {
    // Parse the primary pattern first
    const primary = this.parsePrimaryPattern();

    // Check for cons operator (::)
    if (this.peekOperator("::")) {
      return this.parseConsPattern(primary);
    }

    return primary;
  }

  /**
   * Parse cons pattern continuation: head :: tail
   * Cons is right-associative, so x :: y :: zs parses as x :: (y :: zs)
   */
  private parseConsPattern(head: Pattern): Pattern {
    this.advance(); // consume ::
    const tail = this.parsePattern(); // right-associative, recurse

    return {
      kind: "ConsPattern",
      head,
      tail,
      span: { start: head.span.start, end: tail.span.end },
    };
  }

  /**
   * Parse primary pattern (without cons)
   */
  private parsePrimaryPattern(): Pattern {
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
        args.push(this.parsePrimaryPattern());
      }

      const end = args.at(-1)?.span.end ?? ctor.span.end;
      return {
        kind: "ConstructorPattern",
        name: ctor.lexeme,
        args,
        span: { start: ctor.span.start, end },
      };
    }

    // Check for list pattern: [] or [p1, p2, ...]
    if (this.match(TokenKind.LBracket)) {
      const start = this.previousSpan().start;

      // Empty list pattern
      if (this.match(TokenKind.RBracket)) {
        return {
          kind: "ListPattern",
          elements: [],
          span: { start, end: this.previousSpan().end },
        };
      }

      // Parse list elements
      const elements: Pattern[] = [];
      const first = this.parsePattern();
      elements.push(first);

      while (this.peek(TokenKind.Comma)) {
        this.advance(); // consume comma
        const pat = this.parsePattern();
        elements.push(pat);
      }

      this.expect(TokenKind.RBracket, "close list pattern");

      return {
        kind: "ListPattern",
        elements,
        span: { start, end: this.previousSpan().end },
      };
    }

    // Check for parenthesized pattern or tuple
    if (this.match(TokenKind.LParen)) {
      const start = this.previousSpan().start;

      // Check for empty tuple/unit pattern
      if (this.match(TokenKind.RParen)) {
        return {
          kind: "TuplePattern",
          elements: [],
          span: { start, end: this.previousSpan().end },
        };
      }

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
    baseIndentFloor?: number,
  ): Expr {
    // Parse left operand (a unary expression, which may be an application)
    let left = this.parseUnary(baseIndentFloor);

    // Parse operators and right operands
    while (true) {
      const operatorToken = this.current();

      // Stop if not an operator
      if (operatorToken.kind !== TokenKind.Operator) break;

      // Stop if this is an attribute marker (@) - used for @external, @deprecated, etc.
      // These are not infix operators and should not be parsed as part of expressions
      if (operatorToken.lexeme === "@") break;

      // Get precedence and associativity for this operator
      const { precedence, associativity } = this.getOperatorInfo(
        operatorToken.lexeme,
      );

      // Stop if precedence too low
      if (precedence < minPrecedence) break;

      // Consume operator
      this.advance();

      // Calculate minimum precedence for right side
      // Left-associative: increase precedence (left binds tighter)
      // Right-associative: keep same precedence (right binds tighter)
      // Non-associative: increase precedence (disallow chaining)
      const nextMin = associativity === "right" ? precedence : precedence + 1;

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
   * Parse unary expression (prefix operator)
   *
   * Grammar: UnaryExpr = "-", UnaryExpr | Application
   *
   * Currently only supports unary negation (-).
   * Unary minus binds tighter than all binary operators.
   *
   * Examples:
   *   -x        (negation of variable)
   *   -(x + 1)  (negation of grouped expression)
   *   -(-10)    (double negation with grouping)
   *
   * Note: `--` is NOT allowed as double negation; it starts a comment.
   * Double negation requires explicit grouping: -(-x)
   *
   * @param baseIndentFloor - Minimum indentation for layout
   */
  private parseUnary(baseIndentFloor?: number): Expr {
    const token = this.current();

    // Check for unary minus: must be `-` followed by alphanumeric or `(`
    if (token.kind === TokenKind.Operator && token.lexeme === "-") {
      const nextToken = this.peekAhead(1);
      if (
        nextToken &&
        (nextToken.kind === TokenKind.LowerIdentifier ||
          nextToken.kind === TokenKind.UpperIdentifier ||
          nextToken.kind === TokenKind.Number ||
          nextToken.kind === TokenKind.LParen)
      ) {
        const start = token.span.start;
        this.advance(); // consume the `-`

        // Recursively parse the operand (allows -(-x) with grouping)
        const operand = this.parseUnary(baseIndentFloor);

        return {
          kind: "Unary",
          operator: "-",
          operand,
          span: { start, end: operand.span.end },
        };
      }
    }

    // Not a unary expression, fall through to application
    return this.parseApplication(baseIndentFloor);
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
   * Grammar: PrimaryWithAccess = Primary, {".", (LowerIdentifier | UpperIdentifier)}
   *
   * Allows both record field access (lowercase) and module access (uppercase)
   * Examples: record.field.nested or Vibe.JS.null
   */
  private parsePrimaryWithAccess(): Expr {
    // Parse base expression
    let expr = this.parsePrimary();

    // Parse field accesses (chained with dots)
    while (this.match(TokenKind.Dot)) {
      // Accept both lower and upper case identifiers
      // Lower for record fields, upper for module access
      const token = this.current();
      if (
        token.kind !== TokenKind.LowerIdentifier &&
        token.kind !== TokenKind.UpperIdentifier
      ) {
        throw this.error("field or module name", token);
      }
      const field = this.advance();
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

      // Check for parenthesized operator: (==), (+), etc.
      // Operators in parentheses are treated as function references
      // We look ahead to check for (operator) pattern without consuming tokens
      if (
        this.peek(TokenKind.Operator) &&
        this.peekAhead(1)?.kind === TokenKind.RParen
      ) {
        const opToken = this.advance();
        this.advance(); // consume RParen
        return {
          kind: "Var",
          name: opToken.lexeme,
          namespace: "operator",
          span: { start, end: this.previousSpan().end },
        };
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
        "record field or base",
      );
      if (this.match(TokenKind.Pipe)) {
        const fields = this.parseRecordFields(
          undefined,
          head.span.start.column,
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
    providedBaseIndent?: number,
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
        "record field name",
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
        this.current().span,
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
          firstAfterOf.span,
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
            next.span,
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
        "module name segment",
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

  /**
   * Parse a method/function body: pattern arguments followed by `= expression`.
   * This is shared between:
   * - Standard value declarations (top-level functions)
   * - Protocol default method implementations
   * - Implementation method definitions
   *
   * @returns The parsed arguments and body expression
   */
  private parseMethodBody(): { args: Pattern[]; body: Expr } {
    // Parse pattern arguments (if any)
    // Uses isFunctionParamPatternStart for consistency with standard functions
    const args: Pattern[] = [];
    while (this.isFunctionParamPatternStart(this.current())) {
      args.push(this.parseFunctionParamPattern());
    }

    // Expect equals sign
    this.expect(TokenKind.Equals, "'=' after parameters");

    // Parse the body expression
    const body = this.parseExpression();

    return { args, body };
  }

  private parseExternalDeclaration(): Declaration {
    const atToken = this.expectOperator("@");
    const externalToken = this.expect(
      TokenKind.LowerIdentifier,
      "external tag",
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

  private peekOperator(op: string): boolean {
    const token = this.current();
    return token.kind === TokenKind.Operator && token.lexeme === op;
  }

  private isPatternStart(token: Token): boolean {
    return (
      token.kind === TokenKind.LowerIdentifier ||
      token.kind === TokenKind.UpperIdentifier ||
      token.kind === TokenKind.LParen ||
      token.kind === TokenKind.LBracket
    );
  }

  /**
   * Check if a token can start a simple pattern (variable or wildcard only).
   * Used for function parameters where complex patterns are not allowed.
   */
  private isSimplePatternStart(token: Token): boolean {
    return token.kind === TokenKind.LowerIdentifier;
  }

  /**
   * Check if a token can start a function parameter pattern.
   * Allowed patterns: variables, wildcards, tuples, records, and constructor patterns.
   * Note: Constructor patterns are validated in semantics to ensure single-constructor ADTs.
   * List patterns and cons patterns are NOT allowed in function parameters.
   */
  private isFunctionParamPatternStart(token: Token): boolean {
    return (
      token.kind === TokenKind.LowerIdentifier ||
      token.kind === TokenKind.UpperIdentifier ||
      token.kind === TokenKind.LParen ||
      token.kind === TokenKind.LBrace
    );
  }

  /**
   * Parse a function parameter pattern.
   * Allowed patterns: variables, wildcards, tuples, records, and constructor patterns.
   * Constructor patterns are validated in semantics to ensure they're for single-constructor ADTs.
   * List patterns are NOT allowed in function parameters.
   */
  private parseFunctionParamPattern(): Pattern {
    // Variable or wildcard pattern
    if (this.match(TokenKind.LowerIdentifier)) {
      const tok = this.previous();

      // Underscore is wildcard pattern
      if (tok.lexeme === "_") {
        return { kind: "WildcardPattern", span: tok.span };
      }

      // Otherwise it's a variable pattern
      return { kind: "VarPattern", name: tok.lexeme, span: tok.span };
    }

    // Constructor pattern (validated in semantics for single-constructor ADTs)
    if (this.match(TokenKind.UpperIdentifier)) {
      const ctor = this.previous();

      // Parse constructor arguments (zero or more patterns)
      // Only allow simple patterns as constructor arguments in function params
      const args: Pattern[] = [];
      while (this.isFunctionParamPatternStart(this.current())) {
        args.push(this.parseFunctionParamPattern());
      }

      const end = args.at(-1)?.span.end ?? ctor.span.end;
      return {
        kind: "ConstructorPattern",
        name: ctor.lexeme,
        args,
        span: { start: ctor.span.start, end },
      };
    }

    // Tuple or parenthesized pattern
    if (this.match(TokenKind.LParen)) {
      const start = this.previousSpan().start;

      // Check for empty tuple/unit pattern
      if (this.match(TokenKind.RParen)) {
        return {
          kind: "TuplePattern",
          elements: [],
          span: { start, end: this.previousSpan().end },
        };
      }

      // Parse first pattern
      const first = this.parseFunctionParamPattern();
      const elements: Pattern[] = [first];
      const baseIndent = first.span.start.column;
      let lastEnd = first.span.end;

      // Check for additional comma-separated patterns (tuple)
      while (this.peek(TokenKind.Comma)) {
        const next = this.peekAhead(1);

        // Respect layout rules for multi-line tuples
        if (!next || !this.continuesLayout(baseIndent, lastEnd, next)) break;

        this.advance(); // consume comma
        const pat = this.parseFunctionParamPattern();
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

    // Record pattern (destructuring)
    if (this.match(TokenKind.LBrace)) {
      const start = this.previousSpan().start;

      // Empty record pattern
      if (this.match(TokenKind.RBrace)) {
        return {
          kind: "RecordPattern",
          fields: [],
          span: { start, end: this.previousSpan().end },
        };
      }

      // Parse record field patterns
      const fields: { name: string; pattern?: Pattern }[] = [];

      // First field
      const fieldName = this.expect(
        TokenKind.LowerIdentifier,
        "record field name",
      );
      let fieldPattern: Pattern | undefined;

      // Check for field with pattern: { x = pat }
      if (this.match(TokenKind.Equals)) {
        fieldPattern = this.parseFunctionParamPattern();
      }

      fields.push({ name: fieldName.lexeme, pattern: fieldPattern });

      // Additional fields
      while (this.peek(TokenKind.Comma)) {
        this.advance(); // consume comma

        const nextFieldName = this.expect(
          TokenKind.LowerIdentifier,
          "record field name",
        );
        let nextFieldPattern: Pattern | undefined;

        if (this.match(TokenKind.Equals)) {
          nextFieldPattern = this.parseFunctionParamPattern();
        }

        fields.push({ name: nextFieldName.lexeme, pattern: nextFieldPattern });
      }

      this.expect(TokenKind.RBrace, "close record pattern");

      return {
        kind: "RecordPattern",
        fields,
        span: { start, end: this.previousSpan().end },
      };
    }

    // List patterns are not allowed in function parameters
    const token = this.current();
    if (token.kind === TokenKind.LBracket) {
      throw new ParseError(
        `List patterns are not allowed in function parameters. ` +
          `Use a case expression in the function body instead: ` +
          `\`fn x = case x of [a, b] -> ...\``,
        token.span,
      );
    }

    throw this.error("function parameter pattern", token);
  }

  /**
   * Parse a simple pattern (variable or wildcard only).
   * Complex patterns (constructors, tuples, lists) are not allowed in function parameters.
   * Use a case expression in the function body instead.
   */
  private parseSimplePattern(): Pattern {
    if (this.match(TokenKind.LowerIdentifier)) {
      const tok = this.previous();

      // Underscore is wildcard pattern
      if (tok.lexeme === "_") {
        return { kind: "WildcardPattern", span: tok.span };
      }

      // Otherwise it's a variable pattern
      return { kind: "VarPattern", name: tok.lexeme, span: tok.span };
    }

    // Provide helpful error messages for complex patterns
    const token = this.current();
    if (token.kind === TokenKind.UpperIdentifier) {
      throw new ParseError(
        `Constructor patterns are not allowed in function parameters. ` +
          `Use a case expression in the function body instead: ` +
          `\`fn x = case x of ${token.lexeme} ... -> ...\``,
        token.span,
      );
    }
    if (token.kind === TokenKind.LParen) {
      throw new ParseError(
        `Tuple patterns are not allowed in function parameters. ` +
          `Use a case expression in the function body instead: ` +
          `\`fn x = case x of (a, b) -> ...\``,
        token.span,
      );
    }
    if (token.kind === TokenKind.LBracket) {
      throw new ParseError(
        `List patterns are not allowed in function parameters. ` +
          `Use a case expression in the function body instead: ` +
          `\`fn x = case x of [a, b] -> ...\``,
        token.span,
      );
    }

    throw this.error("variable or wildcard pattern", token);
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
    next: Token,
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
      token.span,
    );
  }
}

/**
/*
 * Historical note: Operator precedence was previously hardcoded here.
 * As of the module-aware parsing update, operator precedence is now
 * derived entirely from infix declarations in source code.
 *
 * The standard library (Vibe.vibe) declares these operators:
 *   infixl 1 |>
 *   infixr 1 <|
 *   infixr 2 ||
 *   infixr 3 &&
 *   infixl 4 == /= < <= > >=
 *   infixr 5 :: ++
 *   infixl 6 + -
 *   infixl 7 * / // %
 *   infixr 8 ^
 *   infixr 9 << >>
 *
 * Modules that import Vibe (or have it auto-injected) will automatically
 * have access to the correct precedence during parsing via the module
 * discovery algorithm that merges operator registries from dependencies.
 */
