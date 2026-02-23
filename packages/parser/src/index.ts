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
  type DecoratedDeclaration,
} from "@vibe/syntax";
import {
  buildRegistryFromTokens,
  getOperatorInfo as getOperatorInfoFromRegistry,
  InfixParseError,
  BUILTIN_OPERATOR_REGISTRY,
  mergeRegistries,
} from "./operator-registry";
import { insertLayoutTokens } from "./layout";

// Re-export BUILTIN_OPERATOR_REGISTRY for use by other packages
export { BUILTIN_OPERATOR_REGISTRY } from "./operator-registry";

/**
 * Error thrown during parsing when unexpected syntax is encountered
 */
export class ParseError extends Error {
  constructor(
    message: string,
    public readonly span: Span,
    public readonly filePath?: string,
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
  const rawTokens = lex(source);

  // Step 2: Insert layout tokens (BlockStart, BlockSep, BlockEnd)
  const tokens = insertLayoutTokens(rawTokens);

  // Step 3: Create parser instance with token stream and operator registry
  const parser = new Parser(tokens, operatorRegistry);

  // Step 4: Parse tokens into AST
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
  // Pre-scan on raw tokens (before layout pass)
  const rawTokens = lex(source);
  const { registry, errors } = buildRegistryFromTokens(rawTokens);
  // Merge with builtin operators to ensure && and || have correct precedence
  const mergedRegistry = mergeRegistries(BUILTIN_OPERATOR_REGISTRY, registry);
  // Run layout pass then parse
  const layoutTokens = insertLayoutTokens(rawTokens);
  const parser = new Parser(layoutTokens, mergedRegistry);
  const program = parser.parseProgram();
  const infixErrors = errors.map((e) => new ParseError(e.message, e.span));
  return { program, operatorRegistry: mergedRegistry, infixErrors };
}

/**
 * Recursive descent parser for Vibe language
 * Implements grammar rules from docs/grammar.ebnf
 */
class Parser {
  /** Current position in token stream */
  private index = 0;

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
   *
   * All top-level constructs must start at column 1.
   */
  parseProgram(): Program {
    // Parse module declaration (required - every Vibe file must start with one)
    if (!this.peekKeyword("module")) {
      const currentToken = this.current();
      throw new ParseError(
        `Every Vibe file must begin with a module declaration.\n` +
          `Expected: module <ModuleName> [exposing (..)]`,
        currentToken.span,
      );
    }

    this.enforceColumn1(this.current(), "module declaration");
    const module = this.parseModuleDeclaration();

    // Parse all import declarations
    const imports: ImportDeclaration[] = [];
    while (this.peekKeyword("import")) {
      this.enforceColumn1(this.current(), "import declaration");
      imports.push(this.parseImport());
    }

    // Parse all top-level declarations (functions, type annotations, externals)
    const declarations: Declaration[] = [];
    while (!this.isAtEnd()) {
      this.enforceColumn1(this.current(), "top-level declaration");
      declarations.push(this.parseDeclaration());
    }

    return { module, imports, declarations };
  }

  /**
   * Enforce that a token starts at column 1 (top-level requirement).
   */
  private enforceColumn1(token: Token, label: string): void {
    if (token.span.start.column !== 1) {
      throw new ParseError(
        `${label} must start at column 1, but found at column ${token.span.start.column}`,
        token.span,
      );
    }
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

    // Parse optional "exposing" clause
    let exposing: Exposing | null = null;
    if (this.peekKeyword("exposing")) {
      this.expectKeyword("exposing");
      exposing = this.parseExposing();
    }

    // Build AST node with full span
    const span: Span = {
      start: moduleToken.span.start,
      end: exposing ? exposing.span.end : end,
    };
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
   * Grammar: Declaration = TypeDeclaration | TypeAliasDeclaration | DecoratedDeclaration | TypeAnnotationDeclaration | ValueDeclaration
   *
   * Examples:
   *   type Maybe a = Just a | Nothing        (Type declaration - ADT)
   *   type alias UserId = number             (Type alias)
   *   @external "module" "func" foo : Int    (Decorated - external FFI)
   *   @get "key" foo : Opaque -> Ret         (Decorated - property get)
   *   @call "key" foo : Opaque -> Ret        (Decorated - method call)
   *   foo : Int -> Int                       (Type annotation)
   *   foo x = x + 1                          (Value declaration)
   *   (+) a b = add a b                      (Operator declaration)
   */
  private parseDeclaration(): Declaration {
    // Check for decorated declaration (@name ...)
    if (this.peekDecorator()) {
      return this.parseDecoratedDeclaration();
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
    const { args, body } = this.parseMethodBody(nameSpan.start.column);
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
        throw new ParseError("Opaque types cannot have constraints", {
          start: typeToken.span.start,
          end: this.previousSpan().end,
        });
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

    let recordFields: RecordFieldType[] | undefined;
    let constructors: ConstructorVariant[] | undefined;

    // Check if this is a record type (= { ... })
    if (this.peek(TokenKind.LBrace)) {
      recordFields = this.parseRecordTypeFields();
    } else {
      // Parse ADT - constructor variants separated by pipe (|)
      constructors = [];
      constructors.push(this.parseConstructorVariant());

      // Parse additional variants separated by |
      while (this.match(TokenKind.Pipe)) {
        constructors.push(this.parseConstructorVariant());
      }
    }

    return {
      kind: "TypeDeclaration",
      name,
      params,
      constraints,
      constructors,
      recordFields,
      span: { start: typeToken.span.start, end: this.previousSpan().end },
    } satisfies TypeDeclaration;
  }

  /**
   * Check if we have a constraint context in a type declaration (looks ahead for =>)
   * The => must appear before the equals sign or end of declaration (for opaque types).
   */
  private peekConstraintContextInTypeDecl(): boolean {
    let i = 0;
    let parenDepth = 0;

    while (this.peekAheadSkipLayout(i)) {
      const tok = this.peekAheadSkipLayout(i);
      if (!tok) break;

      if (tok.kind === TokenKind.LParen) parenDepth++;
      if (tok.kind === TokenKind.RParen) parenDepth--;

      if (
        parenDepth === 0 &&
        tok.kind === TokenKind.Operator &&
        tok.lexeme === "=>"
      ) {
        return true;
      }

      if (tok.kind === TokenKind.Equals && parenDepth === 0) {
        return false;
      }

      if (tok.kind === TokenKind.Keyword && parenDepth === 0) {
        const keyword = tok.lexeme;
        if (
          keyword === "implement" ||
          keyword === "protocol" ||
          keyword === "type" ||
          keyword === "module" ||
          keyword === "import" ||
          keyword === "infix" ||
          keyword === "infixl" ||
          keyword === "infixr"
        ) {
          return false;
        }
      }

      i++;
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
      const arg = this.parseTypeAtom();
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
   * Grammar: protocol UpperIdentifier {LowerIdentifier} where BlockStart {MethodItem BlockSep} BlockEnd
   *
   * Method items in the where-block:
   *   - Type annotation: name : Type
   *   - Default implementation: name args = expr
   *
   * After collecting all items, consecutive annotation+implementation pairs
   * for the same method are merged into a single ProtocolMethod.
   *
   * Example:
   *   protocol Eq a where
   *     eq : a -> a -> Bool
   *     neq : a -> a -> Bool
   *     neq x y = not (eq x y)
   */
  private parseProtocolDeclaration(): ProtocolDeclaration {
    const protocolToken = this.expectKeyword("protocol");

    // Parse optional constraints
    const constraints: Constraint[] = [];
    const hasConstraints = this.peekConstraintContextInProtocol();
    if (hasConstraints) {
      if (this.match(TokenKind.LParen)) {
        do {
          constraints.push(this.parseConstraint());
        } while (this.match(TokenKind.Comma));
        this.expect(TokenKind.RParen, "close constraint list");
      } else {
        constraints.push(this.parseConstraint());
      }
      this.expectOperator("=>");
    }

    const nameToken = this.expect(TokenKind.UpperIdentifier, "protocol name");
    const name = nameToken.lexeme;

    const params: string[] = [];
    while (this.peek(TokenKind.LowerIdentifier)) {
      const param = this.expect(TokenKind.LowerIdentifier, "type parameter");
      params.push(param.lexeme);
    }

    // "where" triggers a layout block
    this.expectKeyword("where");
    this.expectBlockStart();

    // Collect all items in the where-block
    type ProtocolItem =
      | { kind: "annotation"; name: string; type: TypeExpr; span: Span }
      | {
          kind: "implementation";
          name: string;
          args: Pattern[];
          body: Expr;
          span: Span;
        };

    const items: ProtocolItem[] = [];
    items.push(this.parseProtocolItem());
    while (this.matchBlockSep()) {
      items.push(this.parseProtocolItem());
    }
    this.expectBlockEnd();

    // Merge annotations with their default implementations
    const methods: ProtocolMethod[] = [];
    let idx = 0;
    while (idx < items.length) {
      const item = items[idx]!;
      if (item.kind === "annotation") {
        const next = items[idx + 1];
        if (next && next.kind === "implementation" && next.name === item.name) {
          methods.push({
            name: item.name,
            type: item.type,
            defaultImpl: { args: next.args, body: next.body },
            span: { start: item.span.start, end: next.span.end },
          });
          idx += 2;
        } else {
          methods.push({
            name: item.name,
            type: item.type,
            defaultImpl: undefined,
            span: item.span,
          });
          idx += 1;
        }
      } else {
        methods.push({
          name: item.name,
          type: undefined,
          defaultImpl: { args: item.args, body: item.body },
          span: item.span,
        });
        idx += 1;
      }
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
   * Parse a single protocol item (inside the where-block).
   * Returns either a type annotation or a default implementation.
   */
  private parseProtocolItem():
    | { kind: "annotation"; name: string; type: TypeExpr; span: Span }
    | {
        kind: "implementation";
        name: string;
        args: Pattern[];
        body: Expr;
        span: Span;
      } {
    let methodName: string;
    const methodStart = this.current().span.start;

    if (this.match(TokenKind.LParen)) {
      const opToken = this.expect(
        TokenKind.Operator,
        "operator in protocol method",
      );
      methodName = opToken.lexeme;
      this.expect(TokenKind.RParen, "close paren after operator");
    } else {
      const tok = this.expect(TokenKind.LowerIdentifier, "method name");
      methodName = tok.lexeme;
    }

    if (this.peek(TokenKind.Colon)) {
      this.advance();
      const methodType = this.parseTypeExpression();
      return {
        kind: "annotation",
        name: methodName,
        type: methodType,
        span: { start: methodStart, end: methodType.span.end },
      };
    } else {
      const args: Pattern[] = [];
      while (this.isFunctionParamPatternStart(this.current())) {
        args.push(this.parseFunctionParamPattern());
      }
      this.expect(TokenKind.Equals, "'=' after parameters");
      const body = this.parseExpression();
      return {
        kind: "implementation",
        name: methodName,
        args,
        body,
        span: { start: methodStart, end: body.span.end },
      };
    }
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

    // Check for "where" keyword — the layout pass opens a block after it
    const methods: MethodImplementation[] = [];

    if (this.matchKeyword("where")) {
      this.expectBlockStart();

      // Parse first method implementation
      const firstMethod = this.parseImplementationMethod();
      methods.push(firstMethod);

      // Parse remaining methods separated by BlockSep
      while (this.matchBlockSep()) {
        methods.push(this.parseImplementationMethod());
      }

      this.expectBlockEnd();
    }

    const lastMethod =
      methods.length > 0 ? methods[methods.length - 1]! : undefined;
    const span: Span = {
      start: implementToken.span.start,
      end: lastMethod
        ? lastMethod.span.end
        : typeArgs.length > 0
          ? typeArgs[typeArgs.length - 1]!.span.end
          : protocolNameToken.span.end,
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
   * Parse a single method implementation inside an implement...where block.
   */
  private parseImplementationMethod(): MethodImplementation {
    const { name: methodName, span: nameSpan } = this.parseDeclarationName();
    const { args, body: implementation } = this.parseMethodBody();
    return {
      name: methodName,
      args: args.length > 0 ? args : undefined,
      implementation,
      span: { start: nameSpan.start, end: implementation.span.end },
    };
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
    let i = 0;
    while (this.peekAheadSkipLayout(i)) {
      const tok = this.peekAheadSkipLayout(i);
      if (!tok) break;

      if (tok.kind === TokenKind.Operator && tok.lexeme === "=>") {
        return true;
      }

      if (tok.kind === TokenKind.Keyword && tok.lexeme === "where") {
        return false;
      }

      i++;
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
    let i = 0;
    let parenDepth = 0;

    while (this.peekAheadSkipLayout(i)) {
      const tok = this.peekAheadSkipLayout(i);
      if (!tok) break;

      if (tok.kind === TokenKind.LParen) parenDepth++;
      if (tok.kind === TokenKind.RParen) parenDepth--;

      if (
        parenDepth === 0 &&
        tok.kind === TokenKind.Operator &&
        tok.lexeme === "=>"
      ) {
        return true;
      }

      if (tok.kind === TokenKind.Keyword && tok.lexeme === "where") {
        return false;
      }

      i++;
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
    let i = 0;
    let parenDepth = 0;
    const startLine = this.current().span.start.line;

    while (this.peekAheadSkipLayout(i)) {
      const tok = this.peekAheadSkipLayout(i);
      if (!tok) break;

      if (tok.span.start.line !== startLine) {
        return false;
      }

      if (tok.kind === TokenKind.LParen) parenDepth++;
      if (tok.kind === TokenKind.RParen) parenDepth--;

      if (
        parenDepth === 0 &&
        tok.kind === TokenKind.Operator &&
        tok.lexeme === "=>"
      ) {
        return true;
      }

      if (parenDepth === 0) {
        if (tok.kind === TokenKind.Equals) return false;
        if (tok.kind === TokenKind.Keyword) return false;
      }

      i++;
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
    let name = ident.lexeme;
    let end = ident.span.end;

    // Handle qualified types (e.g. Module.Type)
    while (this.match(TokenKind.Dot)) {
      const next = this.expect(TokenKind.UpperIdentifier, "type name part");
      name += "." + next.lexeme;
      end = next.span.end;
    }

    // Only uppercase type constructors can have arguments
    // Lowercase identifiers are type variables and don't take arguments
    // This ensures "a (List a)" parses as two separate types, not "a" applied to "(List a)"
    // Note: Qualified names (e.g. M.T) are always considered constructors/aliases
    const isTypeConstructor =
      ident.kind === TokenKind.UpperIdentifier || name.includes(".");

    const args: TypeExpr[] = [];
    let lastSpan = { start: ident.span.start, end };

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
      name,
      args,
      span: {
        start: ident.span.start,
        end: args.at(-1)?.span.end ?? end,
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
    let name = ident.lexeme;
    let end = ident.span.end;

    // Handle qualified types (e.g. Module.Type)
    while (this.match(TokenKind.Dot)) {
      const next = this.expect(TokenKind.UpperIdentifier, "type name part");
      name += "." + next.lexeme;
      end = next.span.end;
    }

    return {
      kind: "TypeRef",
      name,
      args: [],
      span: { start: ident.span.start, end },
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

    // Check for number literal pattern
    if (this.match(TokenKind.Number)) {
      const tok = this.previous();
      if (tok.lexeme.includes(".")) {
        return { kind: "FloatPattern", value: tok.lexeme, span: tok.span };
      }
      return { kind: "IntPattern", value: tok.lexeme, span: tok.span };
    }

    // Check for string literal pattern
    if (this.match(TokenKind.String)) {
      const tok = this.previous();
      return { kind: "StringPattern", value: tok.lexeme, span: tok.span };
    }

    // Check for char literal pattern
    if (this.match(TokenKind.Char)) {
      const tok = this.previous();
      return { kind: "CharPattern", value: tok.lexeme, span: tok.span };
    }

    // Check for constructor pattern (uppercase identifier)
    if (this.match(TokenKind.UpperIdentifier)) {
      const ctor = this.previous();
      let name = ctor.lexeme;
      let end = ctor.span.end;

      // Handle qualified constructors (e.g. Module.Just)
      while (this.match(TokenKind.Dot)) {
        const next = this.expect(
          TokenKind.UpperIdentifier,
          "constructor name part",
        );
        name += "." + next.lexeme;
        end = next.span.end;
      }

      // Parse constructor arguments (zero or more patterns)
      const args: Pattern[] = [];
      while (this.isPatternStart(this.current())) {
        args.push(this.parsePrimaryPattern());
      }

      const finalEnd = args.at(-1)?.span.end ?? end;
      return {
        kind: "ConstructorPattern",
        name,
        args,
        span: { start: ctor.span.start, end: finalEnd },
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
      const lambdaCol = start.column;
      const args: Pattern[] = [];
      while (!this.matchOperator("->")) {
        if (!this.isPatternStart(this.current())) {
          throw this.error("lambda argument", this.current());
        }
        args.push(this.parsePattern());
      }
      const arrowSpan = this.previousSpan();
      const bodyToken = this.current();
      if (
        bodyToken.span.start.line !== arrowSpan.start.line &&
        bodyToken.span.start.column <= lambdaCol
      ) {
        throw new ParseError(
          `Lambda body must be indented past '\\' (column ${lambdaCol}) when on a separate line, ` +
            `but found at column ${bodyToken.span.start.column}`,
          bodyToken.span,
        );
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
    const ifCol = ifToken.span.start.column;
    const ifLine = ifToken.span.start.line;

    const condition = this.parseExpression();

    const thenToken = this.expectKeyword("then");
    if (
      thenToken.span.start.line !== ifLine &&
      thenToken.span.start.column <= ifCol
    ) {
      throw new ParseError(
        `'then' must be indented past 'if' (column ${ifCol}) when on a separate line, ` +
          `but found at column ${thenToken.span.start.column}`,
        thenToken.span,
      );
    }

    const thenBranch = this.parseExpression();

    const elseToken = this.expectKeyword("else");
    if (
      elseToken.span.start.line !== ifLine &&
      elseToken.span.start.column <= ifCol
    ) {
      throw new ParseError(
        `'else' must be indented past 'if' (column ${ifCol}) when on a separate line, ` +
          `but found at column ${elseToken.span.start.column}`,
        elseToken.span,
      );
    }

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
   * Grammar: LetInExpr = "let", BlockStart, {LetBinding, BlockSep}, BlockEnd, "in", Expr
   * Example:
   *   let
   *     x = 10
   *     y = 20
   *   in
   *     x + y
   *
   * Layout tokens handle indentation: BlockStart opens bindings,
   * BlockSep separates them, BlockEnd closes them before "in".
   */
  private parseLetIn(): Expr {
    const letToken = this.expectKeyword("let");
    const bindings: ValueDeclaration[] = [];

    this.expectBlockStart();

    // Parse first binding
    const firstDecl = this.parseDeclaration();
    if (firstDecl.kind === "ValueDeclaration") {
      bindings.push(firstDecl);
    } else {
      bindings.push(firstDecl as any);
    }

    // Parse remaining bindings separated by BlockSep
    while (this.matchBlockSep()) {
      const decl = this.parseDeclaration();
      if (decl.kind === "ValueDeclaration") {
        bindings.push(decl);
      } else {
        bindings.push(decl as any);
      }
    }

    this.expectBlockEnd();
    this.expectKeyword("in");

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
   * Grammar: CaseExpr = "case", Expr, "of", BlockStart, {CaseBranch, BlockSep}, BlockEnd
   * Grammar: CaseBranch = Pattern, "->", Expr
   *
   * Example:
   *   case value of
   *     Just x -> x
   *     Nothing -> 0
   *
   * Layout tokens handle indentation: BlockStart opens branches,
   * BlockSep separates them, BlockEnd closes them.
   */
  private parseCase(): Expr {
    const caseToken = this.expectKeyword("case");
    const discriminant = this.parseExpression();
    this.expectKeyword("of");

    const branches: Array<{ pattern: Pattern; body: Expr; span: Span }> = [];

    this.expectBlockStart();

    // Parse first branch
    const firstPattern = this.parsePattern();
    this.expectOperator("->");
    const firstBranchIndent = firstPattern.span.start.column + 1;
    const firstBody = this.parseExpression(firstBranchIndent);
    branches.push({
      pattern: firstPattern,
      body: firstBody,
      span: { start: firstPattern.span.start, end: firstBody.span.end },
    });

    // Parse remaining branches separated by BlockSep
    while (this.matchBlockSep()) {
      const pattern = this.parsePattern();
      this.expectOperator("->");
      const branchIndent = pattern.span.start.column + 1;
      const body = this.parseExpression(branchIndent);
      branches.push({
        pattern,
        body,
        span: { start: pattern.span.start, end: body.span.end },
      });
    }

    this.expectBlockEnd();

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
  private parseMethodBody(declColumn?: number): {
    args: Pattern[];
    body: Expr;
  } {
    // Parse pattern arguments (if any)
    // Uses isFunctionParamPatternStart for consistency with standard functions
    const args: Pattern[] = [];
    while (this.isFunctionParamPatternStart(this.current())) {
      args.push(this.parseFunctionParamPattern());
    }

    // Expect equals sign
    const eqToken = this.expect(TokenKind.Equals, "'=' after parameters");

    // Enforce body indentation: when on a separate line, body must be indented past the declaration name
    if (declColumn !== undefined) {
      const bodyToken = this.current();
      if (
        bodyToken.span.start.line !== eqToken.span.start.line &&
        bodyToken.span.start.column <= declColumn
      ) {
        throw new ParseError(
          `Function body must be indented past column ${declColumn} when on a separate line, ` +
            `but found at column ${bodyToken.span.start.column}`,
          bodyToken.span,
        );
      }
    }

    // Parse the body expression
    const body = this.parseExpression();

    return { args, body };
  }

  /**
   * Check if we're at a decorator (@name)
   * Works for both @identifier and @keyword (like @import)
   */
  private peekDecorator(): boolean {
    const current = this.current();
    const next = this.peekAhead(1);
    return (
      current.kind === TokenKind.Operator &&
      current.lexeme === "@" &&
      next !== undefined &&
      (next.kind === TokenKind.LowerIdentifier ||
        next.kind === TokenKind.Keyword)
    );
  }

  /**
   * Parse a decorated declaration generically.
   *
   * Syntax: @decorator [string-args...] name : Type
   *
   * The parser collects all string arguments between the decorator name
   * and the declaration name. Semantic analysis validates:
   *   - The decorator name is known
   *   - The argument count is correct
   *   - The type annotation is valid for the decorator
   *
   * Examples:
   *   @external "module" "export" foo : Int -> Int
   *   @get "key" bar : OpaqueType -> ReturnType
   *   @import "module-path" fs : FS
   */
  private parseDecoratedDeclaration(): DecoratedDeclaration {
    const atToken = this.expectOperator("@");

    // Get decorator name (could be identifier or keyword like "import")
    const decoratorToken = this.current();
    let decorator: string;
    if (decoratorToken.kind === TokenKind.LowerIdentifier) {
      decorator = decoratorToken.lexeme;
      this.advance();
    } else if (decoratorToken.kind === TokenKind.Keyword) {
      decorator = decoratorToken.lexeme;
      this.advance();
    } else {
      throw this.error("decorator name", decoratorToken);
    }

    // Collect all string arguments (may span multiple indented lines)
    const args: string[] = [];
    while (this.current().kind === TokenKind.String) {
      args.push(this.unquote(this.current().lexeme));
      this.advance();
    }

    // Skip any newlines between args and declaration name
    while (this.current().kind === TokenKind.Newline) {
      this.advance();
    }

    // The declaration name must start at column 1 (Elm-style: new top-level element)
    const nameToken = this.current();
    if (nameToken.span.start.column !== 1) {
      throw this.error(
        "declaration name at column 1 (decorator arguments are complete, name must be unindented)",
        nameToken,
      );
    }

    // Parse declaration name and type annotation
    const { name } = this.parseDeclarationName();
    this.expect(TokenKind.Colon, "type annotation after decorated declaration");
    const annotation = this.parseTypeExpression();

    const span: Span = { start: atToken.span.start, end: annotation.span.end };

    return {
      kind: "DecoratedDeclaration",
      decorator,
      args,
      name,
      annotation,
      span,
    };
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

  private matchKeyword(keyword: string): boolean {
    const token = this.current();
    if (token.kind === TokenKind.Keyword && token.lexeme === keyword) {
      this.advance();
      return true;
    }
    return false;
  }

  private peekOperator(op: string): boolean {
    const token = this.current();
    return token.kind === TokenKind.Operator && token.lexeme === op;
  }

  private isPatternStart(token: Token): boolean {
    if (
      token.kind === TokenKind.BlockStart ||
      token.kind === TokenKind.BlockSep ||
      token.kind === TokenKind.BlockEnd
    ) {
      return false;
    }
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
    if (
      token.kind === TokenKind.BlockStart ||
      token.kind === TokenKind.BlockSep ||
      token.kind === TokenKind.BlockEnd
    ) {
      return false;
    }
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
    if (
      token.kind === TokenKind.BlockStart ||
      token.kind === TokenKind.BlockSep ||
      token.kind === TokenKind.BlockEnd
    ) {
      return false;
    }
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
    if (
      token.kind === TokenKind.BlockStart ||
      token.kind === TokenKind.BlockSep ||
      token.kind === TokenKind.BlockEnd
    ) {
      return false;
    }
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

  /**
   * Look ahead in token stream, skipping virtual layout tokens.
   * Used by constraint context lookahead which needs to see past layout boundaries.
   */
  private peekAheadSkipLayout(offset: number): Token | undefined {
    let j = this.index;
    let count = 0;
    while (j < this.tokens.length) {
      const tok = this.tokens[j]!;
      if (
        tok.kind !== TokenKind.BlockStart &&
        tok.kind !== TokenKind.BlockSep &&
        tok.kind !== TokenKind.BlockEnd
      ) {
        if (count === offset) return tok;
        count++;
      }
      j++;
    }
    return undefined;
  }

  // ===== Layout Management Helpers =====
  // These handle virtual layout tokens from the layout preprocessing pass

  /**
   * Match and consume a BlockStart token
   */
  private matchBlockStart(): boolean {
    if (this.current().kind === TokenKind.BlockStart) {
      this.advance();
      return true;
    }
    return false;
  }

  /**
   * Expect and consume a BlockStart token
   */
  private expectBlockStart(): Token {
    return this.expect(TokenKind.BlockStart, "indented block");
  }

  /**
   * Match and consume a BlockSep token
   */
  private matchBlockSep(): boolean {
    if (this.current().kind === TokenKind.BlockSep) {
      this.advance();
      return true;
    }
    return false;
  }

  /**
   * Match and consume a BlockEnd token
   */
  private matchBlockEnd(): boolean {
    if (this.current().kind === TokenKind.BlockEnd) {
      this.advance();
      return true;
    }
    return false;
  }

  /**
   * Expect and consume a BlockEnd token
   */
  private expectBlockEnd(): Token {
    return this.expect(TokenKind.BlockEnd, "end of indented block");
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
