import { tokenizeStream, type LexOptions, type LexSource } from "@vibe/lexer";
import {
  DiagnosticSeverity,
  NodeKind,
  TokenType,
  createSpan,
  type Diagnostic,
  type ExpressionNode,
  type ListNode,
  type NamespaceImportNode,
  type ProgramNode,
  type ReaderMacroKind,
  type ReaderMacroNode,
  type ScopeId,
  type SequenceNode,
  type SequenceNodeKind,
  type VectorNode,
  type SourcePosition,
  type SourceSpan,
  type Token,
} from "@vibe/syntax";

export interface ParseOptions {
  readonly lex?: LexOptions;
}

export interface ParseResult {
  readonly ok: boolean;
  readonly program: ProgramNode;
  readonly diagnostics: Diagnostic[];
}

type SpanCarrier = { span: SourceSpan };

interface SequenceConfig<K extends SequenceNodeKind> {
  readonly kind: K;
  readonly open: Token;
  readonly closing: TokenType;
  readonly structure: string;
  readonly unterminatedCode: string;
  readonly spanStart?: SpanCarrier | null;
}

export const parseSource = async (
  source: LexSource,
  options: ParseOptions = {}
): Promise<ParseResult> => {
  const stream = tokenizeStream(source, options.lex);
  const parser = new Parser(stream);
  const program = await parser.parseProgram();
  const lexSummary = await stream.result;
  const diagnostics = [...lexSummary.diagnostics, ...parser.diagnostics];
  return {
    ok: lexSummary.ok && parser.diagnostics.length === 0,
    program,
    diagnostics,
  };
};

export class Parser {
  private readonly iterator: AsyncIterator<Token>;
  private lookahead: Token | null = null;
  private done = false;
  private readonly scopeAnnotator = new ScopeAnnotator();
  private origin: SourcePosition = { offset: 0, line: 0, column: 0 };
  private lastToken: Token | null = null;
  readonly diagnostics: Diagnostic[] = [];

  constructor(stream: AsyncIterable<Token>) {
    this.iterator = stream[Symbol.asyncIterator]();
  }

  async parseProgram(): Promise<ProgramNode> {
    const body: ExpressionNode[] = [];
    while (!(await this.isAtEnd())) {
      const expr = await this.parseExpression();
      if (expr) {
        body.push(expr);
      }
    }
    const start = body.length > 0 ? body[0]!.span.start : this.origin;
    const end = body.length > 0 ? body[body.length - 1]!.span.end : start;
    const program: ProgramNode = {
      kind: NodeKind.Program,
      span: createSpan(start, end),
      body,
    };
    this.scopeAnnotator.annotate(program);
    return program;
  }

  private async parseExpression(): Promise<ExpressionNode | null> {
    if (await this.isAtEnd()) {
      return null;
    }
    const token = await this.advance();
    if (!token) {
      return null;
    }
    switch (token.kind) {
      case TokenType.LeftParen: {
        const list = await this.parseSequenceNode({
          kind: NodeKind.List,
          open: token,
          closing: TokenType.RightParen,
          structure: "list",
          unterminatedCode: "PARSE_LIST_UNTERMINATED",
        });
        return this.promoteListNode(list as ListNode);
      }
      case TokenType.LeftBracket:
        return this.parseSequenceNode({
          kind: NodeKind.Vector,
          open: token,
          closing: TokenType.RightBracket,
          structure: "vector literal",
          unterminatedCode: "PARSE_VECTOR_UNTERMINATED",
        });

      case TokenType.Quote:
        return this.parseReaderMacro(NodeKind.Quote, token);
      case TokenType.SyntaxQuote:
        return this.parseReaderMacro(NodeKind.SyntaxQuote, token);

      case TokenType.Number:
        return this.createAtomNode(
          NodeKind.Number,
          token,
          this.numberValue(token)
        );
      case TokenType.String:
        return this.createAtomNode(
          NodeKind.String,
          token,
          this.stringValue(token)
        );
      case TokenType.Symbol:
        return this.createAtomNode(
          NodeKind.Symbol,
          token,
          this.stringValue(token)
        );
      case TokenType.Keyword:
        return this.createAtomNode(
          NodeKind.Keyword,
          token,
          this.stringValue(token)
        );
      case TokenType.Boolean:
        return this.createAtomNode(
          NodeKind.Boolean,
          token,
          this.booleanValue(token)
        );
      case TokenType.Nil:
        return this.createAtomNode(NodeKind.Nil, token, null);
      case TokenType.RightParen:
      case TokenType.RightBracket:
        this.reportUnexpectedClosing(token);
        return null;
      default:
        this.reportUnhandledToken(token);
        return null;
    }
  }

  private async parseSequenceNode<K extends SequenceNodeKind>(
    config: SequenceConfig<K>
  ): Promise<SequenceNode<K>> {
    const { kind, open, closing, structure, unterminatedCode, spanStart } =
      config;
    const elements: ExpressionNode[] = [];
    while (!(await this.check(closing)) && !(await this.isAtEnd())) {
      const element = await this.parseExpression();
      if (element) {
        elements.push(element);
      }
    }
    const close = await this.consumeClosing(
      closing,
      open,
      structure,
      unterminatedCode
    );
    const startRef: SpanCarrier = (spanStart ?? open) as SpanCarrier;
    const lastElement =
      elements.length > 0
        ? (elements[elements.length - 1] as SpanCarrier)
        : null;
    const endRef: SpanCarrier =
      (close as SpanCarrier | null) ?? lastElement ?? open;
    return {
      kind,
      elements,
      span: this.spanFromRefs(startRef, endRef),
    } satisfies SequenceNode<K>;
  }

  private promoteListNode(node: ListNode): ExpressionNode {
    const head = node.elements[0];
    if (!head || head.kind !== NodeKind.Symbol) {
      return node;
    }
    if (head.value === "require" || head.value === "external") {
      return {
        kind: NodeKind.NamespaceImport,
        span: node.span,
        importKind: head.value,
        head,
        alias: node.elements[1] ?? null,
        source: node.elements[2] ?? null,
        elements: node.elements,
      } satisfies NamespaceImportNode;
    }
    if (head.value === "import") {
      return {
        kind: NodeKind.NamespaceImport,
        span: node.span,
        importKind: "import",
        head,
        alias: null,
        source: node.elements[1] ?? null,
        elements: node.elements,
      } satisfies NamespaceImportNode;
    }
    return node;
  }

  private async parseReaderMacro<K extends ReaderMacroKind>(
    kind: K,
    macroToken: Token
  ): Promise<ReaderMacroNode<K>> {
    let target: ExpressionNode | null = null;
    if (!(await this.isExpressionTerminator())) {
      target = await this.parseExpression();
    }
    if (!target) {
      this.report(
        "Reader macro requires a following form",
        macroToken.span.start,
        macroToken.span.end,
        "PARSE_MACRO_MISSING_TARGET"
      );
    }
    const endRef: SpanCarrier = (target ?? macroToken) as SpanCarrier;
    return {
      kind,
      target: target ?? null,
      span: this.spanFromRefs(macroToken, endRef),
    } satisfies ReaderMacroNode<K>;
  }

  private createAtomNode<T>(
    kind: NodeKind,
    token: Token,
    value: T
  ): ExpressionNode {
    return {
      kind,
      span: token.span,
      lexeme: token.lexeme,
      value,
    } as ExpressionNode;
  }

  private async consumeClosing(
    expected: TokenType,
    open: Token,
    structure: string,
    code: string
  ): Promise<Token | null> {
    if (await this.check(expected)) {
      return this.advance();
    }
    this.report(
      `Expected ${this.describeToken(expected)} to close ${structure}`,
      open.span.start,
      open.span.end,
      code
    );
    return null;
  }

  private reportMapArity(anchor: SpanCarrier): void {
    this.diagnostics.push({
      message: "Map literal support removed",
      span: this.spanFromRefs(anchor, anchor),
      severity: DiagnosticSeverity.Error,
      code: "PARSE_MAP_UNSUPPORTED",
    });
  }

  private reportUnexpectedClosing(token: Token): void {
    this.diagnostics.push({
      message: `Unexpected ${this.describeToken(
        token.kind
      )} without a matching opener`,
      span: token.span,
      severity: DiagnosticSeverity.Error,
      code: "PARSE_UNEXPECTED_CLOSING",
    });
  }

  private reportUnhandledToken(token: Token): void {
    this.diagnostics.push({
      message: `Unhandled token ${token.kind} in parser`,
      span: token.span,
      severity: DiagnosticSeverity.Error,
      code: "PARSE_UNSUPPORTED_TOKEN",
    });
  }

  private report(
    message: string,
    start: SourcePosition,
    end: SourcePosition,
    code: string
  ): void {
    this.diagnostics.push({
      message,
      span: createSpan(start, end),
      severity: DiagnosticSeverity.Error,
      code,
    });
  }

  private spanFromRefs(
    startRef: SpanCarrier | null | undefined,
    endRef?: SpanCarrier | null
  ): SourceSpan {
    const start = this.resolveStart(startRef);
    const end = this.resolveEnd(endRef ?? startRef ?? null);
    return createSpan(start, end);
  }

  private resolveStart(ref?: SpanCarrier | null): SourcePosition {
    if (ref) {
      return ref.span.start;
    }
    return this.origin;
  }

  private resolveEnd(ref?: SpanCarrier | null): SourcePosition {
    if (ref) {
      return ref.span.end;
    }
    if (this.lastToken) {
      return this.lastToken.span.end;
    }
    return this.origin;
  }

  private describeToken(kind: TokenType): string {
    switch (kind) {
      case TokenType.LeftParen:
        return "(";
      case TokenType.RightParen:
        return ")";
      case TokenType.LeftBracket:
        return "[";
      case TokenType.RightBracket:
        return "]";
      case TokenType.Quote:
        return "'";
      case TokenType.SyntaxQuote:
        return "`";

      default:
        return kind;
    }
  }

  private async isAtEnd(): Promise<boolean> {
    await this.ensureLookahead();
    return this.lookahead === null && this.done;
  }

  private async check(kind: TokenType): Promise<boolean> {
    const token = await this.peek();
    return token?.kind === kind;
  }

  private async advance(): Promise<Token | null> {
    await this.ensureLookahead();
    if (!this.lookahead) {
      return null;
    }
    const token = this.lookahead;
    this.lookahead = null;
    this.lastToken = token;
    return token;
  }

  private async peek(): Promise<Token | null> {
    await this.ensureLookahead();
    return this.lookahead;
  }

  private async ensureLookahead(): Promise<void> {
    if (this.lookahead || this.done) {
      return;
    }
    const { value, done } = await this.iterator.next();
    if (done || !value) {
      this.done = true;
      this.lookahead = null;
      return;
    }
    this.lookahead = value;
    if (this.lastToken === null) {
      this.origin = value.span.start;
    }
  }

  private booleanValue(token: Token): boolean {
    return token.value === true;
  }

  private stringValue(token: Token): string {
    return typeof token.value === "string" ? token.value : token.lexeme;
  }

  private numberValue(token: Token): number {
    if (typeof token.value === "number" && Number.isFinite(token.value)) {
      return token.value;
    }
    const parsed = Number(token.lexeme);
    return Number.isNaN(parsed) ? 0 : parsed;
  }

  private async isExpressionTerminator(): Promise<boolean> {
    const token = await this.peek();
    if (!token) {
      return true;
    }
    return (
      token.kind === TokenType.RightParen ||
      token.kind === TokenType.RightBracket ||
      false
    );
  }
}

class ScopeAnnotator {
  private nextScopeId = 0;

  annotate(program: ProgramNode): void {
    this.nextScopeId = 0;
    const rootScopeId = this.allocateScopeId();
    this.assignScope(program, rootScopeId);
    for (const expr of program.body) {
      if (expr) {
        this.annotateExpression(expr, rootScopeId);
      }
    }
  }

  private annotateExpression(node: ExpressionNode, scopeId: ScopeId): void {
    this.assignScope(node, scopeId);
    switch (node.kind) {
      case NodeKind.List:
        this.annotateList(node, scopeId);
        break;
      case NodeKind.NamespaceImport:
        this.annotateNamespaceImport(node as NamespaceImportNode, scopeId);
        break;
      case NodeKind.Vector:
        this.annotateSequence(node as VectorNode, scopeId);
        break;
      case NodeKind.Quote:
      case NodeKind.SyntaxQuote:
        this.annotateReaderMacro(node as ReaderMacroNode, scopeId);
        break;

      default:
        break;
    }
  }

  private annotateList(node: ListNode, scopeId: ScopeId): void {
    const head = node.elements[0];
    if (head) {
      this.annotateExpression(head, scopeId);
    }
    if (head?.kind === NodeKind.Symbol) {
      switch (head.value) {
        case "let":
          this.annotateLet(node, scopeId);
          return;
        case "fn+":
          this.annotateFn(node, scopeId);
          return;
        case "try":
          this.annotateTry(node, scopeId);
          return;
        default:
          break;
      }
    }
    for (let index = 1; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.annotateExpression(element, scopeId);
      }
    }
  }

  private annotateNamespaceImport(
    node: NamespaceImportNode,
    scopeId: ScopeId
  ): void {
    for (const element of node.elements) {
      if (element) {
        this.annotateExpression(element, scopeId);
      }
    }
  }

  private annotateLet(node: ListNode, parentScopeId: ScopeId): void {
    const bindingsNode = node.elements[1];
    if (!bindingsNode || bindingsNode.kind !== NodeKind.Vector) {
      for (let index = 1; index < node.elements.length; index += 1) {
        const element = node.elements[index];
        if (element) {
          this.annotateExpression(element, parentScopeId);
        }
      }
      return;
    }

    this.assignScope(bindingsNode, parentScopeId);
    const childScopeId = this.allocateScopeId();
    const bindings = bindingsNode.elements;
    for (let index = 0; index < bindings.length; index += 2) {
      const target = bindings[index];
      const init = bindings[index + 1];
      if (target) {
        this.annotateExpression(target, childScopeId);
      }
      if (init) {
        this.annotateExpression(init, childScopeId);
      }
    }

    for (let index = 2; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.annotateExpression(element, childScopeId);
      }
    }
  }

  private annotateFn(node: ListNode, parentScopeId: ScopeId): void {
    const clauseNodes = this.extractFnClauses(node);
    if (clauseNodes.length > 0) {
      this.annotateFnClauses(clauseNodes, parentScopeId);
      return;
    }

    const fnScopeId = this.allocateScopeId();
    const paramsNode = node.elements[1];
    if (paramsNode && paramsNode.kind === NodeKind.Vector) {
      this.assignScope(paramsNode, parentScopeId);
      for (const param of paramsNode.elements) {
        if (param) {
          this.annotateExpression(param, fnScopeId);
        }
      }
    } else if (paramsNode) {
      this.annotateExpression(paramsNode, parentScopeId);
    }

    for (let index = 2; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.annotateExpression(element, fnScopeId);
      }
    }
  }

  private annotateTry(node: ListNode, parentScopeId: ScopeId): void {
    const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
    const catchClause = this.findTryClause(tail, "catch");
    const finallyClause = this.findTryClause(tail, "finally");

    for (const expr of tail) {
      if (
        (catchClause && expr === catchClause) ||
        (finallyClause && expr === finallyClause)
      ) {
        continue;
      }
      this.annotateExpression(expr, parentScopeId);
    }

    if (catchClause) {
      this.assignScope(catchClause, parentScopeId);
      const catchScopeId = this.allocateScopeId();
      const binding = catchClause.elements[1];
      if (binding) {
        this.annotateExpression(binding, catchScopeId);
      }
      for (let index = 2; index < catchClause.elements.length; index += 1) {
        const clauseExpr = catchClause.elements[index];
        if (clauseExpr) {
          this.annotateExpression(clauseExpr, catchScopeId);
        }
      }
    }

    if (finallyClause) {
      this.assignScope(finallyClause, parentScopeId);
      for (let index = 1; index < finallyClause.elements.length; index += 1) {
        const clauseExpr = finallyClause.elements[index];
        if (clauseExpr) {
          this.annotateExpression(clauseExpr, parentScopeId);
        }
      }
    }
  }

  private findTryClause(
    nodes: readonly ExpressionNode[],
    kind: "catch" | "finally"
  ): ListNode | null {
    for (const node of nodes) {
      if (node.kind !== NodeKind.List) {
        continue;
      }
      const clauseHead = node.elements[0];
      if (
        clauseHead &&
        clauseHead.kind === NodeKind.Symbol &&
        clauseHead.value === kind
      ) {
        return node;
      }
    }
    return null;
  }

  private extractFnClauses(node: ListNode): readonly ListNode[] {
    const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
    if (tail.length === 0) {
      return [];
    }
    const clauseLists = tail.filter(
      (element): element is ListNode => element.kind === NodeKind.List
    );
    if (clauseLists.length === tail.length) {
      return clauseLists;
    }
    return [];
  }

  private annotateFnClauses(
    clauses: readonly ListNode[],
    parentScopeId: ScopeId
  ): void {
    for (const clause of clauses) {
      this.assignScope(clause, parentScopeId);
      const clauseScopeId = this.allocateScopeId();
      const paramsNode = clause.elements[0];
      if (paramsNode && paramsNode.kind === NodeKind.Vector) {
        this.assignScope(paramsNode, parentScopeId);
        for (const param of paramsNode.elements) {
          if (param) {
            this.annotateExpression(param, clauseScopeId);
          }
        }
      } else if (paramsNode) {
        this.annotateExpression(paramsNode, parentScopeId);
      }
      for (let index = 1; index < clause.elements.length; index += 1) {
        const element = clause.elements[index];
        if (element) {
          this.annotateExpression(element, clauseScopeId);
        }
      }
    }
  }

  private annotateSequence(node: VectorNode, scopeId: ScopeId): void {
    for (const element of node.elements) {
      if (element) {
        this.annotateExpression(element, scopeId);
      }
    }
  }

  /* Map annotation removed */

  private annotateReaderMacro(node: ReaderMacroNode, scopeId: ScopeId): void {
    if (node.target) {
      this.annotateExpression(node.target, scopeId);
    }
  }

  private assignScope(
    node: ProgramNode | ExpressionNode,
    scopeId: ScopeId
  ): void {
    (node as { scopeId?: ScopeId }).scopeId = scopeId;
  }

  private allocateScopeId(): ScopeId {
    return `scope_${this.nextScopeId++}` as ScopeId;
  }
}
