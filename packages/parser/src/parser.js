import { tokenizeStream } from "@vibe/lexer";
import { DiagnosticSeverity, NodeKind, TokenType, createSpan, } from "@vibe/syntax";
export const parseSource = async (source, options = {}) => {
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
    iterator;
    lookahead = null;
    done = false;
    scopeAnnotator = new ScopeAnnotator();
    origin = { offset: 0, line: 0, column: 0 };
    lastToken = null;
    diagnostics = [];
    constructor(stream) {
        this.iterator = stream[Symbol.asyncIterator]();
    }
    async parseProgram() {
        const body = [];
        while (!(await this.isAtEnd())) {
            const expr = await this.parseExpression();
            if (expr) {
                body.push(expr);
            }
        }
        const start = body.length > 0 ? body[0].span.start : this.origin;
        const end = body.length > 0 ? body[body.length - 1].span.end : start;
        const program = {
            kind: NodeKind.Program,
            span: createSpan(start, end),
            body,
        };
        this.scopeAnnotator.annotate(program);
        return program;
    }
    async parseExpression() {
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
                return this.promoteListNode(list);
            }
            case TokenType.LeftBracket:
                return this.parseSequenceNode({
                    kind: NodeKind.List,
                    open: token,
                    closing: TokenType.RightBracket,
                    structure: "list",
                    unterminatedCode: "PARSE_LIST_UNTERMINATED",
                });
            case TokenType.Number:
                return this.createAtomNode(NodeKind.Number, token, this.numberValue(token));
            case TokenType.String:
                return this.createAtomNode(NodeKind.String, token, this.stringValue(token));
            case TokenType.Symbol:
                return this.createAtomNode(NodeKind.Symbol, token, this.stringValue(token));
            case TokenType.Keyword:
                return this.createAtomNode(NodeKind.Keyword, token, this.stringValue(token));
            case TokenType.Boolean:
                return this.createAtomNode(NodeKind.Boolean, token, this.booleanValue(token));
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
    async parseSequenceNode(config) {
        const { kind, open, closing, structure, unterminatedCode, spanStart } = config;
        const elements = [];
        while (!(await this.check(closing)) && !(await this.isAtEnd())) {
            const element = await this.parseExpression();
            if (element) {
                elements.push(element);
            }
        }
        const close = await this.consumeClosing(closing, open, structure, unterminatedCode);
        const startRef = (spanStart ?? open);
        const lastElement = elements.length > 0
            ? elements[elements.length - 1]
            : null;
        const endRef = close ?? lastElement ?? open;
        return {
            kind,
            elements,
            span: this.spanFromRefs(startRef, endRef),
        };
    }
    promoteListNode(node) {
        const head = node.elements[0];
        if (!head || head.kind !== NodeKind.Symbol) {
            return node;
        }
        if (head.value === "quote" && node.elements.length <= 2) {
            return this.createQuoteNode(node);
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
            };
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
            };
        }
        return node;
    }
    createQuoteNode(node) {
        const target = node.elements[1] ?? null;
        if (!target) {
            this.report("Reader macro requires a following form", node.span.start, node.span.end, "PARSE_MACRO_MISSING_TARGET");
        }
        return {
            kind: NodeKind.Quote,
            target,
            span: node.span,
        };
    }
    createAtomNode(kind, token, value) {
        return {
            kind,
            span: token.span,
            lexeme: token.lexeme,
            value,
        };
    }
    async consumeClosing(expected, open, structure, code) {
        if (await this.check(expected)) {
            return this.advance();
        }
        this.report(`Expected ${this.describeToken(expected)} to close ${structure}`, open.span.start, open.span.end, code);
        return null;
    }
    reportMapArity(anchor) {
        this.diagnostics.push({
            message: "Map literal support removed",
            span: this.spanFromRefs(anchor, anchor),
            severity: DiagnosticSeverity.Error,
            code: "PARSE_MAP_UNSUPPORTED",
        });
    }
    reportUnexpectedClosing(token) {
        this.diagnostics.push({
            message: `Unexpected ${this.describeToken(token.kind)} without a matching opener`,
            span: token.span,
            severity: DiagnosticSeverity.Error,
            code: "PARSE_UNEXPECTED_CLOSING",
        });
    }
    reportUnhandledToken(token) {
        this.diagnostics.push({
            message: `Unhandled token ${token.kind} in parser`,
            span: token.span,
            severity: DiagnosticSeverity.Error,
            code: "PARSE_UNSUPPORTED_TOKEN",
        });
    }
    report(message, start, end, code) {
        this.diagnostics.push({
            message,
            span: createSpan(start, end),
            severity: DiagnosticSeverity.Error,
            code,
        });
    }
    spanFromRefs(startRef, endRef) {
        const start = this.resolveStart(startRef);
        const end = this.resolveEnd(endRef ?? startRef ?? null);
        return createSpan(start, end);
    }
    resolveStart(ref) {
        if (ref) {
            return ref.span.start;
        }
        return this.origin;
    }
    resolveEnd(ref) {
        if (ref) {
            return ref.span.end;
        }
        if (this.lastToken) {
            return this.lastToken.span.end;
        }
        return this.origin;
    }
    describeToken(kind) {
        switch (kind) {
            case TokenType.LeftParen:
                return "(";
            case TokenType.RightParen:
                return ")";
            case TokenType.LeftBracket:
                return "[";
            case TokenType.RightBracket:
                return "]";
            default:
                return kind;
        }
    }
    async isAtEnd() {
        await this.ensureLookahead();
        return this.lookahead === null && this.done;
    }
    async check(kind) {
        const token = await this.peek();
        return token?.kind === kind;
    }
    async advance() {
        await this.ensureLookahead();
        if (!this.lookahead) {
            return null;
        }
        const token = this.lookahead;
        this.lookahead = null;
        this.lastToken = token;
        return token;
    }
    async peek() {
        await this.ensureLookahead();
        return this.lookahead;
    }
    async ensureLookahead() {
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
    booleanValue(token) {
        return token.value === true;
    }
    stringValue(token) {
        return typeof token.value === "string" ? token.value : token.lexeme;
    }
    numberValue(token) {
        if (typeof token.value === "number" && Number.isFinite(token.value)) {
            return token.value;
        }
        const parsed = Number(token.lexeme);
        return Number.isNaN(parsed) ? 0 : parsed;
    }
    async isExpressionTerminator() {
        const token = await this.peek();
        if (!token) {
            return true;
        }
        return (token.kind === TokenType.RightParen ||
            token.kind === TokenType.RightBracket ||
            false);
    }
}
class ScopeAnnotator {
    nextScopeId = 0;
    annotate(program) {
        this.nextScopeId = 0;
        const rootScopeId = this.allocateScopeId();
        this.assignScope(program, rootScopeId);
        for (const expr of program.body) {
            if (expr) {
                this.annotateExpression(expr, rootScopeId);
            }
        }
    }
    annotateExpression(node, scopeId) {
        this.assignScope(node, scopeId);
        switch (node.kind) {
            case NodeKind.List:
                this.annotateList(node, scopeId);
                break;
            case NodeKind.NamespaceImport:
                this.annotateNamespaceImport(node, scopeId);
                break;
            case NodeKind.Quote:
                this.annotateReaderMacro(node, scopeId);
                break;
            default:
                break;
        }
    }
    annotateList(node, scopeId) {
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
    annotateNamespaceImport(node, scopeId) {
        for (const element of node.elements) {
            if (element) {
                this.annotateExpression(element, scopeId);
            }
        }
    }
    annotateLet(node, parentScopeId) {
        const bindingsNode = node.elements[1];
        if (!bindingsNode || bindingsNode.kind !== NodeKind.List) {
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
    annotateFn(node, parentScopeId) {
        const clauseNodes = this.extractFnClauses(node);
        if (clauseNodes.length > 0) {
            this.annotateFnClauses(clauseNodes, parentScopeId);
            return;
        }
        const fnScopeId = this.allocateScopeId();
        const paramsNode = node.elements[1];
        if (paramsNode && paramsNode.kind === NodeKind.List) {
            this.assignScope(paramsNode, parentScopeId);
            for (const param of paramsNode.elements) {
                if (param) {
                    this.annotateExpression(param, fnScopeId);
                }
            }
        }
        else if (paramsNode) {
            this.annotateExpression(paramsNode, parentScopeId);
        }
        for (let index = 2; index < node.elements.length; index += 1) {
            const element = node.elements[index];
            if (element) {
                this.annotateExpression(element, fnScopeId);
            }
        }
    }
    annotateTry(node, parentScopeId) {
        const tail = node.elements.slice(1).filter(Boolean);
        const catchClause = this.findTryClause(tail, "catch");
        const finallyClause = this.findTryClause(tail, "finally");
        for (const expr of tail) {
            if ((catchClause && expr === catchClause) ||
                (finallyClause && expr === finallyClause)) {
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
    findTryClause(nodes, kind) {
        for (const node of nodes) {
            if (node.kind !== NodeKind.List) {
                continue;
            }
            const clauseHead = node.elements[0];
            if (clauseHead &&
                clauseHead.kind === NodeKind.Symbol &&
                clauseHead.value === kind) {
                return node;
            }
        }
        return null;
    }
    extractFnClauses(node) {
        const tail = node.elements.slice(1).filter(Boolean);
        if (tail.length === 0) {
            return [];
        }
        const clauseLists = tail.filter((element) => element.kind === NodeKind.List);
        if (clauseLists.length === tail.length) {
            return clauseLists;
        }
        return [];
    }
    annotateFnClauses(clauses, parentScopeId) {
        for (const clause of clauses) {
            this.assignScope(clause, parentScopeId);
            const clauseScopeId = this.allocateScopeId();
            const paramsNode = clause.elements[0];
            if (paramsNode && paramsNode.kind === NodeKind.List) {
                this.assignScope(paramsNode, parentScopeId);
                for (const param of paramsNode.elements) {
                    if (param) {
                        this.annotateExpression(param, clauseScopeId);
                    }
                }
            }
            else if (paramsNode) {
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
    /* Map annotation removed */
    annotateReaderMacro(node, scopeId) {
        if (node.target) {
            this.annotateExpression(node.target, scopeId);
        }
    }
    assignScope(node, scopeId) {
        node.scopeId = scopeId;
    }
    allocateScopeId() {
        return `scope_${this.nextScopeId++}`;
    }
}
//# sourceMappingURL=parser.js.map