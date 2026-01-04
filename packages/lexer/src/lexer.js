import { DiagnosticSeverity, TokenType, clonePosition, createSpan, } from "@vibe/syntax";
import { CharacterStream } from "./stream";
export class Lexer {
    stream;
    options;
    onToken;
    diagnostics = [];
    offset = 0;
    line = 0;
    column = 0;
    history = "";
    historyStartOffset = 0;
    previousWasCarriageReturn = false;
    constructor(source, options = {}, config = {}) {
        this.stream = new CharacterStream(source);
        this.options = {
            allowShebang: options.allowShebang ?? true,
        };
        this.onToken = config.onToken;
    }
    async lex() {
        await this.skipBom();
        await this.skipShebang();
        this.resetHistory();
        while (true) {
            await this.skipWhitespace();
            this.resetHistory();
            if (await this.isAtEnd()) {
                break;
            }
            const start = this.snapshot();
            const ch = await this.peek();
            if (ch === ";") {
                await this.skipComment();
                continue;
            }
            switch (ch) {
                case "(":
                    await this.advance();
                    this.addToken(TokenType.LeftParen, start);
                    continue;
                case ")":
                    await this.advance();
                    this.addToken(TokenType.RightParen, start);
                    continue;
                case "[":
                    await this.advance();
                    this.addToken(TokenType.LeftBracket, start);
                    continue;
                case "]":
                    await this.advance();
                    this.addToken(TokenType.RightBracket, start);
                    continue;
                /* brace tokens removed */
                // '#' reader-dispatch removed; preserve '#!' shebang handling elsewhere
                case '"':
                    await this.readString(start);
                    continue;
                default:
                case "]":
            }
            if (await this.isNumberStart(ch)) {
                await this.readNumber(start);
                continue;
            }
            if (ch === ":") {
                await this.readKeyword(start);
                continue;
            }
            if (this.isSymbolStart(ch)) {
                await this.readSymbol(start);
                continue;
            }
            await this.advance();
            this.report(`Unexpected character '${ch}'`, start, this.snapshot());
            this.resetHistory();
        }
        return {
            ok: this.diagnostics.length === 0,
            diagnostics: this.diagnostics,
        };
    }
    async skipBom() {
        if (this.offset !== 0) {
            return;
        }
        if ((await this.peek()) === "\uFEFF") {
            await this.advance();
        }
    }
    async skipShebang() {
        if (!this.options.allowShebang || this.offset !== 0) {
            return;
        }
        if ((await this.peek()) === "#" && (await this.peek(1)) === "!") {
            while (!(await this.isAtEnd()) &&
                !this.isLineTerminator(await this.peek())) {
                await this.advance();
            }
            if (!(await this.isAtEnd())) {
                await this.advance();
            }
        }
    }
    async skipWhitespace() {
        while (true) {
            const ch = await this.peek();
            if (ch === "" || (ch !== "," && !this.isWhitespace(ch))) {
                break;
            }
            await this.advance();
        }
    }
    async skipComment() {
        await this.advance();
        while (!(await this.isAtEnd()) &&
            !this.isLineTerminator(await this.peek())) {
            await this.advance();
        }
        if (!(await this.isAtEnd())) {
            await this.advance();
        }
        this.resetHistory();
    }
    async readString(start) {
        await this.advance();
        let value = "";
        while (!(await this.isAtEnd())) {
            const ch = await this.peek();
            if (ch === '"') {
                await this.advance();
                this.addToken(TokenType.String, start, value);
                return;
            }
            if (ch === "\\") {
                await this.advance();
                if (await this.isAtEnd()) {
                    break;
                }
                value += await this.readEscapeSequence(start);
                continue;
            }
            if (this.isLineTerminator(ch)) {
                this.report("Unterminated string literal", start, this.snapshot(), "LEX_STRING_UNTERMINATED");
                await this.advance();
                this.addToken(TokenType.String, start, value);
                return;
            }
            value += await this.advance();
        }
        this.report("Unterminated string literal", start, this.snapshot(), "LEX_STRING_UNTERMINATED");
        this.addToken(TokenType.String, start, value);
    }
    async readEscapeSequence(start) {
        const ch = await this.advance();
        switch (ch) {
            case '"':
                return '"';
            case "'":
                return "'";
            case "\\":
                return "\\";
            case "n":
                return "\n";
            case "r":
                return "\r";
            case "t":
                return "\t";
            case "b":
                return "\b";
            case "f":
                return "\f";
            case "u": {
                let hex = "";
                for (let i = 0; i < 4; i += 1) {
                    const next = await this.peek();
                    if (!this.isHexDigit(next)) {
                        this.report("Invalid unicode escape sequence", start, this.snapshot(), "LEX_STRING_ESCAPE");
                        break;
                    }
                    hex += await this.advance();
                }
                const codePoint = Number.parseInt(hex, 16);
                if (Number.isNaN(codePoint)) {
                    this.report("Invalid unicode escape sequence", start, this.snapshot(), "LEX_STRING_ESCAPE");
                    return "";
                }
                return String.fromCodePoint(codePoint);
            }
            default:
                this.report(`Unknown escape sequence \\${ch}`, start, this.snapshot(), "LEX_STRING_ESCAPE");
                return ch ?? "";
        }
    }
    async readNumber(start) {
        if ((await this.peek()) === "+" || (await this.peek()) === "-") {
            await this.advance();
        }
        await this.consumeDigits();
        if ((await this.peek()) === "." && this.isDigit(await this.peek(1))) {
            await this.advance();
            await this.consumeDigits();
        }
        if (await this.isExponentStart(await this.peek(), await this.peek(1), await this.peek(2))) {
            await this.advance();
            if ((await this.peek()) === "+" || (await this.peek()) === "-") {
                await this.advance();
            }
            await this.consumeDigits();
        }
        const lexeme = this.sliceFrom(start);
        const value = Number(lexeme);
        if (Number.isNaN(value)) {
            this.report("Invalid numeric literal", start, this.snapshot(), "LEX_NUMBER_INVALID");
            return;
        }
        this.addToken(TokenType.Number, start, value);
    }
    async readKeyword(start) {
        await this.advance();
        let doubleColon = false;
        if ((await this.peek()) === ":") {
            doubleColon = true;
            await this.advance();
        }
        if (!this.isSymbolChar(await this.peek())) {
            this.report("Invalid keyword literal", start, this.snapshot(), "LEX_KEYWORD_INVALID");
            return;
        }
        while (this.isSymbolChar(await this.peek())) {
            await this.advance();
        }
        const lexeme = this.sliceFrom(start);
        const value = doubleColon ? lexeme.slice(2) : lexeme.slice(1);
        this.addToken(TokenType.Keyword, start, value);
    }
    async readSymbol(start) {
        let length = 0;
        while (this.isSymbolChar(await this.peek())) {
            await this.advance();
            length += 1;
        }
        const lexeme = this.sliceFrom(start);
        const normalized = lexeme.toLowerCase();
        if (normalized === "nil") {
            this.addToken(TokenType.Nil, start, null);
            return;
        }
        if (normalized === "true" || normalized === "false") {
            this.addToken(TokenType.Boolean, start, normalized === "true");
            return;
        }
        this.addToken(TokenType.Symbol, start, lexeme);
    }
    async consumeDigits() {
        while (this.isDigit(await this.peek())) {
            await this.advance();
        }
    }
    sliceFrom(start) {
        const relativeStart = start.offset - this.historyStartOffset;
        const relativeEnd = this.offset - this.historyStartOffset;
        if (relativeStart < 0 || relativeEnd < relativeStart) {
            return "";
        }
        return this.history.slice(relativeStart, relativeEnd);
    }
    addToken(kind, start, value) {
        const span = createSpan(start, this.snapshot());
        const lexeme = this.sliceFrom(start);
        const token = { kind, lexeme, span };
        if (value !== undefined) {
            token.value = value;
        }
        if (this.onToken) {
            this.onToken(token);
        }
        this.resetHistory();
    }
    report(message, start, end, code) {
        this.diagnostics.push({
            message,
            span: createSpan(start, end),
            severity: DiagnosticSeverity.Error,
            code,
        });
    }
    snapshot() {
        return clonePosition({
            offset: this.offset,
            line: this.line,
            column: this.column,
        });
    }
    async isAtEnd() {
        return (await this.peek()) === "";
    }
    peek(offset = 0) {
        return this.stream.peek(offset);
    }
    async advance() {
        const ch = await this.stream.advance();
        if (ch === "") {
            return "";
        }
        this.history += ch;
        this.offset += 1;
        if (ch === "\r") {
            this.line += 1;
            this.column = 0;
            this.previousWasCarriageReturn = true;
            return ch;
        }
        if (ch === "\n") {
            if (!this.previousWasCarriageReturn) {
                this.line += 1;
            }
            this.column = 0;
            this.previousWasCarriageReturn = false;
            return ch;
        }
        this.previousWasCarriageReturn = false;
        this.column += 1;
        return ch;
    }
    resetHistory() {
        this.history = "";
        this.historyStartOffset = this.offset;
    }
    isWhitespace(ch) {
        return (ch === " " ||
            ch === "\t" ||
            ch === "\n" ||
            ch === "\r" ||
            ch === "\v" ||
            ch === "\f");
    }
    isLineTerminator(ch) {
        return ch === "\n" || ch === "\r";
    }
    isDigit(ch) {
        return ch >= "0" && ch <= "9";
    }
    isHexDigit(ch) {
        return /[0-9a-fA-F]/.test(ch);
    }
    async isNumberStart(ch) {
        const next = await this.peek(1);
        if (this.isDigit(ch)) {
            return true;
        }
        if ((ch === "+" || ch === "-") &&
            (this.isDigit(next) || (next === "." && this.isDigit(await this.peek(2))))) {
            return true;
        }
        if (ch === "." && this.isDigit(next)) {
            return true;
        }
        return false;
    }
    async isExponentStart(current, next, afterNext) {
        if (current !== "e" && current !== "E") {
            return false;
        }
        if (this.isDigit(next)) {
            return true;
        }
        if ((next === "+" || next === "-") && this.isDigit(afterNext)) {
            return true;
        }
        return false;
    }
    isDelimiter(ch) {
        switch (ch) {
            case "":
            case "(":
            case ")":
            case "[":
            case "]":
            case "{":
            case "}":
            case '"':
            case "'":
            case "`":
            case "~":
            case "@":
            case "#":
            case ":":
            case "\\":
            case ";":
                return true;
            default:
                return this.isWhitespace(ch);
        }
    }
    isSymbolSuffix(ch) {
        // Allow Clojure-style suffixes: ?, !, *, +, -, =, <, >, /, #, etc.
        return /[?!*+\-=<>/#]/.test(ch);
    }
    isSymbolStart(ch) {
        if (!ch) {
            return false;
        }
        if (this.isDigit(ch)) {
            return false;
        }
        if (this.isDelimiter(ch)) {
            return false;
        }
        return true;
    }
    isSymbolChar(ch) {
        if (!ch) {
            return false;
        }
        if (this.isDelimiter(ch)) {
            return false;
        }
        // Allow special suffix characters: ?, !, *, etc.
        if (this.isSymbolSuffix(ch)) {
            return true;
        }
        return true;
    }
}
export const tokenizeStream = (source, options) => {
    const queue = [];
    let wake = null;
    let finished = false;
    const pushToken = (token) => {
        queue.push(token);
        if (wake) {
            wake();
            wake = null;
        }
    };
    const lexer = new Lexer(source, options, {
        onToken: pushToken,
    });
    const resultPromise = lexer.lex().then(({ ok, diagnostics }) => {
        finished = true;
        if (wake) {
            wake();
            wake = null;
        }
        return { ok, diagnostics };
    });
    const iterator = (async function* () {
        while (true) {
            if (queue.length > 0) {
                yield queue.shift();
                continue;
            }
            if (finished) {
                break;
            }
            await new Promise((resolve) => {
                wake = resolve;
            });
        }
    })();
    const tokenStream = Object.assign(iterator, {
        result: resultPromise,
    });
    return tokenStream;
};
//# sourceMappingURL=lexer.js.map