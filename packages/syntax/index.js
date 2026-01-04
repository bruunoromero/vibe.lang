export var TokenType;
(function (TokenType) {
    TokenType["LeftParen"] = "left_paren";
    TokenType["RightParen"] = "right_paren";
    TokenType["LeftBracket"] = "left_bracket";
    TokenType["RightBracket"] = "right_bracket";
    TokenType["Number"] = "number";
    TokenType["String"] = "string";
    TokenType["Symbol"] = "symbol";
    TokenType["Keyword"] = "keyword";
    TokenType["Boolean"] = "boolean";
    TokenType["Nil"] = "nil";
})(TokenType || (TokenType = {}));
export var DiagnosticSeverity;
(function (DiagnosticSeverity) {
    DiagnosticSeverity["Info"] = "info";
    DiagnosticSeverity["Warning"] = "warning";
    DiagnosticSeverity["Error"] = "error";
})(DiagnosticSeverity || (DiagnosticSeverity = {}));
export const ok = (value) => ({ ok: true, value });
export const err = (error) => ({ ok: false, error });
export const createPosition = (offset, line, column) => ({
    offset,
    line,
    column,
});
export const clonePosition = (position) => ({
    offset: position.offset,
    line: position.line,
    column: position.column,
});
export const createSpan = (start, end) => ({
    start,
    end,
});
export var NodeKind;
(function (NodeKind) {
    NodeKind["Program"] = "program";
    NodeKind["List"] = "list";
    NodeKind["Quote"] = "quote";
    NodeKind["Symbol"] = "symbol";
    NodeKind["Keyword"] = "keyword";
    NodeKind["Number"] = "number";
    NodeKind["String"] = "string";
    NodeKind["Boolean"] = "boolean";
    NodeKind["Nil"] = "nil";
    NodeKind["NamespaceImport"] = "namespace_import";
})(NodeKind || (NodeKind = {}));
export const BUILTIN_SYMBOLS = [
    // Special forms (required even without user-provided definitions)
    "def",
    "defp",
    "macro+",
    "let",
    "fn+",
    "if",
    "quote",
    "try",
    "throw",
    "require",
    "external",
    "import",
    "spread",
    "unquote",
];
export * from "./destructuring";
//# sourceMappingURL=index.js.map