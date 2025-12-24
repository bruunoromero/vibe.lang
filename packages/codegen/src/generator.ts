import { Buffer } from "node:buffer";
import {
  NodeKind,
  type Diagnostic,
  type ExpressionNode,
  type ListNode,
  type MapNode,
  type NamespaceImportKind,
  type NamespaceImportNode,
  type ProgramNode,
  type SetNode,
  type SourceSpan,
  type StringNode,
  type SymbolNode,
  type VectorNode,
} from "@vibe/syntax";
import type {
  NodeId,
  NodeSymbolInfo,
  FlattenedImportBinding,
  ModuleImportRecord,
  ScopeId,
  SemanticGraph,
  SemanticNodeRecord,
  SymbolId,
  SymbolKind,
  SymbolRecord,
} from "@vibe/semantics";

const DEFAULT_SOURCE_NAME = "vibe-inline";
const DEFAULT_TARGET_FILE = "vibe-generated.js";
const LANG_EXTENSION = ".lang";

export interface GenerateModuleOptions {
  readonly includeAst?: boolean;
  readonly pretty?: number;
  readonly sourceName?: string;
  readonly sourceContent?: string;
  readonly targetFileName?: string;
}

export interface RawSourceMap {
  readonly version: number;
  readonly file: string;
  readonly sources: readonly string[];
  readonly sourcesContent: readonly (string | null)[];
  readonly names: readonly string[];
  readonly mappings: string;
}

export interface CodegenResult {
  readonly ok: boolean;
  readonly diagnostics: readonly Diagnostic[];
  readonly moduleText: string;
  readonly sourceMap: RawSourceMap;
  readonly ir: IrProgram;
}

export interface IrProgram {
  readonly entryScopeId: ScopeId | null;
  readonly scopes: readonly IrScopeSummary[];
  readonly symbols: readonly IrSymbolSummary[];
  readonly nodes: readonly IrNodeSummary[];
  readonly stats: {
    readonly scopeCount: number;
    readonly symbolCount: number;
    readonly nodeCount: number;
  };
}

export interface IrScopeSummary {
  readonly id: ScopeId;
  readonly parentId: ScopeId | null;
  readonly hygieneTag: string;
  readonly symbolIds: readonly SymbolId[];
}

export interface IrSymbolSummary {
  readonly id: SymbolId;
  readonly name: string;
  readonly kind: SymbolKind;
  readonly scopeId: ScopeId;
  readonly hygieneTag: string;
  readonly identifier: string | null;
}

export interface IrNodeSummary {
  readonly nodeId: NodeId;
  readonly kind: NodeKind;
  readonly scopeId: ScopeId;
  readonly hygieneTag: string;
  readonly symbol?: NodeSymbolInfo;
}

export const generateModule = (
  program: ProgramNode,
  graph: SemanticGraph,
  options: GenerateModuleOptions = {}
): CodegenResult => {
  const emitter = new ModuleEmitter(program, graph, {
    includeAst: options.includeAst ?? false,
    pretty: options.pretty ?? 2,
    sourceName: options.sourceName ?? DEFAULT_SOURCE_NAME,
    sourceContent: options.sourceContent,
    targetFileName: options.targetFileName ?? DEFAULT_TARGET_FILE,
  });

  const { code, sourceMap } = emitter.emit();
  const ir = buildIr(graph);
  const inlineMap = Buffer.from(JSON.stringify(sourceMap)).toString("base64");
  const moduleText = `${code}\n//# sourceMappingURL=data:application/json;base64,${inlineMap}`;

  return {
    ok: true,
    diagnostics: [],
    ir,
    moduleText,
    sourceMap,
  };
};

const buildIr = (graph: SemanticGraph): IrProgram => {
  const scopes: IrScopeSummary[] = graph.scopes.map((scope) => ({
    id: scope.id,
    parentId: scope.parentId,
    hygieneTag: scope.hygieneTag,
    symbolIds: [...scope.symbols],
  }));

  const symbols: IrSymbolSummary[] = graph.symbols.map((symbol) => ({
    id: symbol.id,
    name: symbol.name,
    kind: symbol.kind,
    scopeId: symbol.scopeId,
    hygieneTag: symbol.hygieneTag,
    identifier: symbol.alias,
  }));

  const nodes: IrNodeSummary[] = graph.nodes.map((node) => ({
    nodeId: node.nodeId,
    kind: node.kind,
    scopeId: node.scopeId,
    hygieneTag: node.hygieneTag,
    ...(node.symbol ? { symbol: node.symbol } : {}),
  }));

  return {
    entryScopeId: scopes[0]?.id ?? null,
    scopes,
    symbols,
    nodes,
    stats: {
      scopeCount: scopes.length,
      symbolCount: symbols.length,
      nodeCount: nodes.length,
    },
  };
};

interface ModuleEmitterOptions {
  readonly includeAst: boolean;
  readonly pretty: number;
  readonly sourceName: string;
  readonly sourceContent?: string;
  readonly targetFileName: string;
}

interface NamespaceImportSpec {
  readonly kind: NamespaceImportKind;
  readonly aliasNode?: SymbolNode | null;
  readonly aliasIdentifier: string;
  readonly importPath: string;
  readonly sourceSpan: SourceSpan;
  readonly statementNode: ExpressionNode;
  readonly flatten?: readonly FlattenedImportBinding[];
}

class ModuleEmitter {
  private readonly lines: string[] = [];
  private readonly mapBuilder: SourceMapBuilder;
  private readonly namespaceImports: NamespaceImportSpec[] = [];
  private readonly namespaceImportMap = new Map<
    ExpressionNode,
    NamespaceImportSpec
  >();
  private readonly namespaceAliasMap = new Map<string, NamespaceImportSpec>();
  private readonly reservedNamespaceAliases = new Set<string>();
  private anonymousImportCounter = 0;
  private readonly moduleImportIndex = new Map<string, ModuleImportRecord>();
  private readonly semanticLookup: SemanticLookup;

  constructor(
    private readonly program: ProgramNode,
    graph: SemanticGraph,
    private readonly options: ModuleEmitterOptions
  ) {
    this.semanticLookup = new SemanticLookup(graph);
    for (const record of graph.imports) {
      this.moduleImportIndex.set(this.spanKey(record.span), record);
    }
    this.mapBuilder = new SourceMapBuilder(
      options.targetFileName,
      options.sourceName,
      options.sourceContent ?? null
    );
    this.collectNamespaceImports();
  }

  private collectNamespaceImports(): void {
    for (const expr of this.program.body) {
      const spec = this.parseNamespaceImport(expr);
      if (spec) {
        this.namespaceImports.push(spec);
        this.namespaceImportMap.set(spec.statementNode, spec);
        if (spec.aliasNode) {
          this.reserveNamespaceAlias(spec.aliasIdentifier);
        }
        if (spec.aliasNode) {
          this.namespaceAliasMap.set(spec.aliasNode.value, spec);
        }
      }
    }
  }

  private reserveNamespaceAlias(alias: string): void {
    this.reservedNamespaceAliases.add(alias);
  }

  private lookupModuleImport(span: SourceSpan): ModuleImportRecord | undefined {
    return this.moduleImportIndex.get(this.spanKey(span));
  }

  private spanKey(span: SourceSpan): string {
    return `${span.start.offset}:${span.end.offset}`;
  }

  private allocateAnonymousImportAlias(): string {
    while (true) {
      const suffix =
        this.anonymousImportCounter === 0
          ? ""
          : `_${this.anonymousImportCounter}`;
      this.anonymousImportCounter += 1;
      const candidate = `__import__${suffix}`;
      if (this.reservedNamespaceAliases.has(candidate)) {
        continue;
      }
      this.reservedNamespaceAliases.add(candidate);
      return candidate;
    }
  }

  private parseNamespaceImport(
    node: ExpressionNode
  ): NamespaceImportSpec | null {
    if (!node) {
      return null;
    }

    if (node.kind === NodeKind.NamespaceImport) {
      const targetNode = node.source;
      if (!targetNode || targetNode.kind !== NodeKind.String) {
        return null;
      }
      if (node.importKind === "import") {
        const record = this.lookupModuleImport(targetNode.span);
        if (!record) {
          throw new Error(
            `Missing module import metadata for import form (${targetNode.value})`
          );
        }
        const aliasIdentifier =
          record.alias && record.alias.length > 0
            ? record.alias
            : this.allocateAnonymousImportAlias();
        return {
          kind: "import",
          aliasNode: null,
          aliasIdentifier,
          importPath: this.resolveNamespaceImportPath(
            "import",
            record.specifier,
            record
          ),
          sourceSpan: targetNode.span,
          statementNode: node,
          flatten: record.flatten ?? [],
        } satisfies NamespaceImportSpec;
      }
      const aliasNode = node.alias;
      if (!aliasNode || aliasNode.kind !== NodeKind.Symbol) {
        return null;
      }
      return this.createNamespaceImportSpec(
        node.importKind,
        aliasNode,
        targetNode,
        node
      );
    }

    if (node.kind !== NodeKind.List) {
      return null;
    }
    const head = node.elements[0];
    if (!head || head.kind !== NodeKind.Symbol) {
      return null;
    }
    if (head.value !== "require" && head.value !== "external") {
      return null;
    }
    const aliasNode = node.elements[1];
    const targetNode = node.elements[2];
    if (!aliasNode || aliasNode.kind !== NodeKind.Symbol) {
      return null;
    }
    if (!targetNode || targetNode.kind !== NodeKind.String) {
      throw new Error(
        `${head.value} currently requires a string literal argument`
      );
    }
    const kind = head.value as NamespaceImportKind;
    return this.createNamespaceImportSpec(kind, aliasNode, targetNode, node);
  }

  private createNamespaceImportSpec(
    kind: NamespaceImportKind,
    aliasNode: SymbolNode,
    targetNode: StringNode,
    statementNode: ExpressionNode,
    importRecord?: ModuleImportRecord
  ): NamespaceImportSpec {
    const aliasIdentifier = this.resolveBindingIdentifier(aliasNode);
    const record = importRecord ?? this.lookupModuleImport(targetNode.span);
    if (!record) {
      throw new Error(
        `Missing module import metadata for ${aliasNode.value} (${targetNode.value})`
      );
    }
    const specifier = record?.specifier ?? targetNode.value;
    const importPath = this.resolveNamespaceImportPath(kind, specifier, record);
    return {
      kind,
      aliasNode,
      aliasIdentifier,
      importPath,
      sourceSpan: targetNode.span,
      statementNode,
    } satisfies NamespaceImportSpec;
  }

  private resolveNamespaceImportPath(
    kind: NamespaceImportKind,
    specifier: string,
    importRecord?: ModuleImportRecord
  ): string {
    if (kind !== "require" && kind !== "import") {
      return specifier;
    }
    return this.rewriteRequireSpecifier(specifier, importRecord);
  }

  private rewriteRequireSpecifier(
    specifier: string,
    importRecord?: ModuleImportRecord
  ): string {
    if (specifier.endsWith(".js")) {
      return specifier;
    }
    if (isRelativeModuleSpecifier(specifier)) {
      return this.ensureJsExtension(specifier);
    }
    if (specifier.endsWith(LANG_EXTENSION)) {
      return this.stripLangExtension(specifier);
    }
    if (
      importRecord?.moduleId?.endsWith(LANG_EXTENSION) &&
      importRecord.specifier.endsWith(LANG_EXTENSION)
    ) {
      return this.stripLangExtension(specifier);
    }
    return specifier;
  }

  private ensureJsExtension(value: string): string {
    if (value.endsWith(".js")) {
      return value;
    }
    if (value.endsWith(LANG_EXTENSION)) {
      return this.stripLangExtension(value);
    }
    return `${value}.js`;
  }

  private stripLangExtension(value: string): string {
    return `${value.slice(0, -LANG_EXTENSION.length)}.js`;
  }

  emit(): { code: string; sourceMap: RawSourceMap } {
    this.addLine("// Generated by @vibe/codegen");
    this.emitImports();

    if (this.options.includeAst) {
      this.addLine(
        `export const ast = ${JSON.stringify(
          this.program,
          null,
          this.options.pretty
        )};`
      );
      this.addLine("");
    }

    this.emitPrelude();

    for (const expr of this.program.body) {
      if (!expr) {
        continue;
      }
      if (this.isNamespaceImport(expr)) {
        const spec = this.namespaceImportMap.get(expr);
        if (spec) {
          this.emitNamespaceDefinition(spec);
        }
        continue;
      }
      if (this.isTopLevelDef(expr)) {
        this.emitTopLevelDef(expr);
        continue;
      }
      this.addLine(`__result = ${this.emitExpression(expr)};`, expr.span);
    }

    return {
      code: this.lines.join("\n"),
      sourceMap: this.mapBuilder.toJSON(),
    };
  }

  private emitImports(): void {
    // Emit all namespace imports from (require) and (external) statements
    for (const spec of this.namespaceImports) {
      const importLiteral = JSON.stringify(spec.importPath);
      this.addLine(
        `import * as ${spec.aliasIdentifier} from ${importLiteral};`,
        spec.sourceSpan
      );
    }
    if (this.namespaceImports.length > 0) {
      this.addLine("");
    }
  }

  private emitPrelude(): void {
    this.addLine("let __result = null;");
    this.addLine("");
  }

  private emitTopLevelDef(node: ListNode): void {
    const nameNode = node.elements[1];
    const valueNode = node.elements[2];
    if (!nameNode || nameNode.kind !== NodeKind.Symbol) {
      throw new Error("def requires a symbol name");
    }
    const identifier = this.resolveBindingIdentifier(nameNode);
    const valueExpr = valueNode ? this.emitExpression(valueNode) : "null";
    this.addLine(`export const ${identifier} = ${valueExpr};`, node.span);
    this.addLine(`__result = ${identifier};`);
  }

  private emitNamespaceDefinition(spec: NamespaceImportSpec): void {
    if (spec.kind === "import") {
      if (spec.flatten && spec.flatten.length > 0) {
        for (const binding of spec.flatten) {
          const access = this.emitNamespaceMemberAccess(
            spec.aliasIdentifier,
            binding.exportedName,
            spec.kind
          );
          this.addLine(
            `const ${binding.identifier} = ${access};`,
            spec.statementNode.span
          );
          this.addLine(
            `__result = ${binding.identifier};`,
            spec.statementNode.span
          );
        }
      } else {
        this.addLine(
          `__result = ${spec.aliasIdentifier};`,
          spec.statementNode.span
        );
      }
      return;
    }
    const span = spec.aliasNode?.span ?? spec.statementNode.span;
    this.addLine(`__result = ${spec.aliasIdentifier};`, span);
  }

  private isNamespaceImport(node: ExpressionNode): boolean {
    return this.namespaceImportMap.has(node);
  }

  private emitExpression(node: ExpressionNode): string {
    switch (node.kind) {
      case NodeKind.Number:
      case NodeKind.String:
      case NodeKind.Character:
      case NodeKind.Keyword:
        return JSON.stringify(node.value);
      case NodeKind.Boolean:
        return node.value ? "true" : "false";
      case NodeKind.Nil:
        return "null";
      case NodeKind.Symbol:
        return this.emitSymbol(node);
      case NodeKind.List:
        return this.emitListExpression(node);
      case NodeKind.Vector: {
        const elements = node.elements
          .map((element) => (element ? this.emitExpression(element) : "null"))
          .join(", ");
        return `[${elements}]`;
      }
      case NodeKind.Set:
        return `new Set([${node.elements
          .map((element) => (element ? this.emitExpression(element) : "null"))
          .join(", ")}])`;
      case NodeKind.Map:
        return this.emitMap(node);
      default:
        return this.unsupported(node);
    }
  }

  private emitSymbol(node: SymbolNode): string {
    const symbolRecord = this.semanticLookup.getSymbolRecord(node);
    const namespaceAccess = this.parseNamespaceSymbol(node.value);
    if (namespaceAccess) {
      const baseIdentifier = symbolRecord?.alias
        ? symbolRecord.alias
        : this.fallbackIdentifier(namespaceAccess.alias);
      const member = this.normalizeNamespaceMember(
        namespaceAccess.alias,
        namespaceAccess.member
      );
      return this.emitPropertyAccess(baseIdentifier, member);
    }
    if (!symbolRecord) {
      return this.fallbackIdentifier(node.value);
    }

    if (symbolRecord.kind === "builtin") {
      return this.emitBuiltinSymbol(node, symbolRecord);
    }

    if (symbolRecord.kind === "macro") {
      throw new Error(`Cannot reference macro ${symbolRecord.name} at runtime`);
    }

    return symbolRecord.alias;
  }

  private emitBuiltinSymbol(node: SymbolNode, symbol: SymbolRecord): string {
    if (symbol.name === "println") {
      return "console.log";
    }
    if (this.isArithmeticOperator(symbol.name)) {
      throw new Error(
        `Cannot reference arithmetic operator ${symbol.name} as a value yet`
      );
    }
    throw new Error(`Cannot reference builtin ${node.value} as a value yet`);
  }

  private emitListExpression(node: ListNode): string {
    const [head, ...tail] = node.elements;
    if (!head) {
      return "null";
    }

    if (head.kind === NodeKind.Symbol) {
      switch (head.value) {
        case "def":
          throw new Error("def is only supported at the top level");
        case "let":
          return this.emitLet(node);
        case "fn":
          return this.emitFn(node);
        case "if":
          return this.emitIf(tail);
        case "get":
          return this.emitGet(node);
        case "+":
        case "-":
        case "*":
        case "/":
          return this.emitArithmetic(head.value, tail);
        default:
          break;
      }
    }

    return this.emitInvocation(head, tail);
  }

  private parseNamespaceSymbol(
    value: string
  ): { alias: string; member: string } | null {
    const slashIndex = value.indexOf("/");
    if (slashIndex <= 0 || slashIndex === value.length - 1) {
      return null;
    }
    const alias = value.slice(0, slashIndex);
    const member = value.slice(slashIndex + 1);
    if (!alias || !member) {
      return null;
    }
    return { alias, member };
  }

  private emitPropertyAccess(base: string, member: string): string {
    if (this.isIdentifier(member)) {
      return `${base}.${member}`;
    }
    return `${base}[${JSON.stringify(member)}]`;
  }

  private normalizeNamespaceMember(alias: string, member: string): string {
    const spec = this.namespaceAliasMap.get(alias);
    if (spec?.kind === "external") {
      return sanitizeExternalIdentifier(member);
    }
    return member;
  }

  private isIdentifier(value: string): boolean {
    return /^[A-Za-z_$][A-Za-z0-9_$]*$/.test(value);
  }

  private emitInvocation(
    head: ExpressionNode,
    args: readonly (ExpressionNode | null | undefined)[]
  ): string {
    const callee = this.emitExpression(head);
    const argList = args
      .filter((arg): arg is ExpressionNode => Boolean(arg))
      .map((arg) => this.emitExpression(arg))
      .join(", ");
    return `${callee}(${argList})`;
  }

  private emitLet(node: ListNode): string {
    const bindingsNode = node.elements[1];
    if (!bindingsNode || bindingsNode.kind !== NodeKind.Vector) {
      throw new Error("let requires a vector of bindings");
    }

    const declarations: string[] = [];
    for (let index = 0; index < bindingsNode.elements.length; index += 2) {
      const nameNode = bindingsNode.elements[index];
      const initNode = bindingsNode.elements[index + 1];
      if (!nameNode || nameNode.kind !== NodeKind.Symbol || !initNode) {
        continue;
      }
      const identifier = this.resolveBindingIdentifier(nameNode);
      const initExpr = this.emitExpression(initNode);
      declarations.push(`const ${identifier} = ${initExpr};`);
    }

    const body = node.elements.slice(2).filter(Boolean) as ExpressionNode[];
    const bodyStatements = this.buildSequenceStatements(body);
    const blockLines = [...declarations, ...bodyStatements];
    const block = this.indentBlock(blockLines);
    return `(() => {\n${block}\n})()`;
  }

  private emitFn(node: ListNode): string {
    const paramsNode = node.elements[1];
    if (!paramsNode || paramsNode.kind !== NodeKind.Vector) {
      throw new Error("fn requires a vector of parameter symbols");
    }

    const params: string[] = [];
    let restParam: string | undefined = undefined;
    let sawAmpersand = false;

    for (let i = 0; i < paramsNode.elements.length; i++) {
      const param = paramsNode.elements[i];
      if (!param || param.kind !== NodeKind.Symbol) {
        continue;
      }

      if (param.value === "&") {
        sawAmpersand = true;
        const nextParam = paramsNode.elements[i + 1];
        if (nextParam && nextParam.kind === NodeKind.Symbol) {
          restParam = this.resolveBindingIdentifier(nextParam);
          i++; // Skip the next parameter
        }
        continue;
      }

      if (!sawAmpersand) {
        params.push(this.resolveBindingIdentifier(param));
      }
    }

    const paramList = restParam
      ? [...params, `...${restParam}`].join(", ")
      : params.join(", ");

    const body = node.elements.slice(2).filter(Boolean) as ExpressionNode[];
    const bodyBlock = this.emitFunctionBody(body);
    return `(${paramList}) => {
${bodyBlock}
}`;
  }

  private emitGet(node: ListNode): string {
    const aliasNode = node.elements[1];
    const memberNode = node.elements[2];
    if (!aliasNode || aliasNode.kind !== NodeKind.Symbol) {
      throw new Error("get requires a namespace alias symbol");
    }
    if (!memberNode) {
      throw new Error("get requires a member expression");
    }
    const base = this.emitSymbol(aliasNode);
    const memberName = this.resolveMemberName(memberNode);
    return this.emitPropertyAccess(base, memberName);
  }

  private emitNamespaceMemberAccess(
    baseIdentifier: string,
    member: string,
    kind: NamespaceImportKind
  ): string {
    const normalized = this.normalizeNamespaceMemberAccess(member, kind);
    return this.emitPropertyAccess(baseIdentifier, normalized);
  }

  private normalizeNamespaceMemberAccess(
    member: string,
    kind: NamespaceImportKind
  ): string {
    if (kind === "external") {
      return sanitizeExternalIdentifier(member);
    }
    if (kind === "import") {
      return sanitizeImportedMemberName(member);
    }
    return member;
  }

  private emitIf(tail: readonly (ExpressionNode | null | undefined)[]): string {
    const [condNode, thenNode, elseNode] = tail;

    if (!condNode) {
      throw new Error("if requires a condition");
    }
    if (!thenNode) {
      throw new Error("if requires a then branch");
    }

    const cond = this.emitExpression(condNode);
    const thenExpr = this.emitExpression(thenNode);
    const elseExpr = elseNode ? this.emitExpression(elseNode) : "null";

    return `(${cond} ? ${thenExpr} : ${elseExpr})`;
  }

  private resolveMemberName(node: ExpressionNode): string {
    if (node.kind === NodeKind.Symbol || node.kind === NodeKind.String) {
      return node.value;
    }
    throw new Error("get member name must be a symbol or string literal");
  }

  private resolveBindingIdentifier(node: SymbolNode): string {
    const symbolRecord = this.semanticLookup.getSymbolRecord(node);
    if (!symbolRecord) {
      return this.fallbackIdentifier(node.value);
    }
    return symbolRecord.alias;
  }

  private emitArithmetic(
    operator: string,
    args: readonly (ExpressionNode | null | undefined)[]
  ): string {
    const values = args
      .filter((arg): arg is ExpressionNode => Boolean(arg))
      .map((arg) => this.emitExpression(arg));
    if (values.length === 0) {
      switch (operator) {
        case "+":
          return "0";
        case "*":
          return "1";
        case "-":
        case "/":
          return "0";
        default:
          return "0";
      }
    }
    if (operator === "-") {
      if (values.length === 1) {
        return `(-${values[0]})`;
      }
      return `(${values.join(" - ")})`;
    }
    if (operator === "/") {
      if (values.length === 1) {
        return `(1 / ${values[0]})`;
      }
      return `(${values.join(" / ")})`;
    }
    return `(${values.join(` ${operator} `)})`;
  }

  private emitMap(node: MapNode): string {
    const pairs = node.entries
      .map((entry) => {
        if (!entry.key || !entry.value) {
          return null;
        }
        const key = this.emitExpression(entry.key);
        const value = this.emitExpression(entry.value);
        return `[${key}, ${value}]`;
      })
      .filter(Boolean)
      .join(", ");
    return `new Map([${pairs}])`;
  }

  private buildSequenceStatements(
    expressions: readonly ExpressionNode[]
  ): string[] {
    if (expressions.length === 0) {
      return ["return null;"];
    }
    return expressions.map((expression, index) => {
      const code = this.emitExpression(expression);
      return index === expressions.length - 1 ? `return ${code};` : `${code};`;
    });
  }

  private emitFunctionBody(expressions: readonly ExpressionNode[]): string {
    const statements = this.buildSequenceStatements(expressions);
    return this.indentBlock(statements);
  }

  private indentBlock(lines: readonly string[], indent = 2): string {
    const prefix = " ".repeat(indent);
    return lines
      .map((line) => (line.length === 0 ? "" : `${prefix}${line}`))
      .join("\n");
  }

  private isTopLevelDef(node: ExpressionNode): node is ListNode {
    if (node.kind !== NodeKind.List) {
      return false;
    }
    const head = node.elements[0];
    return Boolean(
      head && head.kind === NodeKind.Symbol && head.value === "def"
    );
  }

  private fallbackIdentifier(name: string): string {
    let base = name.replace(/[^a-zA-Z0-9_]/g, "_").replace(/^([0-9])/, "_$1");
    if (base.length === 0) {
      base = "_ident";
    }
    if (RESERVED_IDENTIFIERS.has(base)) {
      return `_${base}`;
    }
    return base;
  }

  private isArithmeticOperator(name: string): boolean {
    return name === "+" || name === "-" || name === "*" || name === "/";
  }

  private unsupported(node: ExpressionNode): never {
    throw new Error(`Unsupported node kind in codegen: ${node.kind}`);
  }

  private addLine(line: string, span?: SourceSpan): void {
    const segments = line.split("\n");
    segments.forEach((segment, index) => {
      if (span && index === 0) {
        this.mapBuilder.addMapping({
          generatedLine: this.lines.length,
          generatedColumn: 0,
          originalLine: span.start.line,
          originalColumn: span.start.column,
        });
      }
      this.lines.push(segment);
    });
  }
}

class SemanticLookup {
  readonly entryScopeId: ScopeId | null;
  private readonly nodeIndex = new Map<string, SemanticNodeRecord>();
  private readonly symbolIndex = new Map<SymbolId, SymbolRecord>();

  constructor(graph: SemanticGraph) {
    this.entryScopeId = graph.scopes[0]?.id ?? null;
    for (const node of graph.nodes) {
      this.nodeIndex.set(this.keyFor(node.scopeId, node.kind, node.span), node);
    }
    for (const symbol of graph.symbols) {
      this.symbolIndex.set(symbol.id, symbol);
    }
  }

  getSymbolRecord(node: SymbolNode): SymbolRecord | null {
    const record = this.getNodeRecord(node);
    const symbolId = record?.symbol?.symbolId;
    if (!symbolId) {
      return null;
    }
    return this.symbolIndex.get(symbolId) ?? null;
  }

  private getNodeRecord(node: {
    kind: NodeKind;
    span: SourceSpan;
    scopeId?: ScopeId;
  }): SemanticNodeRecord | null {
    const scopeId = node.scopeId ?? this.entryScopeId ?? null;
    const key = this.keyFor(scopeId, node.kind, node.span);
    return this.nodeIndex.get(key) ?? null;
  }

  private keyFor(
    scopeId: ScopeId | null,
    kind: NodeKind,
    span: SourceSpan
  ): string {
    return `${scopeId ?? "root"}:${kind}:${span.start.offset}:${
      span.end.offset
    }`;
  }
}

interface MappingSegment {
  readonly generatedLine: number;
  readonly generatedColumn: number;
  readonly sourceIndex: number;
  readonly originalLine: number;
  readonly originalColumn: number;
}

class SourceMapBuilder {
  private readonly lines: MappingSegment[][] = [];

  constructor(
    private readonly fileName: string,
    private readonly sourceName: string,
    private readonly sourceContent: string | null
  ) {}

  addMapping(segment: {
    generatedLine: number;
    generatedColumn: number;
    originalLine: number;
    originalColumn: number;
  }): void {
    const entry: MappingSegment = {
      ...segment,
      sourceIndex: 0,
    };
    while (this.lines.length <= segment.generatedLine) {
      this.lines.push([]);
    }
    this.lines[segment.generatedLine]!.push(entry);
  }

  toJSON(): RawSourceMap {
    return {
      version: 3,
      file: this.fileName,
      sources: [this.sourceName],
      sourcesContent: [this.sourceContent],
      names: [],
      mappings: this.encodeMappings(),
    };
  }

  private encodeMappings(): string {
    const lines: string[] = [];
    let previousSource = 0;
    let previousOriginalLine = 0;
    let previousOriginalColumn = 0;

    for (const segments of this.lines) {
      if (!segments || segments.length === 0) {
        lines.push("");
        continue;
      }
      let previousGeneratedColumn = 0;
      const encodedSegments = segments
        .sort((a, b) => a.generatedColumn - b.generatedColumn)
        .map((segment) => {
          const columnDelta = segment.generatedColumn - previousGeneratedColumn;
          previousGeneratedColumn = segment.generatedColumn;

          const sourceDelta = segment.sourceIndex - previousSource;
          previousSource = segment.sourceIndex;

          const originalLineDelta = segment.originalLine - previousOriginalLine;
          previousOriginalLine = segment.originalLine;

          const originalColumnDelta =
            segment.originalColumn - previousOriginalColumn;
          previousOriginalColumn = segment.originalColumn;

          return (
            encodeVlq(columnDelta) +
            encodeVlq(sourceDelta) +
            encodeVlq(originalLineDelta) +
            encodeVlq(originalColumnDelta)
          );
        })
        .join(",");
      lines.push(encodedSegments);
    }

    return lines.join(";");
  }
}

const base64Chars =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

const encodeVlq = (value: number): string => {
  let vlq = value < 0 ? (-value << 1) + 1 : value << 1;
  let encoded = "";
  do {
    let digit = vlq & 31;
    vlq >>>= 5;
    if (vlq > 0) {
      digit |= 32;
    }
    encoded += base64Chars[digit] ?? "";
  } while (vlq > 0);
  return encoded;
};

const SPECIAL_IDENTIFIER_SERIALIZATIONS = new Map<string, string>([
  ["*", "_STAR"],
  ["?", "_QMARK"],
  ["!", "_BANG"],
  ["+", "_PLUS"],
  ["=", "_EQ"],
  ["<", "_LT"],
  [">", "_GT"],
  ["/", "_SLASH"],
]);

function sanitizeExternalIdentifier(value: string): string {
  if (!value) {
    return "_ident";
  }

  const singleHyphen = value === "-";
  let result = "";
  for (const char of value) {
    if (/^[A-Za-z0-9_]$/.test(char)) {
      result += char;
      continue;
    }
    if (char === "-") {
      result += singleHyphen ? "_DASH" : "_";
      continue;
    }
    const replacement = SPECIAL_IDENTIFIER_SERIALIZATIONS.get(char);
    if (replacement) {
      result += replacement;
      continue;
    }
    result += "_";
  }

  if (/^[0-9]/.test(result)) {
    result = `_${result}`;
  }

  return result.length === 0 ? "_ident" : result;
}

const IMPORT_OPERATOR_NAMES = new Map<string, string>([
  ["<=", "_LT_EQ"],
  [">=", "_GT_EQ"],
  ["<", "_LT"],
  [">", "_GT"],
  ["=", "_EQ"],
  ["+", "_PLUS"],
  ["-", "_DASH"],
  ["*", "_STAR"],
  ["/", "_SLASH"],
  ["!", "_BANG"],
  ["?", "_QMARK"],
  ["~", "_TILDE"],
  ["@", "_AT"],
]);

const IMPORT_TRAILING_SPECIALS = new Set(["?", "!", "*", "+", "/", "%"]);

function sanitizeImportedMemberName(value: string): string {
  const operator = IMPORT_OPERATOR_NAMES.get(value);
  if (operator) {
    return operator;
  }

  let baseValue = value;
  let trailingOperator = "";

  if (value.length > 1) {
    const lastChar = value.charAt(value.length - 1);
    if (IMPORT_TRAILING_SPECIALS.has(lastChar)) {
      const suffix = IMPORT_OPERATOR_NAMES.get(lastChar);
      if (suffix) {
        trailingOperator = suffix;
        baseValue = value.slice(0, -1);
      }
    }
  }

  let result = baseValue.replace(/[^a-zA-Z0-9_]/g, "_");
  result = result.replace(/_+$/, "");
  result += trailingOperator;

  if (/^[0-9]/.test(result)) {
    result = `_${result}`;
  }
  if (result.length === 0 || result === "_") {
    result = "_ident";
  }
  if (RESERVED_IDENTIFIERS.has(result)) {
    result = `_${result}`;
  }
  return result;
}

const RESERVED_IDENTIFIERS = new Set([
  "arguments",
  "await",
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "eval",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "new",
  "null",
  "return",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "yield",
]);

const isRelativeModuleSpecifier = (specifier: string): boolean => {
  return specifier.startsWith("./") || specifier.startsWith("../");
};
