import {
  DiagnosticSeverity,
  NodeKind,
  type Diagnostic,
  type DispatchNode,
  type ExpressionNode,
  type ListNode,
  type MapEntryNode,
  type MapNode,
  type NamespaceImportKind,
  type NamespaceImportNode,
  type ProgramNode,
  type ReaderMacroNode,
  type ScopeId,
  type SetNode,
  type SourceSpan,
  type StringNode,
  type SymbolNode,
  type VectorNode,
  BUILTIN_SYMBOLS,
} from "@vibe/syntax";

export type { ScopeId } from "@vibe/syntax";

export type SymbolId = `symbol_${number}`;
export type NodeId = `node_${number}`;

export type SymbolKind = "var" | "macro" | "parameter" | "builtin";
export type NodeSymbolRole = "definition" | "usage" | "parameter" | "macro";

export interface AnalyzeOptions {
  readonly builtins?: readonly string[];
  readonly moduleId?: string;
  readonly moduleResolver?: ModuleResolver;
  readonly moduleExports?: ModuleExportsLookup;
}

export interface AnalyzeResult {
  readonly ok: boolean;
  readonly diagnostics: Diagnostic[];
  readonly graph: SemanticGraph;
}

export interface SemanticGraph {
  readonly scopes: readonly ScopeRecord[];
  readonly symbols: readonly SymbolRecord[];
  readonly nodes: readonly SemanticNodeRecord[];
  readonly imports: readonly ModuleImportRecord[];
  readonly exports: readonly ModuleExportRecord[];
}

export interface ModuleImportRecord {
  readonly alias: string;
  readonly specifier: string;
  readonly kind: NamespaceImportKind;
  readonly span: SourceSpan;
  readonly moduleId?: string;
}

export interface ModuleExportRecord {
  readonly name: string;
  readonly identifier: string;
  readonly span: SourceSpan;
}

export interface ModuleResolutionRequest {
  readonly fromModuleId: string;
  readonly specifier: string;
  readonly kind: NamespaceImportKind;
}

export interface ModuleResolutionResult {
  readonly ok: boolean;
  readonly moduleId?: string;
  readonly reason?: string;
}

export interface ModuleResolver {
  resolve(request: ModuleResolutionRequest): ModuleResolutionResult;
}

export interface ModuleExportsLookup {
  getExports(moduleId: string): readonly string[] | undefined;
}

export interface ScopeRecord {
  readonly id: ScopeId;
  readonly parentId: ScopeId | null;
  readonly hygieneTag: string;
  readonly symbols: readonly SymbolId[];
}

export interface SymbolRecord {
  readonly id: SymbolId;
  readonly name: string;
  readonly scopeId: ScopeId;
  readonly kind: SymbolKind;
  readonly nodeId: NodeId | null;
  readonly hygieneTag: string;
  readonly alias: string;
}

export interface NodeSymbolInfo {
  readonly name: string;
  readonly role: NodeSymbolRole;
  readonly symbolId?: SymbolId;
}

export interface SemanticNodeRecord {
  readonly nodeId: NodeId;
  readonly kind: NodeKind;
  readonly span: SourceSpan;
  readonly scopeId: ScopeId;
  readonly hygieneTag: string;
  readonly symbol?: NodeSymbolInfo;
}

interface MacroDefinition {
  readonly symbolId: SymbolId;
  readonly params: readonly string[];
  readonly template: ReaderMacroNode<NodeKind.SyntaxQuote>;
}

interface MacroExpansionContext {
  readonly env: Map<string, ExpressionNode>;
  readonly callSpan: SourceSpan;
}

export const analyzeProgram = (
  program: ProgramNode,
  options: AnalyzeOptions = {}
): AnalyzeResult => {
  const analyzer = new SemanticAnalyzer(options);
  analyzer.analyze(program);
  return analyzer.toResult();
};

interface ScopeInternal {
  readonly record: ScopeRecord;
  readonly bindings: Map<string, SymbolRecord>;
  readonly symbolIds: SymbolId[];
}

const HYGIENE_PREFIX = "h";

export class SemanticAnalyzer {
  private readonly diagnostics: Diagnostic[] = [];
  private readonly scopes = new Map<ScopeId, ScopeInternal>();
  private readonly scopeOrder: ScopeId[] = [];
  private readonly symbols = new Map<SymbolId, SymbolRecord>();
  private readonly symbolOrder: SymbolId[] = [];
  private readonly nodes: SemanticNodeRecord[] = [];
  private readonly moduleImports: ModuleImportRecord[] = [];
  private readonly moduleExportsTable: ModuleExportRecord[] = [];
  private readonly moduleImportsByAlias = new Map<string, ModuleImportRecord>();
  private readonly macros = new Map<SymbolId, MacroDefinition>();
  private readonly macroExpansionStack: SymbolId[] = [];
  private readonly builtins: readonly string[];
  private readonly moduleId?: string;
  private readonly moduleResolver?: ModuleResolver;
  private readonly moduleExportsLookup?: ModuleExportsLookup;
  private readonly aliasAllocator = new AliasAllocator();
  private nextScopeId = 0;
  private nextSymbolId = 0;
  private nextNodeId = 0;
  private nextGensymId = 0;
  private rootScopeId: ScopeId | null = null;

  constructor(options: AnalyzeOptions = {}) {
    this.builtins = options.builtins ?? (BUILTIN_SYMBOLS as readonly string[]);
    this.moduleId = options.moduleId;
    this.moduleResolver = options.moduleResolver;
    this.moduleExportsLookup = options.moduleExports;
  }

  analyze(program: ProgramNode): void {
    const rootScopeId = this.initializeRootScope(program);
    this.recordNode(program, rootScopeId);
    for (const expr of program.body) {
      if (expr) {
        this.visit(expr, rootScopeId);
      }
    }
  }

  toResult(): AnalyzeResult {
    return {
      ok: this.diagnostics.length === 0,
      diagnostics: [...this.diagnostics],
      graph: {
        scopes: this.scopeOrder.map((id) => {
          const scope = this.scopes.get(id);
          if (!scope) {
            throw new Error(`Missing scope ${id}`);
          }
          return scope.record;
        }),
        symbols: this.symbolOrder.map((id) => {
          const symbol = this.symbols.get(id);
          if (!symbol) {
            throw new Error(`Missing symbol ${id}`);
          }
          return symbol;
        }),
        nodes: [...this.nodes],
        imports: [...this.moduleImports],
        exports: [...this.moduleExportsTable],
      },
    };
  }

  private visit(node: ExpressionNode, scopeId: ScopeId): void {
    const nodeScopeId = this.getScopeForNode(node, scopeId);
    switch (node.kind) {
      case NodeKind.List:
        if (!this.expandMacroIfNeeded(node, nodeScopeId)) {
          this.handleList(node, nodeScopeId);
        }
        break;
      case NodeKind.NamespaceImport:
        this.recordNode(node, nodeScopeId);
        this.handleNamespaceImport(
          node as NamespaceImportNode,
          nodeScopeId,
          (node as NamespaceImportNode).importKind
        );
        break;
      case NodeKind.Vector:
      case NodeKind.Set:
        this.recordNode(node, nodeScopeId);
        this.visitSequence(node as VectorNode | SetNode, nodeScopeId);
        break;
      case NodeKind.Map:
        this.recordNode(node, nodeScopeId);
        this.visitMap(node, nodeScopeId);
        break;
      case NodeKind.Quote:
        this.recordNode(node, nodeScopeId);
        this.visitReaderMacro(node as ReaderMacroNode, nodeScopeId);
        break;
      case NodeKind.SyntaxQuote:
        this.recordNode(node, nodeScopeId);
        this.visitSyntaxQuote(node, nodeScopeId);
        break;
      case NodeKind.Unquote:
      case NodeKind.UnquoteSplicing:
        this.recordNode(node, nodeScopeId);
        this.visitReaderMacro(node as ReaderMacroNode, nodeScopeId);
        break;
      case NodeKind.Deref:
      case NodeKind.Dispatch:
        this.recordNode(node, nodeScopeId);
        this.visitReaderMacro(
          node as ReaderMacroNode | DispatchNode,
          nodeScopeId
        );
        break;
      case NodeKind.Symbol:
        this.recordSymbolUsage(node, nodeScopeId, "usage");
        break;
      default:
        this.recordNode(node, nodeScopeId);
        break;
    }
  }

  private expandMacroIfNeeded(node: ListNode, scopeId: ScopeId): boolean {
    const head = node.elements[0];
    if (!head || head.kind !== NodeKind.Symbol) {
      return false;
    }

    const binding = this.resolveSymbol(head.value, scopeId);
    if (!binding || binding.kind !== "macro") {
      return false;
    }

    const definition = this.macros.get(binding.id);
    if (!definition) {
      return false;
    }

    this.recordNode(node, scopeId);
    this.recordSymbolUsage(head, scopeId, "usage");

    if (this.macroExpansionStack.includes(binding.id)) {
      this.report(
        `Recursive macro expansion detected for ${head.value}`,
        node.span,
        "SEM_MACRO_RECURSION"
      );
      return true;
    }

    const args = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
    if (args.length !== definition.params.length) {
      this.report(
        `Macro ${head.value} expects ${definition.params.length} argument(s) but received ${args.length}`,
        node.span,
        "SEM_MACRO_ARITY_MISMATCH"
      );
    }
    const env = new Map<string, ExpressionNode>();
    definition.params.forEach((param, index) => {
      const arg = args[index];
      if (!arg) {
        this.report(
          `Missing argument for macro parameter ${param}`,
          node.span,
          "SEM_MACRO_ARG_MISSING"
        );
        return;
      }
      env.set(param, arg);
    });

    this.macroExpansionStack.push(binding.id);
    const context: MacroExpansionContext = {
      env,
      callSpan: node.span,
    };
    this.visitSyntaxQuote(definition.template, scopeId, context);
    this.macroExpansionStack.pop();
    return true;
  }

  private handleList(node: ListNode, scopeId: ScopeId): void {
    this.recordNode(node, scopeId);
    const [head] = node.elements;
    if (!head) {
      return;
    }

    if (head.kind === NodeKind.Symbol) {
      if (this.applySpecialForm(head, node, scopeId)) {
        return;
      }
      this.recordSymbolUsage(head, scopeId, "usage");
    } else {
      this.visit(head, scopeId);
    }

    for (let index = 1; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.visit(element, scopeId);
      }
    }
  }

  private applySpecialForm(
    head: SymbolNode,
    node: ListNode,
    scopeId: ScopeId
  ): boolean {
    switch (head.value) {
      case "def":
        this.recordSymbolUsage(head, scopeId, "usage");
        this.handleDef(node, scopeId);
        return true;
      case "defmacro":
        this.recordSymbolUsage(head, scopeId, "usage");
        this.handleDefMacro(node, scopeId);
        return true;
      case "let":
        this.recordSymbolUsage(head, scopeId, "usage");
        this.handleLet(node, scopeId);
        return true;
      case "fn":
        this.recordSymbolUsage(head, scopeId, "usage");
        this.handleFn(node, scopeId);
        return true;
      case "get":
        this.recordBuiltinUsage(head, scopeId);
        this.handleGet(node, scopeId);
        return true;
      case "require":
        this.recordBuiltinUsage(head, scopeId);
        this.handleNamespaceImport(node, scopeId, "require");
        return true;
      case "external":
        this.recordBuiltinUsage(head, scopeId);
        this.handleNamespaceImport(node, scopeId, "external");
        return true;
      default:
        return false;
    }
  }

  private handleDef(node: ListNode, scopeId: ScopeId): void {
    const bindingNode = node.elements[1];
    const valueNode = node.elements[2];
    if (!bindingNode || bindingNode.kind !== NodeKind.Symbol) {
      if (bindingNode) {
        this.visit(bindingNode, scopeId);
      }
      this.report(
        "Definition forms require a symbol name",
        node.span,
        "SEM_BINDING_REQUIRES_SYMBOL"
      );
    } else {
      const record = this.defineSymbol(
        bindingNode,
        scopeId,
        "var",
        "definition"
      );
      if (record && this.isTopLevelScope(scopeId)) {
        this.recordModuleExport(record, node.span);
      }
    }

    if (valueNode) {
      this.visit(valueNode, scopeId);
    }

    for (let index = 3; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.visit(element, scopeId);
      }
    }
  }

  private handleDefMacro(node: ListNode, scopeId: ScopeId): void {
    const nameNode = node.elements[1];
    const paramsNode = node.elements[2];
    const bodyNodes = node.elements
      .slice(3)
      .filter(Boolean) as ExpressionNode[];

    const macroSymbol =
      nameNode && nameNode.kind === NodeKind.Symbol
        ? this.defineSymbol(nameNode, scopeId, "macro", "macro")
        : null;
    if (!nameNode || nameNode.kind !== NodeKind.Symbol) {
      if (nameNode) {
        this.visit(nameNode, scopeId);
      }
      this.report(
        "defmacro requires a symbol name",
        node.span,
        "SEM_MACRO_REQUIRES_SYMBOL"
      );
    }

    if (!paramsNode || paramsNode.kind !== NodeKind.Vector) {
      if (paramsNode) {
        this.visit(paramsNode, scopeId);
      }
      this.report(
        "defmacro requires a vector of parameter symbols",
        node.span,
        "SEM_MACRO_EXPECTS_VECTOR"
      );
      return;
    }

    this.recordNode(paramsNode, scopeId);
    const params: string[] = [];
    for (const param of paramsNode.elements) {
      if (!param) {
        continue;
      }
      if (param.kind !== NodeKind.Symbol) {
        this.visit(param, scopeId);
        this.report(
          "Macro parameters must be symbols",
          param.span,
          "SEM_MACRO_PARAM_SYMBOL"
        );
        continue;
      }
      if (params.includes(param.value)) {
        this.report(
          `Duplicate macro parameter ${param.value}`,
          param.span,
          "SEM_MACRO_DUPLICATE_PARAM"
        );
        continue;
      }
      params.push(param.value);
    }

    if (bodyNodes.length === 0) {
      this.report(
        "defmacro requires a syntax-quoted body",
        node.span,
        "SEM_MACRO_REQUIRES_BODY"
      );
      return;
    }

    if (bodyNodes.length > 1) {
      this.report(
        "defmacro currently supports a single body expression",
        bodyNodes[1]!.span,
        "SEM_MACRO_SINGLE_BODY"
      );
    }

    const template = bodyNodes[0];
    if (!template || template.kind !== NodeKind.SyntaxQuote) {
      this.report(
        "Macro bodies must be wrapped in a syntax quote (use `...`)",
        (template ?? node).span,
        "SEM_MACRO_EXPECTS_SYNTAX_QUOTE"
      );
      return;
    }

    if (macroSymbol) {
      this.macros.set(macroSymbol.id, {
        symbolId: macroSymbol.id,
        params,
        template: template as ReaderMacroNode<NodeKind.SyntaxQuote>,
      });
    }
  }

  private handleLet(node: ListNode, scopeId: ScopeId): void {
    const bindingsNode = node.elements[1];
    if (!bindingsNode || bindingsNode.kind !== NodeKind.Vector) {
      if (bindingsNode) {
        this.visit(bindingsNode, scopeId);
      }
      this.report(
        "let requires a vector of binding pairs",
        node.span,
        "SEM_LET_EXPECTS_VECTOR"
      );
      this.visitLetBody(node, scopeId);
      return;
    }

    const bindingHints = bindingsNode.elements.filter(
      (element): element is ExpressionNode => Boolean(element)
    );
    const bodyHints = node.elements
      .slice(2)
      .filter((element): element is ExpressionNode => Boolean(element));
    const childScopeId = this.resolveChildScopeId(scopeId, [
      ...bindingHints,
      ...bodyHints,
    ]);
    this.recordNode(bindingsNode, scopeId);
    const bindings = bindingsNode.elements;
    if (bindings.length % 2 !== 0) {
      this.report(
        "let bindings must contain an even number of forms",
        bindingsNode.span,
        "SEM_LET_ODD_BINDINGS"
      );
    }

    for (let index = 0; index < bindings.length; index += 2) {
      const target = bindings[index];
      const init = bindings[index + 1];
      if (init) {
        this.visit(init, childScopeId);
      }
      if (!target) {
        continue;
      }
      if (target.kind === NodeKind.Symbol) {
        this.defineSymbol(target, childScopeId, "var", "definition");
      } else {
        this.visit(target, childScopeId);
        this.report(
          "Binding targets inside let must be symbols",
          target.span,
          "SEM_BINDING_REQUIRES_SYMBOL"
        );
      }
    }

    this.visitLetBody(node, childScopeId);
  }

  private visitLetBody(node: ListNode, scopeId: ScopeId): void {
    for (let index = 2; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.visit(element, scopeId);
      }
    }
  }

  private handleFn(node: ListNode, scopeId: ScopeId): void {
    const paramsNode = node.elements[1];
    const paramHints =
      paramsNode && paramsNode.kind === NodeKind.Vector
        ? paramsNode.elements.filter((element): element is ExpressionNode =>
            Boolean(element)
          )
        : paramsNode
        ? [paramsNode]
        : [];
    const bodyHints = node.elements
      .slice(2)
      .filter((element): element is ExpressionNode => Boolean(element));
    const fnScopeId = this.resolveChildScopeId(scopeId, [
      ...paramHints,
      ...bodyHints,
    ]);

    if (!paramsNode || paramsNode.kind !== NodeKind.Vector) {
      if (paramsNode) {
        this.visit(paramsNode, scopeId);
      }
      this.report(
        "fn requires a vector of parameter symbols",
        node.span,
        "SEM_FN_EXPECTS_VECTOR"
      );
    } else {
      this.recordNode(paramsNode, scopeId);
      for (const param of paramsNode.elements) {
        if (!param) {
          continue;
        }
        if (param.kind === NodeKind.Symbol) {
          this.defineSymbol(param, fnScopeId, "parameter", "parameter");
        } else {
          this.visit(param, fnScopeId);
          this.report(
            "Parameters inside fn must be symbols",
            param.span,
            "SEM_BINDING_REQUIRES_SYMBOL"
          );
        }
      }
    }

    for (let index = 2; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.visit(element, fnScopeId);
      }
    }
  }

  private handleGet(node: ListNode, scopeId: ScopeId): void {
    const aliasNode = node.elements[1];
    const memberNode = node.elements[2];
    if (!aliasNode) {
      this.report(
        "get requires a namespace alias",
        node.span,
        "SEM_GET_EXPECTS_ALIAS"
      );
    } else {
      this.visit(aliasNode, scopeId);
    }
    if (!memberNode) {
      this.report(
        "get requires a member name",
        node.span,
        "SEM_GET_EXPECTS_MEMBER"
      );
    } else if (
      memberNode.kind !== NodeKind.Symbol &&
      memberNode.kind !== NodeKind.String
    ) {
      this.report(
        "get member names must be symbols or string literals",
        memberNode.span,
        "SEM_GET_EXPECTS_MEMBER"
      );
      this.visit(memberNode, scopeId);
    }

    for (let index = 3; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        this.visit(element, scopeId);
      }
    }
  }

  private handleNamespaceImport(
    node: NamespaceImportNode | ListNode,
    scopeId: ScopeId,
    kind: NamespaceImportKind
  ): void {
    if (!this.isTopLevelScope(scopeId)) {
      this.report(
        `${kind} is only supported at the top level`,
        node.span,
        kind === "require" ? "SEM_REQUIRE_TOP_LEVEL" : "SEM_EXTERNAL_TOP_LEVEL"
      );
    }

    const aliasOperand = this.resolveImportAliasOperand(node);
    const pathOperand = this.resolveImportPathOperand(node);

    const aliasNode =
      aliasOperand && aliasOperand.kind === NodeKind.Symbol
        ? aliasOperand
        : null;
    const pathNode =
      pathOperand && pathOperand.kind === NodeKind.String ? pathOperand : null;

    if (!aliasNode) {
      if (aliasOperand) {
        this.visit(aliasOperand, scopeId);
      }
      this.report(
        `${kind} expects a symbol alias`,
        (aliasOperand ?? node).span,
        kind === "require"
          ? "SEM_REQUIRE_EXPECTS_ALIAS"
          : "SEM_EXTERNAL_EXPECTS_ALIAS"
      );
    } else {
      this.defineSymbol(aliasNode, scopeId, "var", "definition");
    }

    if (!pathNode) {
      if (pathOperand) {
        this.visit(pathOperand, scopeId);
      }
      this.report(
        `${kind} expects a string literal path`,
        (pathOperand ?? node).span,
        kind === "require"
          ? "SEM_REQUIRE_EXPECTS_STRING"
          : "SEM_EXTERNAL_EXPECTS_STRING"
      );
    } else if (aliasNode) {
      this.recordNamespaceImport(aliasNode, pathNode, kind);
    }

    const elements = this.resolveImportElements(node);
    for (let index = 3; index < elements.length; index += 1) {
      const element = elements[index];
      if (element) {
        this.visit(element, scopeId);
      }
    }
  }

  private resolveImportAliasOperand(
    node: NamespaceImportNode | ListNode
  ): ExpressionNode | null {
    if (node.kind === NodeKind.NamespaceImport) {
      return node.alias ?? null;
    }
    return node.elements[1] ?? null;
  }

  private resolveImportPathOperand(
    node: NamespaceImportNode | ListNode
  ): ExpressionNode | null {
    if (node.kind === NodeKind.NamespaceImport) {
      return node.source ?? null;
    }
    return node.elements[2] ?? null;
  }

  private resolveImportElements(
    node: NamespaceImportNode | ListNode
  ): readonly ExpressionNode[] {
    return node.elements;
  }

  private recordNamespaceImport(
    aliasNode: SymbolNode,
    pathNode: StringNode,
    kind: NamespaceImportKind
  ): void {
    let moduleId: string | undefined;
    if (kind === "require") {
      moduleId = this.resolveRequireTarget(pathNode);
    }
    const record: ModuleImportRecord = {
      alias: aliasNode.value,
      specifier: pathNode.value,
      kind,
      span: pathNode.span,
      moduleId,
    };
    this.moduleImports.push(record);
    this.moduleImportsByAlias.set(aliasNode.value, record);
  }

  private resolveRequireTarget(pathNode: StringNode): string | undefined {
    if (!this.moduleResolver || !this.moduleId) {
      return undefined;
    }
    const resolution = this.moduleResolver.resolve({
      fromModuleId: this.moduleId,
      specifier: pathNode.value,
      kind: "require",
    });
    if (!resolution.ok) {
      const suffix = resolution.reason ? `: ${resolution.reason}` : "";
      this.report(
        `Unable to resolve module ${pathNode.value}${suffix}`,
        pathNode.span,
        "SEM_REQUIRE_RESOLVE_FAILED"
      );
      return undefined;
    }
    return resolution.moduleId;
  }

  private recordModuleExport(symbol: SymbolRecord, span: SourceSpan): void {
    // If an export with the same name already exists, replace it to ensure
    // re-definitions update the exported identifier instead of duplicating
    // entries (helpful for REPL/repeated top-level defs).
    const existingIndex = this.moduleExportsTable.findIndex(
      (e) => e.name === symbol.name
    );
    if (existingIndex !== -1) {
      // Replace the whole record rather than mutating readonly properties
      this.moduleExportsTable[existingIndex] = {
        name: symbol.name,
        identifier: symbol.alias,
        span,
      };
      return;
    }
    this.moduleExportsTable.push({
      name: symbol.name,
      identifier: symbol.alias,
      span,
    });
  }

  private visitSequence(node: VectorNode | SetNode, scopeId: ScopeId): void {
    for (const element of node.elements) {
      if (element) {
        this.visit(element, scopeId);
      }
    }
  }

  private visitMap(node: MapNode, scopeId: ScopeId): void {
    for (const entry of node.entries) {
      this.recordNode(entry, scopeId);
      if (entry.key) {
        this.visit(entry.key, scopeId);
      }
      if (entry.value) {
        this.visit(entry.value, scopeId);
      }
    }
  }

  private visitSyntaxQuote(
    node: ReaderMacroNode,
    scopeId: ScopeId,
    context?: MacroExpansionContext
  ): void {
    if (node.kind !== NodeKind.SyntaxQuote) {
      this.visitReaderMacro(node, scopeId);
      return;
    }
    if (!node.target) {
      return;
    }
    if (!context) {
      this.visit(node.target, scopeId);
      return;
    }
    const expanded = this.instantiateTemplate(node.target, context);
    if (expanded) {
      this.visit(expanded, scopeId);
    }
  }

  private visitReaderMacro(
    node: ReaderMacroNode | DispatchNode,
    scopeId: ScopeId
  ): void {
    if (node.target) {
      this.visit(node.target, scopeId);
    }
  }

  private instantiateTemplate(
    node: ExpressionNode,
    context: MacroExpansionContext
  ): ExpressionNode | null {
    switch (node.kind) {
      case NodeKind.List:
        return this.instantiateSequence(node, context);
      case NodeKind.Vector:
        return this.instantiateSequence(node, context) as VectorNode;
      case NodeKind.Set:
        return this.instantiateSequence(node, context) as SetNode;
      case NodeKind.Map:
        return this.instantiateMap(node, context);
      case NodeKind.Quote:
      case NodeKind.Deref:
        return this.instantiateReader(node as ReaderMacroNode, context);
      case NodeKind.Dispatch:
        return this.instantiateDispatch(node as DispatchNode, context);
      case NodeKind.SyntaxQuote:
        if (!node.target) {
          return null;
        }
        return this.instantiateTemplate(node.target, context);
      case NodeKind.Unquote:
        return (
          this.materializeArgument(
            node as ReaderMacroNode<NodeKind.Unquote>,
            context
          ) ?? this.createNilLiteral(context.callSpan)
        );
      case NodeKind.UnquoteSplicing:
        this.report(
          "Unquote splicing cannot appear outside of list/vector/set literals",
          node.span,
          "SEM_MACRO_SPLICE_CONTEXT"
        );
        return this.createNilLiteral(context.callSpan);
      default:
        return this.cloneExpression(node);
    }
  }

  private instantiateSequence(
    node: ListNode | VectorNode | SetNode,
    context: MacroExpansionContext
  ): ListNode | VectorNode | SetNode {
    const elements: ExpressionNode[] = [];
    for (const element of node.elements) {
      if (!element) {
        continue;
      }
      if (element.kind === NodeKind.Unquote) {
        const value = this.materializeArgument(
          element as ReaderMacroNode<NodeKind.Unquote>,
          context
        );
        if (value) {
          elements.push(value);
        }
        continue;
      }
      if (element.kind === NodeKind.UnquoteSplicing) {
        const values = this.materializeSplicedArgument(
          element as ReaderMacroNode<NodeKind.UnquoteSplicing>,
          context
        );
        elements.push(...values);
        continue;
      }
      const expanded = this.instantiateTemplate(element, context);
      if (expanded) {
        elements.push(expanded);
      }
    }
    return {
      ...node,
      elements,
    } as ListNode | VectorNode | SetNode;
  }

  private instantiateMap(
    node: MapNode,
    context: MacroExpansionContext
  ): MapNode {
    const entries = node.entries.map((entry) => ({
      ...entry,
      key: entry.key ? this.instantiateTemplate(entry.key, context) : null,
      value: entry.value
        ? this.instantiateTemplate(entry.value, context)
        : null,
    }));
    return {
      ...node,
      entries,
    };
  }

  private instantiateReader(
    node: ReaderMacroNode,
    context: MacroExpansionContext
  ): ReaderMacroNode {
    return {
      ...node,
      target: node.target
        ? this.instantiateTemplate(node.target, context)
        : null,
    };
  }

  private instantiateDispatch(
    node: DispatchNode,
    context: MacroExpansionContext
  ): DispatchNode {
    return {
      ...node,
      target: node.target
        ? this.instantiateTemplate(node.target, context)
        : null,
    };
  }

  private materializeArgument(
    node: ReaderMacroNode<NodeKind.Unquote>,
    context: MacroExpansionContext
  ): ExpressionNode | null {
    if (!node.target) {
      this.report(
        "Unquote requires a target expression",
        node.span,
        "SEM_MACRO_UNQUOTE_EMPTY"
      );
      return null;
    }
    return this.evaluateUnquoteTarget(node.target, context);
  }

  private materializeSplicedArgument(
    node: ReaderMacroNode<NodeKind.UnquoteSplicing>,
    context: MacroExpansionContext
  ): ExpressionNode[] {
    if (!node.target) {
      this.report(
        "Unquote splicing requires a target expression",
        node.span,
        "SEM_MACRO_SPLICE_EMPTY"
      );
      return [];
    }
    const value = this.evaluateUnquoteTarget(node.target, context);
    if (!value) {
      return [];
    }
    if (
      value.kind === NodeKind.List ||
      value.kind === NodeKind.Vector ||
      value.kind === NodeKind.Set
    ) {
      return value.elements
        .filter((element): element is ExpressionNode => Boolean(element))
        .map((element) => this.cloneExpression(element));
    }
    this.report(
      "Unquote splicing requires a sequence expression",
      node.span,
      "SEM_MACRO_SPLICE_SEQUENCE"
    );
    return [value];
  }

  private evaluateUnquoteTarget(
    target: ExpressionNode,
    context: MacroExpansionContext
  ): ExpressionNode | null {
    if (target.kind === NodeKind.Symbol) {
      const value = context.env.get(target.value);
      if (!value) {
        this.report(
          `Unknown macro parameter ${target.value}`,
          target.span,
          "SEM_MACRO_UNKNOWN_PARAM"
        );
        return null;
      }
      return this.cloneExpression(value);
    }
    if (target.kind === NodeKind.List) {
      const head = target.elements[0];
      if (head && head.kind === NodeKind.Symbol && head.value === "gensym") {
        const hintNode = target.elements[1];
        let hint: string | null = null;
        if (hintNode && hintNode.kind === NodeKind.String) {
          hint = hintNode.value;
        } else if (hintNode && hintNode.kind === NodeKind.Symbol) {
          hint = hintNode.value;
        }
        return this.createGensymSymbol(target.span, hint);
      }
    }
    this.report(
      "Unsupported unquote expression inside macro body",
      target.span,
      "SEM_MACRO_UNQUOTE_UNSUPPORTED"
    );
    return null;
  }

  private cloneExpression<T extends ExpressionNode>(node: T): T {
    const copy = this.duplicate(node);
    this.stripScopeMetadata(copy);
    return copy;
  }

  private duplicate<T>(value: T): T {
    const structured = (
      globalThis as {
        structuredClone?: <S>(input: S) => S;
      }
    ).structuredClone;
    if (structured) {
      return structured(value);
    }
    return JSON.parse(JSON.stringify(value)) as T;
  }

  private stripScopeMetadata(
    node: ExpressionNode | MapEntryNode | ProgramNode | null | undefined
  ): void {
    if (!node) {
      return;
    }
    delete (node as { scopeId?: ScopeId }).scopeId;
    switch (node.kind) {
      case NodeKind.List:
      case NodeKind.Vector:
      case NodeKind.Set:
        for (const element of node.elements) {
          this.stripScopeMetadata(element);
        }
        break;
      case NodeKind.Map:
        for (const entry of node.entries) {
          this.stripScopeMetadata(entry);
        }
        break;
      case NodeKind.MapEntry:
        this.stripScopeMetadata(node.key);
        this.stripScopeMetadata(node.value);
        break;
      case NodeKind.Quote:
      case NodeKind.SyntaxQuote:
      case NodeKind.Unquote:
      case NodeKind.UnquoteSplicing:
      case NodeKind.Deref:
        this.stripScopeMetadata((node as ReaderMacroNode).target);
        break;
      case NodeKind.Dispatch:
        this.stripScopeMetadata((node as DispatchNode).target);
        break;
      default:
        break;
    }
  }

  private createNilLiteral(span: SourceSpan): ExpressionNode {
    return {
      kind: NodeKind.Nil,
      span,
      lexeme: "nil",
      value: null,
    };
  }

  private createGensymSymbol(
    span: SourceSpan,
    hint: string | null
  ): SymbolNode {
    const base = hint && hint.length > 0 ? hint : "g";
    const unique = `${base}__${this.nextGensymId++}`;
    return {
      kind: NodeKind.Symbol,
      span,
      lexeme: unique,
      value: unique,
    };
  }

  private initializeRootScope(program: ProgramNode): ScopeId {
    if (this.rootScopeId) {
      return this.rootScopeId;
    }
    const requested = this.resolveScopeIdFromNode(program);
    const rootScope = requested
      ? this.ensureScope(requested, null)
      : this.createScope(null);
    this.rootScopeId = rootScope.record.id;
    for (const name of this.builtins) {
      this.defineBuiltin(name);
    }
    return this.rootScopeId;
  }

  private getScopeForNode<T extends { scopeId?: ScopeId }>(
    node: T,
    fallback: ScopeId
  ): ScopeId {
    const scopeId = this.resolveScopeIdFromNode(node);
    if (!scopeId || scopeId === fallback) {
      return fallback;
    }
    return this.ensureScope(scopeId, fallback).record.id;
  }

  private resolveChildScopeId(
    parentScopeId: ScopeId,
    hints: readonly ExpressionNode[]
  ): ScopeId {
    for (const hint of hints) {
      const scopeId = this.resolveScopeIdFromNode(hint);
      if (scopeId && scopeId !== parentScopeId) {
        return this.ensureScope(scopeId, parentScopeId).record.id;
      }
    }
    return this.createScope(parentScopeId).record.id;
  }

  private resolveScopeIdFromNode(
    node: { scopeId?: ScopeId } | null | undefined
  ): ScopeId | null {
    if (!node || !node.scopeId) {
      return null;
    }
    return node.scopeId;
  }

  private ensureScope(id: ScopeId, parentId: ScopeId | null): ScopeInternal {
    const existing = this.scopes.get(id);
    if (existing) {
      return existing;
    }
    return this.createScope(parentId, id);
  }

  private defineBuiltin(name: string): void {
    if (!this.rootScopeId) {
      throw new Error("Root scope is not initialized");
    }
    const scope = this.scopes.get(this.rootScopeId);
    if (!scope || scope.bindings.has(name)) {
      return;
    }
    const symbolId = this.allocateSymbolId();
    const alias = this.aliasAllocator.allocate(name, symbolId, false);
    const record: SymbolRecord = {
      id: symbolId,
      name,
      scopeId: scope.record.id,
      kind: "builtin",
      nodeId: null,
      hygieneTag: scope.record.hygieneTag,
      alias,
    };
    scope.bindings.set(name, record);
    scope.symbolIds.push(symbolId);
    this.symbols.set(symbolId, record);
    this.symbolOrder.push(symbolId);
  }

  private defineSymbol(
    node: SymbolNode,
    scopeId: ScopeId,
    kind: SymbolKind,
    role: NodeSymbolRole
  ): SymbolRecord | null {
    const scope = this.scopes.get(scopeId);
    if (!scope) {
      throw new Error(`Unknown scope ${scopeId}`);
    }
    // Allow symbol redefinition in the same scope by shadowing the previous
    // binding rather than reporting an error. This is desirable for interactive
    // REPL usage and multiple top-level defs in sequence.
    const symbolId = this.allocateSymbolId();
    const alias = this.aliasAllocator.allocate(
      node.value,
      symbolId,
      this.shouldPreferRootAlias(scope, kind)
    );
    const nodeId = this.recordNode(node, scopeId, {
      name: node.value,
      role,
      symbolId,
    });
    const record: SymbolRecord = {
      id: symbolId,
      name: node.value,
      scopeId,
      kind,
      nodeId,
      hygieneTag: scope.record.hygieneTag,
      alias,
    };
    // Overwrite existing binding to effect shadowing within the same scope.
    scope.bindings.set(node.value, record);
    scope.symbolIds.push(symbolId);
    this.symbols.set(symbolId, record);
    this.symbolOrder.push(symbolId);
    return record;
  }

  private recordSymbolUsage(
    node: SymbolNode,
    scopeId: ScopeId,
    role: NodeSymbolRole
  ): void {
    const namespaceRef = this.parseNamespaceReference(node.value);
    if (namespaceRef) {
      const binding = this.resolveSymbol(namespaceRef.alias, scopeId);
      if (!binding) {
        this.report(
          `Unresolved namespace alias ${namespaceRef.alias}`,
          node.span,
          "SEM_UNRESOLVED_NAMESPACE_ALIAS"
        );
      }
      if (binding) {
        const importRecord = this.moduleImportsByAlias.get(namespaceRef.alias);
        const moduleId = importRecord?.moduleId;
        if (moduleId && this.moduleExportsLookup) {
          const exports = this.moduleExportsLookup.getExports(moduleId);
          if (exports && !exports.includes(namespaceRef.member)) {
            this.report(
              `Namespace ${namespaceRef.alias} does not export ${namespaceRef.member}`,
              node.span,
              "SEM_UNRESOLVED_NAMESPACE_MEMBER"
            );
          }
        }
      }
      this.recordNode(node, scopeId, {
        name: node.value,
        role,
        symbolId: binding?.id,
      });
      return;
    }

    const binding = this.resolveSymbol(node.value, scopeId);
    if (!binding) {
      this.report(
        `Unresolved symbol ${node.value}`,
        node.span,
        "SEM_UNRESOLVED_SYMBOL"
      );
    }
    this.recordNode(node, scopeId, {
      name: node.value,
      role,
      symbolId: binding?.id,
    });
  }

  private recordBuiltinUsage(node: SymbolNode, scopeId: ScopeId): void {
    this.recordNode(node, scopeId, {
      name: node.value,
      role: "usage",
    });
  }

  private parseNamespaceReference(
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

  private resolveSymbol(name: string, scopeId: ScopeId): SymbolRecord | null {
    let current: ScopeId | null = scopeId;
    while (current) {
      const scope = this.scopes.get(current);
      if (scope?.bindings.has(name)) {
        return scope.bindings.get(name) ?? null;
      }
      current = scope?.record.parentId ?? null;
    }
    return null;
  }

  private isTopLevelScope(scopeId: ScopeId): boolean {
    return this.rootScopeId === scopeId;
  }

  private recordNode(
    node: { kind: NodeKind; span: SourceSpan },
    scopeId: ScopeId,
    symbol?: NodeSymbolInfo
  ): NodeId {
    const scope = this.scopes.get(scopeId);
    if (!scope) {
      throw new Error(`Missing scope ${scopeId}`);
    }
    const nodeId = this.allocateNodeId();
    this.nodes.push({
      nodeId,
      kind: node.kind,
      span: node.span,
      scopeId,
      hygieneTag: scope.record.hygieneTag,
      ...(symbol ? { symbol } : {}),
    });
    return nodeId;
  }

  private report(message: string, span: SourceSpan, code: string): void {
    this.diagnostics.push({
      message,
      span,
      severity: DiagnosticSeverity.Error,
      code,
    });
  }

  private createScope(
    parentId: ScopeId | null,
    explicitId?: ScopeId
  ): ScopeInternal {
    const id = explicitId ?? (`scope_${this.nextScopeId++}` as ScopeId);
    if (explicitId) {
      this.syncScopeCounter(explicitId);
    }
    if (this.scopes.has(id)) {
      const existing = this.scopes.get(id);
      if (existing) {
        if (existing.record.parentId !== parentId) {
          throw new Error(
            `Scope ${id} already exists with parent ${existing.record.parentId}`
          );
        }
        return existing;
      }
    }
    const hygieneTag = `${HYGIENE_PREFIX}${id}`;
    const symbolIds: SymbolId[] = [];
    const record: ScopeRecord = {
      id,
      parentId,
      hygieneTag,
      symbols: symbolIds,
    };
    const scope: ScopeInternal = {
      record,
      bindings: new Map<string, SymbolRecord>(),
      symbolIds,
    };
    this.scopes.set(id, scope);
    this.scopeOrder.push(id);
    return scope;
  }

  private syncScopeCounter(id: ScopeId): void {
    const match = /^scope_(\d+)$/.exec(id);
    if (!match) {
      return;
    }
    const index = Number(match[1]);
    if (Number.isNaN(index)) {
      return;
    }
    this.nextScopeId = Math.max(this.nextScopeId, index + 1);
  }

  private allocateSymbolId(): SymbolId {
    return `symbol_${this.nextSymbolId++}` as SymbolId;
  }

  private shouldPreferRootAlias(
    scope: ScopeInternal,
    kind: SymbolKind
  ): boolean {
    return kind === "var" && scope.record.parentId === null;
  }

  private allocateNodeId(): NodeId {
    return `node_${this.nextNodeId++}` as NodeId;
  }
}

class AliasAllocator {
  private readonly used = new Set<string>(["__println", "__result"]);

  allocate(name: string, symbolId: SymbolId, preferRaw: boolean): string {
    const preferred = preferRaw ? name : `${name}__${symbolId}`;
    const sanitized = this.sanitize(preferred);
    return this.reserve(this.ensureIdentifier(sanitized));
  }

  private sanitize(value: string): string {
    let base = value.replace(/[^a-zA-Z0-9_]/g, "_").replace(/^([0-9])/, "_$1");
    if (base.length === 0) {
      base = "_ident";
    }
    return base;
  }

  private ensureIdentifier(value: string): string {
    if (RESERVED_IDENTIFIERS.has(value)) {
      return `_${value}`;
    }
    return value;
  }

  private reserve(initial: string): string {
    let candidate = initial;
    while (this.used.has(candidate)) {
      candidate = `${candidate}_${this.used.size}`;
    }
    this.used.add(candidate);
    return candidate;
  }
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
