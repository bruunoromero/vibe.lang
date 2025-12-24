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
import {
  evaluate,
  createRootEnvironment,
  defineVariable,
  valueToNode,
  nodeToValue,
} from "@vibe/interpreter";

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
  readonly flatten?: readonly FlattenedImportBinding[];
}

export interface FlattenedImportBinding {
  readonly exportedName: string;
  readonly identifier: string;
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

type MacroBody =
  | {
      readonly kind: "template";
      readonly template: ReaderMacroNode<NodeKind.SyntaxQuote>;
    }
  | {
      readonly kind: "expression";
      readonly expression: ExpressionNode;
    };

interface MacroDefinition {
  readonly symbolId: SymbolId;
  readonly params: readonly string[];
  readonly rest?: string;
  readonly body: MacroBody;
}

interface MacroExpansionContext {
  readonly env: Map<string, ExpressionNode>;
  readonly callSpan: SourceSpan;
}

export const analyzeProgram = async (
  program: ProgramNode,
  options: AnalyzeOptions = {}
): Promise<AnalyzeResult> => {
  const analyzer = new SemanticAnalyzer(options);
  await analyzer.analyze(program);
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

  async analyze(program: ProgramNode): Promise<void> {
    const rootScopeId = this.initializeRootScope(program);
    this.recordNode(program, rootScopeId);
    for (const expr of program.body) {
      if (expr) {
        await this.visit(expr, rootScopeId);
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

  private async visit(node: ExpressionNode, scopeId: ScopeId): Promise<void> {
    const nodeScopeId = this.getScopeForNode(node, scopeId);
    switch (node.kind) {
      case NodeKind.List:
        if (!(await this.expandMacroIfNeeded(node, nodeScopeId))) {
          await this.handleList(node, nodeScopeId);
        }
        break;
      case NodeKind.NamespaceImport:
        this.recordNode(node, nodeScopeId);
        await this.handleNamespaceImport(
          node as NamespaceImportNode,
          nodeScopeId,
          (node as NamespaceImportNode).importKind
        );
        break;
      case NodeKind.Vector:
      case NodeKind.Set:
        this.recordNode(node, nodeScopeId);
        await this.visitSequence(node as VectorNode | SetNode, nodeScopeId);
        break;
      case NodeKind.Map:
        this.recordNode(node, nodeScopeId);
        await this.visitMap(node, nodeScopeId);
        break;
      case NodeKind.Quote:
        this.recordNode(node, nodeScopeId);
        await this.visitReaderMacro(node as ReaderMacroNode, nodeScopeId);
        break;
      case NodeKind.SyntaxQuote:
        this.recordNode(node, nodeScopeId);
        await this.visitSyntaxQuote(node, nodeScopeId);
        break;
      case NodeKind.Unquote:
      case NodeKind.UnquoteSplicing:
        this.recordNode(node, nodeScopeId);
        await this.visitReaderMacro(node as ReaderMacroNode, nodeScopeId);
        break;
      case NodeKind.Deref:
      case NodeKind.Dispatch:
        this.recordNode(node, nodeScopeId);
        await this.visitReaderMacro(
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

  private replaceNodeWithExpansion(
    target: ExpressionNode,
    expansion: ExpressionNode
  ): void {
    const targetRecord = target as unknown as Record<string, unknown>;
    for (const key of Object.keys(targetRecord)) {
      delete targetRecord[key];
    }
    Object.assign(targetRecord, expansion);
  }

  private async expandMacroIfNeeded(
    node: ListNode,
    scopeId: ScopeId
  ): Promise<boolean> {
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
    const minArity = definition.params.length;
    const isVariadic = definition.rest !== undefined;

    if (!isVariadic && args.length !== minArity) {
      this.report(
        `Macro ${head.value} expects ${minArity} argument(s) but received ${args.length}`,
        node.span,
        "SEM_MACRO_ARITY_MISMATCH"
      );
    } else if (isVariadic && args.length < minArity) {
      this.report(
        `Macro ${head.value} expects at least ${minArity} argument(s) but received ${args.length}`,
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

    // Handle rest parameter by collecting remaining args into a vector
    if (definition.rest) {
      const restArgs = args.slice(definition.params.length);
      const restVector: VectorNode = {
        kind: NodeKind.Vector,
        span: node.span,
        elements: restArgs,
      };
      env.set(definition.rest, restVector);
    }

    this.macroExpansionStack.push(binding.id);
    let expanded: ExpressionNode | null = null;
    try {
      expanded = await this.expandMacro(definition, {
        env,
        callSpan: node.span,
      });
    } finally {
      this.macroExpansionStack.pop();
    }

    // Recursively expand if the result is also a macro call
    // Pass the current macro ID to detect direct recursion
    if (expanded) {
      await this.fullyExpandAndVisit(expanded, scopeId, 0, 100, binding.id);
      this.replaceNodeWithExpansion(node, expanded);
    }

    return true;
  }

  private async expandMacro(
    definition: MacroDefinition,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    if (definition.body.kind === "template") {
      const template = definition.body.template;
      if (!template.target) {
        return null;
      }
      return await this.instantiateTemplate(template.target, context);
    }

    return await this.evaluateCompileTimeExpression(
      definition.body.expression,
      context
    );
  }

  private async fullyExpandAndVisit(
    node: ExpressionNode,
    scopeId: ScopeId,
    depth: number,
    maxDepth: number = 100,
    parentMacroId?: string
  ): Promise<void> {
    if (depth >= maxDepth) {
      this.report(
        `Macro expansion depth limit (${maxDepth}) exceeded`,
        node.span,
        "SEM_MACRO_MAX_DEPTH"
      );
      await this.visit(node, scopeId);
      return;
    }

    // Check if this is a macro call that needs expansion
    if (node.kind === NodeKind.List) {
      const head = node.elements[0];
      if (head && head.kind === NodeKind.Symbol) {
        const binding = this.resolveSymbol(head.value, scopeId);
        if (binding && binding.kind === "macro") {
          // This is a macro call - expand it with increased depth
          const definition = this.macros.get(binding.id);
          if (definition) {
            this.recordNode(node, scopeId);
            this.recordSymbolUsage(head, scopeId, "usage");

            // Check for direct recursion: if this macro is the immediate parent
            if (parentMacroId === binding.id) {
              this.report(
                `Recursive macro expansion detected for ${head.value}`,
                node.span,
                "SEM_MACRO_RECURSION"
              );
              return;
            }

            // Build environment and expand
            const args = node.elements
              .slice(1)
              .filter(Boolean) as ExpressionNode[];
            const env = new Map<string, ExpressionNode>();
            definition.params.forEach((param, index) => {
              if (args[index]) {
                env.set(param, args[index]!);
              }
            });

            if (definition.rest) {
              const restArgs = args.slice(definition.params.length);
              const restVector: VectorNode = {
                kind: NodeKind.Vector,
                span: node.span,
                elements: restArgs,
              };
              env.set(definition.rest, restVector);
            }

            const expanded = await this.expandMacro(definition, {
              env,
              callSpan: node.span,
            });

            if (expanded) {
              // Recursively expand with increased depth, passing current macro as parent
              await this.fullyExpandAndVisit(
                expanded,
                scopeId,
                depth + 1,
                maxDepth,
                binding.id
              );
            }
            return;
          }
        }
      }
    }

    // Not a macro call, visit normally
    await this.visit(node, scopeId);
  }

  private async handleList(node: ListNode, scopeId: ScopeId): Promise<void> {
    this.recordNode(node, scopeId);
    const [head] = node.elements;
    if (!head) {
      return;
    }

    if (head.kind === NodeKind.Symbol) {
      if (await this.applySpecialForm(head, node, scopeId)) {
        return;
      }
      this.recordSymbolUsage(head, scopeId, "usage");
    } else {
      await this.visit(head, scopeId);
    }

    for (let index = 1; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        await this.visit(element, scopeId);
      }
    }
  }

  private async applySpecialForm(
    head: SymbolNode,
    node: ListNode,
    scopeId: ScopeId
  ): Promise<boolean> {
    switch (head.value) {
      case "def":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleDef(node, scopeId);
        return true;
      case "defmacro":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleDefMacro(node, scopeId);
        return true;
      case "let":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleLet(node, scopeId);
        return true;
      case "fn":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleFn(node, scopeId);
        return true;
      case "require":
        this.recordBuiltinUsage(head, scopeId);
        await this.handleNamespaceImport(node, scopeId, "require");
        return true;
      case "external":
        this.recordBuiltinUsage(head, scopeId);
        await this.handleNamespaceImport(node, scopeId, "external");
        return true;
      default:
        return false;
    }
  }

  private async handleDef(node: ListNode, scopeId: ScopeId): Promise<void> {
    const bindingNode = node.elements[1];
    const valueNode = node.elements[2];
    if (!bindingNode || bindingNode.kind !== NodeKind.Symbol) {
      if (bindingNode) {
        await this.visit(bindingNode, scopeId);
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
      await this.visit(valueNode, scopeId);
    }

    for (let index = 3; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        await this.visit(element, scopeId);
      }
    }
  }

  private async handleDefMacro(
    node: ListNode,
    scopeId: ScopeId
  ): Promise<void> {
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
        await this.visit(nameNode, scopeId);
      }
      this.report(
        "defmacro requires a symbol name",
        node.span,
        "SEM_MACRO_REQUIRES_SYMBOL"
      );
    }

    if (!paramsNode || paramsNode.kind !== NodeKind.Vector) {
      if (paramsNode) {
        await this.visit(paramsNode, scopeId);
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
    let rest: string | undefined = undefined;
    let sawAmpersand = false;

    for (let i = 0; i < paramsNode.elements.length; i++) {
      const param = paramsNode.elements[i];
      if (!param) {
        continue;
      }
      if (param.kind !== NodeKind.Symbol) {
        await this.visit(param, scopeId);
        this.report(
          "Macro parameters must be symbols",
          param.span,
          "SEM_MACRO_PARAM_SYMBOL"
        );
        continue;
      }

      if (param.value === "&") {
        if (sawAmpersand) {
          this.report(
            "Only one & rest parameter allowed",
            param.span,
            "SEM_MACRO_DUPLICATE_REST"
          );
          continue;
        }
        sawAmpersand = true;
        const nextParam = paramsNode.elements[i + 1];
        if (!nextParam || nextParam.kind !== NodeKind.Symbol) {
          this.report(
            "& must be followed by a symbol",
            param.span,
            "SEM_MACRO_REST_REQUIRES_SYMBOL"
          );
        } else {
          rest = nextParam.value;
          i++; // Skip the next parameter since we just consumed it
        }
        continue;
      }

      if (sawAmpersand) {
        this.report(
          "No parameters allowed after & rest parameter",
          param.span,
          "SEM_MACRO_PARAMS_AFTER_REST"
        );
        continue;
      }

      if (params.includes(param.value) || param.value === rest) {
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
        "defmacro requires a body expression",
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

    const bodyNode = bodyNodes[0];
    if (!bodyNode) {
      this.report(
        "defmacro requires a body expression",
        node.span,
        "SEM_MACRO_REQUIRES_BODY"
      );
      return;
    }

    const body: MacroBody =
      bodyNode.kind === NodeKind.SyntaxQuote
        ? {
            kind: "template",
            template: bodyNode as ReaderMacroNode<NodeKind.SyntaxQuote>,
          }
        : {
            kind: "expression",
            expression: bodyNode,
          };

    if (macroSymbol) {
      this.macros.set(macroSymbol.id, {
        symbolId: macroSymbol.id,
        params,
        rest,
        body,
      });
    }
  }

  private async handleLet(node: ListNode, scopeId: ScopeId): Promise<void> {
    const bindingsNode = node.elements[1];
    if (!bindingsNode || bindingsNode.kind !== NodeKind.Vector) {
      if (bindingsNode) {
        await this.visit(bindingsNode, scopeId);
      }
      this.report(
        "let requires a vector of binding pairs",
        node.span,
        "SEM_LET_EXPECTS_VECTOR"
      );
      await this.visitLetBody(node, scopeId);
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
        await this.visit(init, childScopeId);
      }
      if (!target) {
        continue;
      }
      if (target.kind === NodeKind.Symbol) {
        this.defineSymbol(target, childScopeId, "var", "definition");
      } else {
        await this.visit(target, childScopeId);
        this.report(
          "Binding targets inside let must be symbols",
          target.span,
          "SEM_BINDING_REQUIRES_SYMBOL"
        );
      }
    }

    await this.visitLetBody(node, childScopeId);
  }

  private async visitLetBody(node: ListNode, scopeId: ScopeId): Promise<void> {
    for (let index = 2; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        await this.visit(element, scopeId);
      }
    }
  }

  private async handleFn(node: ListNode, scopeId: ScopeId): Promise<void> {
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
        await this.visit(paramsNode, scopeId);
      }
      this.report(
        "fn requires a vector of parameter symbols",
        node.span,
        "SEM_FN_EXPECTS_VECTOR"
      );
    } else {
      this.recordNode(paramsNode, scopeId);
      let sawAmpersand = false;

      for (let i = 0; i < paramsNode.elements.length; i++) {
        const param = paramsNode.elements[i];
        if (!param) {
          continue;
        }

        if (param.kind === NodeKind.Symbol && param.value === "&") {
          if (sawAmpersand) {
            this.report(
              "Only one & rest parameter allowed",
              param.span,
              "SEM_FN_DUPLICATE_REST"
            );
            continue;
          }
          sawAmpersand = true;
          const nextParam = paramsNode.elements[i + 1];
          if (!nextParam || nextParam.kind !== NodeKind.Symbol) {
            this.report(
              "& must be followed by a symbol",
              param.span,
              "SEM_FN_REST_REQUIRES_SYMBOL"
            );
          } else {
            this.defineSymbol(nextParam, fnScopeId, "parameter", "parameter");
            i++; // Skip the next parameter since we just consumed it
          }
          continue;
        }

        if (sawAmpersand) {
          this.report(
            "No parameters allowed after & rest parameter",
            param.span,
            "SEM_FN_PARAMS_AFTER_REST"
          );
          continue;
        }

        if (param.kind === NodeKind.Symbol) {
          this.defineSymbol(param, fnScopeId, "parameter", "parameter");
        } else {
          await this.visit(param, fnScopeId);
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
        await this.visit(element, fnScopeId);
      }
    }
  }

  private async handleNamespaceImport(
    node: NamespaceImportNode | ListNode,
    scopeId: ScopeId,
    kind: NamespaceImportKind
  ): Promise<void> {
    if (!this.isTopLevelScope(scopeId)) {
      this.report(
        `${kind} is only supported at the top level`,
        node.span,
        kind === "require"
          ? "SEM_REQUIRE_TOP_LEVEL"
          : kind === "import"
          ? "SEM_IMPORT_TOP_LEVEL"
          : "SEM_EXTERNAL_TOP_LEVEL"
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

    // For 'import' (without alias), handle differently
    if (kind === "import") {
      // Import doesn't need an alias - it flattens bindings
      if (!pathNode) {
        if (pathOperand) {
          await this.visit(pathOperand, scopeId);
        }
        this.report(
          `import expects a string literal path`,
          (pathOperand ?? node).span,
          "SEM_IMPORT_EXPECTS_STRING"
        );
      } else {
        this.recordImportWithFlattening(node, pathNode, scopeId);
      }
    } else {
      // require and external need an alias
      if (!aliasNode) {
        if (aliasOperand) {
          await this.visit(aliasOperand, scopeId);
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
          await this.visit(pathOperand, scopeId);
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
    }

    const elements = this.resolveImportElements(node);
    for (let index = 3; index < elements.length; index += 1) {
      const element = elements[index];
      if (element) {
        await this.visit(element, scopeId);
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

  private recordImportWithFlattening(
    node: NamespaceImportNode | ListNode,
    pathNode: StringNode,
    scopeId: ScopeId
  ): void {
    let moduleId: string | undefined;
    if (this.moduleResolver && this.moduleId) {
      const resolution = this.moduleResolver.resolve({
        fromModuleId: this.moduleId,
        specifier: pathNode.value,
        kind: "import",
      });
      if (!resolution.ok) {
        const suffix = resolution.reason ? `: ${resolution.reason}` : "";
        this.report(
          `Unable to resolve module ${pathNode.value}${suffix}`,
          pathNode.span,
          "SEM_IMPORT_RESOLVE_FAILED"
        );
        return;
      }
      moduleId = resolution.moduleId;
    }

    let flattenBindings: FlattenedImportBinding[] | undefined;

    if (moduleId) {
      if (!this.moduleExportsLookup) {
        this.report(
          `Module ${pathNode.value} does not provide export metadata`,
          pathNode.span,
          "SEM_IMPORT_MISSING_EXPORTS"
        );
        return;
      }

      const exports = this.moduleExportsLookup.getExports(moduleId);
      if (!exports) {
        this.report(
          `Module ${pathNode.value} does not provide export metadata`,
          pathNode.span,
          "SEM_IMPORT_MISSING_EXPORTS"
        );
        return;
      }

      flattenBindings = [];
      for (const exportedName of exports) {
        const syntheticNode: SymbolNode = {
          kind: NodeKind.Symbol,
          span: node.span,
          value: exportedName,
          lexeme: exportedName,
        };
        const record = this.defineSymbol(
          syntheticNode,
          scopeId,
          "var",
          "definition"
        );
        if (record) {
          flattenBindings.push({
            exportedName,
            identifier: record.alias,
          });
        }
      }
    }

    this.moduleImports.push({
      alias: "", // No alias for flattened imports
      specifier: pathNode.value,
      kind: "import",
      span: pathNode.span,
      moduleId,
      flatten: flattenBindings,
    });
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

  private async visitSequence(
    node: VectorNode | SetNode,
    scopeId: ScopeId
  ): Promise<void> {
    for (const element of node.elements) {
      if (element) {
        await this.visit(element, scopeId);
      }
    }
  }

  private async visitMap(node: MapNode, scopeId: ScopeId): Promise<void> {
    for (const entry of node.entries) {
      this.recordNode(entry, scopeId);
      if (entry.key) {
        await this.visit(entry.key, scopeId);
      }
      if (entry.value) {
        await this.visit(entry.value, scopeId);
      }
    }
  }

  private async visitSyntaxQuote(
    node: ReaderMacroNode,
    scopeId: ScopeId,
    context?: MacroExpansionContext
  ): Promise<void> {
    if (node.kind !== NodeKind.SyntaxQuote) {
      await this.visitReaderMacro(node, scopeId);
      return;
    }
    if (!node.target) {
      return;
    }
    // When not in a macro expansion context, just visit the target normally
    if (!context) {
      await this.visit(node.target, scopeId);
      return;
    }
    // This path is no longer used - macro expansion now goes through expandMacroOnce
    const expanded = await this.instantiateTemplate(node.target, context);
    if (expanded) {
      await this.visit(expanded, scopeId);
    }
  }

  private async visitReaderMacro(
    node: ReaderMacroNode | DispatchNode,
    scopeId: ScopeId
  ): Promise<void> {
    if (node.target) {
      await this.visit(node.target, scopeId);
    }
  }

  private async instantiateTemplate(
    node: ExpressionNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    switch (node.kind) {
      case NodeKind.List:
        return await this.instantiateSequence(node, context);
      case NodeKind.Vector:
        return (await this.instantiateSequence(node, context)) as VectorNode;
      case NodeKind.Set:
        return (await this.instantiateSequence(node, context)) as SetNode;
      case NodeKind.Map:
        return await this.instantiateMap(node, context);
      case NodeKind.Quote:
      case NodeKind.Deref:
        return await this.instantiateReader(node as ReaderMacroNode, context);
      case NodeKind.Dispatch:
        return await this.instantiateDispatch(node as DispatchNode, context);
      case NodeKind.SyntaxQuote:
        if (!node.target) {
          return null;
        }
        return await this.instantiateTemplate(node.target, context);
      case NodeKind.Unquote:
        return (
          (await this.materializeArgument(
            node as ReaderMacroNode<NodeKind.Unquote>,
            context
          )) ?? this.createNilLiteral(context.callSpan)
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

  private async instantiateSequence(
    node: ListNode | VectorNode | SetNode,
    context: MacroExpansionContext
  ): Promise<ListNode | VectorNode | SetNode> {
    const elements: ExpressionNode[] = [];
    for (const element of node.elements) {
      if (!element) {
        continue;
      }
      if (element.kind === NodeKind.Unquote) {
        const value = await this.materializeArgument(
          element as ReaderMacroNode<NodeKind.Unquote>,
          context
        );
        if (value) {
          elements.push(value);
        }
        continue;
      }
      if (element.kind === NodeKind.UnquoteSplicing) {
        const values = await this.materializeSplicedArgument(
          element as ReaderMacroNode<NodeKind.UnquoteSplicing>,
          context
        );
        elements.push(...values);
        continue;
      }
      const expanded = await this.instantiateTemplate(element, context);
      if (expanded) {
        elements.push(expanded);
      }
    }
    const instantiated = {
      ...node,
      elements,
    } as ListNode | VectorNode | SetNode;
    this.clearScopeMetadata(instantiated);
    return instantiated;
  }

  private async instantiateMap(
    node: MapNode,
    context: MacroExpansionContext
  ): Promise<MapNode> {
    const entries: MapEntryNode[] = [];
    for (const entry of node.entries) {
      const entryCopy: MapEntryNode = {
        ...entry,
        key: entry.key
          ? await this.instantiateTemplate(entry.key, context)
          : null,
        value: entry.value
          ? await this.instantiateTemplate(entry.value, context)
          : null,
      };
      this.clearScopeMetadata(entryCopy);
      entries.push(entryCopy);
    }
    const instantiated: MapNode = {
      ...node,
      entries,
    };
    this.clearScopeMetadata(instantiated);
    return instantiated;
  }

  private async instantiateReader(
    node: ReaderMacroNode,
    context: MacroExpansionContext
  ): Promise<ReaderMacroNode> {
    const instantiated = {
      ...node,
      target: node.target
        ? await this.instantiateTemplate(node.target, context)
        : null,
    };
    this.clearScopeMetadata(instantiated);
    return instantiated;
  }

  private async instantiateDispatch(
    node: DispatchNode,
    context: MacroExpansionContext
  ): Promise<DispatchNode> {
    const instantiated = {
      ...node,
      target: node.target
        ? await this.instantiateTemplate(node.target, context)
        : null,
    };
    this.clearScopeMetadata(instantiated);
    return instantiated;
  }

  private async materializeArgument(
    node: ReaderMacroNode<NodeKind.Unquote>,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    if (!node.target) {
      this.report(
        "Unquote requires a target expression",
        node.span,
        "SEM_MACRO_UNQUOTE_EMPTY"
      );
      return null;
    }
    return await this.evaluateUnquoteTarget(node.target, context);
  }

  private async materializeSplicedArgument(
    node: ReaderMacroNode<NodeKind.UnquoteSplicing>,
    context: MacroExpansionContext
  ): Promise<ExpressionNode[]> {
    if (!node.target) {
      this.report(
        "Unquote splicing requires a target expression",
        node.span,
        "SEM_MACRO_SPLICE_EMPTY"
      );
      return [];
    }
    const value = await this.evaluateUnquoteTarget(node.target, context);
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
        .map((element) =>
          this.cloneExpression(element, { preserveScopeMetadata: true })
        );
    }
    this.report(
      "Unquote splicing requires a sequence expression",
      node.span,
      "SEM_MACRO_SPLICE_SEQUENCE"
    );
    return [value];
  }

  private async evaluateUnquoteTarget(
    target: ExpressionNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    // Direct parameter substitution
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
      return this.cloneExpression(value, { preserveScopeMetadata: true });
    }

    return await this.evaluateCompileTimeExpression(target, context);
  }

  private async evaluateCompileTimeExpression(
    node: ExpressionNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    // Create an interpreter environment with macro parameter bindings
    const env = createRootEnvironment();
    for (const [name, value] of context.env.entries()) {
      try {
        const interpValue = nodeToValue(value);
        defineVariable(env, name, interpValue);
      } catch (error) {
        // If conversion fails, skip this binding
        continue;
      }
    }

    // Evaluate the expression using the full interpreter
    // Share the analyzer's gensym counter with the interpreter context
    const evalContext = {
      callDepth: 0,
      gensymCounter: { value: this.nextGensymId },
    };
    const result = await evaluate(node, env, evalContext);

    // Update the analyzer's counter after evaluation
    this.nextGensymId = evalContext.gensymCounter.value;

    if (!result.ok) {
      // Report interpreter errors
      for (const diagnostic of result.diagnostics) {
        this.report(
          diagnostic.message,
          diagnostic.span,
          diagnostic.code ?? "EVAL_ERROR"
        );
      }
      return null;
    }

    if (!result.value) {
      return null;
    }

    // Convert the result value back to an AST node
    try {
      return valueToNode(result.value, context.callSpan);
    } catch (error) {
      const message =
        error instanceof Error ? error.message : "Unknown conversion error";
      this.report(
        `Cannot convert compile-time result to AST: ${message}`,
        context.callSpan,
        "SEM_MACRO_EVAL_ERROR"
      );
      return null;
    }
  }

  private cloneExpression<T extends ExpressionNode>(
    node: T,
    options?: { preserveScopeMetadata?: boolean }
  ): T {
    const copy = this.duplicate(node);
    if (!options?.preserveScopeMetadata) {
      this.stripScopeMetadata(copy);
    }
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

  private clearScopeMetadata(
    node: ExpressionNode | MapEntryNode | ProgramNode | null | undefined
  ): void {
    if (!node) {
      return;
    }
    delete (node as { scopeId?: ScopeId }).scopeId;
  }

  private stripScopeMetadata(
    node: ExpressionNode | MapEntryNode | ProgramNode | null | undefined
  ): void {
    if (!node) {
      return;
    }
    this.clearScopeMetadata(node);
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
  private readonly used = new Set<string>(["__println", "eq", "count"]);

  private readonly operatorNames = new Map<string, string>([
    // Comparison operators
    ["<=", "_LT_EQ"],
    [">=", "_GT_EQ"],
    ["<", "_LT"],
    [">", "_GT"],
    ["=", "_EQ"],
    // Arithmetic operators
    ["+", "_PLUS"],
    ["-", "_DASH"],
    ["*", "_STAR"],
    ["/", "_SLASH"],
    // Logical operators
    ["!", "_BANG"],
    ["?", "_QMARK"],
    // Special
    ["~", "_TILDE"],
    ["@", "_AT"],
  ]);

  allocate(name: string, symbolId: SymbolId, preferRaw: boolean): string {
    const sanitized = this.sanitize(name);
    // If we're not preferring raw, append the symbol ID to the sanitized name
    const preferred = preferRaw ? sanitized : `${sanitized}__${symbolId}`;
    return this.reserve(this.ensureIdentifier(preferred));
  }

  private sanitize(value: string): string {
    // If the entire value is a known operator, use its name
    const operatorName = this.operatorNames.get(value);
    if (operatorName) {
      return operatorName;
    }

    // For compound names, check for trailing special characters first
    const trailingSpecialChars = new Set(["?", "!", "*", "+", "/", "%"]);
    let trailingOp = "";
    let baseValue = value;

    // Check if the value ends with a special char that should be preserved
    if (value.length > 1) {
      const lastChar = value.charAt(value.length - 1);
      if (lastChar && trailingSpecialChars.has(lastChar)) {
        const opName = this.operatorNames.get(lastChar);
        if (opName) {
          trailingOp = opName; // opName already includes leading underscore
          baseValue = value.slice(0, -1);
        }
      }
    }

    // Convert the base to underscores (without the trailing operator)
    let result = baseValue.replace(/[^a-zA-Z0-9_]/g, "_");

    // Remove trailing underscores before adding the operator suffix
    result = result.replace(/_+$/, "");

    // Add the trailing operator name
    result += trailingOp;

    // Ensure result doesn't start with a digit
    if (/^[0-9]/.test(result)) {
      result = "_" + result;
    }

    if (result.length === 0 || result === "_") {
      result = "_ident";
    }

    return result;
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
