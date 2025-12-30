import path from "node:path";
import { pathToFileURL } from "node:url";
import {
  DiagnosticSeverity,
  NodeKind,
  type Diagnostic,
  type ExpressionNode,
  type ListNode,
  type NamespaceImportKind,
  type NamespaceImportNode,
  type ProgramNode,
  type ReaderMacroNode,
  type ScopeId,
  type SourceSpan,
  type StringNode,
  type SymbolNode,
  BUILTIN_SYMBOLS,
  parseBindingPattern,
  type BindingPattern,
  type PatternError,
  type PatternErrorKind,
} from "@vibe/syntax";
import {
  evaluate,
  createRootEnvironment,
  extendEnvironment,
  defineVariable,
  valueToNode,
  nodeToValue,
  makeExternalNamespace,
  type Environment,
  type Value,
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
  readonly disableMacroRuntime?: boolean;
}

export interface AnalyzeResult {
  readonly ok: boolean;
  readonly diagnostics: Diagnostic[];
  readonly graph: SemanticGraph;
  readonly macroRuntime?: ReadonlyMap<string, Value>;
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
  readonly exportedIdentifier?: string;
}

export type ModuleExportKind = "var" | "macro";

export interface ModuleMacroDependency {
  readonly kind: "external" | "require";
  readonly alias: string;
  readonly specifier: string;
}

export interface ModuleExportedMacroClause {
  readonly params: readonly string[];
  readonly rest?: string;
  readonly body: ExpressionNode;
}

export interface ModuleExportedMacro {
  readonly clauses: readonly ModuleExportedMacroClause[];
  readonly dependencies?: readonly ModuleMacroDependency[];
}

export interface ModuleExportEntry {
  readonly name: string;
  readonly kind: ModuleExportKind;
  readonly macro?: ModuleExportedMacro;
  readonly macroRuntimeValue?: Value;
  readonly identifier?: string;
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
  getExports(moduleId: string): readonly ModuleExportEntry[] | undefined;
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

interface FnClauseDescriptor {
  readonly paramsNode: ExpressionNode | null;
  readonly bodyNodes: readonly ExpressionNode[];
  readonly span: SourceSpan;
}

interface TryCatchClauseDescriptor {
  readonly clause: ListNode;
  readonly binding: ExpressionNode | null;
  readonly bodyNodes: readonly ExpressionNode[];
}

interface TryFinallyClauseDescriptor {
  readonly clause: ListNode;
  readonly bodyNodes: readonly ExpressionNode[];
}

interface TryStructure {
  readonly bodyNodes: readonly ExpressionNode[];
  readonly catchClause?: TryCatchClauseDescriptor;
  readonly finallyClause?: TryFinallyClauseDescriptor;
}

type MacroBody =
  | {
      readonly kind: "template";
      readonly template: ReaderMacroNode<NodeKind.Quote>;
    }
  | {
      readonly kind: "expression";
      readonly expression: ExpressionNode;
    };

interface MacroClauseDescriptor {
  readonly paramsNode: ExpressionNode | null;
  readonly bodyNodes: readonly ExpressionNode[];
  readonly span: SourceSpan;
}

interface MacroClauseDefinition {
  readonly params: readonly string[];
  readonly rest?: string;
  readonly body: MacroBody;
}

interface MacroLiteralDefinition {
  readonly clauses: readonly MacroClauseDefinition[];
  readonly dependencies?: readonly ModuleMacroDependency[];
}

interface MacroDefinition {
  readonly symbolId: SymbolId;
  readonly clauses: readonly MacroClauseDefinition[];
  readonly dependencies?: readonly ModuleMacroDependency[];
  readonly originModuleId?: string;
}

type MacroClauseSelectionStatus = "exact" | "variadic" | "tooFew" | "tooMany";

interface MacroClauseSelection {
  readonly clause: MacroClauseDefinition;
  readonly status: MacroClauseSelectionStatus;
}

interface MacroExpansionContext {
  readonly env: Map<string, ExpressionNode>;
  readonly callSpan: SourceSpan;
  readonly dependencies?: readonly ModuleMacroDependency[];
  readonly originModuleId?: string;
  autoGensyms?: Map<string, string>;
}

const PATTERN_ERROR_CODE_MAP: Record<PatternErrorKind, string> = {
  PatternUnsupportedNode: "SEM_PATTERN_UNSUPPORTED",
  PatternRestRequiresTarget: "SEM_PATTERN_REST_TARGET",
  PatternRestDuplicate: "SEM_PATTERN_REST_DUPLICATE",
  PatternAsRequiresSymbol: "SEM_PATTERN_AS_SYMBOL",
  PatternAsDuplicate: "SEM_PATTERN_AS_DUPLICATE",
  PatternMapKeyMissingValue: "SEM_PATTERN_MAP_VALUE",
  PatternMapKeyUnsupported: "SEM_PATTERN_MAP_KEY",
  PatternMapKeysRequiresVector: "SEM_PATTERN_KEYS_VECTOR",
  PatternMapStringsRequiresVector: "SEM_PATTERN_STRS_VECTOR",
  PatternMapSymbolsRequiresVector: "SEM_PATTERN_SYMS_VECTOR",
  PatternMapDefaultsRequireMap: "SEM_PATTERN_OR_MAP",
  PatternDuplicateBinding: "SEM_PATTERN_DUPLICATE",
};

const DEFAULT_PATTERN_ERROR_CODE = "SEM_PATTERN_ERROR";

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
  private readonly macroDependencyCache = new Map<string, Value>();
  private readonly macroRuntimeEnv: Environment = createRootEnvironment();
  private readonly macroRuntimeBindings = new Map<string, Value>();
  private readonly builtins: readonly string[];
  private readonly moduleId?: string;
  private readonly moduleResolver?: ModuleResolver;
  private readonly moduleExportsLookup?: ModuleExportsLookup;
  private readonly disableMacroRuntime: boolean;
  private readonly aliasAllocator = new AliasAllocator();
  private nextScopeId = 0;
  private nextSymbolId = 0;
  private nextNodeId = 0;
  private nextGensymId = 0;
  private rootScopeId: ScopeId | null = null;
  private syntaxQuoteDepth = 0;

  constructor(options: AnalyzeOptions = {}) {
    this.builtins = options.builtins ?? (BUILTIN_SYMBOLS as readonly string[]);
    this.moduleId = options.moduleId;
    this.moduleResolver = options.moduleResolver;
    this.moduleExportsLookup = options.moduleExports;
    this.disableMacroRuntime = options.disableMacroRuntime ?? false;
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
      macroRuntime:
        this.macroRuntimeBindings.size > 0
          ? new Map(this.macroRuntimeBindings)
          : undefined,
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

      case NodeKind.Quote:
        this.recordNode(node, nodeScopeId);
        await this.visitQuote(node as ReaderMacroNode, nodeScopeId);
        break;

      case NodeKind.Symbol:
        if (
          this.syntaxQuoteDepth === 0 &&
          this.isAutoGensymPlaceholder(node.value)
        ) {
          this.report(
            `Auto gensym placeholder ${node.value} may only appear inside a syntax-quoted template`,
            node.span,
            "SEM_GENSYM_PLACEHOLDER_CONTEXT"
          );
        }
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

    this.macroExpansionStack.push(binding.id);
    let expanded: ExpressionNode | null = null;
    try {
      expanded = await this.expandMacroCall(
        definition,
        head.value,
        args,
        node.span
      );
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

  private async expandMacroCall(
    definition: MacroDefinition,
    macroName: string,
    args: readonly ExpressionNode[],
    callSpan: SourceSpan
  ): Promise<ExpressionNode | null> {
    if (definition.clauses.length === 0) {
      return null;
    }

    const selection = this.selectMacroClause(definition, args.length);
    if (!selection) {
      return null;
    }

    if (selection.status === "tooFew") {
      this.report(
        `Macro ${macroName} expects at least ${selection.clause.params.length} argument(s) but received ${args.length}`,
        callSpan,
        "SEM_MACRO_ARITY_MISMATCH"
      );
    } else if (selection.status === "tooMany") {
      this.report(
        `Macro ${macroName} expects ${selection.clause.params.length} argument(s) but received ${args.length}`,
        callSpan,
        "SEM_MACRO_ARITY_MISMATCH"
      );
    }

    const env = this.bindMacroArguments(selection.clause, args, callSpan);
    return await this.expandMacroBody(selection.clause.body, {
      env,
      callSpan,
      dependencies: definition.dependencies,
      originModuleId: definition.originModuleId ?? this.moduleId,
    });
  }

  private selectMacroClause(
    definition: MacroDefinition,
    argCount: number
  ): MacroClauseSelection | null {
    if (definition.clauses.length === 0) {
      return null;
    }

    const fixedClauses = definition.clauses.filter((clause) => !clause.rest);
    const variadicClause = definition.clauses.find((clause) => clause.rest);

    const exactMatch = fixedClauses.find(
      (clause) => clause.params.length === argCount
    );
    if (exactMatch) {
      return { clause: exactMatch, status: "exact" };
    }

    if (variadicClause && argCount >= variadicClause.params.length) {
      return { clause: variadicClause, status: "variadic" };
    }

    const sorted = [...definition.clauses].sort(
      (a, b) => a.params.length - b.params.length
    );
    for (const clause of sorted) {
      if (argCount < clause.params.length) {
        return { clause, status: "tooFew" };
      }
    }

    return {
      clause: sorted[sorted.length - 1]!,
      status: "tooMany",
    };
  }

  private bindMacroArguments(
    clause: MacroClauseDefinition,
    args: readonly ExpressionNode[],
    callSpan: SourceSpan
  ): Map<string, ExpressionNode> {
    const env = new Map<string, ExpressionNode>();
    clause.params.forEach((param, index) => {
      const arg = args[index];
      if (!arg) {
        this.report(
          `Missing argument for macro parameter ${param}`,
          callSpan,
          "SEM_MACRO_ARG_MISSING"
        );
        return;
      }
      env.set(param, arg);
    });

    if (clause.rest) {
      const restArgs = args.slice(clause.params.length);
      const restVector: ListNode = {
        kind: NodeKind.List,
        span: callSpan,
        elements: restArgs,
      };
      env.set(clause.rest, restVector);
    }

    return env;
  }

  private async expandMacroBody(
    body: MacroBody,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    if (body.kind === "template") {
      const template = body.template;
      if (!template.target) {
        return null;
      }
      return await this.instantiateTemplate(template.target, context);
    }

    return await this.evaluateCompileTimeExpression(body.expression, context);
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
            const expanded = await this.expandMacroCall(
              definition,
              head.value,
              args,
              node.span
            );

            if (expanded) {
              this.replaceNodeWithExpansion(node, expanded);
              await this.fullyExpandAndVisit(
                node,
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
        await this.handleDef(node, scopeId, "public");
        return true;
      case "defp":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleDef(node, scopeId, "private");
        return true;
      case "let":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleLet(node, scopeId);
        return true;
      case "fn+":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleFn(node, scopeId);
        return true;
      case "try":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleTry(node, scopeId);
        return true;
      case "throw":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleThrow(node, scopeId);
        return true;
      case "spread":
        this.recordSymbolUsage(head, scopeId, "usage");
        await this.handleSpread(node, scopeId);
        return true;
      case "macro+":
        this.recordSymbolUsage(head, scopeId, "usage");
        this.report(
          "macro+ literals are only supported inside binding forms",
          node.span,
          "SEM_MACRO_LITERAL_CONTEXT"
        );
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

  private async handleDef(
    node: ListNode,
    scopeId: ScopeId,
    visibility: "public" | "private" = "public"
  ): Promise<void> {
    const bindingNode = node.elements[1];
    const valueNode = node.elements[2];
    const macroLiteralNode =
      valueNode && this.isMacroLiteralNode(valueNode)
        ? (valueNode as ListNode)
        : null;
    const macroLiteral = macroLiteralNode
      ? await this.analyzeMacroLiteral(macroLiteralNode, scopeId)
      : null;
    const isMacroBinding = Boolean(macroLiteralNode);
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
      const symbolKind: SymbolKind = isMacroBinding ? "macro" : "var";
      const role: NodeSymbolRole = isMacroBinding ? "macro" : "definition";
      const record = this.defineSymbol(bindingNode, scopeId, symbolKind, role);
      if (record) {
        if (macroLiteral) {
          this.registerMacroLiteral(record, macroLiteral);
        } else if (
          this.isTopLevelScope(scopeId) &&
          !isMacroBinding &&
          visibility === "public"
        ) {
          this.recordModuleExport(record, node.span);
        }
      }
    }

    if (!isMacroBinding && valueNode) {
      await this.visit(valueNode, scopeId);
      if (bindingNode && bindingNode.kind === NodeKind.Symbol) {
        await this.exposeDefinitionToMacroRuntime(
          bindingNode.value,
          valueNode,
          scopeId
        );
      }
    }

    for (let index = 3; index < node.elements.length; index += 1) {
      const element = node.elements[index];
      if (element) {
        await this.visit(element, scopeId);
      }
    }
  }

  private createMacroBody(node: ExpressionNode): MacroBody {
    return node.kind === NodeKind.Quote
      ? {
          kind: "template",
          template: node as ReaderMacroNode<NodeKind.Quote>,
        }
      : {
          kind: "expression",
          expression: node,
        };
  }

  private collectInScopeDependencies(
    scopeId: ScopeId
  ): readonly ModuleMacroDependency[] | undefined {
    const scope = this.scopes.get(scopeId);
    if (!scope) {
      return undefined;
    }
    const dependencies: ModuleMacroDependency[] = [];
    for (const record of this.moduleImports) {
      if (
        (record.kind !== "external" && record.kind !== "require") ||
        !record.alias
      ) {
        continue;
      }
      const depKind: ModuleMacroDependency["kind"] =
        record.kind === "external" ? "external" : "require";
      dependencies.push({
        kind: depKind,
        alias: record.alias,
        specifier: record.specifier,
      });
    }
    return dependencies.length > 0 ? dependencies : undefined;
  }

  private async analyzeMacroLiteral(
    node: ListNode,
    scopeId: ScopeId
  ): Promise<MacroLiteralDefinition | null> {
    const clauseDescriptors = this.extractMacroLiteralClauses(node);
    if (clauseDescriptors.length === 0) {
      const paramsNode = node.elements[1];
      if (paramsNode) {
        await this.visit(paramsNode, scopeId);
      }
      this.report(
        "macro requires a list of parameter symbols",
        node.span,
        "SEM_MACRO_EXPECTS_LIST"
      );
      return null;
    }

    const macroClauses: MacroClauseDefinition[] = [];
    const seenFixedArities = new Set<number>();
    let variadicClauseIndex: number | null = null;

    for (
      let clauseIndex = 0;
      clauseIndex < clauseDescriptors.length;
      clauseIndex++
    ) {
      const clause = clauseDescriptors[clauseIndex]!;
      const paramInfo = await this.analyzeMacroClauseParameters(
        clause,
        scopeId
      );
      if (!paramInfo) {
        continue;
      }

      if (paramInfo.rest) {
        if (variadicClauseIndex !== null) {
          this.report(
            "macro allows only one variadic clause",
            clause.paramsNode?.span ?? clause.span,
            "SEM_MACRO_MULTIPLE_REST_CLAUSES"
          );
        } else {
          variadicClauseIndex = clauseIndex;
          if (clauseIndex !== clauseDescriptors.length - 1) {
            this.report(
              "Variadic macro clause must appear last",
              clause.paramsNode?.span ?? clause.span,
              "SEM_MACRO_REST_POSITION"
            );
          }
        }
      } else {
        const arity = paramInfo.params.length;
        if (seenFixedArities.has(arity)) {
          this.report(
            `macro already defines a clause for ${arity} argument(s)`,
            clause.paramsNode?.span ?? clause.span,
            "SEM_MACRO_DUPLICATE_ARITY"
          );
        } else {
          seenFixedArities.add(arity);
        }
      }

      if (clause.bodyNodes.length === 0) {
        this.report(
          "macro requires a body expression",
          clause.span,
          "SEM_MACRO_REQUIRES_BODY"
        );
        continue;
      }
      if (clause.bodyNodes.length > 1) {
        this.report(
          "macro currently supports a single body expression",
          clause.bodyNodes[1]!.span,
          "SEM_MACRO_SINGLE_BODY"
        );
      }

      const bodyNode = clause.bodyNodes[0];
      if (!bodyNode) {
        continue;
      }

      macroClauses.push({
        params: paramInfo.params,
        ...(paramInfo.rest ? { rest: paramInfo.rest } : {}),
        body: this.createMacroBody(bodyNode),
      });
    }

    if (macroClauses.length === 0) {
      return null;
    }

    return {
      clauses: macroClauses,
      dependencies: this.collectInScopeDependencies(scopeId),
    };
  }

  private registerMacroLiteral(
    symbol: SymbolRecord,
    literal: MacroLiteralDefinition
  ): void {
    if (literal.clauses.length === 0) {
      return;
    }
    this.macros.set(symbol.id, {
      symbolId: symbol.id,
      clauses: literal.clauses,
      dependencies: literal.dependencies,
      originModuleId: this.moduleId,
    });
  }

  private async handleLet(node: ListNode, scopeId: ScopeId): Promise<void> {
    const bindingsNode = node.elements[1];
    if (!bindingsNode || bindingsNode.kind !== NodeKind.List) {
      if (bindingsNode) {
        await this.visit(bindingsNode, scopeId);
      }
      this.report(
        "let requires a list of binding pairs",
        node.span,
        "SEM_LET_EXPECTS_LIST"
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
      const macroLiteralNode =
        init && this.isMacroLiteralNode(init) ? (init as ListNode) : null;
      const macroLiteral = macroLiteralNode
        ? await this.analyzeMacroLiteral(macroLiteralNode, childScopeId)
        : null;
      const isMacroBinding = Boolean(macroLiteralNode);
      if (!macroLiteralNode && init) {
        await this.visit(init, childScopeId);
      }
      if (!target) {
        continue;
      }
      if (target.kind === NodeKind.Symbol) {
        const symbolKind: SymbolKind = isMacroBinding ? "macro" : "var";
        const role: NodeSymbolRole = isMacroBinding ? "macro" : "definition";
        const record = this.defineSymbol(
          target,
          childScopeId,
          symbolKind,
          role
        );
        if (record && macroLiteral) {
          this.registerMacroLiteral(record, macroLiteral);
        }
      } else {
        const pattern = this.parseBindingPatternOrReport(target);
        if (!pattern) {
          continue;
        }
        await this.visitPatternDefaults(pattern, childScopeId);
        this.declarePatternBindings(pattern, childScopeId, "var", "definition");
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
    const clauses = this.extractFnClauses(node);
    if (clauses.length === 0) {
      const paramsNode = node.elements[1];
      if (paramsNode) {
        await this.visit(paramsNode, scopeId);
      }
      this.report(
        "fn requires a list of parameter symbols",
        node.span,
        "SEM_FN_EXPECTS_LIST"
      );
      for (let index = 2; index < node.elements.length; index += 1) {
        const element = node.elements[index];
        if (element) {
          await this.visit(element, scopeId);
        }
      }
      return;
    }

    const seenFixedArities = new Set<number>();
    let variadicClauseIndex: number | null = null;

    for (let clauseIndex = 0; clauseIndex < clauses.length; clauseIndex += 1) {
      const clause = clauses[clauseIndex]!;
      const clauseScopeId = this.resolveChildScopeId(scopeId, [
        ...this.collectFnParamHints(clause.paramsNode),
        ...clause.bodyNodes,
      ]);

      const paramInfo = await this.analyzeFnClauseParameters(
        clause,
        clauseScopeId,
        scopeId
      );

      if (paramInfo) {
        if (paramInfo.isVariadic) {
          if (variadicClauseIndex !== null) {
            this.report(
              "fn allows only one variadic clause",
              clause.paramsNode?.span ?? clause.span,
              "SEM_FN_MULTIPLE_REST_CLAUSES"
            );
          } else {
            variadicClauseIndex = clauseIndex;
            if (clauseIndex !== clauses.length - 1) {
              this.report(
                "Variadic fn clause must appear last",
                clause.paramsNode?.span ?? clause.span,
                "SEM_FN_REST_POSITION"
              );
            }
          }
        } else if (seenFixedArities.has(paramInfo.arity)) {
          this.report(
            `fn already defines a clause for ${paramInfo.arity} argument(s)`,
            clause.paramsNode?.span ?? clause.span,
            "SEM_FN_DUPLICATE_ARITY"
          );
        } else {
          seenFixedArities.add(paramInfo.arity);
        }
      }

      if (clause.bodyNodes.length === 0) {
        this.report(
          "fn clause requires a body expression",
          clause.span,
          "SEM_FN_CLAUSE_REQUIRES_BODY"
        );
      }
      for (const bodyNode of clause.bodyNodes) {
        await this.visit(bodyNode, clauseScopeId);
      }
    }
  }

  private async handleTry(node: ListNode, scopeId: ScopeId): Promise<void> {
    const structure = this.partitionTryStructure(node);
    if (
      structure.bodyNodes.length === 0 &&
      !structure.catchClause &&
      !structure.finallyClause
    ) {
      this.report(
        "try requires at least one body expression, catch clause, or finally clause",
        node.span,
        "SEM_TRY_EXPECTS_BODY"
      );
    }

    for (const bodyNode of structure.bodyNodes) {
      await this.visit(bodyNode, scopeId);
    }

    if (structure.catchClause) {
      const { clause, binding, bodyNodes } = structure.catchClause;
      const catchScopeId = this.resolveChildScopeId(scopeId, [
        ...(binding ? [binding] : []),
        ...bodyNodes,
      ]);
      if (!binding || binding.kind !== NodeKind.Symbol) {
        if (binding) {
          await this.visit(binding, scopeId);
        }
        this.report(
          "catch requires a symbol binding",
          clause.span,
          "SEM_TRY_CATCH_REQUIRES_SYMBOL"
        );
      } else {
        this.defineSymbol(binding, catchScopeId, "var", "definition");
      }

      if (bodyNodes.length === 0) {
        this.report(
          "catch requires at least one body expression",
          clause.span,
          "SEM_TRY_CATCH_REQUIRES_BODY"
        );
      }

      for (const expr of bodyNodes) {
        await this.visit(expr, catchScopeId);
      }
    }

    if (structure.finallyClause) {
      const { clause, bodyNodes } = structure.finallyClause;
      if (bodyNodes.length === 0) {
        this.report(
          "finally requires at least one body expression",
          clause.span,
          "SEM_TRY_FINALLY_REQUIRES_BODY"
        );
      }
      for (const expr of bodyNodes) {
        await this.visit(expr, scopeId);
      }
    }
  }

  private async handleThrow(node: ListNode, scopeId: ScopeId): Promise<void> {
    const argNode = node.elements[1];
    if (!argNode) {
      this.report(
        "throw requires a value expression",
        node.span,
        "SEM_THROW_REQUIRES_VALUE"
      );
    } else {
      await this.visit(argNode, scopeId);
    }

    for (let index = 2; index < node.elements.length; index += 1) {
      const extra = node.elements[index];
      if (!extra) {
        continue;
      }
      this.report(
        "throw accepts exactly one argument",
        extra.span,
        "SEM_THROW_TOO_MANY_ARGS"
      );
      await this.visit(extra, scopeId);
    }
  }

  private async handleSpread(node: ListNode, scopeId: ScopeId): Promise<void> {
    const argNode = node.elements[1];
    if (!argNode) {
      this.report(
        "spread requires a value expression",
        node.span,
        "SEM_SPREAD_REQUIRES_VALUE"
      );
    } else {
      await this.visit(argNode, scopeId);
    }

    for (let index = 2; index < node.elements.length; index += 1) {
      const extra = node.elements[index];
      if (!extra) {
        continue;
      }
      this.report(
        "spread accepts exactly one argument",
        extra.span,
        "SEM_SPREAD_TOO_MANY_ARGS"
      );
      await this.visit(extra, scopeId);
    }
  }

  private partitionTryStructure(node: ListNode): TryStructure {
    const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
    const bodyNodes: ExpressionNode[] = [];
    let catchClause: TryCatchClauseDescriptor | undefined;
    let finallyClause: TryFinallyClauseDescriptor | undefined;
    let encounteredFinally = false;

    for (const expr of tail) {
      if (expr.kind === NodeKind.List) {
        const head = expr.elements[0];
        if (head && head.kind === NodeKind.Symbol) {
          if (head.value === "catch") {
            let skipClause = false;
            if (encounteredFinally) {
              this.report(
                "catch clauses must appear before finally clauses",
                expr.span,
                "SEM_TRY_CLAUSE_ORDER"
              );
              skipClause = true;
            }
            if (catchClause) {
              this.report(
                "try supports only one catch clause",
                expr.span,
                "SEM_TRY_DUPLICATE_CATCH"
              );
              skipClause = true;
            }
            if (skipClause) {
              continue;
            }
            catchClause = {
              clause: expr,
              binding: expr.elements[1] ?? null,
              bodyNodes: expr.elements
                .slice(2)
                .filter((element): element is ExpressionNode =>
                  Boolean(element)
                ),
            };
            continue;
          } else if (head.value === "finally") {
            if (finallyClause) {
              this.report(
                "try supports only one finally clause",
                expr.span,
                "SEM_TRY_DUPLICATE_FINALLY"
              );
              continue;
            }
            finallyClause = {
              clause: expr,
              bodyNodes: expr.elements
                .slice(1)
                .filter((element): element is ExpressionNode =>
                  Boolean(element)
                ),
            };
            encounteredFinally = true;
            continue;
          }
        }
      }

      if (catchClause || finallyClause) {
        this.report(
          "try body expressions must appear before catch/finally clauses",
          expr.span,
          "SEM_TRY_BODY_ORDER"
        );
      }
      bodyNodes.push(expr);
    }

    return {
      bodyNodes,
      ...(catchClause ? { catchClause } : {}),
      ...(finallyClause ? { finallyClause } : {}),
    };
  }

  private collectFnParamHints(
    node: ExpressionNode | null
  ): readonly ExpressionNode[] {
    if (!node) {
      return [];
    }
    if (node.kind === NodeKind.List) {
      return node.elements.filter((element): element is ExpressionNode =>
        Boolean(element)
      );
    }
    return [node];
  }

  private extractFnClauses(node: ListNode): readonly FnClauseDescriptor[] {
    const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
    if (tail.length === 0) {
      return [];
    }

    const clauseLists = tail.filter(
      (element): element is ListNode => element.kind === NodeKind.List
    );
    if (clauseLists.length !== tail.length) {
      return [];
    }

    return clauseLists.map((clause) => ({
      paramsNode: clause.elements[0] ?? null,
      bodyNodes: clause.elements
        .slice(1)
        .filter((element): element is ExpressionNode => Boolean(element)),
      span: clause.span,
    }));
  }

  private extractMacroLiteralClauses(
    node: ListNode
  ): readonly MacroClauseDescriptor[] {
    const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
    if (tail.length === 0) {
      return [];
    }

    const clauseLists = tail.filter(
      (element): element is ListNode => element.kind === NodeKind.List
    );
    if (clauseLists.length !== tail.length) {
      return [];
    }

    return clauseLists.map((clause) => ({
      paramsNode: clause.elements[0] ?? null,
      bodyNodes: clause.elements
        .slice(1)
        .filter((element): element is ExpressionNode => Boolean(element)),
      span: clause.span,
    }));
  }

  private isMacroLiteralNode(node: ExpressionNode | null): node is ListNode {
    if (!node || node.kind !== NodeKind.List) {
      return false;
    }
    const head = node.elements[0];
    return Boolean(
      head && head.kind === NodeKind.Symbol && head.value === "macro+"
    );
  }

  private async analyzeFnClauseParameters(
    clause: FnClauseDescriptor,
    clauseScopeId: ScopeId,
    parentScopeId: ScopeId
  ): Promise<{ arity: number; isVariadic: boolean } | null> {
    const { paramsNode } = clause;
    if (!paramsNode || paramsNode.kind !== NodeKind.List) {
      if (paramsNode) {
        await this.visit(paramsNode, parentScopeId);
      }
      this.report(
        "fn requires a list of parameter symbols",
        paramsNode?.span ?? clause.span,
        "SEM_FN_EXPECTS_LIST"
      );
      return null;
    }

    this.recordNode(paramsNode, parentScopeId);
    let arity = 0;
    let sawAmpersand = false;
    let isVariadic = false;

    for (let i = 0; i < paramsNode.elements.length; i += 1) {
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
          this.defineSymbol(nextParam, clauseScopeId, "parameter", "parameter");
          isVariadic = true;
          i += 1;
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

      const pattern = this.parseBindingPatternOrReport(param);
      if (!pattern) {
        continue;
      }
      await this.visitPatternDefaults(pattern, clauseScopeId);
      this.declarePatternBindings(
        pattern,
        clauseScopeId,
        "parameter",
        "parameter"
      );
      arity += 1;
    }

    return { arity, isVariadic };
  }

  private async analyzeMacroClauseParameters(
    clause: MacroClauseDescriptor,
    scopeId: ScopeId
  ): Promise<{ params: string[]; rest?: string } | null> {
    const { paramsNode } = clause;
    if (!paramsNode || paramsNode.kind !== NodeKind.List) {
      if (paramsNode) {
        await this.visit(paramsNode, scopeId);
      }
      this.report(
        "macro requires a list of parameter symbols",
        paramsNode?.span ?? clause.span,
        "SEM_MACRO_EXPECTS_LIST"
      );
      return null;
    }

    this.recordNode(paramsNode, scopeId);
    const params: string[] = [];
    let rest: string | undefined;
    let sawAmpersand = false;

    for (let index = 0; index < paramsNode.elements.length; index += 1) {
      const param = paramsNode.elements[index];
      if (!param) {
        continue;
      }

      if (param.kind !== NodeKind.Symbol) {
        await this.visit(param, scopeId);
        this.report(
          "macro parameters must be symbols",
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
        const nextParam = paramsNode.elements[index + 1];
        if (!nextParam || nextParam.kind !== NodeKind.Symbol) {
          this.report(
            "& must be followed by a symbol",
            param.span,
            "SEM_MACRO_REST_REQUIRES_SYMBOL"
          );
        } else {
          rest = nextParam.value;
          index += 1;
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

    return { params, rest };
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

      let importRecord: ModuleImportRecord | null = null;

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
        importRecord = this.recordNamespaceImport(aliasNode, pathNode, kind);
        if (
          kind === "require" &&
          importRecord.moduleId &&
          this.moduleExportsLookup
        ) {
          this.registerRequireMacroBindings(
            aliasNode,
            scopeId,
            importRecord.moduleId,
            pathNode.span
          );
        }
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
  ): ModuleImportRecord {
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
    return record;
  }

  private registerRequireMacroBindings(
    aliasNode: SymbolNode,
    scopeId: ScopeId,
    moduleId: string,
    span: SourceSpan
  ): void {
    if (!this.moduleExportsLookup) {
      return;
    }
    const exports = this.moduleExportsLookup.getExports(moduleId);
    if (!exports) {
      return;
    }
    for (const entry of exports) {
      if (entry.kind !== "macro") {
        continue;
      }
      if (!entry.macro) {
        continue;
      }
      const namespaced = `${aliasNode.value}/${entry.name}`;
      const syntheticNode: SymbolNode = {
        kind: NodeKind.Symbol,
        span: aliasNode.span,
        value: namespaced,
        lexeme: namespaced,
      };
      const record = this.defineSymbol(
        syntheticNode,
        scopeId,
        "macro",
        "macro"
      );
      if (record) {
        this.registerImportedMacro(record, entry.macro, moduleId);
      }
    }
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

      const bindings: FlattenedImportBinding[] = [];
      for (const entry of exports) {
        const exportedName = entry.name;
        const syntheticNode: SymbolNode = {
          kind: NodeKind.Symbol,
          span: node.span,
          value: exportedName,
          lexeme: exportedName,
        };
        const symbolKind: SymbolKind = entry.kind === "macro" ? "macro" : "var";
        const role: NodeSymbolRole =
          entry.kind === "macro" ? "macro" : "definition";
        const record = this.defineSymbol(
          syntheticNode,
          scopeId,
          symbolKind,
          role
        );
        if (record) {
          if (entry.kind === "macro") {
            if (!entry.macro) {
              this.report(
                `Module ${pathNode.value} is missing macro metadata for ${exportedName}`,
                pathNode.span,
                "SEM_IMPORT_MACRO_METADATA"
              );
              continue;
            }
            this.registerImportedMacro(record, entry.macro, moduleId);
            continue;
          }
          if (entry.macroRuntimeValue) {
            this.bindMacroRuntimeValue(exportedName, entry.macroRuntimeValue);
          }
          bindings.push({
            exportedName,
            identifier: record.alias,
            exportedIdentifier: (entry as any).identifier ?? exportedName,
          });
        }
      }
      if (bindings.length > 0) {
        flattenBindings = bindings;
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

  private registerImportedMacro(
    symbol: SymbolRecord,
    metadata: ModuleExportedMacro,
    originModuleId?: string
  ): void {
    if (!metadata.clauses || metadata.clauses.length === 0) {
      return;
    }
    const clauses: MacroClauseDefinition[] = metadata.clauses.map((clause) => {
      const bodyClone = this.cloneExpression(clause.body);
      return {
        params: [...clause.params],
        ...(clause.rest ? { rest: clause.rest } : {}),
        body: this.createMacroBody(bodyClone),
      };
    });

    this.macros.set(symbol.id, {
      symbolId: symbol.id,
      clauses,
      dependencies: metadata.dependencies?.map((dep) => ({ ...dep })),
      originModuleId: originModuleId ?? this.moduleId,
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
      if (process.env.VIBE_DEBUG) {
        try {
          console.debug(
            `[SemanticAnalyzer] update export name=${symbol.name} identifier=${
              symbol.alias
            } span=${JSON.stringify(span)}`
          );
        } catch {}
      }
      // Replace the whole record rather than mutating readonly properties
      this.moduleExportsTable[existingIndex] = {
        name: symbol.name,
        identifier: symbol.alias,
        span,
      };
      return;
    }
    if (process.env.VIBE_DEBUG) {
      try {
        console.debug(
          `[SemanticAnalyzer] add export name=${symbol.name} identifier=${
            symbol.alias
          } span=${JSON.stringify(span)}`
        );
      } catch {}
    }
    this.moduleExportsTable.push({
      name: symbol.name,
      identifier: symbol.alias,
      span,
    });
  }

  private async visitQuote(
    node: ReaderMacroNode,
    scopeId: ScopeId,
    context?: MacroExpansionContext
  ): Promise<void> {
    if (!node.target) {
      return;
    }
    // When not in a macro expansion context, just visit the target normally
    if (!context) {
      this.syntaxQuoteDepth += 1;
      try {
        // Walk the quoted structure without expanding macros or recording
        // symbol usages so quoted forms remain literal data at runtime.
        await this.visitQuoted(node.target, scopeId);
      } finally {
        this.syntaxQuoteDepth -= 1;
      }
      return;
    }
    const expanded = await this.instantiateTemplate(node.target, context);
    if (expanded) {
      await this.visit(expanded, scopeId);
    }
  }

  private async visitQuoted(node: ExpressionNode, scopeId: ScopeId) {
    const nodeScopeId = this.getScopeForNode(node, scopeId);
    switch (node.kind) {
      case NodeKind.List: {
        this.recordNode(node, nodeScopeId);
        for (const el of (node as any).elements) {
          if (el) await this.visitQuoted(el, nodeScopeId);
        }
        break;
      }

      case NodeKind.Symbol:
      case NodeKind.Keyword:
      case NodeKind.Number:
      case NodeKind.String:
      case NodeKind.Boolean:
      case NodeKind.Nil: {
        // Record the node but do not resolve usages or alter bindings.
        this.recordNode(node, nodeScopeId);
        break;
      }
      case NodeKind.Quote:
      case NodeKind.NamespaceImport: {
        // For nested reader-macros inside a quoted form, walk their target(s)
        // conservatively without triggering expansion.
        this.recordNode(node, nodeScopeId);
        if ((node as any).target) {
          await this.visitQuoted((node as any).target, nodeScopeId);
        }
        if (node.kind === NodeKind.NamespaceImport) {
          for (const el of (node as NamespaceImportNode).elements) {
            if (el) await this.visitQuoted(el, nodeScopeId);
          }
        }
        break;
      }
      default:
        this.recordNode(node, nodeScopeId);
        break;
    }
  }

  private async instantiateTemplate(
    node: ExpressionNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    switch (node.kind) {
      case NodeKind.List:
        // Check if this is an unquote form: (unquote x)
        if (this.isUnquoteCall(node)) {
          return await this.handleUnquoteCall(node, context);
        }
        return await this.instantiateSequence(node, context);

      case NodeKind.Quote:
        if (!node.target) {
          return null;
        }
        return await this.instantiateTemplate(
          node.target,
          this.forkAutoGensymContext(context)
        );
      case NodeKind.Symbol:
        return this.instantiateSymbolNode(node as SymbolNode, context);
      default:
        return this.cloneExpression(node);
    }
  }

  private async instantiateSequence(
    node: ListNode,
    context: MacroExpansionContext
  ): Promise<ListNode> {
    const elements: ExpressionNode[] = [];
    for (const element of node.elements) {
      if (!element) {
        continue;
      }
      if (element.kind === NodeKind.List && this.isSpreadCall(element)) {
        const spreadValues = await this.handleSpreadElement(element, context);
        if (spreadValues) {
          elements.push(...spreadValues);
        }
        continue;
      }
      // Check if element is (unquote x)
      if (element.kind === NodeKind.List && this.isUnquoteCall(element)) {
        const value = await this.handleUnquoteCall(element, context);
        if (value) {
          elements.push(value);
        }
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
    } as ListNode;
    this.clearScopeMetadata(instantiated);
    return instantiated;
  }

  /* Map instantiation removed */

  private instantiateSymbolNode(
    node: SymbolNode,
    context: MacroExpansionContext
  ): SymbolNode {
    if (!this.isAutoGensymPlaceholder(node.value)) {
      return this.cloneExpression(node);
    }

    if (node.value.includes("/")) {
      this.report(
        `Auto gensym placeholder ${node.value} cannot include a namespace`,
        node.span,
        "SEM_GENSYM_PLACEHOLDER_NAMESPACE"
      );
      return this.cloneExpression(node);
    }

    const placeholders = this.ensureAutoGensymMap(context);
    const existing = placeholders.get(node.value);
    if (existing) {
      return this.createSymbolLiteral(node.span, existing);
    }

    const hint = node.value.slice(0, -1);
    const generated = this.createGensymSymbol(
      node.span,
      hint.length > 0 ? hint : null
    );
    placeholders.set(node.value, generated.value);
    return generated;
  }

  private forkAutoGensymContext(
    context: MacroExpansionContext
  ): MacroExpansionContext {
    return {
      ...context,
      autoGensyms: new Map<string, string>(),
    };
  }

  private ensureAutoGensymMap(
    context: MacroExpansionContext
  ): Map<string, string> {
    if (!context.autoGensyms) {
      context.autoGensyms = new Map<string, string>();
    }
    return context.autoGensyms;
  }

  private isUnquoteCall(node: ListNode): boolean {
    const head = node.elements[0];
    return head?.kind === NodeKind.Symbol && head.value === "unquote";
  }

  private isSpreadCall(node: ListNode): boolean {
    const head = node.elements[0];
    return head?.kind === NodeKind.Symbol && head.value === "spread";
  }

  private async handleUnquoteCall(
    node: ListNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    const arg = node.elements[1];
    if (!arg) {
      this.report(
        "Unquote requires a target expression",
        node.span,
        "SEM_MACRO_UNQUOTE_EMPTY"
      );
      return null;
    }
    return await this.evaluateUnquoteTarget(arg, context);
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
      return this.cloneExpression(value);
    }

    return await this.evaluateCompileTimeExpression(target, context);
  }

  private async handleSpreadElement(
    node: ListNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode[] | null> {
    const target = node.elements[1];
    if (
      !target ||
      target.kind !== NodeKind.List ||
      !this.isUnquoteCall(target)
    ) {
      this.report(
        "spread inside quote must wrap (unquote ...)",
        node.span,
        "SEM_MACRO_SPREAD_UNQUOTE"
      );
      return null;
    }
    const value = await this.handleUnquoteCall(target as ListNode, context);
    if (!value) {
      return null;
    }
    if (value.kind === NodeKind.List) {
      const expanded: ExpressionNode[] = [];
      for (const element of value.elements) {
        if (element) {
          expanded.push(this.cloneExpression(element));
        }
      }
      return expanded;
    }
    this.report(
      "spread expects a list inside quote",
      node.span,
      "SEM_MACRO_SPREAD_SEQUENCE"
    );
    return [value];
  }

  private async evaluateCompileTimeExpression(
    node: ExpressionNode,
    context: MacroExpansionContext
  ): Promise<ExpressionNode | null> {
    const value = await this.evaluateCompileTimeValue(node, context);
    if (!value) {
      return null;
    }

    try {
      return valueToNode(value, context.callSpan);
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

  private async evaluateCompileTimeValue(
    node: ExpressionNode,
    context: MacroExpansionContext
  ): Promise<Value | null> {
    const env = extendEnvironment(this.macroRuntimeEnv);
    await this.seedMacroDependencies(
      env,
      context.dependencies,
      context.callSpan,
      context.originModuleId
    );

    for (const [name, value] of context.env.entries()) {
      try {
        const interpValue = nodeToValue(value);
        defineVariable(env, name, interpValue);
      } catch {
        continue;
      }
    }

    const evalContext = {
      callDepth: 0,
      gensymCounter: { value: this.nextGensymId },
    };
    const result = await evaluate(node, env, evalContext);
    this.nextGensymId = evalContext.gensymCounter.value;

    if (!result.ok) {
      for (const diagnostic of result.diagnostics) {
        this.report(
          diagnostic.message,
          diagnostic.span,
          diagnostic.code ?? "EVAL_ERROR"
        );
      }
      return null;
    }

    return result.value ?? null;
  }

  private bindMacroRuntimeValue(name: string, value: Value): void {
    defineVariable(this.macroRuntimeEnv, name, value);
    this.macroRuntimeBindings.set(name, value);
  }

  private shouldExposeDefinitionToMacroRuntime(scopeId: ScopeId): boolean {
    return this.isTopLevelScope(scopeId);
  }

  private async exposeDefinitionToMacroRuntime(
    name: string,
    valueNode: ExpressionNode,
    scopeId: ScopeId
  ): Promise<void> {
    if (
      this.disableMacroRuntime ||
      !this.shouldExposeDefinitionToMacroRuntime(scopeId)
    ) {
      return;
    }

    const value = await this.evaluateCompileTimeValue(valueNode, {
      env: new Map(),
      callSpan: valueNode.span,
      dependencies: this.collectInScopeDependencies(scopeId),
      originModuleId: this.moduleId,
    });

    if (value) {
      this.bindMacroRuntimeValue(name, value);
    }
  }

  private async seedMacroDependencies(
    env: Environment,
    dependencies: readonly ModuleMacroDependency[] | undefined,
    span: SourceSpan,
    originModuleId?: string
  ): Promise<void> {
    if (!dependencies || dependencies.length === 0) {
      return;
    }
    for (const dependency of dependencies) {
      const resolvedSpecifier = this.resolveMacroDependencySpecifier(
        dependency,
        originModuleId,
        span
      );
      if (!resolvedSpecifier) {
        continue;
      }
      const cacheKey = this.dependencyCacheKey(
        dependency.kind,
        resolvedSpecifier
      );
      let cached = this.macroDependencyCache.get(cacheKey);
      if (!cached) {
        try {
          const module = await import(resolvedSpecifier);
          cached = makeExternalNamespace(module);
          this.macroDependencyCache.set(cacheKey, cached);
        } catch (error) {
          const message =
            error instanceof Error ? error.message : String(error);
          this.report(
            `Failed to load ${dependency.kind} dependency ${dependency.alias} (${dependency.specifier}): ${message}`,
            span,
            "SEM_MACRO_DEPENDENCY_FAILED"
          );
          continue;
        }
      }
      defineVariable(env, dependency.alias, cached);
    }
  }

  private dependencyCacheKey(
    kind: ModuleMacroDependency["kind"],
    specifier: string
  ): string {
    return `${kind}:${specifier}`;
  }

  private resolveMacroDependencySpecifier(
    dependency: ModuleMacroDependency,
    originModuleId: string | undefined,
    span: SourceSpan
  ): string | null {
    if (dependency.kind === "external") {
      return dependency.specifier;
    }
    if (dependency.kind === "require") {
      const specifier = dependency.specifier;
      if (specifier.startsWith("./") || specifier.startsWith("../")) {
        if (!originModuleId) {
          this.report(
            `Cannot resolve relative macro dependency ${specifier} without module context`,
            span,
            "SEM_MACRO_DEPENDENCY_FAILED"
          );
          return null;
        }
        const resolved = path.resolve(path.dirname(originModuleId), specifier);
        return pathToFileURL(resolved).href;
      }
      if (specifier.startsWith("/")) {
        return pathToFileURL(specifier).href;
      }
      return specifier;
    }
    return null;
  }

  private parseBindingPatternOrReport(
    node: ExpressionNode
  ): BindingPattern | null {
    const result = parseBindingPattern(node);
    if (!result.ok) {
      this.reportPatternErrors(result.errors);
      return null;
    }
    return result.pattern;
  }

  private reportPatternErrors(errors: readonly PatternError[]): void {
    for (const error of errors) {
      const code =
        PATTERN_ERROR_CODE_MAP[error.kind] ?? DEFAULT_PATTERN_ERROR_CODE;
      this.report(error.message, error.span, code);
    }
  }

  private declarePatternBindings(
    pattern: BindingPattern,
    scopeId: ScopeId,
    kind: SymbolKind,
    role: NodeSymbolRole
  ): void {
    switch (pattern.kind) {
      case "symbol":
        this.defineSymbol(pattern.node, scopeId, kind, role);
        return;
      case "sequence":
        for (const element of pattern.elements) {
          this.declarePatternBindings(element, scopeId, kind, role);
        }
        if (pattern.rest) {
          this.declarePatternBindings(pattern.rest, scopeId, kind, role);
        }
        if (pattern.as) {
          this.defineSymbol(pattern.as, scopeId, kind, role);
        }
        return;
      default:
        return;
    }
  }

  private async visitPatternDefaults(
    pattern: BindingPattern,
    scopeId: ScopeId
  ): Promise<void> {
    if (pattern.kind === "sequence") {
      for (const element of pattern.elements) {
        await this.visitPatternDefaults(element, scopeId);
      }
      if (pattern.rest) {
        await this.visitPatternDefaults(pattern.rest, scopeId);
      }
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
    node: ExpressionNode | ProgramNode | null | undefined
  ): void {
    if (!node) {
      return;
    }
    delete (node as { scopeId?: ScopeId }).scopeId;
  }

  private assignScopeMetadata(
    node: ExpressionNode | ProgramNode | null | undefined,
    scopeId: ScopeId
  ): void {
    if (!node) {
      return;
    }
    (node as { scopeId?: ScopeId }).scopeId = scopeId;
  }

  private stripScopeMetadata(
    node: ExpressionNode | ProgramNode | null | undefined
  ): void {
    if (!node) {
      return;
    }
    this.clearScopeMetadata(node);
    switch (node.kind) {
      case NodeKind.List:
        for (const element of node.elements) {
          this.stripScopeMetadata(element);
        }
        break;

      case NodeKind.Quote:
        this.stripScopeMetadata((node as ReaderMacroNode).target);
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
    return this.createSymbolLiteral(span, unique);
  }

  private createSymbolLiteral(span: SourceSpan, name: string): SymbolNode {
    return {
      kind: NodeKind.Symbol,
      span,
      lexeme: name,
      value: name,
    };
  }

  private isAutoGensymPlaceholder(value: string): boolean {
    return value.endsWith("#");
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
    const existing = this.scopes.get(scopeId);
    if (existing && existing.record.parentId !== fallback) {
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
          if (
            exports &&
            !exports.some((entry) => entry.name === namespaceRef.member)
          ) {
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
    node: ExpressionNode | ProgramNode,
    scopeId: ScopeId,
    symbol?: NodeSymbolInfo
  ): NodeId {
    const scope = this.scopes.get(scopeId);
    if (!scope) {
      throw new Error(`Missing scope ${scopeId}`);
    }
    this.assignScopeMetadata(node, scopeId);
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
  private readonly used = new Set<string>([]);

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
    ["#", "_HASH"],
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
    const trailingSpecialChars = new Set(["?", "!", "*", "+", "/", "%", "#"]);
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
    if (process.env.VIBE_DEBUG) {
      try {
        // lightweight debug to trace alias allocation
        console.debug(
          `[AliasAllocator] reserve initial=${initial} usedSize=${this.used.size}`
        );
      } catch {}
    }
    while (this.used.has(candidate)) {
      if (process.env.VIBE_DEBUG) {
        try {
          console.debug(
            `[AliasAllocator] conflict candidate=${candidate} nextSuffix=${this.used.size}`
          );
        } catch {}
      }
      candidate = `${candidate}_${this.used.size}`;
    }
    this.used.add(candidate);
    if (process.env.VIBE_DEBUG) {
      try {
        console.debug(`[AliasAllocator] reserved ${candidate}`);
      } catch {}
    }
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
