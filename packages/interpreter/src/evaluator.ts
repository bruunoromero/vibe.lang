import type {
  ExpressionNode,
  NodeKind,
  SourceSpan,
  ListNode,
  VectorNode,
  SetNode,
  MapNode,
  Diagnostic,
  NamespaceImportNode,
  ReaderMacroNode,
  SymbolNode,
  BindingPattern,
  PatternError,
  PatternErrorKind,
} from "@vibe/syntax";
import {
  DiagnosticSeverity,
  NodeKind as NK,
  parseBindingPattern,
} from "@vibe/syntax";
import {
  symbol as runtimeSymbol,
  symbol_QMARK as runtimeSymbol_QMARK,
} from "@vibe/runtime";
import type { RuntimeSymbol } from "@vibe/runtime";
import type { Environment } from "./environment";
import {
  createEnvironment,
  extendEnvironment,
  lookupVariable,
  defineVariable,
} from "./environment";
import type { FunctionClauseValue, FunctionValue, Value } from "./values";
import {
  isCallable,
  isFunction,
  isBuiltin,
  isTruthy,
  makeNumber,
  makeString,
  makeBoolean,
  makeNil,
  makeSymbol,
  makeError,
  makeList,
  makeVector,
  makeSet,
  makeMap,
  makeFunction,
  makeBuiltin,
  makeExternalNamespace,
  isSequence,
  isSymbol,
  isString,
  isErrorValue,
} from "./values";

const MAX_CALL_DEPTH = 1000;

const EXTERNAL_MEMBER_SERIALIZATIONS: Record<string, string> = {
  "!": "_BANG",
  "?": "_QMARK",
  "*": "_STAR",
  "+": "_PLUS",
  "-": "_DASH",
  "<": "_LT",
  ">": "_GT",
  "=": "_EQ",
  "/": "_SLASH",
  "#": "_HASH",
};

type AutoGensymScope = Map<string, string>;

const sanitizeExternalMemberName = (member: string): string => {
  let result = "";
  for (const char of member) {
    if (/^[A-Za-z0-9_]$/.test(char)) {
      result += char;
      continue;
    }
    const replacement = EXTERNAL_MEMBER_SERIALIZATIONS[char];
    result += replacement ?? "_";
  }
  return result.length === 0 ? "_member" : result;
};

class RuntimeThrow extends Error {
  constructor(readonly error: Error, readonly span: SourceSpan) {
    super(error.message);
    this.name = "RuntimeThrow";
  }
}

const runtimeThrowToResult = (signal: RuntimeThrow): EvalResult => ({
  ok: false,
  diagnostics: [
    {
      message: `Unhandled throw: ${signal.error.message}`,
      span: signal.span,
      severity: DiagnosticSeverity.Error,
      code: "INTERP_THROW_UNHANDLED",
    },
  ],
});

const throwRuntimeError = (error: Error, span: SourceSpan): never => {
  throw new RuntimeThrow(error, span);
};

type CachedPattern =
  | { ok: true; pattern: BindingPattern }
  | { ok: false; errors: readonly PatternError[] };

const patternCache = new WeakMap<ExpressionNode, CachedPattern>();

const INTERP_PATTERN_ERROR_CODES: Record<PatternErrorKind, string> = {
  PatternUnsupportedNode: "INTERP_PATTERN_UNSUPPORTED",
  PatternRestRequiresTarget: "INTERP_PATTERN_REST_TARGET",
  PatternRestDuplicate: "INTERP_PATTERN_REST_DUPLICATE",
  PatternAsRequiresSymbol: "INTERP_PATTERN_AS_SYMBOL",
  PatternAsDuplicate: "INTERP_PATTERN_AS_DUPLICATE",
  PatternMapKeyMissingValue: "INTERP_PATTERN_MAP_VALUE",
  PatternMapKeyUnsupported: "INTERP_PATTERN_MAP_KEY",
  PatternMapKeysRequiresVector: "INTERP_PATTERN_KEYS_VECTOR",
  PatternMapStringsRequiresVector: "INTERP_PATTERN_STRS_VECTOR",
  PatternMapSymbolsRequiresVector: "INTERP_PATTERN_SYMS_VECTOR",
  PatternMapDefaultsRequireMap: "INTERP_PATTERN_OR_MAP",
  PatternDuplicateBinding: "INTERP_PATTERN_DUPLICATE",
};

const DEFAULT_PATTERN_ERROR_CODE = "INTERP_PATTERN_ERROR";

interface EvaluatorFnClause {
  readonly paramsNode: ExpressionNode | null;
  readonly bodyNodes: readonly ExpressionNode[];
  readonly span: SourceSpan;
}

export interface EvalResult<T = Value> {
  readonly ok: boolean;
  readonly value?: T;
  readonly diagnostics: readonly Diagnostic[];
}

export interface EvalContext {
  readonly callDepth: number;
  gensymCounter?: { value: number };
}

/**
 * Evaluate an expression node in the given environment (async version for module loading).
 */
const evaluateNode = async (
  node: ExpressionNode,
  env: Environment,
  context: EvalContext = { callDepth: 0 }
): Promise<EvalResult> => {
  if (context.callDepth >= MAX_CALL_DEPTH) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Call stack depth limit exceeded (${MAX_CALL_DEPTH})`,
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_MAX_CALL_DEPTH",
        },
      ],
    };
  }

  switch (node.kind) {
    case NK.Number:
      return { ok: true, value: makeNumber(node.value), diagnostics: [] };
    case NK.String:
      return { ok: true, value: makeString(node.value), diagnostics: [] };
    case NK.Boolean:
      return { ok: true, value: makeBoolean(node.value), diagnostics: [] };
    case NK.Nil:
      return { ok: true, value: makeNil(), diagnostics: [] };
    case NK.Keyword: {
      const label = node.value.startsWith(":") ? node.value : `:${node.value}`;
      return { ok: true, value: makeSymbol(label), diagnostics: [] };
    }
    case NK.Symbol:
      return evaluateSymbol(node.value, node.span, env);
    case NK.List:
      return await evaluateList(node, env, context);
    case NK.Vector:
      return await evaluateVector(node, env, context);
    case NK.Set:
      return await evaluateSet(node, env, context);
    case NK.Map:
      return await evaluateMap(node, env, context);
    case NK.NamespaceImport:
      // NamespaceImportNodes are promoted from lists, handle them as special forms
      if (node.importKind === "require") {
        return await evaluateRequire(node, env, context);
      } else if (node.importKind === "import") {
        return await evaluateImport(node, env, context);
      } else {
        return evaluateExternal(node, env, context);
      }
    case NK.Quote:
      if (!node.target) {
        return {
          ok: false,
          diagnostics: [
            {
              message: "Quote expression is missing target",
              span: node.span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_QUOTE_MISSING_TARGET",
            },
          ],
        };
      }
      return evaluateQuote(node.target, env);
    case NK.SyntaxQuote:
      return await evaluateSyntaxQuote(node as ReaderMacroNode, env, context);
    case NK.Unquote:
    case NK.UnquoteSplicing:
      return {
        ok: false,
        diagnostics: [
          {
            message: "unquote forms may only appear inside a syntax quote",
            span: node.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_SYNTAX_UNQUOTE_CONTEXT",
          },
        ],
      };
    default:
      return {
        ok: false,
        diagnostics: [
          {
            message: `Unsupported node kind for evaluation: ${node.kind}`,
            span: node.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_UNSUPPORTED_NODE",
          },
        ],
      };
  }
};

export const evaluate = async (
  node: ExpressionNode,
  env: Environment,
  context: EvalContext = { callDepth: 0 }
): Promise<EvalResult> => {
  try {
    return await evaluateNode(node, env, context);
  } catch (error) {
    if (error instanceof RuntimeThrow) {
      return runtimeThrowToResult(error);
    }
    throw error;
  }
};

const evaluateSymbol = (
  name: string,
  span: SourceSpan,
  env: Environment
): EvalResult => {
  // Check if this is a namespace access (e.g., "math/add" or "runtime/add*")
  const slashIndex = name.indexOf("/");
  if (slashIndex > 0 && slashIndex < name.length - 1) {
    const alias = name.slice(0, slashIndex);
    const member = name.slice(slashIndex + 1);

    const namespace = lookupVariable(env, alias);
    if (namespace === undefined) {
      return {
        ok: false,
        diagnostics: [
          {
            message: `Undefined namespace: ${alias}`,
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_UNDEFINED_NAMESPACE",
          },
        ],
      };
    }

    // Handle external namespaces (JS modules) - dynamic property access
    if (namespace.kind === "external") {
      try {
        // Convert vibe symbol to JS property name
        // In vibe: add*, sub*, eq*, etc.
        // In JS: add_STAR, sub_STAR, eq_STAR, etc.
        const jsPropertyName = sanitizeExternalMemberName(member);
        const jsValue = namespace.module[jsPropertyName];
        if (jsValue === undefined) {
          throw new Error(
            `Property ${member} (${jsPropertyName}) not found in module ${alias}`
          );
        }

        // Wrap JS functions as callables
        if (typeof jsValue === "function") {
          const wrappedFn = makeBuiltin(
            `${alias}/${member}`,
            (args: readonly Value[], span: SourceSpan) => {
              try {
                const jsArgs = args.map(valueToJS);
                const result = jsValue(...jsArgs);
                return jsToValue(result);
              } catch (error) {
                throw new Error(
                  `Error calling ${alias}/${member}: ${
                    error instanceof Error ? error.message : String(error)
                  }`
                );
              }
            }
          );
          return { ok: true, value: wrappedFn, diagnostics: [] };
        }

        // For non-functions, convert to vibe value
        return { ok: true, value: jsToValue(jsValue), diagnostics: [] };
      } catch (error) {
        return {
          ok: false,
          diagnostics: [
            {
              message: `Error accessing ${alias}/${member}: ${
                error instanceof Error ? error.message : String(error)
              }`,
              span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_EXTERNAL_ACCESS_ERROR",
            },
          ],
        };
      }
    }

    // Namespace should be a map
    if (namespace.kind !== "map") {
      return {
        ok: false,
        diagnostics: [
          {
            message: `${alias} is not a namespace (expected map, got ${namespace.kind})`,
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_NOT_NAMESPACE",
          },
        ],
      };
    }

    // Look up the member in the namespace
    const value = namespace.entries.get(member);
    if (value === undefined) {
      return {
        ok: false,
        diagnostics: [
          {
            message: `Undefined member ${member} in namespace ${alias}`,
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_UNDEFINED_MEMBER",
          },
        ],
      };
    }

    return { ok: true, value, diagnostics: [] };
  }

  // Regular variable lookup
  const value = lookupVariable(env, name);
  if (value === undefined) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Undefined variable: ${name}`,
          span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_UNDEFINED_VARIABLE",
        },
      ],
    };
  }
  return { ok: true, value, diagnostics: [] };
};

const evaluateList = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  if (node.elements.length === 0) {
    return { ok: true, value: makeList([]), diagnostics: [] };
  }

  const head = node.elements[0];
  if (head && head.kind === NK.Symbol) {
    const specialResult = await tryEvaluateSpecialForm(
      node,
      head,
      env,
      context
    );
    if (specialResult) {
      return specialResult;
    }
  }

  // Regular function call - evaluate head and args
  const headResult = await evaluateNode(head!, env, context);
  if (!headResult.ok) {
    return headResult;
  }

  const fn = headResult.value!;
  const argNodes = node.elements.slice(1);
  const args: Value[] = [];
  const diagnostics: Diagnostic[] = [];

  for (const argNode of argNodes) {
    const argResult = await evaluateNode(argNode, env, context);
    if (!argResult.ok) {
      diagnostics.push(...argResult.diagnostics);
      continue;
    }
    args.push(argResult.value!);
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  return await applyFunction(fn, args, node.span, context);
};

const evaluateVector = async (
  node: VectorNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const elements: Value[] = [];
  const diagnostics: Diagnostic[] = [];

  for (const elem of node.elements) {
    if (!elem) continue;
    const elemResult = await evaluateNode(elem, env, context);
    if (!elemResult.ok) {
      diagnostics.push(...elemResult.diagnostics);
      continue;
    }
    elements.push(elemResult.value!);
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  return { ok: true, value: makeVector(elements), diagnostics: [] };
};

const evaluateSet = async (
  node: SetNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const elements: Value[] = [];
  const diagnostics: Diagnostic[] = [];

  for (const elem of node.elements) {
    if (!elem) continue;
    const elemResult = await evaluateNode(elem, env, context);
    if (!elemResult.ok) {
      diagnostics.push(...elemResult.diagnostics);
      continue;
    }
    elements.push(elemResult.value!);
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  return { ok: true, value: makeSet(elements), diagnostics: [] };
};

const evaluateMap = async (
  node: MapNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const entries = new Map<string, Value>();
  const diagnostics: Diagnostic[] = [];

  for (const entry of node.entries) {
    if (!entry.key || !entry.value) continue;

    const keyResult = await evaluateNode(entry.key, env, context);
    if (!keyResult.ok) {
      diagnostics.push(...keyResult.diagnostics);
      continue;
    }

    const valueResult = await evaluateNode(entry.value, env, context);
    if (!valueResult.ok) {
      diagnostics.push(...valueResult.diagnostics);
      continue;
    }

    const key = keyResult.value!;
    if (!isSymbol(key) && !isString(key)) {
      diagnostics.push({
        message: "Map keys must be symbols or strings",
        span: entry.key.span,
        severity: DiagnosticSeverity.Error,
        code: "INTERP_MAP_KEY_TYPE",
      });
      continue;
    }

    const keyStr = isSymbol(key) ? key.value : key.value;
    entries.set(keyStr, valueResult.value!);
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  return { ok: true, value: makeMap(entries), diagnostics: [] };
};

const tryEvaluateSpecialForm = async (
  node: ListNode,
  head: ExpressionNode & { kind: NodeKind.Symbol },
  env: Environment,
  context: EvalContext
): Promise<EvalResult | null> => {
  switch (head.value) {
    case "def":
      return evaluateDef(node, env, context);
    case "defp":
      return evaluateDef(node, env, context);
    case "let":
      return await evaluateLet(node, env, context);
    case "fn":
      return evaluateFn(node, env);
    case "if":
      return await evaluateIf(node, env, context);
    case "quote":
      return evaluateQuoteForm(node, env);
    case "do":
      return await evaluateDo(node, env, context);
    case "try":
      return await evaluateTry(node, env, context);
    case "throw":
      return await evaluateThrow(node, env, context);
    case "require":
      return await evaluateRequire(node, env, context);
    case "import":
      return await evaluateImport(node, env, context);
    case "external":
      return evaluateExternal(node, env, context);
    case "gensym":
      return evaluateGensym(node, env, context);
    default:
      return null;
  }
};

const evaluateDef = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const nameNode = node.elements[1];
  const valueNode = node.elements[2];

  if (!nameNode || nameNode.kind !== NK.Symbol) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "def requires a symbol name",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_DEF_REQUIRES_SYMBOL",
        },
      ],
    };
  }

  if (!valueNode) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "def requires a value",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_DEF_REQUIRES_VALUE",
        },
      ],
    };
  }

  const valueResult = await evaluateNode(valueNode, env, context);
  if (!valueResult.ok) {
    return valueResult;
  }

  defineVariable(env, nameNode.value, valueResult.value!);
  return { ok: true, value: valueResult.value, diagnostics: [] };
};

const evaluateFn = (node: ListNode, env: Environment): EvalResult => {
  const clauses = collectFnClauses(node);
  const errorResult = (message: string, span: SourceSpan, code: string) => ({
    ok: false,
    diagnostics: [
      {
        message,
        span,
        severity: DiagnosticSeverity.Error,
        code,
      },
    ],
  });

  if (clauses.length === 0) {
    return errorResult(
      "fn requires a parameter vector",
      node.span,
      "INTERP_FN_REQUIRES_VECTOR"
    );
  }

  const functionClauses: FunctionClauseValue[] = [];
  const seenFixedArities = new Set<number>();
  let variadicClauseIndex: number | null = null;

  for (let clauseIndex = 0; clauseIndex < clauses.length; clauseIndex += 1) {
    const clause = clauses[clauseIndex]!;
    const paramsNode = clause.paramsNode;
    if (!paramsNode || paramsNode.kind !== NK.Vector) {
      return errorResult(
        "fn requires a parameter vector",
        paramsNode?.span ?? clause.span,
        "INTERP_FN_REQUIRES_VECTOR"
      );
    }

    const params: BindingPattern[] = [];
    let restParam: string | undefined;
    let sawRestMarker = false;

    for (let i = 0; i < paramsNode.elements.length; i += 1) {
      const paramNode = paramsNode.elements[i];
      if (!paramNode) {
        continue;
      }

      if (paramNode.kind === NK.Symbol && paramNode.value === "&") {
        if (sawRestMarker) {
          return errorResult(
            "fn allows only a single & rest parameter",
            paramNode.span,
            "INTERP_FN_DUPLICATE_REST"
          );
        }
        sawRestMarker = true;
        const restNode = paramsNode.elements[i + 1];
        if (!restNode || restNode.kind !== NK.Symbol) {
          return errorResult(
            "& must be followed by a symbol name",
            paramNode.span,
            "INTERP_FN_REST_REQUIRES_SYMBOL"
          );
        }
        restParam = restNode.value;
        i += 1;
        continue;
      }

      if (sawRestMarker) {
        return errorResult(
          "Parameters cannot appear after & rest parameter",
          paramNode.span,
          "INTERP_FN_PARAMS_AFTER_REST"
        );
      }

      const patternResult = resolvePattern(paramNode);
      if (!patternResult.ok) {
        return {
          ok: false,
          diagnostics: diagnosticsFromPatternErrors(patternResult.errors),
        };
      }
      params.push(patternResult.pattern);
    }

    if (restParam) {
      if (variadicClauseIndex !== null) {
        return errorResult(
          "fn allows only one variadic clause",
          paramsNode.span,
          "INTERP_FN_MULTIPLE_REST"
        );
      }
      variadicClauseIndex = clauseIndex;
      if (clauseIndex !== clauses.length - 1) {
        return errorResult(
          "Variadic fn clause must appear last",
          paramsNode.span,
          "INTERP_FN_REST_POSITION"
        );
      }
    } else {
      const arity = params.length;
      if (seenFixedArities.has(arity)) {
        return errorResult(
          `Function already defines a clause for ${arity} argument(s)`,
          paramsNode.span,
          "INTERP_FN_DUPLICATE_ARITY"
        );
      }
      seenFixedArities.add(arity);
    }

    if (clause.bodyNodes.length === 0) {
      return errorResult(
        "fn requires a body",
        clause.span,
        "INTERP_FN_REQUIRES_BODY"
      );
    }

    functionClauses.push({
      params,
      ...(restParam ? { rest: restParam } : {}),
      body: clause.bodyNodes,
    });
  }

  const fn = makeFunction(functionClauses, env, node.span);
  return { ok: true, value: fn, diagnostics: [] };
};

const collectFnClauses = (node: ListNode): EvaluatorFnClause[] => {
  const paramsNode = node.elements[1];
  if (paramsNode && paramsNode.kind === NK.Vector) {
    return [
      {
        paramsNode,
        bodyNodes: node.elements
          .slice(2)
          .filter((element): element is ExpressionNode => Boolean(element)),
        span: node.span,
      },
    ];
  }

  const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
  if (tail.length === 0) {
    return [];
  }

  const clauseLists = tail.filter(
    (element): element is ListNode => element.kind === NK.List
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
};

const evaluateQuoteForm = (node: ListNode, env: Environment): EvalResult => {
  const target = node.elements[1];
  if (!target) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "quote requires a target expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_QUOTE_REQUIRES_TARGET",
        },
      ],
    };
  }
  return evaluateQuote(target, env);
};

/**
 * Convert an AST node to a quoted Value without evaluation.
 * This turns symbols into symbol values, lists into list values, etc.
 */
const evaluateQuote = (node: ExpressionNode, env: Environment): EvalResult => {
  switch (node.kind) {
    case NK.Number:
      return { ok: true, value: makeNumber(node.value), diagnostics: [] };
    case NK.String:
      return { ok: true, value: makeString(node.value), diagnostics: [] };
    case NK.Boolean:
      return { ok: true, value: makeBoolean(node.value), diagnostics: [] };
    case NK.Nil:
      return { ok: true, value: makeNil(), diagnostics: [] };
    case NK.Symbol:
      return { ok: true, value: makeSymbol(node.value), diagnostics: [] };
    case NK.List: {
      const elements: Value[] = [];
      for (const elem of node.elements) {
        const result = evaluateQuote(elem, env);
        if (!result.ok) return result;
        elements.push(result.value!);
      }
      return { ok: true, value: makeList(elements), diagnostics: [] };
    }
    case NK.Vector: {
      const elements: Value[] = [];
      for (const elem of node.elements) {
        const result = evaluateQuote(elem, env);
        if (!result.ok) return result;
        elements.push(result.value!);
      }
      return { ok: true, value: makeVector(elements), diagnostics: [] };
    }
    case NK.Set: {
      const elements: Value[] = [];
      for (const elem of node.elements) {
        const result = evaluateQuote(elem, env);
        if (!result.ok) return result;
        elements.push(result.value!);
      }
      return { ok: true, value: makeSet(elements), diagnostics: [] };
    }
    case NK.Map: {
      const quotedEntries = new Map<string, Value>();
      for (const entry of node.entries) {
        if (!entry.key || !entry.value) {
          continue;
        }

        const keyResult = evaluateQuote(entry.key, env);
        if (!keyResult.ok) return keyResult;
        const key = keyResult.value!;

        if (!isSymbol(key) && !isString(key)) {
          return {
            ok: false,
            diagnostics: [
              {
                message: "Map keys must be symbols or strings",
                span: entry.key.span,
                severity: DiagnosticSeverity.Error,
                code: "INTERP_MAP_KEY_TYPE",
              },
            ],
          };
        }

        const valueResult = evaluateQuote(entry.value, env);
        if (!valueResult.ok) return valueResult;

        const keyStr = key.value;
        quotedEntries.set(keyStr, valueResult.value!);
      }
      return { ok: true, value: makeMap(quotedEntries), diagnostics: [] };
    }
    case NK.Quote:
      // Nested quote - just quote the target
      if (!node.target) {
        return {
          ok: false,
          diagnostics: [
            {
              message: "Quote expression is missing target",
              span: node.span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_QUOTE_MISSING_TARGET",
            },
          ],
        };
      }
      return evaluateQuote(node.target, env);
    default:
      return {
        ok: false,
        diagnostics: [
          {
            message: `Cannot quote node kind: ${node.kind}`,
            span: node.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_QUOTE_UNSUPPORTED",
          },
        ],
      };
  }
};

const evaluateSyntaxQuote = async (
  node: ReaderMacroNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  if (!node.target) {
    return { ok: true, value: makeNil(), diagnostics: [] };
  }
  const gensyms: AutoGensymScope = new Map();
  return await instantiateSyntaxNode(node.target, env, context, gensyms);
};

const isAutoGensymPlaceholder = (value: string): boolean => value.endsWith("#");

const ensureGensymCounter = (context: EvalContext): { value: number } => {
  if (!context.gensymCounter) {
    context.gensymCounter = { value: 0 };
  }
  return context.gensymCounter;
};

const allocateRuntimeGensym = (hint: string, context: EvalContext): string => {
  const counter = ensureGensymCounter(context);
  const base = hint.length > 0 ? hint : "g";
  const unique = `${base}__${counter.value++}`;
  return unique;
};

const instantiateAutoGensymSymbol = (
  node: SymbolNode,
  gensyms: AutoGensymScope,
  context: EvalContext
): EvalResult<Value> => {
  if (node.value.includes("/")) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Auto gensym placeholder ${node.value} cannot include a namespace`,
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_SYNTAX_GENSYM_NAMESPACE",
        },
      ],
    };
  }

  const key = node.value;
  let replacement = gensyms.get(key);
  if (!replacement) {
    const hint = key.slice(0, -1);
    replacement = allocateRuntimeGensym(hint, context);
    gensyms.set(key, replacement);
  }
  return { ok: true, value: makeSymbol(replacement), diagnostics: [] };
};

const instantiateSyntaxNode = async (
  node: ExpressionNode,
  env: Environment,
  context: EvalContext,
  gensyms: AutoGensymScope
): Promise<EvalResult<Value>> => {
  switch (node.kind) {
    case NK.List:
      return await instantiateSyntaxSequence(
        node.elements,
        env,
        context,
        gensyms,
        "list",
        node.span
      );
    case NK.Vector:
      return await instantiateSyntaxSequence(
        node.elements,
        env,
        context,
        gensyms,
        "vector",
        node.span
      );
    case NK.Set:
      return await instantiateSyntaxSequence(
        node.elements,
        env,
        context,
        gensyms,
        "set",
        node.span
      );
    case NK.Map:
      return await instantiateSyntaxMap(node as MapNode, env, context, gensyms);
    case NK.SyntaxQuote: {
      const target = (node as ReaderMacroNode).target;
      if (!target) {
        return { ok: true, value: makeNil(), diagnostics: [] };
      }
      return await instantiateSyntaxNode(
        target,
        env,
        context,
        new Map<string, string>()
      );
    }
    case NK.Unquote:
      return await evaluateUnquoteNode(
        node as ReaderMacroNode<NodeKind.Unquote>,
        env,
        context
      );
    case NK.UnquoteSplicing:
      return {
        ok: false,
        diagnostics: [
          {
            message:
              "Unquote splicing cannot appear outside of list/vector/set literals",
            span: node.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_SYNTAX_SPLICE_CONTEXT",
          },
        ],
      };
    case NK.Symbol:
      if (isAutoGensymPlaceholder(node.value)) {
        return instantiateAutoGensymSymbol(
          node as SymbolNode,
          gensyms,
          context
        );
      }
      return evaluateQuote(node, env);
    default:
      return evaluateQuote(node, env);
  }
};

const instantiateSyntaxSequence = async (
  elements: readonly (ExpressionNode | null)[],
  env: Environment,
  context: EvalContext,
  gensyms: AutoGensymScope,
  container: "list" | "vector" | "set",
  span: SourceSpan
): Promise<EvalResult<Value>> => {
  const values: Value[] = [];
  for (const element of elements) {
    if (!element) {
      continue;
    }
    if (element.kind === NK.Unquote) {
      const result = await evaluateUnquoteNode(
        element as ReaderMacroNode<NodeKind.Unquote>,
        env,
        context
      );
      if (!result.ok) {
        return result;
      }
      if (result.value) {
        values.push(result.value);
      }
      continue;
    }
    if (element.kind === NK.UnquoteSplicing) {
      const result = await evaluateUnquoteSplicing(
        element as ReaderMacroNode<NodeKind.UnquoteSplicing>,
        env,
        context
      );
      if (!result.ok) {
        return { ok: false, diagnostics: result.diagnostics };
      }
      values.push(...(result.value ?? []));
      continue;
    }
    const nested = await instantiateSyntaxNode(element, env, context, gensyms);
    if (!nested.ok) {
      return nested;
    }
    if (nested.value) {
      values.push(nested.value);
    }
  }

  switch (container) {
    case "list":
      return { ok: true, value: makeList(values), diagnostics: [] };
    case "vector":
      return { ok: true, value: makeVector(values), diagnostics: [] };
    case "set":
      return { ok: true, value: makeSet(values), diagnostics: [] };
    default:
      return {
        ok: false,
        diagnostics: [
          {
            message: "Unsupported syntax-quote container",
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_SYNTAX_CONTAINER",
          },
        ],
      };
  }
};

const instantiateSyntaxMap = async (
  node: MapNode,
  env: Environment,
  context: EvalContext,
  gensyms: AutoGensymScope
): Promise<EvalResult<Value>> => {
  const entries = new Map<string, Value>();
  for (const entry of node.entries) {
    if (!entry.key || !entry.value) {
      continue;
    }
    const keyResult = await instantiateSyntaxNode(
      entry.key,
      env,
      context,
      gensyms
    );
    if (!keyResult.ok) {
      return keyResult;
    }
    const keyValue = keyResult.value;
    if (!keyValue || (!isSymbol(keyValue) && !isString(keyValue))) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "Map keys must be symbols or strings",
            span: entry.key.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_SYNTAX_MAP_KEY_TYPE",
          },
        ],
      };
    }
    const valueResult = await instantiateSyntaxNode(
      entry.value,
      env,
      context,
      gensyms
    );
    if (!valueResult.ok) {
      return valueResult;
    }
    const keyName = keyValue.value;
    entries.set(keyName, valueResult.value ?? makeNil());
  }
  return { ok: true, value: makeMap(entries), diagnostics: [] };
};

const evaluateUnquoteNode = async (
  node: ReaderMacroNode<NodeKind.Unquote>,
  env: Environment,
  context: EvalContext
): Promise<EvalResult<Value>> => {
  if (!node.target) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "Unquote requires a target expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_SYNTAX_UNQUOTE_EMPTY",
        },
      ],
    };
  }
  const result = await evaluateNode(node.target, env, context);
  if (!result.ok) {
    return result;
  }
  return {
    ok: true,
    value: result.value ?? makeNil(),
    diagnostics: [],
  };
};

const evaluateUnquoteSplicing = async (
  node: ReaderMacroNode<NodeKind.UnquoteSplicing>,
  env: Environment,
  context: EvalContext
): Promise<EvalResult<readonly Value[]>> => {
  if (!node.target) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "Unquote splicing requires a target expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_SYNTAX_SPLICE_EMPTY",
        },
      ],
    };
  }
  const result = await evaluateNode(node.target, env, context);
  if (!result.ok) {
    return { ok: false, diagnostics: result.diagnostics };
  }
  const value = result.value ?? makeNil();
  if (!isSequence(value)) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "Unquote splicing requires a sequence expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_SYNTAX_SPLICE_SEQUENCE",
        },
      ],
    };
  }
  return { ok: true, value: value.elements, diagnostics: [] };
};

const evaluateLet = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const bindingsNode = node.elements[1];

  if (!bindingsNode || bindingsNode.kind !== NK.Vector) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "let requires a vector of bindings",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_LET_REQUIRES_VECTOR",
        },
      ],
    };
  }

  const letEnv = extendEnvironment(env);
  const bindings = bindingsNode.elements;
  const diagnostics: Diagnostic[] = [];

  for (let i = 0; i < bindings.length; i += 2) {
    const targetNode = bindings[i];
    const valueNode = bindings[i + 1];

    if (!targetNode) {
      continue;
    }

    const patternResult = resolvePattern(targetNode);
    if (!patternResult.ok) {
      diagnostics.push(...diagnosticsFromPatternErrors(patternResult.errors));
      continue;
    }

    if (!valueNode) {
      const message =
        patternResult.pattern.kind === "symbol"
          ? `Missing value for binding ${patternResult.pattern.node.value}`
          : "Missing value for binding";
      diagnostics.push({
        message,
        span: targetNode.span,
        severity: DiagnosticSeverity.Error,
        code: "INTERP_LET_MISSING_VALUE",
      });
      continue;
    }

    const valueResult = await evaluateNode(valueNode, letEnv, context);
    if (!valueResult.ok) {
      diagnostics.push(...valueResult.diagnostics);
      continue;
    }

    const bindResult = await bindPatternValue(
      patternResult.pattern,
      valueResult.value ?? makeNil(),
      letEnv,
      context
    );
    if (!bindResult.ok) {
      diagnostics.push(...bindResult.diagnostics);
    }
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  const bodyNodes = node.elements.slice(2);
  let lastResult: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };

  for (const bodyNode of bodyNodes) {
    lastResult = await evaluateNode(bodyNode, letEnv, context);
    if (!lastResult.ok) {
      return lastResult;
    }
  }

  return lastResult;
};

const evaluateIf = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const condNode = node.elements[1];
  const thenNode = node.elements[2];
  const elseNode = node.elements[3];

  if (!condNode) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "if requires a condition",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_IF_REQUIRES_COND",
        },
      ],
    };
  }

  const condResult = await evaluateNode(condNode, env, context);
  if (!condResult.ok) {
    return condResult;
  }

  if (isTruthy(condResult.value!)) {
    if (thenNode) {
      return await evaluateNode(thenNode, env, context);
    }
    return { ok: true, value: makeNil(), diagnostics: [] };
  } else {
    if (elseNode) {
      return await evaluateNode(elseNode, env, context);
    }
    return { ok: true, value: makeNil(), diagnostics: [] };
  }
};

const evaluateDo = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const bodyNodes = node.elements.slice(1);
  let lastResult: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };

  for (const bodyNode of bodyNodes) {
    lastResult = await evaluateNode(bodyNode, env, context);
    if (!lastResult.ok) {
      return lastResult;
    }
  }

  return lastResult;
};

type SequenceOutcome =
  | { kind: "value"; result: EvalResult }
  | { kind: "thrown"; error: Error; span: SourceSpan };

const evaluateSequenceNodes = async (
  nodes: readonly ExpressionNode[],
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  let lastResult: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };
  for (const expr of nodes) {
    lastResult = await evaluateNode(expr, env, context);
    if (!lastResult.ok) {
      return lastResult;
    }
  }
  return lastResult;
};

const runSequenceSafely = async (
  nodes: readonly ExpressionNode[],
  env: Environment,
  context: EvalContext,
  fallbackSpan: SourceSpan
): Promise<SequenceOutcome> => {
  try {
    const result = await evaluateSequenceNodes(nodes, env, context);
    return { kind: "value", result };
  } catch (error) {
    if (error instanceof RuntimeThrow) {
      return { kind: "thrown", error: error.error, span: error.span };
    }
    return {
      kind: "thrown",
      error: error instanceof Error ? error : new Error(String(error)),
      span: fallbackSpan,
    };
  }
};

interface TryFormLayout {
  readonly bodyNodes: readonly ExpressionNode[];
  readonly catchClause?: ListNode;
  readonly finallyClause?: ListNode;
}

const partitionTryForm = (node: ListNode): TryFormLayout => {
  const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
  const bodyNodes: ExpressionNode[] = [];
  let catchClause: ListNode | undefined;
  let finallyClause: ListNode | undefined;

  for (const expr of tail) {
    if (expr.kind === NK.List) {
      const head = expr.elements[0];
      if (head && head.kind === NK.Symbol) {
        if (head.value === "catch" && !catchClause && !finallyClause) {
          catchClause = expr;
          continue;
        }
        if (head.value === "finally" && !finallyClause) {
          finallyClause = expr;
          continue;
        }
      }
    }
    bodyNodes.push(expr);
  }

  return {
    bodyNodes,
    ...(catchClause ? { catchClause } : {}),
    ...(finallyClause ? { finallyClause } : {}),
  };
};

const evaluateTry = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const layout = partitionTryForm(node);
  let completion: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };
  let pendingThrow: { error: Error; span: SourceSpan } | null = null;

  const bodyOutcome = await runSequenceSafely(
    layout.bodyNodes,
    env,
    context,
    node.span
  );
  if (bodyOutcome.kind === "value") {
    if (!bodyOutcome.result.ok) {
      return bodyOutcome.result;
    }
    completion = bodyOutcome.result;
  } else {
    pendingThrow = { error: bodyOutcome.error, span: bodyOutcome.span };
  }

  if (pendingThrow && layout.catchClause) {
    const bindingNode = layout.catchClause.elements[1];
    if (!bindingNode || bindingNode.kind !== NK.Symbol) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "catch clause requires a symbol binding",
            span: layout.catchClause.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_TRY_CATCH_BINDING",
          },
        ],
      };
    }
    const catchEnv = extendEnvironment(env);
    defineVariable(catchEnv, bindingNode.value, makeError(pendingThrow.error));
    const catchBodyNodes = layout.catchClause.elements
      .slice(2)
      .filter((element): element is ExpressionNode => Boolean(element));
    const catchOutcome = await runSequenceSafely(
      catchBodyNodes,
      catchEnv,
      context,
      layout.catchClause.span
    );
    if (catchOutcome.kind === "value") {
      if (!catchOutcome.result.ok) {
        return catchOutcome.result;
      }
      completion = catchOutcome.result;
      pendingThrow = null;
    } else {
      pendingThrow = {
        error: catchOutcome.error,
        span: catchOutcome.span,
      };
    }
  }

  if (layout.finallyClause) {
    const finallyBodyNodes = layout.finallyClause.elements
      .slice(1)
      .filter((element): element is ExpressionNode => Boolean(element));
    const finallyOutcome = await runSequenceSafely(
      finallyBodyNodes,
      env,
      context,
      layout.finallyClause.span
    );
    if (finallyOutcome.kind === "value") {
      if (!finallyOutcome.result.ok) {
        return finallyOutcome.result;
      }
    } else {
      pendingThrow = {
        error: finallyOutcome.error,
        span: finallyOutcome.span,
      };
    }
  }

  if (pendingThrow) {
    throwRuntimeError(pendingThrow.error, pendingThrow.span);
  }

  return completion;
};

const coerceValueToError = (value: Value): Error => {
  if (isErrorValue(value)) {
    return value.error;
  }
  const jsValue = valueToJS(value);
  if (jsValue instanceof Error) {
    return jsValue;
  }
  if (typeof jsValue === "string") {
    return new Error(jsValue);
  }
  try {
    return new Error(
      typeof jsValue === "object" ? JSON.stringify(jsValue) : String(jsValue)
    );
  } catch {
    return new Error(String(jsValue));
  }
};

const evaluateThrow = async (
  node: ListNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const argNode = node.elements[1];
  if (!argNode) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "throw requires a value expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_THROW_REQUIRES_VALUE",
        },
      ],
    };
  }

  const extraArgs = node.elements
    .slice(2)
    .filter((element): element is ExpressionNode => Boolean(element));
  if (extraArgs.length > 0) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "throw accepts exactly one argument",
          span: extraArgs[0]!.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_THROW_TOO_MANY_ARGS",
        },
      ],
    };
  }

  const valueResult = await evaluateNode(argNode, env, context);
  if (!valueResult.ok) {
    return valueResult;
  }

  const thrownValue = valueResult.value ?? makeNil();
  const errorObject = coerceValueToError(thrownValue);
  throwRuntimeError(errorObject, node.span);
  // Unreachable, but satisfies the return type contract.
  return { ok: true, value: makeNil(), diagnostics: [] };
};

const applyFunction = async (
  fn: Value,
  args: readonly Value[],
  span: SourceSpan,
  context: EvalContext
): Promise<EvalResult> => {
  if (isBuiltin(fn)) {
    return { ok: true, value: fn.fn(args, span), diagnostics: [] };
  }

  if (isFunction(fn)) {
    const clause = selectFunctionClause(fn, args.length);
    if (!clause) {
      const available = fn.clauses
        .map((c) => (c.rest ? `${c.params.length}+` : `${c.params.length}`))
        .join(", ");
      const signatureInfo =
        available.length > 0 ? ` (available: ${available})` : "";
      return {
        ok: false,
        diagnostics: [
          {
            message: `Function cannot accept ${args.length} argument(s)${signatureInfo}`,
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_ARITY_MISMATCH",
          },
        ],
      };
    }

    const fnEnv = extendEnvironment(fn.closure);
    const invokeContext: EvalContext = { callDepth: context.callDepth + 1 };
    for (let index = 0; index < clause.params.length; index += 1) {
      const pattern = clause.params[index]!;
      const argument = args[index] ?? makeNil();
      const bindResult = await bindPatternValue(
        pattern,
        argument,
        fnEnv,
        invokeContext
      );
      if (!bindResult.ok) {
        return { ok: false, diagnostics: bindResult.diagnostics };
      }
    }
    if (clause.rest) {
      const restArgs = args.slice(clause.params.length);
      defineVariable(fnEnv, clause.rest, makeList(restArgs));
    }

    let lastResult: EvalResult = {
      ok: true,
      value: makeNil(),
      diagnostics: [],
    };
    for (const expr of clause.body) {
      lastResult = await evaluateNode(expr, fnEnv, invokeContext);
      if (!lastResult.ok) {
        return lastResult;
      }
    }

    return lastResult;
  }

  return {
    ok: false,
    diagnostics: [
      {
        message: "Cannot apply non-function value",
        span,
        severity: DiagnosticSeverity.Error,
        code: "INTERP_NOT_CALLABLE",
      },
    ],
  };
};

const selectFunctionClause = (
  fn: FunctionValue,
  argCount: number
): FunctionClauseValue | null => {
  let variadicClause: FunctionClauseValue | null = null;
  for (const clause of fn.clauses) {
    if (clause.rest) {
      variadicClause = clause;
      continue;
    }
    if (clause.params.length === argCount) {
      return clause;
    }
  }
  if (variadicClause && argCount >= variadicClause.params.length) {
    return variadicClause;
  }
  return null;
};

const resolvePattern = (node: ExpressionNode): CachedPattern => {
  const cached = patternCache.get(node);
  if (cached) {
    return cached;
  }
  const result = parseBindingPattern(node);
  const entry: CachedPattern = result.ok
    ? { ok: true, pattern: result.pattern }
    : { ok: false, errors: result.errors };
  patternCache.set(node, entry);
  return entry;
};

const diagnosticsFromPatternErrors = (
  errors: readonly PatternError[]
): Diagnostic[] =>
  errors.map((error) => ({
    message: error.message,
    span: error.span,
    severity: DiagnosticSeverity.Error,
    code: INTERP_PATTERN_ERROR_CODES[error.kind] ?? DEFAULT_PATTERN_ERROR_CODE,
  }));

const okVoid = (): EvalResult<void> => ({ ok: true, diagnostics: [] });

const bindPatternValue = async (
  pattern: BindingPattern,
  value: Value,
  env: Environment,
  context: EvalContext
): Promise<EvalResult<void>> => {
  switch (pattern.kind) {
    case "symbol":
      defineVariable(env, pattern.node.value, value);
      return okVoid();
    case "vector":
      return await bindVectorPattern(pattern, value, env, context);
    case "map":
      return await bindMapPattern(pattern, value, env, context);
    default:
      return okVoid();
  }
};

const bindVectorPattern = async (
  pattern: Extract<BindingPattern, { kind: "vector" }>,
  value: Value,
  env: Environment,
  context: EvalContext
): Promise<EvalResult<void>> => {
  const elements = sequenceElements(value);
  for (let index = 0; index < pattern.elements.length; index += 1) {
    const elementPattern = pattern.elements[index]!;
    const elementValue = elements[index] ?? makeNil();
    const result = await bindPatternValue(
      elementPattern,
      elementValue,
      env,
      context
    );
    if (!result.ok) {
      return result;
    }
  }
  if (pattern.rest) {
    const restValues = elements.slice(pattern.elements.length);
    const restResult = await bindPatternValue(
      pattern.rest,
      makeVector(restValues),
      env,
      context
    );
    if (!restResult.ok) {
      return restResult;
    }
  }
  if (pattern.as) {
    defineVariable(env, pattern.as.value, value);
  }
  return okVoid();
};

const bindMapPattern = async (
  pattern: Extract<BindingPattern, { kind: "map" }>,
  value: Value,
  env: Environment,
  context: EvalContext
): Promise<EvalResult<void>> => {
  const entries =
    value.kind === "map" ? value.entries : new Map<string, Value>();
  if (pattern.as) {
    defineVariable(env, pattern.as.value, value);
  }
  const defaults = new Map<string, ExpressionNode>();
  for (const entry of pattern.defaults) {
    defaults.set(entry.binding, entry.value);
  }
  for (const property of pattern.properties) {
    const key = mapKeyToString(property.key);
    let resolved = entries.get(key);
    if (resolved === undefined && property.pattern.kind === "symbol") {
      const fallbackExpr = defaults.get(property.pattern.node.value);
      if (fallbackExpr) {
        const fallbackResult = await evaluateNode(fallbackExpr, env, context);
        if (!fallbackResult.ok) {
          return { ok: false, diagnostics: fallbackResult.diagnostics };
        }
        resolved = fallbackResult.value ?? makeNil();
      }
    }
    const bindResult = await bindPatternValue(
      property.pattern,
      resolved ?? makeNil(),
      env,
      context
    );
    if (!bindResult.ok) {
      return bindResult;
    }
  }
  return okVoid();
};

const mapKeyToString = (key: {
  readonly kind: string;
  readonly value: string;
}): string => {
  if (key.kind === "keyword") {
    return key.value.startsWith(":") ? key.value : `:${key.value}`;
  }
  if (key.kind === "string") {
    return key.value;
  }
  return key.value;
};

const sequenceElements = (value: Value): Value[] => {
  switch (value.kind) {
    case "vector":
    case "list":
    case "set":
      return [...value.elements];
    default:
      return [];
  }
};

/**
 * Evaluate (require alias "path") - import a vibe source file (async)
 */
const evaluateRequire = async (
  node: ListNode | NamespaceImportNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const aliasNode =
    node.kind === NK.NamespaceImport ? node.alias : node.elements[1];
  const pathNode =
    node.kind === NK.NamespaceImport ? node.source : node.elements[2];

  if (!aliasNode || aliasNode.kind !== NK.Symbol) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "require expects a symbol as the first argument",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_REQUIRE_ALIAS",
        },
      ],
    };
  }

  if (!pathNode || pathNode.kind !== NK.String) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "require expects a string path as the second argument",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_REQUIRE_PATH",
        },
      ],
    };
  }

  try {
    const { parseSource } = await import("@vibe/parser");
    const { readFileSync } = await import("fs");
    const { resolve } = await import("path");

    const filePath = resolve(pathNode.value);
    const source = readFileSync(filePath, "utf-8");
    const parseResult = await parseSource(source);

    if (!parseResult.ok) {
      return {
        ok: false,
        diagnostics: parseResult.diagnostics,
      };
    }

    // Create a new environment for the module
    const moduleEnv = extendEnvironment(env);
    const diagnostics: Diagnostic[] = [];
    let lastValue: Value = makeNil();

    // Evaluate all expressions in the module
    for (const expr of parseResult.program.body) {
      const result = await evaluateNode(expr, moduleEnv, context);
      if (!result.ok) {
        diagnostics.push(...result.diagnostics);
        continue;
      }
      if (result.value !== undefined) {
        lastValue = result.value;
      }
    }

    if (diagnostics.length > 0) {
      return { ok: false, diagnostics };
    }

    // Create a namespace object from the module environment's bindings
    const namespace = makeMap(new Map(moduleEnv.bindings));

    // Bind the namespace to the alias in the current environment
    defineVariable(env, aliasNode.value, namespace);

    return { ok: true, value: namespace, diagnostics: [] };
  } catch (error) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Failed to require module: ${
            error instanceof Error ? error.message : String(error)
          }`,
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_REQUIRE_FAILED",
        },
      ],
    };
  }
};

/**
 * Evaluate (import "path") - import a vibe source file with flattened namespace (async)
 */
const evaluateImport = async (
  node: ListNode | NamespaceImportNode,
  env: Environment,
  context: EvalContext
): Promise<EvalResult> => {
  const pathNode =
    node.kind === NK.NamespaceImport ? node.source : node.elements[1];

  if (!pathNode || pathNode.kind !== NK.String) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "import expects a string path as the argument",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_IMPORT_PATH",
        },
      ],
    };
  }

  try {
    const { parseSource } = await import("@vibe/parser");
    const { readFileSync } = await import("fs");
    const { resolve } = await import("path");

    const filePath = resolve(pathNode.value);
    const source = readFileSync(filePath, "utf-8");
    const parseResult = await parseSource(source);

    if (!parseResult.ok) {
      return {
        ok: false,
        diagnostics: parseResult.diagnostics,
      };
    }

    // Create a new environment for the module
    const moduleEnv = extendEnvironment(env);
    const diagnostics: Diagnostic[] = [];

    // Evaluate all expressions in the module
    for (const expr of parseResult.program.body) {
      const result = await evaluateNode(expr, moduleEnv, context);
      if (!result.ok) {
        diagnostics.push(...result.diagnostics);
        continue;
      }
    }

    if (diagnostics.length > 0) {
      return { ok: false, diagnostics };
    }

    // Flatten all module bindings into the current environment
    for (const [name, value] of moduleEnv.bindings) {
      defineVariable(env, name, value);
    }

    return { ok: true, value: makeNil(), diagnostics: [] };
  } catch (error) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Failed to import module: ${
            error instanceof Error ? error.message : String(error)
          }`,
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_IMPORT_FAILED",
        },
      ],
    };
  }
};

/**
 * Evaluate (external alias "module") - import a JS/TS module
 */
const evaluateExternal = (
  node: ListNode | NamespaceImportNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  const aliasNode =
    node.kind === NK.NamespaceImport ? node.alias : node.elements[1];
  const moduleNode =
    node.kind === NK.NamespaceImport ? node.source : node.elements[2];

  if (!aliasNode || aliasNode.kind !== NK.Symbol) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "external expects a symbol as the first argument",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_EXTERNAL_ALIAS",
        },
      ],
    };
  }

  if (!moduleNode || moduleNode.kind !== NK.String) {
    return {
      ok: false,
      diagnostics: [
        {
          message:
            "external expects a string module specifier as the second argument",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_EXTERNAL_MODULE",
        },
      ],
    };
  }

  // Import the JS/TS module using Bun's dynamic import
  // For now, use synchronous require since we're in a sync context
  try {
    const jsModule = require(moduleNode.value);

    // Create an external namespace value that wraps the JS module
    // This allows dynamic property access without pre-validating exports
    const namespace = makeExternalNamespace(jsModule);
    defineVariable(env, aliasNode.value, namespace);

    return { ok: true, value: namespace, diagnostics: [] };
  } catch (error) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Failed to import external module: ${
            error instanceof Error ? error.message : String(error)
          }`,
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_EXTERNAL_FAILED",
        },
      ],
    };
  }
};

const evaluateGensym = (
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  // Optional hint argument
  const hintNode = node.elements[1];
  let hint = "g";

  if (hintNode) {
    if (hintNode.kind !== NK.String) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "gensym expects an optional string hint as argument",
            span: node.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_GENSYM_HINT",
          },
        ],
      };
    }
    hint = hintNode.value;
  }

  const unique = allocateRuntimeGensym(hint, context);
  return { ok: true, value: makeSymbol(unique), diagnostics: [] };
};

/**
 * Convert a vibe Value to a JavaScript value
 */
const valueToJS = (value: Value): any => {
  switch (value.kind) {
    case "number":
      return value.value;
    case "string":
      return value.value;
    case "boolean":
      return value.value;
    case "nil":
      return null;
    case "symbol":
      return runtimeSymbol(value.value);
    case "list":
    case "vector":
      return value.elements.map(valueToJS);
    case "set":
      return new Set(value.elements.map(valueToJS));
    case "map":
      const obj: Record<string, any> = {};
      for (const [k, v] of value.entries) {
        obj[k] = valueToJS(v);
      }
      return obj;
    case "function":
    case "builtin":
      return (...args: any[]) => {
        const vibeArgs = args.map(jsToValue);
        if (value.kind === "builtin") {
          return valueToJS(
            value.fn(vibeArgs, {
              start: { line: 0, column: 0, offset: 0 },
              end: { line: 0, column: 0, offset: 0 },
            })
          );
        } else {
          // Can't easily call vibe functions from JS without context
          throw new Error("Cannot call vibe function from JavaScript");
        }
      };
    case "external":
      return value.module;
    case "error":
      return value.error;
    default:
      return null;
  }
};

/**
 * Convert a JavaScript value to a vibe Value
 */
const jsToValue = (jsValue: any): Value => {
  if (jsValue === null || jsValue === undefined) {
    return makeNil();
  }
  if (typeof jsValue === "number") {
    return makeNumber(jsValue);
  }
  if (typeof jsValue === "string") {
    return makeString(jsValue);
  }
  if (typeof jsValue === "boolean") {
    return makeBoolean(jsValue);
  }
  if (Array.isArray(jsValue)) {
    return makeList(jsValue.map(jsToValue));
  }
  if (jsValue instanceof Set) {
    return makeSet(Array.from(jsValue).map(jsToValue));
  }
  if (runtimeSymbol_QMARK(jsValue)) {
    return makeSymbol((jsValue as RuntimeSymbol).name);
  }
  if (jsValue instanceof Error) {
    return makeError(jsValue);
  }
  if (typeof jsValue === "object") {
    const entries = new Map<string, Value>();
    for (const [k, v] of Object.entries(jsValue)) {
      entries.set(k, jsToValue(v));
    }
    return makeMap(entries);
  }
  // For functions and other types, wrap as a string representation
  return makeString(String(jsValue));
};

/**
 * Create the root environment.
 *
 * NOTE: No builtins are pre-loaded. All runtime primitives are accessed
 * through the external module system (e.g., @vibe/runtime).
 * Special forms (def, fn, if, let, gensym, etc.) are handled directly in the evaluator.
 * The REPL loads prelude for convenience.
 */
export const createRootEnvironment = (): Environment => {
  return createEnvironment();
};
