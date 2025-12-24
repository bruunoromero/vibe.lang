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
} from "@vibe/syntax";
import { DiagnosticSeverity, NodeKind as NK } from "@vibe/syntax";
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
import type { Value } from "./values";
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
};

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
export const evaluate = async (
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
  const headResult = await evaluate(head!, env, context);
  if (!headResult.ok) {
    return headResult;
  }

  const fn = headResult.value!;
  const argNodes = node.elements.slice(1);
  const args: Value[] = [];
  const diagnostics: Diagnostic[] = [];

  for (const argNode of argNodes) {
    const argResult = await evaluate(argNode, env, context);
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
    const elemResult = await evaluate(elem, env, context);
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
    const elemResult = await evaluate(elem, env, context);
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

    const keyResult = await evaluate(entry.key, env, context);
    if (!keyResult.ok) {
      diagnostics.push(...keyResult.diagnostics);
      continue;
    }

    const valueResult = await evaluate(entry.value, env, context);
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

  const valueResult = await evaluate(valueNode, env, context);
  if (!valueResult.ok) {
    return valueResult;
  }

  defineVariable(env, nameNode.value, valueResult.value!);
  return { ok: true, value: valueResult.value, diagnostics: [] };
};

const evaluateFn = (node: ListNode, env: Environment): EvalResult => {
  const paramsNode = node.elements[1];

  if (!paramsNode || paramsNode.kind !== NK.Vector) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "fn requires a parameter vector",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_FN_REQUIRES_VECTOR",
        },
      ],
    };
  }

  const params: string[] = [];
  let restParam: string | undefined;
  let sawRestMarker = false;

  for (let i = 0; i < paramsNode.elements.length; i++) {
    const paramNode = paramsNode.elements[i];
    if (!paramNode) {
      continue;
    }
    if (paramNode.kind !== NK.Symbol) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "fn parameter must be a symbol",
            span: paramNode.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_FN_PARAM_SYMBOL",
          },
        ],
      };
    }

    if (paramNode.value === "&") {
      if (sawRestMarker) {
        return {
          ok: false,
          diagnostics: [
            {
              message: "fn allows only a single & rest parameter",
              span: paramNode.span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_FN_DUPLICATE_REST",
            },
          ],
        };
      }
      sawRestMarker = true;
      const restNode = paramsNode.elements[i + 1];
      if (!restNode || restNode.kind !== NK.Symbol) {
        return {
          ok: false,
          diagnostics: [
            {
              message: "& must be followed by a symbol name",
              span: paramNode.span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_FN_REST_REQUIRES_SYMBOL",
            },
          ],
        };
      }
      restParam = restNode.value;
      i += 1;
      continue;
    }

    if (sawRestMarker) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "Parameters cannot appear after & rest parameter",
            span: paramNode.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_FN_PARAMS_AFTER_REST",
          },
        ],
      };
    }

    params.push(paramNode.value);
  }

  const body = node.elements.slice(2);
  if (body.length === 0) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "fn requires a body",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_FN_REQUIRES_BODY",
        },
      ],
    };
  }

  const fn = makeFunction(params, body, env, node.span, restParam);
  return { ok: true, value: fn, diagnostics: [] };
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
    const nameNode = bindings[i];
    const valueNode = bindings[i + 1];

    if (!nameNode) continue;

    if (nameNode.kind !== NK.Symbol) {
      diagnostics.push({
        message: "Binding name must be a symbol",
        span: nameNode.span,
        severity: DiagnosticSeverity.Error,
        code: "INTERP_LET_NAME_SYMBOL",
      });
      continue;
    }

    if (!valueNode) {
      diagnostics.push({
        message: `Missing value for binding ${nameNode.value}`,
        span: nameNode.span,
        severity: DiagnosticSeverity.Error,
        code: "INTERP_LET_MISSING_VALUE",
      });
      continue;
    }

    const valueResult = await evaluate(valueNode, letEnv, context);
    if (!valueResult.ok) {
      diagnostics.push(...valueResult.diagnostics);
      continue;
    }

    defineVariable(letEnv, nameNode.value, valueResult.value!);
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  const bodyNodes = node.elements.slice(2);
  let lastResult: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };

  for (const bodyNode of bodyNodes) {
    lastResult = await evaluate(bodyNode, letEnv, context);
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

  const condResult = await evaluate(condNode, env, context);
  if (!condResult.ok) {
    return condResult;
  }

  if (isTruthy(condResult.value!)) {
    if (thenNode) {
      return await evaluate(thenNode, env, context);
    }
    return { ok: true, value: makeNil(), diagnostics: [] };
  } else {
    if (elseNode) {
      return await evaluate(elseNode, env, context);
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
    lastResult = await evaluate(bodyNode, env, context);
    if (!lastResult.ok) {
      return lastResult;
    }
  }

  return lastResult;
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
    // Check arity
    const minArity = fn.params.length;
    const isVariadic = fn.rest !== undefined;

    if (!isVariadic && args.length !== minArity) {
      return {
        ok: false,
        diagnostics: [
          {
            message: `Function expects ${minArity} argument(s) but received ${args.length}`,
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_ARITY_MISMATCH",
          },
        ],
      };
    }

    if (isVariadic && args.length < minArity) {
      return {
        ok: false,
        diagnostics: [
          {
            message: `Function expects at least ${minArity} argument(s) but received ${args.length}`,
            span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_ARITY_MISMATCH",
          },
        ],
      };
    }

    // Create new environment extending the function's closure
    const fnEnv = extendEnvironment(fn.closure);

    // Bind parameters
    fn.params.forEach((param, index) => {
      defineVariable(fnEnv, param, args[index]!);
    });

    // Bind rest parameter if present
    if (fn.rest) {
      const restArgs = args.slice(fn.params.length);
      defineVariable(fnEnv, fn.rest, makeList(restArgs));
    }

    // Evaluate body
    const newContext: EvalContext = { callDepth: context.callDepth + 1 };
    let lastResult: EvalResult = {
      ok: true,
      value: makeNil(),
      diagnostics: [],
    };
    for (const expr of fn.body) {
      lastResult = await evaluate(expr, fnEnv, newContext);
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
      const result = await evaluate(expr, moduleEnv, context);
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
      const result = await evaluate(expr, moduleEnv, context);
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
  // Initialize gensym counter if not present
  if (!context.gensymCounter) {
    context.gensymCounter = { value: 0 };
  }

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

  const unique = `${hint}__${context.gensymCounter.value++}`;
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
