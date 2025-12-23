import type {
  ExpressionNode,
  NodeKind,
  SourceSpan,
  ListNode,
  VectorNode,
  MapNode,
  Diagnostic,
} from "@vibe/syntax";
import { DiagnosticSeverity, NodeKind as NK } from "@vibe/syntax";
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
  isSequence,
  isSymbol,
  isString,
} from "./values";
import { createBuiltins } from "./builtins";

const MAX_CALL_DEPTH = 1000;

export interface EvalResult<T = Value> {
  readonly ok: boolean;
  readonly value?: T;
  readonly diagnostics: readonly Diagnostic[];
}

export interface EvalContext {
  readonly callDepth: number;
}

/**
 * Evaluate an expression node in the given environment.
 */
export const evaluate = (
  node: ExpressionNode,
  env: Environment,
  context: EvalContext = { callDepth: 0 }
): EvalResult => {
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
      return evaluateList(node, env, context);
    case NK.Vector:
      return evaluateVector(node, env, context);
    case NK.Set:
      return evaluateSet(node, env, context);
    case NK.Map:
      return evaluateMap(node, env, context);
    case NK.Quote:
      // Quote returns the target as a literal value without evaluation
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

const evaluateList = (
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  if (node.elements.length === 0) {
    // Empty list evaluates to empty list value
    return { ok: true, value: makeList([]), diagnostics: [] };
  }

  const head = node.elements[0];
  if (!head) {
    return { ok: true, value: makeList([]), diagnostics: [] };
  }

  // Check for special forms
  if (head.kind === NK.Symbol) {
    const specialResult = evaluateSpecialForm(head.value, node, env, context);
    if (specialResult) {
      return specialResult;
    }
  }

  // Not a special form, evaluate as function application
  const headResult = evaluate(head, env, context);
  if (!headResult.ok) {
    return headResult;
  }

  if (!isCallable(headResult.value!)) {
    return {
      ok: false,
      diagnostics: [
        {
          message: `Cannot call non-function value: ${headResult.value!.kind}`,
          span: head.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_NOT_CALLABLE",
        },
      ],
    };
  }

  // Evaluate arguments
  const args: Value[] = [];
  const diagnostics: Diagnostic[] = [];
  for (let i = 1; i < node.elements.length; i++) {
    const arg = node.elements[i];
    if (!arg) continue;
    const argResult = evaluate(arg, env, context);
    if (!argResult.ok) {
      diagnostics.push(...argResult.diagnostics);
      continue;
    }
    args.push(argResult.value!);
  }

  if (diagnostics.length > 0) {
    return { ok: false, diagnostics };
  }

  // Apply function
  return applyFunction(headResult.value!, args, node.span, context);
};

const evaluateSpecialForm = (
  name: string,
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult | null => {
  switch (name) {
    case "def":
      return evaluateDef(node, env, context);
    case "let":
      return evaluateLet(node, env, context);
    case "fn":
      return evaluateFn(node, env);
    case "if":
      return evaluateIf(node, env, context);
    case "quote":
      return evaluateQuoteForm(node, env);
    case "do":
      return evaluateDo(node, env, context);
    default:
      return null;
  }
};

const evaluateDef = (
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
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
          message: "def requires a value expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_DEF_REQUIRES_VALUE",
        },
      ],
    };
  }

  const valueResult = evaluate(valueNode, env, context);
  if (!valueResult.ok) {
    return valueResult;
  }

  defineVariable(env, nameNode.value, valueResult.value!);
  return { ok: true, value: valueResult.value, diagnostics: [] };
};

const evaluateLet = (
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
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

  if (bindingsNode.elements.length % 2 !== 0) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "let bindings must have an even number of forms",
          span: bindingsNode.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_LET_ODD_BINDINGS",
        },
      ],
    };
  }

  // Create new environment for let bindings
  const letEnv = extendEnvironment(env);

  // Process bindings
  for (let i = 0; i < bindingsNode.elements.length; i += 2) {
    const nameNode = bindingsNode.elements[i];
    const valueNode = bindingsNode.elements[i + 1];

    if (!nameNode || nameNode.kind !== NK.Symbol) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "let binding name must be a symbol",
            span: nameNode?.span ?? bindingsNode.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_LET_NAME_SYMBOL",
          },
        ],
      };
    }

    if (!valueNode) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "let binding missing value",
            span: bindingsNode.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_LET_MISSING_VALUE",
          },
        ],
      };
    }

    const valueResult = evaluate(valueNode, letEnv, context);
    if (!valueResult.ok) {
      return valueResult;
    }

    defineVariable(letEnv, nameNode.value, valueResult.value!);
  }

  // Evaluate body forms
  if (node.elements.length < 3) {
    return { ok: true, value: makeNil(), diagnostics: [] };
  }

  let lastResult: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };
  for (let i = 2; i < node.elements.length; i++) {
    const form = node.elements[i];
    if (!form) continue;
    lastResult = evaluate(form, letEnv, context);
    if (!lastResult.ok) {
      return lastResult;
    }
  }

  return lastResult;
};

const evaluateFn = (node: ListNode, env: Environment): EvalResult => {
  const paramsNode = node.elements[1];

  if (!paramsNode || paramsNode.kind !== NK.Vector) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "fn requires a vector of parameters",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_FN_REQUIRES_VECTOR",
        },
      ],
    };
  }

  const params: string[] = [];
  let rest: string | undefined = undefined;
  let sawAmpersand = false;

  for (let i = 0; i < paramsNode.elements.length; i++) {
    const param = paramsNode.elements[i];
    if (!param) continue;

    if (param.kind !== NK.Symbol) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "fn parameter must be a symbol",
            span: param.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_FN_PARAM_SYMBOL",
          },
        ],
      };
    }

    if (param.value === "&") {
      if (sawAmpersand) {
        return {
          ok: false,
          diagnostics: [
            {
              message: "fn can only have one & rest parameter",
              span: param.span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_FN_DUPLICATE_REST",
            },
          ],
        };
      }
      sawAmpersand = true;
      const nextParam = paramsNode.elements[i + 1];
      if (!nextParam || nextParam.kind !== NK.Symbol) {
        return {
          ok: false,
          diagnostics: [
            {
              message: "& must be followed by a rest parameter name",
              span: param.span,
              severity: DiagnosticSeverity.Error,
              code: "INTERP_FN_REST_REQUIRES_SYMBOL",
            },
          ],
        };
      }
      rest = nextParam.value;
      i++; // Skip the rest parameter name
      continue;
    }

    if (sawAmpersand) {
      return {
        ok: false,
        diagnostics: [
          {
            message: "fn cannot have parameters after & rest parameter",
            span: param.span,
            severity: DiagnosticSeverity.Error,
            code: "INTERP_FN_PARAMS_AFTER_REST",
          },
        ],
      };
    }

    params.push(param.value);
  }

  const body = node.elements.slice(2).filter((n): n is ExpressionNode => !!n);

  if (body.length === 0) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "fn requires at least one body expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_FN_REQUIRES_BODY",
        },
      ],
    };
  }

  const fn = makeFunction(params, body, env, node.span, rest);
  return { ok: true, value: fn, diagnostics: [] };
};

const evaluateIf = (
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
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

  const condResult = evaluate(condNode, env, context);
  if (!condResult.ok) {
    return condResult;
  }

  if (isTruthy(condResult.value!)) {
    if (!thenNode) {
      return { ok: true, value: makeNil(), diagnostics: [] };
    }
    return evaluate(thenNode, env, context);
  } else {
    if (!elseNode) {
      return { ok: true, value: makeNil(), diagnostics: [] };
    }
    return evaluate(elseNode, env, context);
  }
};

const evaluateQuoteForm = (node: ListNode, env: Environment): EvalResult => {
  const target = node.elements[1];
  if (!target) {
    return {
      ok: false,
      diagnostics: [
        {
          message: "quote requires an expression",
          span: node.span,
          severity: DiagnosticSeverity.Error,
          code: "INTERP_QUOTE_REQUIRES_EXPR",
        },
      ],
    };
  }
  return evaluateQuote(target, env);
};

const evaluateQuote = (node: ExpressionNode, env: Environment): EvalResult => {
  // Convert AST node to a value literal without evaluation
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
        if (!elem) continue;
        const elemResult = evaluateQuote(elem, env);
        if (!elemResult.ok) return elemResult;
        elements.push(elemResult.value!);
      }
      return { ok: true, value: makeList(elements), diagnostics: [] };
    }
    case NK.Vector: {
      const elements: Value[] = [];
      for (const elem of node.elements) {
        if (!elem) continue;
        const elemResult = evaluateQuote(elem, env);
        if (!elemResult.ok) return elemResult;
        elements.push(elemResult.value!);
      }
      return { ok: true, value: makeVector(elements), diagnostics: [] };
    }
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

const evaluateDo = (
  node: ListNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  if (node.elements.length < 2) {
    return { ok: true, value: makeNil(), diagnostics: [] };
  }

  let lastResult: EvalResult = { ok: true, value: makeNil(), diagnostics: [] };
  for (let i = 1; i < node.elements.length; i++) {
    const form = node.elements[i];
    if (!form) continue;
    lastResult = evaluate(form, env, context);
    if (!lastResult.ok) {
      return lastResult;
    }
  }

  return lastResult;
};

const evaluateVector = (
  node: VectorNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  const elements: Value[] = [];
  const diagnostics: Diagnostic[] = [];

  for (const elem of node.elements) {
    if (!elem) continue;
    const elemResult = evaluate(elem, env, context);
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

const evaluateSet = (
  node: VectorNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  const elements: Value[] = [];
  const diagnostics: Diagnostic[] = [];

  for (const elem of node.elements) {
    if (!elem) continue;
    const elemResult = evaluate(elem, env, context);
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

const evaluateMap = (
  node: MapNode,
  env: Environment,
  context: EvalContext
): EvalResult => {
  const entries = new Map<string, Value>();
  const diagnostics: Diagnostic[] = [];

  for (const entry of node.entries) {
    if (!entry.key || !entry.value) continue;

    const keyResult = evaluate(entry.key, env, context);
    if (!keyResult.ok) {
      diagnostics.push(...keyResult.diagnostics);
      continue;
    }

    const valueResult = evaluate(entry.value, env, context);
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

const applyFunction = (
  fn: Value,
  args: readonly Value[],
  span: SourceSpan,
  context: EvalContext
): EvalResult => {
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
      lastResult = evaluate(expr, fnEnv, newContext);
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
 * Create an environment populated with builtin functions.
 */
export const createRootEnvironment = (): Environment => {
  const env = createEnvironment();
  const builtins = createBuiltins();
  for (const [name, value] of Object.entries(builtins)) {
    defineVariable(env, name, value);
  }
  return env;
};
