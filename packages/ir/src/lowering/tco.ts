/**
 * Tail Call Optimization (TCO) Rewriter
 *
 * Detects self-tail-calls in function bodies and rewrites them into
 * IRSelfLoop / IRLoopContinue nodes. Codegen emits these as while(true) loops.
 *
 * Handles:
 * - Top-level value definitions: f x y = ... f newX newY ...
 * - Local let-bindings: let go acc = ... go newAcc ... in go init
 *
 * Does NOT handle:
 * - Mutual recursion (f calls g calls f)
 * - Non-IRVarPattern parameters (tuple/constructor destructuring in params)
 */

import type { IRExpr, IRPattern } from "../types";

/**
 * Attempt to rewrite self-tail-calls in a function body into loop form.
 *
 * @param funcName - The name of the function being defined
 * @param params - The function's parameter patterns (after dict params)
 * @param body - The lowered function body
 * @param dictParamCount - Number of leading dictionary parameters (skipped in IRLoopContinue args)
 * @returns The rewritten body wrapped in IRSelfLoop, or the original body if no tail calls found
 */
export function rewriteSelfTailCalls(
  funcName: string,
  params: IRPattern[],
  body: IRExpr,
  dictParamCount: number,
): IRExpr {
  // Extract param names — bail if any param isn't a simple variable
  const paramNames: string[] = [];
  for (const p of params) {
    if (p.kind !== "IRVarPattern") return body;
    paramNames.push(p.name);
  }

  if (paramNames.length === 0) return body;

  const expectedArgCount = dictParamCount + paramNames.length;
  let foundTailCall = false;

  function rewrite(expr: IRExpr, inTailPos: boolean): IRExpr {
    switch (expr.kind) {
      case "IRApply": {
        // Self-tail-call: f(args...)
        if (
          inTailPos &&
          expr.callee.kind === "IRVar" &&
          expr.callee.name === funcName &&
          expr.callee.namespace === "value" &&
          expr.args.length === expectedArgCount
        ) {
          foundTailCall = true;
          return {
            kind: "IRLoopContinue",
            args: expr.args.slice(dictParamCount),
            span: expr.span,
          };
        }
        // IIFE from let-binding: (\x -> body)(value)
        // The lambda body is in tail position if the IIFE itself is.
        if (
          inTailPos &&
          expr.callee.kind === "IRLambda" &&
          expr.args.length === 1
        ) {
          const innerBody = rewrite(expr.callee.body, true);
          if (innerBody === expr.callee.body) return expr;
          return {
            ...expr,
            callee: { ...expr.callee, body: innerBody },
          };
        }
        return expr;
      }

      case "IRIf": {
        const thenBranch = rewrite(expr.thenBranch, inTailPos);
        const elseBranch = rewrite(expr.elseBranch, inTailPos);
        if (thenBranch === expr.thenBranch && elseBranch === expr.elseBranch) {
          return expr;
        }
        return { ...expr, thenBranch, elseBranch };
      }

      case "IRCase": {
        let changed = false;
        const branches = expr.branches.map((b) => {
          const body = rewrite(b.body, inTailPos);
          if (body !== b.body) changed = true;
          return body === b.body ? b : { ...b, body };
        });
        if (!changed) return expr;
        return { ...expr, branches };
      }

      case "IRLambda":
        return expr;

      case "IRSelfLoop":
      case "IRLoopContinue":
        return expr;

      default:
        return expr;
    }
  }

  const rewritten = rewrite(body, true);
  if (!foundTailCall) return body;

  return {
    kind: "IRSelfLoop",
    paramNames,
    body: rewritten,
    span: body.span,
  };
}
