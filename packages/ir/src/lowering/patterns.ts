/**
 * Pattern Lowering
 *
 * Transforms AST patterns to IR form.
 */

import type { Pattern } from "@vibe/syntax";
import type { IRPattern } from "../types";
import { IRError } from "../types";
import type { LoweringContext } from "./context";

/**
 * Lower an AST pattern to IR form.
 */
export function lowerPattern(
  pattern: Pattern,
  ctx: LoweringContext
): IRPattern {
  switch (pattern.kind) {
    case "VarPattern":
      return {
        kind: "IRVarPattern",
        name: pattern.name,
        span: pattern.span,
      };

    case "WildcardPattern":
      return {
        kind: "IRWildcardPattern",
        span: pattern.span,
      };

    case "IntPattern":
      return {
        kind: "IRLiteralPattern",
        value: Number(pattern.value),
        literalType: "int",
        span: pattern.span,
      };

    case "FloatPattern":
      return {
        kind: "IRLiteralPattern",
        value: Number(pattern.value),
        literalType: "float",
        span: pattern.span,
      };

    case "StringPattern":
      return {
        kind: "IRLiteralPattern",
        value: pattern.value,
        literalType: "string",
        span: pattern.span,
      };

    case "CharPattern":
      return {
        kind: "IRLiteralPattern",
        value: pattern.value,
        literalType: "char",
        span: pattern.span,
      };

    case "ConstructorPattern": {
      const tag = ctx.constructorTags.get(pattern.name) ?? 0;
      return {
        kind: "IRConstructorPattern",
        name: pattern.name,
        args: pattern.args.map((p) => lowerPattern(p, ctx)),
        tag,
        span: pattern.span,
      };
    }

    case "TuplePattern":
      return {
        kind: "IRTuplePattern",
        elements: pattern.elements.map((p) => lowerPattern(p, ctx)),
        span: pattern.span,
      };

    case "ListPattern":
      return {
        kind: "IRListPattern",
        elements: pattern.elements.map((p) => lowerPattern(p, ctx)),
        span: pattern.span,
      };

    case "ConsPattern":
      return {
        kind: "IRConsPattern",
        head: lowerPattern(pattern.head, ctx),
        tail: lowerPattern(pattern.tail, ctx),
        span: pattern.span,
      };

    case "RecordPattern":
      return {
        kind: "IRRecordPattern",
        fields: pattern.fields.map((f) => ({
          name: f.name,
          pattern: f.pattern ? lowerPattern(f.pattern, ctx) : undefined,
        })),
        span: pattern.span,
      };

    default:
      const _exhaustive: never = pattern;
      throw new IRError(
        `Unknown pattern kind: ${(pattern as any).kind}`,
        (pattern as any).span
      );
  }
}
