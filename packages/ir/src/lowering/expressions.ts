/**
 * Expression Lowering
 *
 * This module implements the core transformations from AST expressions to IR:
 *
 * 1. Let-binding lifting: Flattens nested `let...in` to sequential top-level bindings
 * 2. Case lowering: Converts case expressions on Bool/primitives to if-then-else chains
 * 3. Record update desugaring: Transforms `{ r | field = value }` to record construction
 * 4. Infix resolution: Converts infix operators to function application
 * 5. Paren elimination: Removes parentheses (they're just for grouping)
 * 6. Module access resolution: Transforms module.field chains into IRModuleAccess nodes
 */

import type { Expr, Pattern, Span, ValueDeclaration } from "@vibe/syntax";
import {
  BUILTIN_MODULE_NAME,
  BOOL_TYPE_NAME,
  SHORT_CIRCUIT_OPERATORS,
} from "@vibe/syntax";
import type {
  IRExpr,
  IRPattern,
  IRRecordField,
  IRModuleAccess,
} from "../types";
import { IRError } from "../types";
import type { LoweringContext } from "./context";
import { lowerPattern } from "./patterns";

/**
 * Lower an AST expression to IR form.
 * This is the main entry point for expression lowering.
 */
export function lowerExpr(expr: Expr, ctx: LoweringContext): IRExpr {
  switch (expr.kind) {
    case "Var":
      return lowerVar(expr, ctx);

    case "Number":
      return lowerNumber(expr);

    case "String":
      return {
        kind: "IRLiteral",
        value: expr.value,
        literalType: "string",
        span: expr.span,
      };

    case "Char":
      return {
        kind: "IRLiteral",
        value: expr.value,
        literalType: "char",
        span: expr.span,
      };

    case "Lambda":
      return {
        kind: "IRLambda",
        params: expr.args.map((p) => lowerPattern(p, ctx)),
        body: lowerExpr(expr.body, ctx),
        span: expr.span,
      };

    case "Apply":
      return {
        kind: "IRApply",
        callee: lowerExpr(expr.callee, ctx),
        args: expr.args.map((a) => lowerExpr(a, ctx)),
        span: expr.span,
      };

    case "If":
      return {
        kind: "IRIf",
        condition: lowerExpr(expr.condition, ctx),
        thenBranch: lowerExpr(expr.thenBranch, ctx),
        elseBranch: lowerExpr(expr.elseBranch, ctx),
        span: expr.span,
      };

    case "LetIn":
      return lowerLetIn(expr, ctx);

    case "Case":
      return lowerCase(expr, ctx);

    case "Infix":
      return lowerInfix(expr, ctx);

    case "Unary":
      return {
        kind: "IRUnary",
        operator: expr.operator,
        operand: lowerExpr(expr.operand, ctx),
        span: expr.span,
      };

    case "Paren":
      // Parentheses are just for grouping, lower the inner expression
      return lowerExpr(expr.expression, ctx);

    case "Tuple":
      return {
        kind: "IRTuple",
        elements: expr.elements.map((e) => lowerExpr(e, ctx)),
        span: expr.span,
      };

    case "Unit":
      return {
        kind: "IRUnit",
        span: expr.span,
      };

    case "List":
      return {
        kind: "IRList",
        elements: expr.elements.map((e) => lowerExpr(e, ctx)),
        span: expr.span,
      };

    case "ListRange":
      // Desugar [a..b] to range a b (requires runtime function)
      return {
        kind: "IRApply",
        callee: {
          kind: "IRApply",
          callee: {
            kind: "IRVar",
            name: "range",
            namespace: "value",
            span: expr.span,
          },
          args: [lowerExpr(expr.start, ctx)],
          span: expr.span,
        },
        args: [lowerExpr(expr.end, ctx)],
        span: expr.span,
      };

    case "Record":
      return {
        kind: "IRRecord",
        fields: expr.fields.map((f) => ({
          name: f.name,
          value: lowerExpr(f.value, ctx),
          span: f.span,
        })),
        span: expr.span,
      };

    case "RecordUpdate":
      return lowerRecordUpdate(expr, ctx);

    case "FieldAccess": {
      // First, try to resolve as a module-qualified access (e.g., JS.null or Vibe.JS.null)
      const moduleAccess = tryResolveModuleAccess(expr, ctx);
      if (moduleAccess) {
        return moduleAccess;
      }
      // Otherwise, it's a regular record field access
      return {
        kind: "IRFieldAccess",
        target: lowerExpr(expr.target, ctx),
        field: expr.field,
        span: expr.span,
      };
    }

    default:
      const _exhaustive: never = expr;
      throw new IRError(
        `Unknown expression kind: ${(expr as any).kind}`,
        (expr as any).span
      );
  }
}

/**
 * Try to resolve a FieldAccess chain as a module-qualified access.
 *
 * This handles cases like:
 * - `JS.null` when there's an `import Vibe.JS as JS`
 * - `Vibe.JS.null` when there's an `import Vibe.JS`
 *
 * Returns an IRModuleAccess node if successful, or null if this is not a module access.
 */
function tryResolveModuleAccess(
  expr: Extract<Expr, { kind: "FieldAccess" }>,
  ctx: LoweringContext
): IRModuleAccess | null {
  // Collect the chain of field accesses to reconstruct the module path
  const parts: string[] = [];
  let current: Expr = expr;

  // Traverse backwards through FieldAccess expressions
  while (current.kind === "FieldAccess") {
    parts.unshift(current.field);
    current = current.target;
  }

  // The base should be a Var to be a module reference
  if (current.kind !== "Var") {
    return null;
  }

  // The base name (e.g., "Vibe" from "Vibe.JS.null" or "JS" from "JS.null" when using alias)
  const baseName = current.name;
  parts.unshift(baseName);

  // Now we have the full path like ["Vibe", "JS", "null"] or ["JS", "null"] for alias access
  // Try to find a matching import for the module path

  for (const imp of ctx.imports) {
    const importParts = imp.moduleName.split(".");

    // Check for module alias match first (e.g., "import Vibe.JS as JS" with "JS.null")
    // If the base name matches the alias and we have remaining parts, resolve from that module
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = ctx.dependencies.get(imp.moduleName);
      if (!depModule) {
        continue;
      }

      // Get the remaining parts after the alias (e.g., ["null"] from ["JS", "null"])
      const fieldParts = parts.slice(1);

      // Look up the value in the module
      if (fieldParts.length === 1) {
        const field = fieldParts[0]!;
        const valueInfo = depModule.values[field];
        if (valueInfo) {
          // Get the external name if this is an external binding
          const decl = valueInfo.declaration;
          let externalName: string | undefined;
          if (decl.kind === "ExternalDeclaration") {
            externalName = decl.target.exportName;
          }

          return {
            kind: "IRModuleAccess",
            importAlias: imp.alias,
            moduleName: imp.moduleName,
            valueName: field,
            externalName,
            span: expr.span,
          };
        }
      }
    }

    // Check if this import could match our path (non-alias case)
    // If import is "Vibe.JS" and we access "Vibe.JS.null", it matches
    if (importParts.length <= parts.length - 1) {
      let matches = true;
      for (let i = 0; i < importParts.length; i++) {
        if (importParts[i] !== parts[i]) {
          matches = false;
          break;
        }
      }

      if (matches) {
        const depModule = ctx.dependencies.get(imp.moduleName);
        if (!depModule) {
          continue;
        }

        // Get the remaining parts after the module name
        const fieldParts = parts.slice(importParts.length);

        // Look up the value in the module
        if (fieldParts.length === 1) {
          const field = fieldParts[0]!;
          const valueInfo = depModule.values[field];
          if (valueInfo) {
            // Get the external name if this is an external binding
            const decl = valueInfo.declaration;
            let externalName: string | undefined;
            if (decl.kind === "ExternalDeclaration") {
              externalName = decl.target.exportName;
            }

            // For unaliased imports, use the last segment of the module name as the alias
            const alias = imp.alias || importParts[importParts.length - 1]!;

            return {
              kind: "IRModuleAccess",
              importAlias: alias,
              moduleName: imp.moduleName,
              valueName: field,
              externalName,
              span: expr.span,
            };
          }
        }
      }
    }
  }

  return null;
}

/**
 * Lower a variable reference.
 * Distinguishes between value references (lowercase) and constructor references (uppercase).
 */
function lowerVar(
  expr: Extract<Expr, { kind: "Var" }>,
  ctx: LoweringContext
): IRExpr {
  // Check if this is a constructor
  if (expr.namespace === "upper") {
    const ctorInfo = ctx.semantics.constructors[expr.name];
    if (ctorInfo) {
      const tag = ctx.constructorTags.get(expr.name) ?? 0;
      // Zero-arity constructor: emit as IRConstructor directly
      if (ctorInfo.arity === 0) {
        return {
          kind: "IRConstructor",
          name: expr.name,
          args: [],
          tag,
          span: expr.span,
        };
      }
      // Non-zero arity: emit as variable that will be applied
      return {
        kind: "IRVar",
        name: expr.name,
        namespace: "constructor",
        span: expr.span,
      };
    }
  }

  return {
    kind: "IRVar",
    name: expr.name,
    namespace: "value",
    span: expr.span,
  };
}

/**
 * Lower a number literal.
 * Determines if it's an int or float based on the string representation.
 */
function lowerNumber(expr: Extract<Expr, { kind: "Number" }>): IRExpr {
  const isFloat =
    expr.value.includes(".") ||
    expr.value.includes("e") ||
    expr.value.includes("E");
  return {
    kind: "IRLiteral",
    value: isFloat ? parseFloat(expr.value) : parseInt(expr.value, 10),
    literalType: isFloat ? "float" : "int",
    span: expr.span,
  };
}

// ============================================================================
// Let-Binding Lifting
// ============================================================================

/**
 * Lower a let...in expression.
 *
 * Strategy: Lift all bindings to the module level with unique names,
 * then return the body with references updated.
 *
 * Example:
 *   let x = 1
 *       y = x + 1
 *   in x + y
 *
 * Becomes (conceptually):
 *   $x_0 = 1
 *   $y_1 = $x_0 + 1
 *   body: $x_0 + $y_1
 *
 * For now, we use a simpler approach: convert to nested lambdas applied immediately.
 * This preserves semantics and is easier to implement.
 *
 * let x = e1 in e2  =>  (\x -> e2) e1
 */
function lowerLetIn(
  expr: Extract<Expr, { kind: "LetIn" }>,
  ctx: LoweringContext
): IRExpr {
  // For multiple bindings, we chain them
  // let x = e1; y = e2 in body => (\x -> (\y -> body) e2) e1
  // But we need to be careful about ordering - bindings can reference earlier ones

  let result = lowerExpr(expr.body, ctx);

  // Process bindings in reverse order to build up the chain
  for (let i = expr.bindings.length - 1; i >= 0; i--) {
    const binding = expr.bindings[i];
    if (!binding) continue;

    // Lower the binding's body
    const bindingValue = lowerValueBody(binding, ctx);

    // Create pattern from binding name and args
    const pattern: IRPattern =
      binding.args.length === 0
        ? { kind: "IRVarPattern", name: binding.name, span: binding.span }
        : { kind: "IRVarPattern", name: binding.name, span: binding.span };

    // Wrap result in a lambda and apply the binding value
    const lambda: IRExpr = {
      kind: "IRLambda",
      params: [pattern],
      body: result,
      span: expr.span,
    };

    // If binding has args, it's a function - wrap value in lambda
    let valueExpr: IRExpr;
    if (binding.args.length > 0) {
      valueExpr = {
        kind: "IRLambda",
        params: binding.args.map((p) => lowerPattern(p, ctx)),
        body: bindingValue,
        span: binding.span,
      };
    } else {
      valueExpr = bindingValue;
    }

    result = {
      kind: "IRApply",
      callee: lambda,
      args: [valueExpr],
      span: expr.span,
    };
  }

  return result;
}

/**
 * Lower the body of a value declaration.
 */
function lowerValueBody(decl: ValueDeclaration, ctx: LoweringContext): IRExpr {
  return lowerExpr(decl.body, ctx);
}

// ============================================================================
// Case Expression Lowering
// ============================================================================

/**
 * Lower a case expression.
 *
 * For Bool discrimination, convert to if-then-else:
 *   case b of
 *     True -> e1
 *     False -> e2
 * Becomes:
 *   if b then e1 else e2
 *
 * For other ADTs, keep as IRCase for codegen to handle with switch/match.
 */
function lowerCase(
  expr: Extract<Expr, { kind: "Case" }>,
  ctx: LoweringContext
): IRExpr {
  const discriminant = lowerExpr(expr.discriminant, ctx);
  const branches = expr.branches;

  // Check if this is a Bool case (can lower to if-then-else)
  if (isBoolCase(branches, ctx)) {
    return lowerBoolCase(discriminant, branches, ctx, expr.span);
  }

  // Check if all branches are literal patterns (primitives)
  if (allLiteralPatterns(branches)) {
    return lowerLiteralCase(discriminant, branches, ctx, expr.span);
  }

  // General ADT case - keep as IRCase
  return {
    kind: "IRCase",
    discriminant,
    branches: branches.map((b) => ({
      pattern: lowerPattern(b.pattern, ctx),
      body: lowerExpr(b.body, ctx),
      span: b.span,
    })),
    span: expr.span,
  };
}

/**
 * Check if a case expression is over Bool (True/False patterns only).
 * Must verify these are the built-in Bool constructors, not user-defined ones.
 */
function isBoolCase(
  branches: Array<{ pattern: Pattern; body: Expr; span: Span }>,
  ctx: LoweringContext
): boolean {
  if (branches.length !== 2) return false;

  // Check if we have True and False patterns
  const patterns = branches.map((b) => b.pattern);
  const truePattern = patterns.find(
    (p) =>
      p.kind === "ConstructorPattern" &&
      p.name === "True" &&
      p.args.length === 0
  );
  const falsePattern = patterns.find(
    (p) =>
      p.kind === "ConstructorPattern" &&
      p.name === "False" &&
      p.args.length === 0
  );

  if (!truePattern || !falsePattern) return false;

  // Verify these are the built-in Bool constructors (not user-defined)
  const trueCtorInfo = ctx.semantics.constructors["True"];
  const falseCtorInfo = ctx.semantics.constructors["False"];

  return (
    trueCtorInfo?.moduleName === BUILTIN_MODULE_NAME &&
    trueCtorInfo?.parentType === BOOL_TYPE_NAME &&
    falseCtorInfo?.moduleName === BUILTIN_MODULE_NAME &&
    falseCtorInfo?.parentType === BOOL_TYPE_NAME
  );
}

/**
 * Lower a Bool case to if-then-else.
 */
function lowerBoolCase(
  discriminant: IRExpr,
  branches: Array<{ pattern: Pattern; body: Expr; span: Span }>,
  ctx: LoweringContext,
  span: Span
): IRExpr {
  let trueBranch: IRExpr | null = null;
  let falseBranch: IRExpr | null = null;

  for (const branch of branches) {
    if (
      branch.pattern.kind === "ConstructorPattern" &&
      branch.pattern.name === "True"
    ) {
      trueBranch = lowerExpr(branch.body, ctx);
    } else if (
      branch.pattern.kind === "ConstructorPattern" &&
      branch.pattern.name === "False"
    ) {
      falseBranch = lowerExpr(branch.body, ctx);
    }
  }

  if (!trueBranch || !falseBranch) {
    throw new IRError("Bool case missing True or False branch", span);
  }

  return {
    kind: "IRIf",
    condition: discriminant,
    thenBranch: trueBranch,
    elseBranch: falseBranch,
    span,
  };
}

/**
 * Check if all branches have literal patterns.
 */
function allLiteralPatterns(
  branches: Array<{ pattern: Pattern; body: Expr; span: Span }>
): boolean {
  // We can't directly detect literal patterns from current AST
  // as literals in patterns would be ConstructorPattern (for True/False)
  // or VarPattern (which matches anything)
  // For now, return false - primitive cases handled separately
  return false;
}

/**
 * Lower a case on literal values to if-then-else chain.
 */
function lowerLiteralCase(
  discriminant: IRExpr,
  branches: Array<{ pattern: Pattern; body: Expr; span: Span }>,
  ctx: LoweringContext,
  span: Span
): IRExpr {
  // This would be implemented when we support literal patterns
  // For now, fall through to general case handling
  return {
    kind: "IRCase",
    discriminant,
    branches: branches.map((b) => ({
      pattern: lowerPattern(b.pattern, ctx),
      body: lowerExpr(b.body, ctx),
      span: b.span,
    })),
    span,
  };
}

// ============================================================================
// Record Update Desugaring
// ============================================================================

/**
 * Lower a record update expression.
 *
 * { r | x = 1, y = 2 } becomes:
 * { x = 1, y = 2, z = r.z, ... }  (copying all non-updated fields)
 *
 * This requires knowing the record type's fields, which we get from:
 * 1. Type aliases with record types
 * 2. Inferred types from semantic analysis
 */
function lowerRecordUpdate(
  expr: Extract<Expr, { kind: "RecordUpdate" }>,
  ctx: LoweringContext
): IRExpr {
  // For now, emit a simplified form that codegen can handle
  // The base is a variable name, and we need to copy unchanged fields

  // Look up the inferred type of the base variable to get all fields
  const baseType = ctx.semantics.types[expr.base];

  // Convert the base variable reference
  const baseVar: IRExpr = {
    kind: "IRVar",
    name: expr.base,
    namespace: "value",
    span: expr.span,
  };

  // If we can't determine the type, emit a special IR node that codegen handles
  // For now, we'll emit field accesses for all fields we need to copy
  const updatedFields = new Set(expr.fields.map((f) => f.name));

  // Try to get all field names from the type
  let allFields: string[] = [];
  if (baseType && baseType.kind === "record") {
    allFields = Object.keys(baseType.fields);
  }

  // Build the new record fields
  const irFields: IRRecordField[] = [];

  // First, add all unchanged fields
  for (const fieldName of allFields) {
    if (!updatedFields.has(fieldName)) {
      irFields.push({
        name: fieldName,
        value: {
          kind: "IRFieldAccess",
          target: baseVar,
          field: fieldName,
          span: expr.span,
        },
        span: expr.span,
      });
    }
  }

  // Then add updated fields
  for (const field of expr.fields) {
    irFields.push({
      name: field.name,
      value: lowerExpr(field.value, ctx),
      span: field.span,
    });
  }

  // If we couldn't determine all fields, use IRRecordUpdate with spread semantics
  // This lets codegen generate proper { ...base, updates... } syntax
  if (allFields.length === 0) {
    return {
      kind: "IRRecordUpdate",
      base: baseVar,
      updates: expr.fields.map((f) => ({
        name: f.name,
        value: lowerExpr(f.value, ctx),
        span: f.span,
      })),
      span: expr.span,
    };
  }

  return {
    kind: "IRRecord",
    fields: irFields,
    span: expr.span,
  };
}

// ============================================================================
// Infix Resolution
// ============================================================================

/**
 * Lower an infix expression to function application.
 *
 * a + b becomes: (+) a b
 * Or more precisely: apply (apply (+) a) b
 *
 * For short-circuit operators (&& and ||), the right operand is wrapped
 * in a thunk to enable true short-circuit evaluation:
 *
 * a && b becomes: (&&) a (() -> b)
 *
 * This allows the runtime to only evaluate b if a is True (for &&)
 * or False (for ||).
 */
function lowerInfix(
  expr: Extract<Expr, { kind: "Infix" }>,
  ctx: LoweringContext
): IRExpr {
  const left = lowerExpr(expr.left, ctx);
  const right = lowerExpr(expr.right, ctx);

  // The operator becomes a variable reference
  const opVar: IRExpr = {
    kind: "IRVar",
    name: expr.operator,
    namespace: "value",
    span: expr.span,
  };

  // For short-circuit operators, wrap the right operand in a thunk
  const rightArg: IRExpr = SHORT_CIRCUIT_OPERATORS.has(expr.operator)
    ? {
        kind: "IRLambda",
        params: [], // No parameters - this is a thunk () -> Bool
        body: right,
        span: expr.right.span,
      }
    : right;

  // Apply operator to left, then to right (curried)
  return {
    kind: "IRApply",
    callee: {
      kind: "IRApply",
      callee: opVar,
      args: [left],
      span: expr.span,
    },
    args: [rightArg],
    span: expr.span,
  };
}
