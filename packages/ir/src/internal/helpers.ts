/**
 * Internal helper functions for IR lowering.
 * These are not part of the public API.
 */

import type { Expr, TypeExpr } from "@vibe/syntax";
import type { IRType } from "../types";
import type { LoweringContext } from "../lowering";

/**
 * Format a type as a string key for synthetic value naming.
 * E.g., `Int` -> "Int", `List Int` -> "List_Int"
 */
export function formatTypeKey(type: IRType | undefined): string {
  if (!type) return "Unknown";
  switch (type.kind) {
    case "var":
      // Use lowercase 'v' prefix to match codegen's formatTypeKey for consistency
      return `v${type.id}`;
    case "con":
      if (type.args.length === 0) return type.name;
      return `${type.name}_${type.args.map(formatTypeKey).join("_")}`;
    case "fun":
      return `fn_${formatTypeKey(type.from)}_${formatTypeKey(type.to)}`;
    case "tuple":
      return `tuple_${type.elements.map(formatTypeKey).join("_")}`;
    case "list":
      return `list_${formatTypeKey(type.element)}`;
    case "record":
      return `record`;
  }
}

/**
 * Substitute protocol method references in an expression with concrete implementations.
 * This is used when lowering default implementations for specific instances.
 *
 * @param expr The expression to transform
 * @param methodSubstitutions Map from method name to concrete implementation expression
 * @returns A new expression with substitutions applied
 */
export function substituteProtocolMethods(
  expr: Expr,
  methodSubstitutions: Map<string, Expr>,
): Expr {
  switch (expr.kind) {
    case "Var":
      // Check if this variable is a protocol method that needs substitution
      if (methodSubstitutions.has(expr.name)) {
        return methodSubstitutions.get(expr.name)!;
      }
      return expr;

    case "Infix": {
      // Check if the operator is a protocol method
      if (methodSubstitutions.has(expr.operator)) {
        // Transform `x op y` to `(substitute op) x y`
        const newOp = methodSubstitutions.get(expr.operator)!;
        const left = substituteProtocolMethods(expr.left, methodSubstitutions);
        const right = substituteProtocolMethods(
          expr.right,
          methodSubstitutions,
        );
        return {
          kind: "Apply",
          callee: {
            kind: "Apply",
            callee: newOp,
            args: [left],
            span: expr.span,
          },
          args: [right],
          span: expr.span,
        };
      }
      return {
        kind: "Infix",
        left: substituteProtocolMethods(expr.left, methodSubstitutions),
        operator: expr.operator,
        right: substituteProtocolMethods(expr.right, methodSubstitutions),
        span: expr.span,
      };
    }

    case "Lambda":
      return {
        kind: "Lambda",
        args: expr.args,
        body: substituteProtocolMethods(expr.body, methodSubstitutions),
        span: expr.span,
      };

    case "Apply":
      return {
        kind: "Apply",
        callee: substituteProtocolMethods(expr.callee, methodSubstitutions),
        args: expr.args.map((arg) =>
          substituteProtocolMethods(arg, methodSubstitutions),
        ),
        span: expr.span,
      };

    case "If":
      return {
        kind: "If",
        condition: substituteProtocolMethods(
          expr.condition,
          methodSubstitutions,
        ),
        thenBranch: substituteProtocolMethods(
          expr.thenBranch,
          methodSubstitutions,
        ),
        elseBranch: substituteProtocolMethods(
          expr.elseBranch,
          methodSubstitutions,
        ),
        span: expr.span,
      };

    case "LetIn":
      return {
        kind: "LetIn",
        bindings: expr.bindings.map((b) => ({
          ...b,
          body: substituteProtocolMethods(b.body, methodSubstitutions),
        })),
        body: substituteProtocolMethods(expr.body, methodSubstitutions),
        span: expr.span,
      };

    case "Case":
      return {
        kind: "Case",
        discriminant: substituteProtocolMethods(
          expr.discriminant,
          methodSubstitutions,
        ),
        branches: expr.branches.map((branch) => ({
          ...branch,
          body: substituteProtocolMethods(branch.body, methodSubstitutions),
        })),
        span: expr.span,
      };

    case "Paren":
      return {
        kind: "Paren",
        expression: substituteProtocolMethods(
          expr.expression,
          methodSubstitutions,
        ),
        span: expr.span,
      };

    case "Tuple":
      return {
        kind: "Tuple",
        elements: expr.elements.map((e) =>
          substituteProtocolMethods(e, methodSubstitutions),
        ),
        span: expr.span,
      };

    case "List":
      return {
        kind: "List",
        elements: expr.elements.map((e) =>
          substituteProtocolMethods(e, methodSubstitutions),
        ),
        span: expr.span,
      };

    case "ListRange":
      return {
        kind: "ListRange",
        start: substituteProtocolMethods(expr.start, methodSubstitutions),
        end: substituteProtocolMethods(expr.end, methodSubstitutions),
        span: expr.span,
      };

    case "Record":
      return {
        kind: "Record",
        fields: expr.fields.map((f) => ({
          ...f,
          value: substituteProtocolMethods(f.value, methodSubstitutions),
        })),
        span: expr.span,
      };

    case "RecordUpdate":
      return {
        kind: "RecordUpdate",
        base: expr.base,
        fields: expr.fields.map((f) => ({
          ...f,
          value: substituteProtocolMethods(f.value, methodSubstitutions),
        })),
        span: expr.span,
      };

    case "FieldAccess":
      return {
        kind: "FieldAccess",
        target: substituteProtocolMethods(expr.target, methodSubstitutions),
        field: expr.field,
        span: expr.span,
      };

    // Literals don't need substitution
    case "Number":
    case "String":
    case "Char":
    case "Unit":
      return expr;

    case "Unary":
      return {
        kind: "Unary",
        operator: expr.operator,
        operand: substituteProtocolMethods(expr.operand, methodSubstitutions),
        span: expr.span,
      };
  }
}

/**
 * Convert a TypeExpr (from AST) to internal Type representation.
 * This is a simplified conversion that handles common cases.
 */
export function convertTypeExprToType(
  typeExpr: TypeExpr,
  ctx: LoweringContext,
): any {
  switch (typeExpr.kind) {
    case "TypeRef":
      if (typeExpr.args.length === 0) {
        // Check if it's a type variable (lowercase)
        const firstChar = typeExpr.name[0];
        if (firstChar && firstChar === firstChar.toLowerCase()) {
          return { kind: "var", id: typeExpr.name.charCodeAt(0) };
        }
        return { kind: "con", name: typeExpr.name, args: [] };
      }
      return {
        kind: "con",
        name: typeExpr.name,
        args: typeExpr.args.map((a) => convertTypeExprToType(a, ctx)),
      };

    case "FunctionType":
      return {
        kind: "fun",
        from: convertTypeExprToType(typeExpr.from, ctx),
        to: convertTypeExprToType(typeExpr.to, ctx),
      };

    case "TupleType":
      return {
        kind: "tuple",
        elements: typeExpr.elements.map((e) => convertTypeExprToType(e, ctx)),
      };

    case "RecordType":
      const fields: Record<string, any> = {};
      for (const field of typeExpr.fields) {
        fields[field.name] = convertTypeExprToType(field.type, ctx);
      }
      return { kind: "record", fields };

    case "QualifiedType":
      // For qualified types, return the underlying type
      // Constraints are handled separately
      return convertTypeExprToType(typeExpr.type, ctx);

    default:
      return { kind: "var", id: -1 };
  }
}

/**
 * Convert a TypeExpr directly to IRType (without going through internal Type).
 */
export function convertTypeExprToIRType(typeExpr: TypeExpr): IRType {
  switch (typeExpr.kind) {
    case "TypeRef":
      if (typeExpr.args.length === 0) {
        const firstChar = typeExpr.name[0];
        if (firstChar && firstChar === firstChar.toLowerCase()) {
          return { kind: "var", id: typeExpr.name.charCodeAt(0) };
        }
        return { kind: "con", name: typeExpr.name, args: [] };
      }
      return {
        kind: "con",
        name: typeExpr.name,
        args: typeExpr.args.map(convertTypeExprToIRType),
      };

    case "FunctionType":
      return {
        kind: "fun",
        from: convertTypeExprToIRType(typeExpr.from),
        to: convertTypeExprToIRType(typeExpr.to),
      };

    case "TupleType":
      return {
        kind: "tuple",
        elements: typeExpr.elements.map(convertTypeExprToIRType),
      };

    case "RecordType":
      const fields: Record<string, IRType> = {};
      for (const field of typeExpr.fields) {
        fields[field.name] = convertTypeExprToIRType(field.type);
      }
      return { kind: "record", fields };

    case "QualifiedType":
      return convertTypeExprToIRType(typeExpr.type);

    default:
      return { kind: "var", id: -1 };
  }
}
