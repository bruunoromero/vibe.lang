/**
 * @vibe/ir - Intermediate Representation for Vibe Compiler
 *
 * Main entry point that provides the `lower` function to transform
 * semantic analysis output into IR suitable for code generation.
 */

import type { Program } from "@vibe/syntax";
import { sanitizeOperator } from "@vibe/syntax";
import type { SemanticModule } from "@vibe/semantics";
import type {
  IRProgram,
  IRValue,
  IRProtocol,
  IRInstance,
  IRConstructorInfo,
  IRConstraint,
  IRType,
  IRExpr,
  IRPattern,
  SCC,
} from "./types";
import {
  createLoweringContext,
  lowerExpr,
  lowerPattern,
  convertType,
  convertConstraints,
  type LoweringContext,
} from "./lowering";
import {
  buildDependencyGraph,
  findSCCs,
  validateTopologicalOrder,
} from "./dependency";

// Re-export all types
export * from "./types";
export {
  buildDependencyGraph,
  findSCCs,
  validateTopologicalOrder,
} from "./dependency";
export {
  lowerExpr,
  lowerPattern,
  convertType,
  convertConstraints,
  createLoweringContext,
} from "./lowering";
export {
  printProgram,
  printValue,
  printExpr,
  printPattern,
  printType,
  printConstraint,
  printSCC,
  printDependencyOrder,
  printExprCompact,
  type PrettyPrintOptions,
} from "./printer";

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Format a type as a string key for synthetic value naming.
 * E.g., `Int` -> "Int", `List Int` -> "List_Int"
 */
function formatTypeKey(type: IRType | undefined): string {
  if (!type) return "Unknown";
  switch (type.kind) {
    case "var":
      return `Var${type.id}`;
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

import type { Expr } from "@vibe/syntax";

/**
 * Substitute protocol method references in an expression with concrete implementations.
 * This is used when lowering default implementations for specific instances.
 *
 * @param expr The expression to transform
 * @param methodSubstitutions Map from method name to concrete implementation expression
 * @returns A new expression with substitutions applied
 */
function substituteProtocolMethods(
  expr: Expr,
  methodSubstitutions: Map<string, Expr>
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
          methodSubstitutions
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
          substituteProtocolMethods(arg, methodSubstitutions)
        ),
        span: expr.span,
      };

    case "If":
      return {
        kind: "If",
        condition: substituteProtocolMethods(
          expr.condition,
          methodSubstitutions
        ),
        thenBranch: substituteProtocolMethods(
          expr.thenBranch,
          methodSubstitutions
        ),
        elseBranch: substituteProtocolMethods(
          expr.elseBranch,
          methodSubstitutions
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
          methodSubstitutions
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
          methodSubstitutions
        ),
        span: expr.span,
      };

    case "Tuple":
      return {
        kind: "Tuple",
        elements: expr.elements.map((e) =>
          substituteProtocolMethods(e, methodSubstitutions)
        ),
        span: expr.span,
      };

    case "List":
      return {
        kind: "List",
        elements: expr.elements.map((e) =>
          substituteProtocolMethods(e, methodSubstitutions)
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

// ============================================================================
// Main Lower Function
// ============================================================================

/**
 * Options for IR lowering.
 */
export interface LowerOptions {
  /**
   * Whether to validate the dependency order after computation.
   * Useful for debugging, but adds overhead.
   * @default false
   */
  validateDependencies?: boolean;

  /**
   * Package name this module belongs to.
   * Used for generating correct import paths.
   */
  packageName?: string;

  /**
   * Pre-analyzed dependency modules for resolving module-qualified accesses.
   * Maps module name to its semantic analysis result.
   */
  dependencies?: Map<string, SemanticModule>;
}

/**
 * Lower a semantic module to IR form.
 *
 * This is the main entry point for the IR phase. It:
 * 1. Lowers all value declarations to IR expressions
 * 2. Computes dependency order using SCC analysis
 * 3. Extracts protocol and constraint metadata
 * 4. Prepares constructor info for runtime dispatch
 *
 * @param program The original AST program
 * @param semantics The semantic analysis result
 * @param options Optional configuration
 * @returns The lowered IR program
 */
export function lower(
  program: Program,
  semantics: SemanticModule,
  options: LowerOptions = {}
): IRProgram {
  const imports = program.imports || [];
  const dependencies =
    options.dependencies || new Map<string, SemanticModule>();
  const ctx = createLoweringContext(semantics, imports, dependencies);

  // Lower all value declarations
  const values: Record<string, IRValue> = {};
  const declarationOrder: string[] = [];

  for (const [name, valueInfo] of Object.entries(semantics.values)) {
    declarationOrder.push(name);

    const decl = valueInfo.declaration;
    const type = semantics.types[name];

    // Check if this is an external declaration (no body)
    const isExternal = decl.kind === "ExternalDeclaration";

    // Lower the expression body (external declarations have no body)
    let body: IRExpr;
    let params: IRPattern[] = [];

    if (isExternal) {
      // External declarations have no body - create a placeholder
      body = {
        kind: "IRUnit",
        span: decl.span,
      };
    } else {
      // Regular value declaration
      body = lowerExpr(decl.body, ctx);
      params = decl.args.map((p) => lowerPattern(p, ctx));
    }

    // Convert type and constraints
    const irType = type ? convertType(type) : { kind: "var" as const, id: -1 };

    // Extract constraints from the value's type scheme for dictionary-passing
    const typeScheme = semantics.typeSchemes[name];
    const constraints: IRConstraint[] = typeScheme
      ? convertConstraints(typeScheme.constraints)
      : [];

    const irValue: IRValue = {
      name,
      params,
      body,
      type: irType,
      constraints,
      isExternal,
      externalTarget: isExternal
        ? {
            modulePath: (decl as import("@vibe/syntax").ExternalDeclaration)
              .target.modulePath,
            exportName: (decl as import("@vibe/syntax").ExternalDeclaration)
              .target.exportName,
          }
        : undefined,
      span: decl.span,
    };

    values[name] = irValue;
  }

  // Build dependency graph and find SCCs
  const depGraph = buildDependencyGraph(values);
  const sccs = findSCCs(depGraph, declarationOrder);

  // Validate if requested
  if (options.validateDependencies) {
    const validation = validateTopologicalOrder(sccs, depGraph);
    if (!validation.valid) {
      console.warn("Dependency order validation failed:", validation.errors);
    }
  }

  // Extract constructor info with tags
  const constructors: Record<string, IRConstructorInfo> = {};
  for (const [name, info] of Object.entries(semantics.constructors)) {
    constructors[name] = {
      name,
      parentType: info.parentType,
      arity: info.arity,
      tag: ctx.constructorTags.get(name) ?? 0,
      moduleName: info.moduleName,
    };
  }

  // Extract protocol metadata
  const protocols: Record<string, IRProtocol> = {};
  for (const [name, proto] of Object.entries(semantics.protocols)) {
    const methods: Array<{ name: string; type: IRType; hasDefault: boolean }> =
      [];
    // proto.methods is a Map
    for (const [methodName, methodInfo] of proto.methods) {
      methods.push({
        name: methodName,
        type: methodInfo.type
          ? convertType(methodInfo.type)
          : { kind: "var", id: -1 },
        hasDefault: methodInfo.defaultImpl !== undefined,
      });
    }
    protocols[name] = {
      name: proto.name,
      params: proto.params,
      methods,
    };
  }

  // Track synthetic values created for default implementations
  const syntheticValues: Record<string, IRValue> = {};

  // Extract instance metadata
  const instances: IRInstance[] = semantics.instances.map((inst) => {
    // inst.methods is a Map<string, Expr>
    const methodsObj: Record<string, string> = {};
    for (const [methodName, methodExpr] of inst.methods) {
      // Get the implementation name from the expression
      if (methodExpr.kind === "Var") {
        methodsObj[methodName] = methodExpr.name;
      } else if (methodExpr.kind === "FieldAccess") {
        // Handle module-qualified access like Int.add
        // Build the full path by traversing nested FieldAccess
        const parts: string[] = [methodExpr.field];
        let current = methodExpr.target;
        while (current.kind === "FieldAccess") {
          parts.unshift(current.field);
          current = current.target;
        }
        if (current.kind === "Var") {
          parts.unshift(current.name);
        }
        methodsObj[methodName] = parts.join(".");
      } else if (methodExpr.kind === "Lambda") {
        // Lambda expressions from default implementations need to be lowered
        // and turned into synthetic top-level values.
        // Generate a unique name for this default implementation.
        // Sanitize the method name to ensure it's a valid JS identifier.
        const typeKey =
          inst.typeArgs.length > 0
            ? formatTypeKey(convertType(inst.typeArgs[0]))
            : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const syntheticName = `$default_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;

        // Build method substitutions for protocol methods used in the body.
        // For each method in this instance that has a concrete implementation,
        // we substitute references to that method with its implementation.
        const methodSubstitutions = new Map<string, Expr>();
        for (const [otherMethodName, otherMethodExpr] of inst.methods) {
          // Only substitute non-default implementations (Var or FieldAccess)
          if (
            otherMethodExpr.kind === "Var" ||
            otherMethodExpr.kind === "FieldAccess"
          ) {
            methodSubstitutions.set(otherMethodName, otherMethodExpr);
          }
        }

        // Apply substitutions to the lambda expression before lowering
        const substitutedExpr = substituteProtocolMethods(
          methodExpr,
          methodSubstitutions
        );

        // Lower the substituted lambda to IR
        const irLambda = lowerExpr(substitutedExpr, ctx);

        // Get the method type from the protocol if available
        const protocol = semantics.protocols[inst.protocolName];
        const methodInfo = protocol?.methods.get(methodName);
        const methodType = methodInfo?.type
          ? convertType(methodInfo.type)
          : { kind: "var" as const, id: -1 };

        // Create a synthetic IRValue for this default implementation
        const syntheticValue: IRValue = {
          name: syntheticName,
          params: [], // Lambda is in the body
          body: irLambda,
          type: methodType,
          constraints: [],
          isExternal: false,
          span: methodExpr.span,
        };

        syntheticValues[syntheticName] = syntheticValue;
        methodsObj[methodName] = syntheticName;
      } else {
        // For other complex expressions, use a placeholder
        methodsObj[methodName] = `<expr:${methodExpr.kind}>`;
      }
    }
    return {
      protocolName: inst.protocolName,
      typeArgs: inst.typeArgs.map((t) => convertType(t)),
      constraints: inst.constraints.map((c) => ({
        protocolName: c.protocolName,
        typeArgs: c.typeArgs.map((t) => convertType(t)),
      })),
      methods: methodsObj,
    };
  });

  // Convert synthetic values object to array
  const syntheticDefaultImpls = Object.values(syntheticValues);

  // Build constraint metadata map
  const constraintMetadata = new Map<string, IRConstraint[]>();
  for (const [name, value] of Object.entries(values)) {
    if (value.constraints.length > 0) {
      constraintMetadata.set(name, value.constraints);
    }
  }

  // Collect external imports
  const externalImports = new Set<string>();
  for (const value of Object.values(values)) {
    if (value.isExternal && value.externalTarget) {
      externalImports.add(value.externalTarget.modulePath);
    }
  }

  // Build import alias mappings from program imports
  const importAliases: import("./types").IRImportAlias[] = [];
  for (const imp of imports) {
    // Use explicit alias if provided, otherwise use last segment of module name
    const alias = imp.alias || imp.moduleName.split(".").pop()!;
    importAliases.push({
      alias,
      moduleName: imp.moduleName,
    });
  }

  return {
    moduleName: semantics.module?.name,
    values,
    dependencyOrder: sccs,
    liftedBindings: ctx.liftedBindings,
    syntheticDefaultImpls,
    adts: semantics.adts,
    constructors,
    protocols,
    instances,
    constraintMetadata,
    externalImports,
    importAliases,
    sourceModule: semantics,
    sourceProgram: program,
    packageName: options.packageName,
    exports: semantics.exports,
  };
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Convert a TypeExpr (from AST) to internal Type representation.
 * This is a simplified conversion that handles common cases.
 */
function convertTypeExprToType(
  typeExpr: import("@vibe/syntax").TypeExpr,
  ctx: LoweringContext
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
function convertTypeExprToIRType(
  typeExpr: import("@vibe/syntax").TypeExpr
): IRType {
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

// ============================================================================
// IR Inspection Utilities
// ============================================================================

/**
 * Get statistics about an IR program.
 */
export function getIRStats(ir: IRProgram): {
  valueCount: number;
  sccCount: number;
  mutualRecursionGroups: number;
  liftedBindingCount: number;
  externalImportCount: number;
  protocolCount: number;
  instanceCount: number;
} {
  return {
    valueCount: Object.keys(ir.values).length,
    sccCount: ir.dependencyOrder.length,
    mutualRecursionGroups: ir.dependencyOrder.filter(
      (s) => s.isMutuallyRecursive
    ).length,
    liftedBindingCount: ir.liftedBindings.length,
    externalImportCount: ir.externalImports.size,
    protocolCount: Object.keys(ir.protocols).length,
    instanceCount: ir.instances.length,
  };
}

/**
 * Check if a value depends on another value.
 */
export function dependsOn(
  ir: IRProgram,
  valueName: string,
  dependencyName: string
): boolean {
  const depGraph = buildDependencyGraph(ir.values);
  const deps = depGraph.get(valueName);
  return deps?.has(dependencyName) ?? false;
}
