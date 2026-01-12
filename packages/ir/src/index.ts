/**
 * @vibe/ir - Intermediate Representation for Vibe Compiler
 *
 * Main entry point that provides the `lower` function to transform
 * semantic analysis output into IR suitable for code generation.
 */

import type { Program } from "@vibe/syntax";
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
  const ctx = createLoweringContext(semantics);

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

  // Extract instance metadata
  const instances: IRInstance[] = semantics.instances.map((inst) => {
    // inst.methods is a Map<string, Expr>
    const methodsObj: Record<string, string> = {};
    for (const [methodName, methodExpr] of inst.methods) {
      // Get the implementation name from the expression
      if (methodExpr.kind === "Var") {
        methodsObj[methodName] = methodExpr.name;
      } else {
        // For complex expressions, use a placeholder
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

  return {
    moduleName: semantics.module?.name,
    values,
    dependencyOrder: sccs,
    liftedBindings: ctx.liftedBindings,
    adts: semantics.adts,
    constructors,
    protocols,
    instances,
    constraintMetadata,
    externalImports,
    sourceModule: semantics,
    sourceProgram: program,
    packageName: options.packageName,
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
