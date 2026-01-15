/**
 * IR Lowering Implementation
 *
 * Main orchestration logic for lowering a semantic module to IR.
 */

import type { Program, Expr } from "@vibe/syntax";
import { sanitizeOperator } from "@vibe/syntax";
import type { SemanticModule } from "@vibe/semantics";
import type {
  IRProgram,
  IRValue,
  IRProtocol,
  IRInstance,
  IRConstructorInfo,
  IRConstraint,
  IRExpr,
  IRType,
  IRPattern,
} from "../types";
import {
  createLoweringContext,
  lowerExpr,
  lowerPattern,
  convertType,
  convertConstraints,
} from "../lowering";
import {
  buildDependencyGraph,
  findSCCs,
  validateTopologicalOrder,
} from "../dependency";
import { formatTypeKey, substituteProtocolMethods } from "../internal/helpers";

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
        kind: "IRUnit" as const,
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
    // Convert superclass constraints
    const superclassConstraints: IRConstraint[] =
      proto.superclassConstraints.map((c) => ({
        protocolName: c.protocolName,
        typeArgs: c.typeArgs.map((t) => convertType(t)),
      }));
    protocols[name] = {
      name: proto.name,
      params: proto.params,
      superclassConstraints,
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
        // Lambda expressions need to be lowered and turned into synthetic top-level values.
        // Determine if this is an explicit inline implementation or an inherited default:
        // - Explicit: `implement Show A where toString a = showA a` -> use $impl_ prefix
        // - Default: method inherited from protocol's default impl -> use $default_ prefix
        const isExplicit = inst.explicitMethods.has(methodName);
        const typeKey =
          inst.typeArgs.length > 0
            ? formatTypeKey(convertType(inst.typeArgs[0]))
            : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const prefix = isExplicit ? "$impl" : "$default";
        const syntheticName = `${prefix}_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;

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
        // Pass instance constraints so the method can accept dictionary parameters
        const instanceConstraints = inst.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => convertType(t)),
        }));
        const syntheticValue: IRValue = {
          name: syntheticName,
          params: [], // Lambda is in the body
          body: irLambda,
          type: methodType,
          constraints: instanceConstraints,
          isExternal: false,
          span: methodExpr.span,
        };

        syntheticValues[syntheticName] = syntheticValue;
        methodsObj[methodName] = syntheticName;
      } else {
        // For other expressions (literals, etc.), create a synthetic value
        // containing the lowered expression
        const typeKey =
          inst.typeArgs.length > 0
            ? formatTypeKey(convertType(inst.typeArgs[0]))
            : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const syntheticName = `$impl_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;

        // Lower the expression to IR
        const irExpr = lowerExpr(methodExpr, ctx);

        // Get the method type from the protocol if available
        const protocol = semantics.protocols[inst.protocolName];
        const methodInfo = protocol?.methods.get(methodName);
        const methodType = methodInfo?.type
          ? convertType(methodInfo.type)
          : { kind: "var" as const, id: -1 };

        // Create a synthetic IRValue for this implementation
        // Pass instance constraints so the method can accept dictionary parameters
        const instanceConstraints = inst.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => convertType(t)),
        }));
        const syntheticValue: IRValue = {
          name: syntheticName,
          params: [],
          body: irExpr,
          type: methodType,
          constraints: instanceConstraints,
          isExternal: false,
          span: methodExpr.span,
        };

        syntheticValues[syntheticName] = syntheticValue;
        methodsObj[methodName] = syntheticName;
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
  const importAliases: import("../types").IRImportAlias[] = [];
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
