/**
 * @vibe/ir - Intermediate Representation for Vibe Compiler
 *
 * Main entry point that provides the `lower` function to transform
 * semantic analysis output into IR suitable for code generation.
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
export * from "./utils";
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

// Re-export main lower function and options
export { lower, type LowerOptions } from "./impl/lower";

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
  const depGraph = buildDependencyGraph(ir.values, ir.instances, ir.protocols, ir.constructors);
  const deps = depGraph.get(valueName);
  return deps?.has(dependencyName) ?? false;
}
