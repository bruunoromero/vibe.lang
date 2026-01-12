/**
 * Dependency Analysis with Strongly Connected Components (SCC)
 *
 * This module implements Tarjan's algorithm for finding SCCs in the dependency
 * graph of value declarations. This is essential for:
 *
 * 1. Proper initialization order: Values must be defined before use
 * 2. Mutual recursion detection: Values in the same SCC are mutually recursive
 * 3. Code generation: Mutually recursive values need special handling (letrec)
 *
 * The algorithm runs in O(V + E) where V is values and E is dependencies.
 */

import type { IRExpr, IRPattern, IRValue, SCC } from "./types";

/**
 * Build a dependency graph from IR values.
 * Returns an adjacency list: Map<valueName, Set<dependencyNames>>
 */
export function buildDependencyGraph(
  values: Record<string, IRValue>
): Map<string, Set<string>> {
  const graph = new Map<string, Set<string>>();
  const valueNames = new Set(Object.keys(values));

  for (const [name, value] of Object.entries(values)) {
    const deps = new Set<string>();
    collectDependencies(value.body, deps, valueNames);

    // Also check params for pattern bindings (though rare to have deps there)
    for (const param of value.params) {
      collectPatternDependencies(param, deps, valueNames);
    }

    graph.set(name, deps);
  }

  return graph;
}

/**
 * Recursively collect free variable references from an IR expression.
 * Only includes names that are in the valueNames set (top-level values).
 */
function collectDependencies(
  expr: IRExpr,
  deps: Set<string>,
  valueNames: Set<string>
): void {
  switch (expr.kind) {
    case "IRVar":
      if (expr.namespace === "value" && valueNames.has(expr.name)) {
        deps.add(expr.name);
      }
      break;

    case "IRLiteral":
    case "IRUnit":
      // No dependencies
      break;

    case "IRLambda":
      // Lambda params shadow outer names, but we still traverse body
      // (proper shadowing would require scope tracking, but for top-level
      // dependency analysis, it's safe to over-approximate)
      collectDependencies(expr.body, deps, valueNames);
      break;

    case "IRApply":
      collectDependencies(expr.callee, deps, valueNames);
      for (const arg of expr.args) {
        collectDependencies(arg, deps, valueNames);
      }
      break;

    case "IRIf":
      collectDependencies(expr.condition, deps, valueNames);
      collectDependencies(expr.thenBranch, deps, valueNames);
      collectDependencies(expr.elseBranch, deps, valueNames);
      break;

    case "IRCase":
      collectDependencies(expr.discriminant, deps, valueNames);
      for (const branch of expr.branches) {
        collectDependencies(branch.body, deps, valueNames);
      }
      break;

    case "IRTuple":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, valueNames);
      }
      break;

    case "IRList":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, valueNames);
      }
      break;

    case "IRRecord":
      for (const field of expr.fields) {
        collectDependencies(field.value, deps, valueNames);
      }
      break;

    case "IRFieldAccess":
      collectDependencies(expr.target, deps, valueNames);
      break;

    case "IRConstructor":
      for (const arg of expr.args) {
        collectDependencies(arg, deps, valueNames);
      }
      break;
  }
}

/**
 * Collect dependencies from patterns (for completeness).
 */
function collectPatternDependencies(
  pattern: IRPattern,
  deps: Set<string>,
  valueNames: Set<string>
): void {
  switch (pattern.kind) {
    case "IRVarPattern":
    case "IRWildcardPattern":
    case "IRLiteralPattern":
      // No dependencies in patterns
      break;

    case "IRConstructorPattern":
      for (const arg of pattern.args) {
        collectPatternDependencies(arg, deps, valueNames);
      }
      break;

    case "IRTuplePattern":
      for (const elem of pattern.elements) {
        collectPatternDependencies(elem, deps, valueNames);
      }
      break;
  }
}

/**
 * Tarjan's algorithm state
 */
type TarjanState = {
  index: number;
  stack: string[];
  onStack: Set<string>;
  indices: Map<string, number>;
  lowlinks: Map<string, number>;
  sccs: string[][];
};

/**
 * Find strongly connected components using Tarjan's algorithm.
 *
 * Returns SCCs in reverse topological order (dependencies first).
 * Within each SCC, values are ordered by their original declaration order.
 */
export function findSCCs(
  graph: Map<string, Set<string>>,
  declarationOrder: string[]
): SCC[] {
  const state: TarjanState = {
    index: 0,
    stack: [],
    onStack: new Set(),
    indices: new Map(),
    lowlinks: new Map(),
    sccs: [],
  };

  // Process nodes in declaration order for deterministic output
  for (const name of declarationOrder) {
    if (!state.indices.has(name)) {
      strongconnect(name, graph, state);
    }
  }

  // Convert raw SCCs to typed SCCs with metadata
  // Tarjan produces SCCs in reverse topological order, which is what we want
  // (leaves first, roots last)
  const result: SCC[] = [];

  for (const scc of state.sccs) {
    // Sort values within SCC by declaration order
    const orderMap = new Map(declarationOrder.map((name, idx) => [name, idx]));
    const sortedValues = [...scc].sort((a, b) => {
      const aIdx = orderMap.get(a) ?? Infinity;
      const bIdx = orderMap.get(b) ?? Infinity;
      return aIdx - bIdx;
    });

    result.push({
      values: sortedValues,
      isMutuallyRecursive: scc.length > 1,
    });
  }

  return result;
}

/**
 * Tarjan's strongconnect function
 */
function strongconnect(
  v: string,
  graph: Map<string, Set<string>>,
  state: TarjanState
): void {
  // Set the depth index for v to the smallest unused index
  state.indices.set(v, state.index);
  state.lowlinks.set(v, state.index);
  state.index++;
  state.stack.push(v);
  state.onStack.add(v);

  // Consider successors of v
  const successors = graph.get(v) ?? new Set();
  for (const w of successors) {
    if (!state.indices.has(w)) {
      // Successor w has not yet been visited; recurse on it
      strongconnect(w, graph, state);
      state.lowlinks.set(
        v,
        Math.min(state.lowlinks.get(v)!, state.lowlinks.get(w)!)
      );
    } else if (state.onStack.has(w)) {
      // Successor w is in stack and hence in the current SCC
      state.lowlinks.set(
        v,
        Math.min(state.lowlinks.get(v)!, state.indices.get(w)!)
      );
    }
  }

  // If v is a root node, pop the stack and generate an SCC
  if (state.lowlinks.get(v) === state.indices.get(v)) {
    const scc: string[] = [];
    let w: string;
    do {
      w = state.stack.pop()!;
      state.onStack.delete(w);
      scc.push(w);
    } while (w !== v);

    state.sccs.push(scc);
  }
}

/**
 * Verify that the SCC ordering is a valid topological sort.
 * Used for testing and debugging.
 */
export function validateTopologicalOrder(
  sccs: SCC[],
  graph: Map<string, Set<string>>
): { valid: boolean; errors: string[] } {
  const errors: string[] = [];
  const emitted = new Set<string>();

  for (const scc of sccs) {
    // Check that all dependencies of this SCC are already emitted
    // (except for dependencies within the same SCC)
    const sccSet = new Set(scc.values);

    for (const name of scc.values) {
      const deps = graph.get(name) ?? new Set();
      for (const dep of deps) {
        if (!sccSet.has(dep) && !emitted.has(dep)) {
          errors.push(
            `Value '${name}' depends on '${dep}' which has not been emitted yet`
          );
        }
      }
    }

    // Mark all values in this SCC as emitted
    for (const name of scc.values) {
      emitted.add(name);
    }
  }

  return { valid: errors.length === 0, errors };
}
