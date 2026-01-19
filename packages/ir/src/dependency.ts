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

import type { IRExpr, IRPattern, IRValue, SCC, IRProgram, IRType, IRInstance, IRProtocol, IRConstructorInfo } from "./types";
import { formatTypeKey, findMatchingInstance, findPolymorphicInstance } from "./utils";

/**
 * Build a dependency graph from IR values.
 * Returns an adjacency list: Map<valueName, Set<dependencyNames>>
 */
export function buildDependencyGraph(
  values: Record<string, IRValue>,
  instances: IRInstance[],
  protocols: Record<string, IRProtocol> = {},
  constructors: Record<string, IRConstructorInfo> = {}
): Map<string, Set<string>> {
  const graph = new Map<string, Set<string>>();
  
  // Set of all available top-level names (values and instances)
  const availableNames = new Set(Object.keys(values));
  
  // Helper map to identify protocol methods
  // mapping: method name -> protocol name
  const protocolMethodMap = new Map<string, string>();
  for (const protocol of Object.values(protocols)) {
    for (const method of protocol.methods) {
      protocolMethodMap.set(method.name, protocol.name);
    }
  }
  
  // Add instance names to available names
  for (const inst of instances) {
    if (!inst.typeArgs[0]) continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const instName = `$dict_${inst.protocolName}_${typeKey}`;
    availableNames.add(instName);
  }

  // 1. Collect dependencies for values
  for (const [name, value] of Object.entries(values)) {
    const deps = new Set<string>();
    collectDependencies(value.body, deps, availableNames, instances, protocolMethodMap, constructors);

    // Also check params for pattern bindings (though rare to have deps there)
    for (const param of value.params) {
      collectPatternDependencies(param, deps, availableNames);
    }

    graph.set(name, deps);
  }

  // 2. Collect dependencies for instances
  for (const inst of instances) {
    if (!inst.typeArgs[0]) continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const instName = `$dict_${inst.protocolName}_${typeKey}`;
    const deps = new Set<string>();

    // Dependencies on method implementations
    for (const implName of Object.values(inst.methods)) {
      if (availableNames.has(implName)) {
        deps.add(implName);
      }
    }

    // Dependencies on constrained dictionaries (e.g. Eq a => Set a)
    for (const constraint of inst.constraints) {
       const typeArg = constraint.typeArgs[0];
       if (typeArg && typeArg.kind !== "var") {
           const constraintKey = formatTypeKey(typeArg);
           const constraintDict = `$dict_${constraint.protocolName}_${constraintKey}`;
           if (availableNames.has(constraintDict)) {
               deps.add(constraintDict);
           }
       }
    }

    graph.set(instName, deps);
  }

  return graph;
}

/**
 * Recursively collect free variable references from an IR expression.
 * Only includes names that are in the availableNames set (top-level values).
 */
function collectDependencies(
  expr: IRExpr,
  deps: Set<string>,
  availableNames: Set<string>,
  instances: IRInstance[],
  protocolMethodMap: Map<string, string>,
  constructors: Record<string, IRConstructorInfo>
): void {
  switch (expr.kind) {
    case "IRVar":
      if (expr.namespace === "value") {
        if (availableNames.has(expr.name)) {
          deps.add(expr.name);
        }
        
        // Check for protocol constraint dependency
        if (expr.constraint) {
          // Resolve the dictionary this constraint points to
          // If it's a concrete type, we need the specific instance dictionary
          const typeArg = expr.constraint.typeArgs[0];
          if (typeArg && typeArg.kind !== "var") {
             // Resolve dictionary for this type
             const dictName = resolveDictionaryName(expr.constraint.protocolName, typeArg, instances);
             if (dictName && availableNames.has(dictName)) {
               deps.add(dictName);
             }
          }
        }
      }
      break;

    case "IRLiteral":
    case "IRUnit":
      // No dependencies
      break;

    case "IRLambda":
      // Lambda params shadow outer names, but we still traverse body
      collectDependencies(expr.body, deps, availableNames, instances, protocolMethodMap, constructors);
      break;

    case "IRApply":
      // Check for protocol method application
      // form: method arg1 arg2 ...
      // If we can identify 'method' as a protocol method, try to infer the dictionary from arg1
      if (expr.callee.kind === "IRVar" && protocolMethodMap.has(expr.callee.name)) {
         const protocolName = protocolMethodMap.get(expr.callee.name);
         if (protocolName && expr.args.length > 0) {
            const firstArg = expr.args[0]!;
            let inferenceType: IRType | undefined;

            // Try to infer type from literal
            if (firstArg.kind === "IRLiteral") {
               const litType = firstArg.literalType;
               // Map literal type to Vibe type name
               const typeName = litType.charAt(0).toUpperCase() + litType.slice(1); // "Int", "Float", "String"
               if (typeName === "Int" || typeName === "Float" || typeName === "String" || typeName === "Bool") {
                   inferenceType = { kind: "con", name: typeName, args: [] };
               } else if (litType === "char") {
                   inferenceType = { kind: "con", name: "Char", args: [] };
               }
            }
            // If IRVar, check if it has type info attached
            else if (firstArg.kind === "IRVar" && firstArg.type) {
                inferenceType = firstArg.type;
            }
            // Handle Constructor (e.g. A == A)
            else if (firstArg.kind === "IRConstructor") {
                const ctorInfo = constructors[firstArg.name];
                if (ctorInfo) {
                    inferenceType = { kind: "con", name: ctorInfo.parentType, args: [] };
                }
            }
            
            if (inferenceType) {
                const dictName = resolveDictionaryName(protocolName, inferenceType, instances);
                if (dictName && availableNames.has(dictName)) {
                    deps.add(dictName);
                }
            }
         }
      }

      collectDependencies(expr.callee, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;

    case "IRIf":
      collectDependencies(expr.condition, deps, availableNames, instances, protocolMethodMap, constructors);
      collectDependencies(expr.thenBranch, deps, availableNames, instances, protocolMethodMap, constructors);
      collectDependencies(expr.elseBranch, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
      
    case "IRCase":
      collectDependencies(expr.discriminant, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const branch of expr.branches) {
        collectDependencies(branch.body, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;

    case "IRTuple":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;

    case "IRList":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;

    case "IRRecord":
      for (const field of expr.fields) {
        collectDependencies(field.value, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
      
    case "IRRecordUpdate":
      collectDependencies(expr.base, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const field of expr.updates) {
        collectDependencies(field.value, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;

    case "IRFieldAccess":
      collectDependencies(expr.target, deps, availableNames, instances, protocolMethodMap, constructors);
      break;

    case "IRConstructor":
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
  }
}

/**
 * Resolve the dictionary name for a protocol and type.
 */
function resolveDictionaryName(
  protocolName: string, 
  type: IRType, 
  instances: IRInstance[]
): string | null {
  // Try exact match first
  const typeKey = formatTypeKey(type);
  const exactKey = `${protocolName}_${typeKey}`;
  
  // Check exact/structural match
  const match = findMatchingInstance(protocolName, type, instances);
  if (match) {
    return `$dict_${match.key}`;
  }
  
  // Check polymorphic match
  const polyMatch = findPolymorphicInstance(protocolName, instances);
  if (polyMatch) {
    return `$dict_${polyMatch.key}`;
  }
  
  return `$dict_${exactKey}`; // Default guess
}

/**
 * Collect dependencies from patterns (for completeness).
 */
function collectPatternDependencies(
  pattern: IRPattern,
  deps: Set<string>,
  availableNames: Set<string>
): void {
  switch (pattern.kind) {
    case "IRVarPattern":
    case "IRWildcardPattern":
    case "IRLiteralPattern":
      // No dependencies in patterns
      break;

    case "IRConstructorPattern":
      for (const arg of pattern.args) {
        collectPatternDependencies(arg, deps, availableNames);
      }
      break;

    case "IRTuplePattern":
      for (const elem of pattern.elements) {
        collectPatternDependencies(elem, deps, availableNames);
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
