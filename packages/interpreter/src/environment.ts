import type { Value } from "./values";

/**
 * Lexical environment for variable bindings.
 * Forms a chain through parent references to implement lexical scoping.
 */
export interface Environment {
  readonly bindings: Map<string, Value>;
  readonly parent: Environment | null;
}

/**
 * Create a new empty environment with no parent.
 */
export const createEnvironment = (): Environment => ({
  bindings: new Map(),
  parent: null,
});

/**
 * Create a child environment that extends a parent.
 */
export const extendEnvironment = (parent: Environment): Environment => ({
  bindings: new Map(),
  parent,
});

/**
 * Look up a variable in the environment chain.
 * Returns undefined if not found.
 */
export const lookupVariable = (
  env: Environment,
  name: string
): Value | undefined => {
  const value = env.bindings.get(name);
  if (value !== undefined) {
    return value;
  }
  if (env.parent) {
    return lookupVariable(env.parent, name);
  }
  return undefined;
};

/**
 * Define a variable in the current environment scope.
 * Shadows any binding in parent scopes.
 */
export const defineVariable = (
  env: Environment,
  name: string,
  value: Value
): void => {
  env.bindings.set(name, value);
};

/**
 * Set a variable in the environment chain.
 * Finds the first scope that has the binding and updates it.
 * If not found anywhere, defines in the current scope.
 */
export const setVariable = (
  env: Environment,
  name: string,
  value: Value
): void => {
  if (env.bindings.has(name)) {
    env.bindings.set(name, value);
    return;
  }
  if (env.parent) {
    setVariable(env.parent, name, value);
    return;
  }
  // Not found anywhere, define in current scope
  env.bindings.set(name, value);
};
