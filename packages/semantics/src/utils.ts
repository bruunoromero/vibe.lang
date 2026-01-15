import type {
  Type,
  TypeVar,
  TypeCon,
  TypeFun,
  Substitution,
  Constraint,
  Scope,
  ConstraintContext,
} from "./types";

// ===== Fresh Type Variable Counter =====
let nextTypeVarId = 0;

/**
 * Reset the type variable counter. Used for testing.
 */
export function resetTypeVarCounter(start: number = 0): void {
  nextTypeVarId = start;
}

/**
 * Get the current type variable counter value. Used for testing.
 */
export function getTypeVarCounter(): number {
  return nextTypeVarId;
}

/**
 * Create a fresh type variable with a unique ID.
 */
export function freshType(): TypeVar {
  return { kind: "var", id: nextTypeVarId++ };
}

// ===== Type Constructors =====

/**
 * Helper to create a List type using TypeCon.
 * List is represented as TypeCon with name "List" and one type argument.
 */
export function listType(element: Type): TypeCon {
  return { kind: "con", name: "List", args: [element] };
}

/**
 * Check if a type is a List type.
 */
export function isListType(type: Type): type is TypeCon {
  return type.kind === "con" && type.name === "List" && type.args.length === 1;
}

/**
 * Get the element type of a List type. Returns undefined if not a list.
 */
export function getListElement(type: Type): Type | undefined {
  if (isListType(type)) {
    return type.args[0];
  }
  return undefined;
}

/**
 * Create a function type.
 */
export function fn(a: Type, b: Type, c?: Type): TypeFun {
  if (c) {
    return fn(a, fn(b, c));
  }
  return { kind: "fun", from: a, to: b };
}

/**
 * Create a curried function type from a list of argument types and a result type.
 */
export function fnChain(args: Type[], result: Type): Type {
  return args.reduceRight((acc, arg) => fn(arg, acc), result);
}

// ===== Type Equality and Comparison =====

/**
 * Check if two types are structurally equal.
 */
export function typesEqual(t1: Type, t2: Type): boolean {
  if (t1.kind !== t2.kind) return false;
  switch (t1.kind) {
    case "var":
      return (t2 as TypeVar).id === t1.id;
    case "con":
      return (
        (t2 as TypeCon).name === t1.name &&
        t1.args.length === (t2 as TypeCon).args.length &&
        t1.args.every((a, i) => typesEqual(a, (t2 as TypeCon).args[i]!))
      );
    case "fun":
      return (
        typesEqual(t1.from, (t2 as TypeFun).from) &&
        typesEqual(t1.to, (t2 as TypeFun).to)
      );
    case "tuple":
      return (
        t1.elements.length === (t2 as typeof t1).elements.length &&
        t1.elements.every((e, i) =>
          typesEqual(e, (t2 as typeof t1).elements[i]!)
        )
      );
    case "record": {
      const r2 = t2 as typeof t1;
      const keys1 = Object.keys(t1.fields).sort();
      const keys2 = Object.keys(r2.fields).sort();
      return (
        keys1.length === keys2.length &&
        keys1.every(
          (k, i) => k === keys2[i] && typesEqual(t1.fields[k]!, r2.fields[k]!)
        )
      );
    }
  }
}

// ===== Substitution =====

/**
 * Apply a type substitution, resolving type variables to their bound types.
 */
export function applySubstitution(
  type: Type,
  substitution: Substitution
): Type {
  if (type.kind === "var") {
    const replacement = substitution.get(type.id);
    return replacement ? applySubstitution(replacement, substitution) : type;
  }
  if (type.kind === "fun") {
    return fn(
      applySubstitution(type.from, substitution),
      applySubstitution(type.to, substitution)
    );
  }
  if (type.kind === "tuple") {
    return {
      kind: "tuple",
      elements: type.elements.map((t) => applySubstitution(t, substitution)),
    };
  }
  if (type.kind === "record") {
    const fields: Record<string, Type> = {};
    for (const [k, v] of Object.entries(type.fields)) {
      fields[k] = applySubstitution(v, substitution);
    }
    return { kind: "record", fields };
  }
  if (type.kind === "con") {
    return {
      kind: "con",
      name: type.name,
      args: type.args.map((t) => applySubstitution(t, substitution)),
    };
  }
  return type;
}

/**
 * Apply substitution to constraints, resolving type variables.
 */
export function applySubstitutionToConstraints(
  constraints: Constraint[],
  substitution: Substitution
): Constraint[] {
  return constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => applySubstitution(t, substitution)),
  }));
}

// ===== Free Type Variables =====

/**
 * Compute the set of free type variables in a type.
 * A type variable is "free" if it appears in the type and isn't bound by a quantifier.
 * This is used during generalization to determine which type variables should be quantified.
 *
 * Example:
 * - getFreeTypeVars(number) = {}
 * - getFreeTypeVars(a -> b) = {a.id, b.id}
 * - getFreeTypeVars([a]) = {a.id}
 */
export function getFreeTypeVars(
  type: Type,
  substitution: Substitution
): Set<number> {
  const concrete = applySubstitution(type, substitution);

  if (concrete.kind === "var") {
    return new Set([concrete.id]);
  }

  if (concrete.kind === "con") {
    const result = new Set<number>();
    for (const arg of concrete.args) {
      for (const v of getFreeTypeVars(arg, substitution)) {
        result.add(v);
      }
    }
    return result;
  }

  if (concrete.kind === "fun") {
    const result = new Set<number>();
    for (const v of getFreeTypeVars(concrete.from, substitution)) {
      result.add(v);
    }
    for (const v of getFreeTypeVars(concrete.to, substitution)) {
      result.add(v);
    }
    return result;
  }

  if (concrete.kind === "tuple") {
    const result = new Set<number>();
    for (const el of concrete.elements) {
      for (const v of getFreeTypeVars(el, substitution)) {
        result.add(v);
      }
    }
    return result;
  }

  if (concrete.kind === "record") {
    const result = new Set<number>();
    for (const fieldType of Object.values(concrete.fields)) {
      for (const v of getFreeTypeVars(fieldType, substitution)) {
        result.add(v);
      }
    }
    return result;
  }

  return new Set();
}

/**
 * Compute the set of free type variables in a scope.
 * This includes all type variables that appear in any type scheme in the scope
 * but are NOT quantified by that scheme.
 *
 * Used during generalization to avoid quantifying over variables that are
 * already bound in the enclosing scope.
 */
export function getFreeTypeVarsInScope(
  scope: Scope,
  substitution: Substitution
): Set<number> {
  const result = new Set<number>();

  // Collect free variables from this scope
  for (const scheme of scope.symbols.values()) {
    const typeFree = getFreeTypeVars(scheme.type, substitution);
    for (const v of typeFree) {
      // Only include variables that are NOT quantified in the scheme
      if (!scheme.vars.has(v)) {
        result.add(v);
      }
    }
  }

  // Recursively collect from parent scope
  if (scope.parent) {
    for (const v of getFreeTypeVarsInScope(scope.parent, substitution)) {
      result.add(v);
    }
  }

  return result;
}

/**
 * Collect all type variable IDs in a type, in order of first appearance.
 */
export function collectTypeVarIds(type: Type): Set<number> {
  const ids = new Set<number>();
  collectTypeVarIdsHelper(type, ids);
  return ids;
}

function collectTypeVarIdsHelper(type: Type, ids: Set<number>): void {
  switch (type.kind) {
    case "var":
      ids.add(type.id);
      break;
    case "fun":
      collectTypeVarIdsHelper(type.from, ids);
      collectTypeVarIdsHelper(type.to, ids);
      break;
    case "tuple":
      for (const el of type.elements) {
        collectTypeVarIdsHelper(el, ids);
      }
      break;
    case "con":
      for (const arg of type.args) {
        collectTypeVarIdsHelper(arg, ids);
      }
      break;
    case "record":
      for (const v of Object.values(type.fields)) {
        collectTypeVarIdsHelper(v, ids);
      }
      break;
  }
}

/**
 * Collect all unique type variable IDs from a type in order of first occurrence.
 */
export function collectTypeVarIdsOrdered(
  type: Type,
  result: number[],
  seen: Set<number>
): void {
  switch (type.kind) {
    case "var":
      if (!seen.has(type.id)) {
        seen.add(type.id);
        result.push(type.id);
      }
      break;
    case "con":
      for (const arg of type.args) {
        collectTypeVarIdsOrdered(arg, result, seen);
      }
      break;
    case "fun":
      collectTypeVarIdsOrdered(type.from, result, seen);
      collectTypeVarIdsOrdered(type.to, result, seen);
      break;
    case "tuple":
      for (const el of type.elements) {
        collectTypeVarIdsOrdered(el, result, seen);
      }
      break;
    case "record":
      for (const v of Object.values(type.fields)) {
        collectTypeVarIdsOrdered(v, result, seen);
      }
      break;
  }
}

// ===== Type Formatting =====

/**
 * Format a type for display in error messages.
 */
export function formatType(type: Type): string {
  switch (type.kind) {
    case "var":
      return `t${type.id}`;
    case "con":
      if (type.args.length === 0) {
        return type.name;
      }
      return `${type.name} ${type.args.map(formatType).join(" ")}`;
    case "fun":
      const from =
        type.from.kind === "fun"
          ? `(${formatType(type.from)})`
          : formatType(type.from);
      return `${from} -> ${formatType(type.to)}`;
    case "tuple":
      return `(${type.elements.map(formatType).join(", ")})`;
    case "record":
      const fields = Object.entries(type.fields)
        .map(([k, v]) => `${k}: ${formatType(v)}`)
        .join(", ");
      return `{ ${fields} }`;
  }
}

// ===== Constraint Context =====

/**
 * Create a fresh constraint context for collecting protocol constraints.
 */
export function createConstraintContext(): ConstraintContext {
  return { constraints: [] };
}

/**
 * Add a constraint to the context (deduplicating by protocol name and type args).
 */
export function addConstraint(
  ctx: ConstraintContext,
  constraint: Constraint
): void {
  // Check if we already have this constraint
  const isDuplicate = ctx.constraints.some(
    (c) =>
      c.protocolName === constraint.protocolName &&
      c.typeArgs.length === constraint.typeArgs.length &&
      c.typeArgs.every((t, i) => typesEqual(t, constraint.typeArgs[i]!))
  );
  if (!isDuplicate) {
    ctx.constraints.push(constraint);
  }
}

// ===== Annotation Helpers =====

/**
 * Flatten a function type into its parameter types.
 */
export function flattenFunctionParams(type: Type): Type[] {
  const params: Type[] = [];
  let current: Type = type;
  while (current.kind === "fun") {
    params.push(current.from);
    current = current.to;
  }
  return params;
}

// ===== Type Variable Substitution =====

/**
 * Apply a type variable substitution (mapping var IDs to new types).
 */
export function applyVarSubstitution(
  type: Type,
  subst: Map<number, Type>
): Type {
  switch (type.kind) {
    case "var": {
      const replacement = subst.get(type.id);
      return replacement ?? type;
    }
    case "fun":
      return {
        kind: "fun",
        from: applyVarSubstitution(type.from, subst),
        to: applyVarSubstitution(type.to, subst),
      };
    case "tuple":
      return {
        kind: "tuple",
        elements: type.elements.map((el) => applyVarSubstitution(el, subst)),
      };
    case "con":
      return {
        kind: "con",
        name: type.name,
        args: type.args.map((arg) => applyVarSubstitution(arg, subst)),
      };
    case "record":
      const fields: Record<string, Type> = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = applyVarSubstitution(v, subst);
      }
      return { kind: "record", fields };
    default:
      return type;
  }
}

/**
 * Apply a substitution (type var ID -> Type) to a type.
 */
export function applyTypeSubstitution(
  type: Type,
  substitution: Map<number, Type>
): Type {
  switch (type.kind) {
    case "var": {
      const mapped = substitution.get(type.id);
      return mapped ?? type;
    }
    case "con":
      return {
        kind: "con",
        name: type.name,
        args: type.args.map((arg) => applyTypeSubstitution(arg, substitution)),
      };
    case "fun":
      return {
        kind: "fun",
        from: applyTypeSubstitution(type.from, substitution),
        to: applyTypeSubstitution(type.to, substitution),
      };
    case "tuple":
      return {
        kind: "tuple",
        elements: type.elements.map((el) =>
          applyTypeSubstitution(el, substitution)
        ),
      };
    case "record": {
      const fields: Record<string, Type> = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = applyTypeSubstitution(v, substitution);
      }
      return { kind: "record", fields };
    }
  }
}
