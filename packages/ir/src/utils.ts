/**
 * @vibe/ir - Instance Resolution Utilities
 *
 * Utilities for protocol instance lookup, type-key formatting,
 * type variable substitution, and dictionary resolution.
 */

import type { IRType, IRInstance } from "./types";

/**
 * Format a type as a key string for instance lookup.
 */
export function formatTypeKey(type: IRType | undefined): string {
  if (!type) return "unknown";

  switch (type.kind) {
    case "con":
      if (type.args.length === 0) return type.name;
      return `${type.name}_${type.args.map(formatTypeKey).join("_")}`;
    case "var":
      return `v${type.id}`;
    case "fun":
      return `fn_${formatTypeKey(type.from)}_${formatTypeKey(type.to)}`;
    case "tuple":
      return `tuple_${type.elements.map(formatTypeKey).join("_")}`;
    case "record":
      return `record`;
    case "list":
      return `list_${formatTypeKey(type.element)}`;
    default:
      return "unknown";
  }
}

/**
 * Check if a type is a type variable (or contains only type variables).
 * Used to determine if a type is concrete enough for dictionary resolution.
 */
export function isTypeVariable(type: IRType): boolean {
  switch (type.kind) {
    case "var":
      return true;
    case "con":
      return false;
    case "list":
      // List with a concrete element is concrete enough
      return isTypeVariable(type.element);
    case "tuple":
      return type.elements.every(isTypeVariable);
    case "fun":
      return isTypeVariable(type.from) && isTypeVariable(type.to);
    case "record":
      return Object.values(type.fields).every(isTypeVariable);
    default:
      return false;
  }
}

/**
 * Build a substitution map from type variable IDs to concrete types.
 *
 * Given an instance's type args (which may contain type variables) and
 * the concrete types at a call site, builds a mapping from type variable IDs
 * to their concrete substitutions.
 *
 * @param instanceTypeArgs - The type args from the instance definition
 * @param concreteTypes - The concrete types at the call site
 * @returns Map from type variable ID to concrete type
 */
export function buildTypeVarSubstitution(
  instanceTypeArgs: IRType[],
  concreteTypes: IRType[],
): Map<number, IRType> {
  const subst = new Map<number, IRType>();

  function collectSubst(instType: IRType, concreteType: IRType): void {
    if (instType.kind === "var") {
      // Type variable - record the substitution
      subst.set(instType.id, concreteType);
    } else if (instType.kind === "con" && concreteType.kind === "con") {
      // Type constructor - recurse into args
      for (
        let i = 0;
        i < instType.args.length && i < concreteType.args.length;
        i++
      ) {
        collectSubst(instType.args[i]!, concreteType.args[i]!);
      }
    } else if (instType.kind === "list" && concreteType.kind === "list") {
      collectSubst(instType.element, concreteType.element);
    } else if (instType.kind === "tuple" && concreteType.kind === "tuple") {
      for (
        let i = 0;
        i < instType.elements.length && i < concreteType.elements.length;
        i++
      ) {
        collectSubst(instType.elements[i]!, concreteType.elements[i]!);
      }
    } else if (instType.kind === "fun" && concreteType.kind === "fun") {
      collectSubst(instType.from, concreteType.from);
      collectSubst(instType.to, concreteType.to);
    }
  }

  for (
    let i = 0;
    i < instanceTypeArgs.length && i < concreteTypes.length;
    i++
  ) {
    collectSubst(instanceTypeArgs[i]!, concreteTypes[i]!);
  }

  return subst;
}

/**
 * Apply a type variable substitution to get the concrete type.
 *
 * @param type - The type (may be a type variable)
 * @param subst - The substitution map from type var IDs to concrete types
 * @returns The substituted concrete type, or the original type if no substitution found
 */
export function applyTypeSubstitution(
  type: IRType,
  subst: Map<number, IRType>,
): IRType {
  if (type.kind === "var") {
    return subst.get(type.id) ?? type;
  }
  return type;
}

/**
 * Check if an instance type pattern matches a concrete type by structure.
 *
 * This ignores type variable IDs and just checks that the type constructor
 * structure matches. For example:
 * - `List v97` matches `List Int` (same constructor, different args)
 * - `List v97` matches `List v123` (same constructor, both have variable args)
 * - `v96` matches anything (bare type variable is fully polymorphic)
 * - `Int` only matches `Int` (concrete types must be identical)
 *
 * @param instType - The instance's type pattern (may contain type variables)
 * @param concreteType - The concrete type we're checking against
 * @returns true if the instance type pattern matches the concrete type
 */
export function typeStructureMatches(
  instType: IRType,
  concreteType: IRType,
): boolean {
  // A type variable in the instance matches anything
  if (instType.kind === "var") {
    return true;
  }

  // Both must be the same kind of type constructor
  if (instType.kind === "con" && concreteType.kind === "con") {
    // Type constructor names must match
    if (instType.name !== concreteType.name) {
      return false;
    }

    // Arity must match
    if (instType.args.length !== concreteType.args.length) {
      return false;
    }

    // All type arguments must structurally match
    for (let i = 0; i < instType.args.length; i++) {
      if (!typeStructureMatches(instType.args[i]!, concreteType.args[i]!)) {
        return false;
      }
    }

    return true;
  }

  // Handle list type
  if (instType.kind === "list" && concreteType.kind === "list") {
    return typeStructureMatches(instType.element, concreteType.element);
  }

  // Handle tuple types
  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0; i < instType.elements.length; i++) {
      if (
        !typeStructureMatches(instType.elements[i]!, concreteType.elements[i]!)
      ) {
        return false;
      }
    }
    return true;
  }

  // Handle function types
  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return (
      typeStructureMatches(instType.from, concreteType.from) &&
      typeStructureMatches(instType.to, concreteType.to)
    );
  }

  // Different kinds don't match
  return false;
}

/**
 * Find a polymorphic instance for a protocol.
 *
 * Polymorphic instances have type variables (e.g., `implement Show a => Show (List a)`)
 * instead of concrete types. This function finds such an instance for the given protocol.
 *
 * Returns the instance key (e.g., "ExampleProtocol_v96") or null if not found.
 */
export function findPolymorphicInstance(
  protocolName: string,
  instances: IRInstance[],
): { key: string; instance: IRInstance } | null {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName) continue;

    // Check if this instance has a type variable in its first type argument
    const firstTypeArg = inst.typeArgs[0];
    if (firstTypeArg && firstTypeArg.kind === "var") {
      const typeKey = formatTypeKey(firstTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst,
      };
    }
  }
  return null;
}

/**
 * Find an instance that matches a concrete type by structure.
 *
 * This handles parameterized type constructors like `List a`. For example,
 * if we have `implement ExampleProtocol (List a)` and the concrete type is
 * `List Int`, this will find that instance because the type constructor `List`
 * matches, even though the type arguments differ.
 *
 * This function does NOT match bare type variable instances (like `implement Protocol a`).
 * Those are handled by `findPolymorphicInstance` as a fallback.
 *
 * @param protocolName - The protocol to find an instance for
 * @param concreteType - The concrete type we're matching against
 * @param instances - Available instances to search
 * @returns The matching instance key and instance, or null if not found
 */
export function findMatchingInstance(
  protocolName: string,
  concreteType: IRType,
  instances: IRInstance[],
): { key: string; instance: IRInstance } | null {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName) continue;

    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg) continue;

    // Skip bare type variable instances - those are handled by findPolymorphicInstance
    // We only want to match instances with concrete type constructors here
    if (instTypeArg.kind === "var") continue;

    // Check if the instance type structurally matches the concrete type
    if (typeStructureMatches(instTypeArg, concreteType)) {
      const typeKey = formatTypeKey(instTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst,
      };
    }
  }
  return null;
}
