/**
 * @vibe/codegen - Instance Resolution
 *
 * Utilities for protocol instance lookup, type-key formatting,
 * type variable substitution, and dictionary resolution.
 */

import type { IRType, IRInstance, IRConstraint, IRProgram } from "@vibe/ir";

// Re-export types used by consumers
export type { IRType, IRInstance, IRConstraint };

/**
 * Information needed for instance resolution during codegen.
 */
export interface InstanceContext {
  /** Protocol instances available */
  instances: IRInstance[];

  /** Generated instance dictionary names: "Protocol_Type" -> "$dict_Protocol_Type" */
  instanceDictNames: Map<string, string>;

  /** Set of instance keys defined locally in this module */
  localInstanceKeys: Set<string>;

  /** Map from instance keys to their source module name */
  instanceModules: Map<string, string>;

  /** Map from instance keys to their constraint info */
  constrainedInstances: Map<string, IRConstraint[]>;

  /** Import aliases for qualified module references */
  importAliases: { moduleName: string; alias: string }[];

  /** Dictionary parameters currently in scope (polymorphic context) */
  dictParamsInScope: Set<string>;

  /** Concrete constraints from enclosing function */
  concreteConstraints: Map<string, IRType>;

  /** Expected return type for the current expression context */
  expectedReturnType: IRType | undefined;
}

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
  concreteTypes: IRType[]
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
  subst: Map<number, IRType>
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
  concreteType: IRType
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
  instances: IRInstance[]
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
  instances: IRInstance[]
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

/**
 * Get the import alias for a module name.
 *
 * Looks up how the module was imported and returns the alias.
 * For example, if "Vibe.JS" was imported as "JS", returns "JS".
 * If no alias is found, returns the last segment of the module name.
 */
export function getImportAliasForModule(
  moduleName: string,
  importAliases: { moduleName: string; alias: string }[]
): string {
  // Check the import aliases in the program
  for (const alias of importAliases) {
    if (alias.moduleName === moduleName) {
      return alias.alias;
    }
  }

  // Default: use the last segment of the module name
  // e.g., "Vibe.JS" -> "Vibe" (first segment for prelude modules)
  // or just "Vibe" -> "Vibe"
  const segments = moduleName.split(".");
  return segments[0] || moduleName;
}

/**
 * Resolve a dictionary reference for a protocol and concrete type.
 *
 * Returns the fully qualified dictionary expression. For simple instances
 * like `implement Num Int`, returns "$dict_Num_Int".
 *
 * For constrained instances like `implement Eq a => ExampleProtocol a`,
 * returns the dictionary function applied to resolved constraint dictionaries,
 * e.g., "$dict_ExampleProtocol_v96($dict_Eq_Int)" when called with concrete type Int.
 *
 * For polymorphic instances (type variable in type args), we find the matching
 * polymorphic instance and apply the constraint dictionaries for the concrete type.
 *
 * @param protocolName - The protocol we're resolving for
 * @param typeKey - The type key string (e.g., "Int", "List_Int")
 * @param ctx - Instance context with all needed lookup tables
 * @param concreteType - The concrete type we're resolving for (e.g., Int). Used to
 *                       resolve constraint dictionaries for polymorphic instances.
 *                       For single-parameter protocols, this is the first type arg.
 * @param allConcreteTypes - Optional array of ALL concrete type args at the call site.
 *                           Used for multi-parameter protocols to properly resolve constraints.
 */
export function resolveDictReference(
  protocolName: string,
  typeKey: string,
  ctx: InstanceContext,
  concreteType?: IRType,
  allConcreteTypes?: IRType[]
): string {
  const key = `${protocolName}_${typeKey}`;

  // Get the base dictionary name
  let dictRef: string | null = null;
  let instanceKey: string = key;

  // Check if it's defined locally in this module (exact match)
  if (ctx.localInstanceKeys.has(key)) {
    dictRef = `$dict_${key}`;
  } else {
    // Check if it's in the known instances (from imports)
    const sourceModule = ctx.instanceModules.get(key);
    if (sourceModule) {
      // Use the source module's import alias for the dictionary reference
      const importAlias = getImportAliasForModule(
        sourceModule,
        ctx.importAliases
      );
      dictRef = `${importAlias}.$dict_${key}`;
    }
  }

  // If no exact match found, try to find a structurally matching instance
  // This handles cases like `implement Protocol (List a)` matching `List Int`
  if (!dictRef && concreteType) {
    const structuralMatch = findMatchingInstance(
      protocolName,
      concreteType,
      ctx.instances
    );
    if (structuralMatch) {
      instanceKey = structuralMatch.key;
      if (ctx.localInstanceKeys.has(instanceKey)) {
        dictRef = `$dict_${instanceKey}`;
      } else {
        const sourceModule = ctx.instanceModules.get(instanceKey);
        if (sourceModule) {
          const importAlias = getImportAliasForModule(
            sourceModule,
            ctx.importAliases
          );
          dictRef = `${importAlias}.$dict_${instanceKey}`;
        }
      }
    }
  }

  // If still no match, try to find a fully polymorphic instance
  // A polymorphic instance has a bare type variable (e.g., `implement Eq a => Protocol a`)
  if (!dictRef) {
    const polymorphicMatch = findPolymorphicInstance(
      protocolName,
      ctx.instances
    );
    if (polymorphicMatch) {
      instanceKey = polymorphicMatch.key;
      if (ctx.localInstanceKeys.has(instanceKey)) {
        dictRef = `$dict_${instanceKey}`;
      } else {
        const sourceModule = ctx.instanceModules.get(instanceKey);
        if (sourceModule) {
          const importAlias = getImportAliasForModule(
            sourceModule,
            ctx.importAliases
          );
          dictRef = `${importAlias}.$dict_${instanceKey}`;
        }
      }
    }
  }

  if (!dictRef) {
    // No instance found - this is a user error (missing instance declaration)
    // Format a helpful error message
    const typeName = typeKey.startsWith("v")
      ? "a type variable"
      : `'${typeKey}'`;
    throw new Error(
      `No instance of '${protocolName}' found for ${typeName}. ` +
        `You may need to add: implement ${protocolName} ${typeKey} where ...`
    );
  }

  // Check if this instance is constrained (needs dictionary parameters)
  const constraints = ctx.constrainedInstances.get(instanceKey);
  const matchedInstance = ctx.instances.find(
    (inst) =>
      inst.protocolName === protocolName &&
      `${protocolName}_${formatTypeKey(inst.typeArgs[0])}` === instanceKey
  );

  if (constraints && constraints.length > 0) {
    // The dictionary is a function - we need to pass constraint dictionaries
    // For each constraint, resolve the appropriate dictionary based on the correct type arg
    const constraintDicts: string[] = [];
    const seenProtocols = new Set<string>();

    // Build type variable substitution if we have all concrete types and the instance
    const typeVarSubst =
      allConcreteTypes && matchedInstance
        ? buildTypeVarSubstitution(matchedInstance.typeArgs, allConcreteTypes)
        : new Map<number, IRType>();

    for (const constraint of constraints) {
      if (!seenProtocols.has(constraint.protocolName)) {
        seenProtocols.add(constraint.protocolName);

        // Get the type variable from the constraint and resolve it using substitution
        const constraintTypeArg = constraint.typeArgs[0];
        let resolvedType: IRType | undefined;

        if (constraintTypeArg && constraintTypeArg.kind === "var") {
          // Use substitution to find the concrete type for this type variable
          resolvedType = typeVarSubst.get(constraintTypeArg.id);
          // If substitution didn't find a match, the constraint is on a type variable
          // that wasn't matched to any concrete operand type - use polymorphic path
        } else if (constraintTypeArg) {
          // Already a concrete type
          resolvedType = constraintTypeArg;
        }

        // NOTE: We deliberately do NOT fallback to concreteType here.
        // If we couldn't resolve the type variable, it means the constraint is on
        // a type parameter that isn't determined by the call's operands.
        // In this case, we should use the polymorphic path.

        if (resolvedType && resolvedType.kind === "con") {
          // We have a concrete type - resolve the constraint dictionary for it
          constraintDicts.push(
            resolveDictionaryForType(constraint.protocolName, resolvedType, ctx)
          );
        } else if (resolvedType && resolvedType.kind === "list") {
          // Handle list type - resolve Appendable for List
          constraintDicts.push(
            resolveDictionaryForType(constraint.protocolName, resolvedType, ctx)
          );
        } else {
          // Polymorphic context - pass through the dictionary parameter
          constraintDicts.push(`$dict_${constraint.protocolName}`);
        }
      }
    }

    // Apply the dictionary function with constraint dictionaries
    return `${dictRef}(${constraintDicts.join(", ")})`;
  }

  return dictRef;
}

/**
 * Resolve a protocol constraint to a dictionary reference.
 *
 * If the type is concrete (e.g., Int), we look up the instance dictionary.
 * If the type is a type variable, we pass through $dict_Protocol (polymorphic).
 */
export function resolveDictionaryForType(
  protocolName: string,
  type: IRType | undefined,
  ctx: InstanceContext
): string {
  if (!type) {
    // Unknown type - use polymorphic pass-through
    return `$dict_${protocolName}`;
  }

  // If it's a concrete type, look up the instance dictionary
  if (type.kind === "con") {
    const typeKey = formatTypeKey(type);
    // Pass the concrete type so constrained instances can resolve their constraint dicts
    return resolveDictReference(protocolName, typeKey, ctx, type);
  }

  // Handle list type - convert to con type for lookup
  if (type.kind === "list") {
    const listAsCon: IRType = {
      kind: "con",
      name: "List",
      args: [type.element],
    };
    const typeKey = formatTypeKey(listAsCon);
    return resolveDictReference(protocolName, typeKey, ctx, listAsCon);
  }

  // Type variable - pass through the polymorphic dictionary
  return `$dict_${protocolName}`;
}
