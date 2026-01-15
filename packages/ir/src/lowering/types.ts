/**
 * Type Conversion
 *
 * Converts semantic types to IR types.
 */

import type { IRType, IRConstraint } from "../types";

/**
 * Convert a semantic Type to IRType.
 * These are structurally identical, but having separate types allows
 * the packages to evolve independently.
 */
export function convertType(type: any): IRType {
  switch (type.kind) {
    case "var":
      return { kind: "var", id: type.id };
    case "con":
      return { kind: "con", name: type.name, args: type.args.map(convertType) };
    case "fun":
      return {
        kind: "fun",
        from: convertType(type.from),
        to: convertType(type.to),
      };
    case "tuple":
      return { kind: "tuple", elements: type.elements.map(convertType) };
    case "record":
      const fields: Record<string, IRType> = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = convertType(v);
      }
      return { kind: "record", fields };
    case "list":
      return { kind: "list", element: convertType(type.element) };
    default:
      // Unknown type kind, return as-is (shouldn't happen)
      return type;
  }
}

/**
 * Convert semantic constraints to IR constraints.
 */
export function convertConstraints(constraints: any[]): IRConstraint[] {
  return constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map(convertType),
  }));
}
