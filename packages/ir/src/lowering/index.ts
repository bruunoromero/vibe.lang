/**
 * IR Lowering Transformations
 *
 * This module exports all lowering functionality through a clean facade.
 * The implementation is split across focused modules for maintainability.
 */

// Re-export context
export {
  createLoweringContext,
  freshName,
  type LoweringContext,
} from "./context";

// Re-export expression lowering
export { lowerExpr } from "./expressions";

// Re-export pattern lowering
export { lowerPattern } from "./patterns";

// Re-export type conversion
export { convertType, convertConstraints } from "./types";
