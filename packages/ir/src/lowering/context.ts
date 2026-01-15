/**
 * Lowering Context Management
 *
 * This module manages the context used during IR lowering transformations.
 */

import type { ImportDeclaration } from "@vibe/syntax";
import type { SemanticModule } from "@vibe/semantics";
import type { IRValue } from "../types";

/**
 * Context maintained during lowering transformations.
 */
export type LoweringContext = {
  /** Semantic module for type lookups */
  semantics: SemanticModule;

  /** Lifted bindings accumulated during lowering */
  liftedBindings: IRValue[];

  /** Counter for generating unique names */
  nameCounter: number;

  /** Constructor info with tags */
  constructorTags: Map<string, number>;

  /** Record type field info for desugaring record updates */
  recordFields: Map<string, string[]>;

  /** Import declarations from the source program */
  imports: ImportDeclaration[];

  /** Dependency modules for resolving module-qualified accesses */
  dependencies: Map<string, SemanticModule>;
};

/**
 * Create a fresh lowering context.
 */
export function createLoweringContext(
  semantics: SemanticModule,
  imports: ImportDeclaration[] = [],
  dependencies: Map<string, SemanticModule> = new Map()
): LoweringContext {
  const ctx: LoweringContext = {
    semantics,
    liftedBindings: [],
    nameCounter: 0,
    constructorTags: new Map(),
    recordFields: new Map(),
    imports,
    dependencies,
  };

  // Assign tags to constructors
  assignConstructorTags(ctx);

  // Build record field info from type aliases
  buildRecordFieldInfo(ctx);

  return ctx;
}

/**
 * Assign runtime tags to ADT constructors.
 * Tags are assigned per-ADT, starting from 0.
 */
function assignConstructorTags(ctx: LoweringContext): void {
  for (const [adtName, adt] of Object.entries(ctx.semantics.adts)) {
    for (let i = 0; i < adt.constructors.length; i++) {
      const ctorName = adt.constructors[i];
      if (ctorName) {
        ctx.constructorTags.set(ctorName, i);
      }
    }
  }
}

/**
 * Build record field info from type aliases with record types.
 */
function buildRecordFieldInfo(ctx: LoweringContext): void {
  for (const [name, alias] of Object.entries(ctx.semantics.typeAliases)) {
    if (alias.value.kind === "RecordType") {
      ctx.recordFields.set(
        name,
        alias.value.fields.map((f) => f.name)
      );
    }
  }
}

/**
 * Generate a unique name for lifted bindings.
 */
export function freshName(ctx: LoweringContext, base: string): string {
  const name = `$${base}_${ctx.nameCounter}`;
  ctx.nameCounter++;
  return name;
}
