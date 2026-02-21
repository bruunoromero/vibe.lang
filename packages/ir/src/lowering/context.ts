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

  /**
   * Names imported via namespace import (exposing (..)) that need
   * module-qualified access in generated code.
   * Maps value/constructor name → source module name.
   * Explicit exposing (e.g., `exposing (Just, Nothing)`) produces named
   * JS imports, so those names are NOT in this map.
   */
  namespaceImportedNames: Map<string, string>;
};

/**
 * Create a fresh lowering context.
 */
export function createLoweringContext(
  semantics: SemanticModule,
  imports: ImportDeclaration[] = [],
  dependencies: Map<string, SemanticModule> = new Map(),
): LoweringContext {
  const ctx: LoweringContext = {
    semantics,
    liftedBindings: [],
    nameCounter: 0,
    constructorTags: new Map(),
    recordFields: new Map(),
    imports,
    dependencies,
    namespaceImportedNames: new Map(),
  };

  // Assign tags to constructors
  assignConstructorTags(ctx);

  // Build record field info from type aliases
  buildRecordFieldInfo(ctx);

  // Build namespace-imported names from exposing(..) imports.
  // Only values/constructors from `exposing (..)` need module-qualified
  // access (the codegen emits a namespace import for them).
  // Explicit exposing (e.g., `exposing (Just, Nothing)`) produces named
  // JS imports, so those names are available unqualified.
  buildNamespaceImportedNames(ctx);

  return ctx;
}

/**
 * Assign runtime tags to ADT constructors.
 * Tags are assigned per-ADT, starting from 0.
 *
 * For local constructors, we use the bare name (e.g., "Just").
 * For constructors from dependencies, we also add qualified names:
 *   - Full module path: "Vibe.Result.Ok"
 *   - Import alias if used: "R.Ok" (when `import Vibe.Result as R`)
 */
function assignConstructorTags(ctx: LoweringContext): void {
  // 1. Assign tags for local constructors (bare names)
  for (const [adtName, adt] of Object.entries(ctx.semantics.adts)) {
    for (let i = 0; i < adt.constructors.length; i++) {
      const ctorName = adt.constructors[i];
      if (ctorName) {
        ctx.constructorTags.set(ctorName, i);
      }
    }
  }

  // 2. Assign tags for constructors from dependencies using qualified names
  for (const imp of ctx.imports) {
    const depModule = ctx.dependencies.get(imp.moduleName);
    if (!depModule) continue;

    for (const [adtName, adt] of Object.entries(depModule.adts)) {
      for (let i = 0; i < adt.constructors.length; i++) {
        const ctorName = adt.constructors[i];
        if (ctorName) {
          // Full module path: "Vibe.Result.Ok"
          const fullQualified = `${imp.moduleName}.${ctorName}`;
          ctx.constructorTags.set(fullQualified, i);

          // If import has an alias: "R.Ok"
          if (imp.alias) {
            const aliasQualified = `${imp.alias}.${ctorName}`;
            ctx.constructorTags.set(aliasQualified, i);
          }
        }
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
        alias.value.fields.map((f) => f.name),
      );
    }
  }
}

/**
 * Build the set of names that need module-qualified access in generated code.
 *
 * Only `exposing (..)` imports produce namespace JS imports (import * as Alias).
 * Values/constructors from these modules must be referenced as `Alias.name`.
 *
 * Explicit `exposing (Name, Type(..))` imports produce named JS imports
 * (import { Name } from "..."), so those names are available unqualified.
 */
function buildNamespaceImportedNames(ctx: LoweringContext): void {
  const allExposingModules = new Set<string>();
  for (const imp of ctx.imports) {
    if (imp.exposing?.kind === "All") {
      allExposingModules.add(imp.moduleName);
    }
  }

  if (allExposingModules.size === 0) return;

  // Values/operators: filter importedValues to only namespace-imported ones
  for (const [name, moduleName] of ctx.semantics.importedValues) {
    if (allExposingModules.has(moduleName)) {
      ctx.namespaceImportedNames.set(name, moduleName);
    }
  }

  // Constructors: check each All-exposing dependency for matching constructors
  for (const imp of ctx.imports) {
    if (imp.exposing?.kind !== "All") continue;
    const dep = ctx.dependencies.get(imp.moduleName);
    if (!dep) continue;

    for (const ctorName of Object.keys(ctx.semantics.constructors)) {
      if (dep.constructors[ctorName]) {
        ctx.namespaceImportedNames.set(ctorName, imp.moduleName);
      }
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
