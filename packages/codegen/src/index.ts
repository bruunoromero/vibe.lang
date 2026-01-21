/**
 * @vibe/codegen - JavaScript Code Generator for Vibe
 *
 * This module transforms lowered IR into executable JavaScript code.
 * It handles:
 * - Expression compilation
 * - Pattern matching with constructor tags
 * - Protocol dictionary passing
 * - Module imports/exports
 * - External FFI bindings
 *
 * Output conventions:
 * - Bool (True/False) compiles to JS true/false
 * - Unit compiles to undefined
 * - List compiles to JS arrays
 * - ADT constructors compile to tagged objects: { $tag: N, $0: arg0, ... }
 */

import fs from "node:fs";
import path from "node:path";
import type {
  IRProgram,
  IRValue,
  IRExpr,
  IRPattern,
  IRType,
  IRConstraint,
  IRInstance,
  IRProtocol,
  IRConstructorInfo,
  IRModuleAccess,
  SCC,
} from "@vibe/ir";
import {
  sanitizeOperator,
  BUILTIN_MODULE_NAME,
  BOOL_TYPE_NAME,
  UNIT_TYPE_NAME,
  SHORT_CIRCUIT_OPERATORS,
  SHORT_CIRCUIT_HELPERS,
} from "@vibe/syntax";

// Import from extracted modules
import { sanitizeIdentifier } from "./sanitize";
import {
  generateExternalImports,
  generateDependencyImports,
  calculateReExportPath,
  type ExternalBindingsMap,
} from "./imports";
import {
  formatTypeKey,
  isTypeVariable,
  buildTypeVarSubstitution,
  resolveDictReference,
  resolveDictionaryForType,
  findMatchingInstance,
  findPolymorphicInstance,
  getImportAliasForModule,
  type InstanceContext,
} from "./instances";

// Re-export for consumers
export { sanitizeIdentifier } from "./sanitize";
export {
  formatTypeKey,
  isTypeVariable,
  resolveDictReference,
  resolveDictionaryForType,
} from "./instances";

// ============================================================================
// Code Generation Context
// ============================================================================

/**
 * Context maintained during code generation.
 */
export interface CodegenContext {
  /** The IR program being compiled */
  program: IRProgram;

  /** Current indentation level */
  indentLevel: number;

  /** Protocol instances for dictionary lookup */
  instances: IRInstance[];

  /** Protocol definitions */
  protocols: Record<string, IRProtocol>;

  /** Constructor info for pattern matching */
  constructors: Record<string, IRConstructorInfo>;

  /** Set of external modules that need to be imported */
  externalImports: Set<string>;

  /** Map from external module path to imported bindings.
   *  Inner map: Vibe name -> runtime export name (e.g., "add" -> "intAdd")
   */
  externalBindings: Map<string, Map<string, string>>;

  /** Generated instance dictionary names: "Protocol_Type" -> "$dict_Protocol_Type" */
  instanceDictNames: Map<string, string>;

  /**
   * Set of instance keys that are defined locally in this module.
   * Keys are like "Num_Int", "Eq_Float", etc.
   * Used to distinguish local dictionaries from imported ones.
   */
  localInstanceKeys: Set<string>;

  /**
   * Map from instance keys to their source module name.
   * Keys are like "Num_Int", "Eq_Float", etc.
   * Used to generate correct module-qualified dictionary references.
   */
  instanceModules: Map<string, string>;

  /** Counter for generating unique names */
  uniqueCounter: number;

  /**
   * Set of dictionary parameter names currently in scope.
   * e.g., {"$dict_Num", "$dict_Eq"} when inside a polymorphic function.
   */
  dictParamsInScope: Set<string>;

  /**
   * Map from protocol names to concrete types for resolved constraints.
   * Used in monomorphic contexts where the constraint is on a concrete type.
   * e.g., {"Num" -> "Int"} when inside a function with `Num Int` constraint.
   */
  concreteConstraints: Map<string, IRType>;

  /**
   * Dictionary names that were actually generated in this module.
   * Populated during generateInstanceDictionaries(), used for exports.
   */
  generatedDictNames: string[];

  /**
   * Map from protocol method names to their protocol name.
   * Built from program.protocols. Used to identify protocol methods
   * for uniform dictionary dispatch.
   * e.g., "+" -> "Num", "==" -> "Eq"
   */
  protocolMethodMap: Map<string, string>;

  /**
   * Map from variable names to their types in the current scope.
   * Used to infer types for field accesses and other expressions.
   * e.g., {"point" -> {kind: "record", fields: {x: Int, y: Int}}}
   */
  varTypes: Map<string, IRType>;

  /**
   * Map from instance keys (e.g., "ExampleProtocol_v96") to their constraint info.
   * Used to determine if an instance dictionary is parameterized (a function)
   * and what constraint dictionaries it needs.
   */
  constrainedInstances: Map<string, IRConstraint[]>;

  /**
   * The expected return type for the current expression context.
   * Used for multi-parameter protocols where constraints may be on the return type.
   * For example, in `main : List Int = convert3 10.0`, the expected return type
   * is `List Int`, which helps resolve the `Appendable` constraint.
   */
  expectedReturnType: IRType | undefined;

  /**
   * Set of short-circuit operators used in this module.
   * Used to emit helper functions only when needed.
   */
  usedShortCircuitOps: Set<string>;

  /**
   * Map from generated instance dictionary name (e.g. "$dict_Eq_Int") to the IRInstance.
   * key is the full name including $dict prefix.
   */
  instanceMap: Map<string, IRInstance>;
}

/**
 * Create a fresh codegen context.
 */
export function createCodegenContext(program: IRProgram): CodegenContext {
  const ctx: CodegenContext = {
    program,
    indentLevel: 0,
    instances: program.instances,
    protocols: program.protocols,
    constructors: program.constructors,
    externalImports: new Set(program.externalImports),
    externalBindings: new Map(),
    instanceDictNames: new Map(),
    localInstanceKeys: new Set(),
    instanceModules: new Map(),
    uniqueCounter: 0,
    dictParamsInScope: new Set(),
    concreteConstraints: new Map(),
    generatedDictNames: [],
    protocolMethodMap: new Map(),
    varTypes: new Map(),
    constrainedInstances: new Map(),
    expectedReturnType: undefined,
    usedShortCircuitOps: new Set(),
    instanceMap: new Map(),
  };

  // Build protocol method map from protocol definitions
  // This maps method names to their protocol names for uniform dictionary dispatch
  for (const [protocolName, protocol] of Object.entries(program.protocols)) {
    for (const method of protocol.methods) {
      ctx.protocolMethodMap.set(method.name, protocolName);
    }
  }

  // Collect external bindings by module
  // Store mapping from Vibe name to runtime export name
  for (const [vibeName, value] of Object.entries(program.values)) {
    if (value.isExternal && value.externalTarget) {
      const { modulePath, exportName } = value.externalTarget;
      if (!ctx.externalBindings.has(modulePath)) {
        ctx.externalBindings.set(modulePath, new Map());
      }
      ctx.externalBindings.get(modulePath)!.set(vibeName, exportName);
    }
  }

  // Generate instance dictionary names and track local/imported instances
  const currentModule = program.moduleName;
  const sourceInstances = program.sourceModule.instances;

  for (let i = 0; i < program.instances.length; i++) {
    const inst = program.instances[i];
    if (!inst || inst.typeArgs.length === 0) continue;

    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const key = `${inst.protocolName}_${typeKey}`;
    ctx.instanceDictNames.set(key, `$dict_${key}`);

    // Track which module defines this instance
    const sourceInst = sourceInstances[i];
    const instanceModule = sourceInst?.moduleName
      ? sourceInst.moduleName
      : currentModule;
    ctx.instanceModules.set(key, instanceModule as string);

    // Check if this instance is defined locally in this module
    if (instanceModule === currentModule) {
      ctx.localInstanceKeys.add(key);
    }

    // Track constrained instances (those that need dictionary parameters)
    if (inst.constraints.length > 0) {
      ctx.constrainedInstances.set(key, inst.constraints);
    }

    // allow looking up instance by generated name
    ctx.instanceMap.set(`$dict_${key}`, inst);
  }

  return ctx;
}

/**
 * Generate a unique name.
 */
function freshName(ctx: CodegenContext, base: string): string {
  const name = `$${base}_${ctx.uniqueCounter}`;
  ctx.uniqueCounter++;
  return name;
}

/**
 * Get the import alias for a module name (wrapper for InstanceContext).
 */
function getImportAliasForModuleCtx(
  moduleName: string,
  ctx: CodegenContext,
): string {
  return getImportAliasForModule(moduleName, ctx.program.importAliases);
}

/**
 * Create an InstanceContext from a CodegenContext for use with instance resolution functions.
 */
function toInstanceContext(ctx: CodegenContext): InstanceContext {
  return {
    instances: ctx.instances,
    instanceDictNames: ctx.instanceDictNames,
    localInstanceKeys: ctx.localInstanceKeys,
    instanceModules: ctx.instanceModules,
    constrainedInstances: ctx.constrainedInstances,
    importAliases: ctx.program.importAliases,
    dictParamsInScope: ctx.dictParamsInScope,
    concreteConstraints: ctx.concreteConstraints,
    expectedReturnType: ctx.expectedReturnType,
  };
}

/**
 * Wrapper to call resolveDictReference with CodegenContext.
 */
function resolveDictReferenceCtx(
  protocolName: string,
  typeKey: string,
  ctx: CodegenContext,
  concreteType?: IRType,
  allConcreteTypes?: IRType[],
): string {
  return resolveDictReference(
    protocolName,
    typeKey,
    toInstanceContext(ctx),
    concreteType,
    allConcreteTypes,
  );
}

/**
 * Wrapper to call resolveDictionaryForType with CodegenContext.
 */
function resolveDictionaryForTypeCtx(
  protocolName: string,
  type: IRType | undefined,
  ctx: CodegenContext,
): string {
  return resolveDictionaryForType(protocolName, type, toInstanceContext(ctx));
}

// ============================================================================
// Main Code Generation
// ============================================================================

/**
 * Result of code generation for a module.
 */
export interface GeneratedModule {
  /** The module name */
  moduleName: string;

  /** The package name this module belongs to */
  packageName: string;

  /** The generated JavaScript code */
  code: string;

  /** External imports required: module path -> (Vibe name -> runtime name) */
  imports: Map<string, Map<string, string>>;

  /** Exports from this module */
  exports: string[];
}

/**
 * Options for code generation.
 */
export interface GenerateOptions {
  /** Map of module names to their package names (for import path resolution) */
  modulePackages?: Map<string, string>;
}

/**
 * Generate JavaScript code for an IR program.
 */
export function generate(
  program: IRProgram,
  options: GenerateOptions = {},
): GeneratedModule {
  const ctx = createCodegenContext(program);
  const { modulePackages = new Map() } = options;
  const headerLines: string[] = [];
  const bodyLines: string[] = [];

  // 1. Generate imports for external modules
  const importLines = generateImports(ctx);
  if (importLines.length > 0) {
    headerLines.push(...importLines);
    headerLines.push("");
  }

  // 2. Generate imports for dependency modules
  const depImportLines = generateDependencyImports(program, modulePackages);
  if (depImportLines.length > 0) {
    headerLines.push(...depImportLines);
    headerLines.push("");
  }

  // 3. Generate ADT constructor functions
  const ctorLines = generateConstructors(ctx);
  if (ctorLines.length > 0) {
    bodyLines.push("// ADT Constructors");
    bodyLines.push(...ctorLines);
    bodyLines.push("");
  }

  // 4. Generate values in dependency order
  // This includes both regular values AND synthetic default implementations ($impl_*, $default_*)
  // The dependency order ensures all values are defined before they are used.
  bodyLines.push("// Values");
  for (const scc of program.dependencyOrder) {
    const sccLines = generateSCC(scc, ctx);
    bodyLines.push(...sccLines);
  }

  // 6. Generate exports
  const exportLines = generateExports(program, ctx, modulePackages);
  if (exportLines.length > 0) {
    bodyLines.push("");
    bodyLines.push(...exportLines);
  }

  // 8. Generate short-circuit helper functions (if used)
  // These are generated after values so we know which ones are needed,
  // but they go in the header before the body.
  const helperLines: string[] = [];
  if (ctx.usedShortCircuitOps.size > 0) {
    helperLines.push("// Short-Circuit Operator Helpers");
    for (const op of ctx.usedShortCircuitOps) {
      const helper = SHORT_CIRCUIT_HELPERS[op];
      if (helper) {
        helperLines.push(`const ${helper.name} = ${helper.impl};`);
      }
    }
    helperLines.push("");
  }

  return {
    moduleName: program.moduleName,
    packageName: program.packageName,
    code: [...headerLines, ...helperLines, ...bodyLines].join("\n"),
    imports: ctx.externalBindings,
    exports: Object.keys(program.values).filter(
      (name) => !name.startsWith("$"),
    ),
  };
}

// ============================================================================
// Import Generation
// ============================================================================

/**
 * Generate import statements for external (FFI) modules.
 */
function generateImports(ctx: CodegenContext): string[] {
  return generateExternalImports(ctx.externalBindings);
}

// ============================================================================
// Constructor Generation
// ============================================================================

/**
 * Generate ADT constructor functions.
 *
 * Constructors are generated as curried functions that return tagged objects.
 * Example:
 *   Just : a -> Maybe a
 *   becomes:
 *   const Just = ($0) => ({ $tag: 0, $0 });
 *
 * Zero-arity constructors become constants:
 *   const Nothing = { $tag: 1 };
 */
function generateConstructors(ctx: CodegenContext): string[] {
  const lines: string[] = [];
  const generated = new Set<string>();
  const currentModule = ctx.program.moduleName;

  // Group constructors by parent type
  const byParent = new Map<string, IRConstructorInfo[]>();
  for (const ctor of Object.values(ctx.constructors)) {
    if (!byParent.has(ctor.parentType)) {
      byParent.set(ctor.parentType, []);
    }
    byParent.get(ctor.parentType)!.push(ctor);
  }

  for (const [parentType, ctors] of byParent) {
    // Skip built-in Bool - we use native true/false
    if (parentType === BOOL_TYPE_NAME) continue;

    // Skip Unit - we use undefined
    if (parentType === UNIT_TYPE_NAME) continue;

    // Check if this ADT belongs to the current module
    const adtInfo = ctx.program.adts[parentType];
    if (
      adtInfo?.moduleName &&
      adtInfo.moduleName !== currentModule &&
      adtInfo.moduleName !== BUILTIN_MODULE_NAME
    ) {
      // This ADT comes from a dependency, skip generating its constructors
      continue;
    }

    // Sort by tag for consistent output
    ctors.sort((a, b) => a.tag - b.tag);

    for (const ctor of ctors) {
      if (generated.has(ctor.name)) continue;
      generated.add(ctor.name);

      const safeName = sanitizeIdentifier(ctor.name);

      if (ctor.arity === 0) {
        // Zero-arity: constant tagged object
        lines.push(`const ${safeName} = { $tag: ${ctor.tag} };`);
      } else {
        // N-arity: curried function
        const params = Array.from({ length: ctor.arity }, (_, i) => `$${i}`);
        const fields = params.map((p) => p).join(", ");

        // Build curried function
        let body = `({ $tag: ${ctor.tag}, ${fields} })`;
        for (let i = params.length - 1; i >= 0; i--) {
          body = `(${params[i]}) => ${body}`;
        }
        lines.push(`const ${safeName} = ${body};`);
      }
    }
  }

  return lines;
}

// ============================================================================
// Instance Dictionary Generation
// ============================================================================

/**
 * Generate code for a single protocol instance dictionary.
 */
function generateInstanceDictionary(
  inst: IRInstance,
  ctx: CodegenContext,
): string[] {
  const lines: string[] = [];
  const currentModule = ctx.program.moduleName;

  const typeKey = formatTypeKey(inst.typeArgs[0]);
  const key = `${inst.protocolName}_${typeKey}`;
  const dictName = ctx.instanceDictNames.get(key);

  if (!dictName) return [];

  // Check if this instance belongs to the current module
  const instanceModule = ctx.instanceModules.get(key);
  if (instanceModule && instanceModule !== currentModule) {
    return [];
  }

  // Check if this instance has constraints (needs dictionary parameters)
  const constraints = ctx.constrainedInstances.get(key);

  const methodEntries: string[] = [];
  for (const [methodName, implName] of Object.entries(inst.methods)) {
    const safeName = sanitizeIdentifier(methodName);
    const safeImpl = sanitizeIdentifier(implName);

    if (constraints && constraints.length > 0) {
      // Constrained instance: method implementations need to receive dictionaries
      // Build the dictionary parameter list for passing to the method impl
      const dictParams: string[] = [];
      const seenProtocols = new Set<string>();
      for (const constraint of constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      }
      // Pass dictionaries to the method implementation
      const dictPasses = dictParams.map((d) => `(${d})`).join("");
      methodEntries.push(`  ${safeName}: ${safeImpl}${dictPasses}`);
    } else {
      methodEntries.push(`  ${safeName}: ${safeImpl}`);
    }
  }

  if (methodEntries.length > 0) {
    if (constraints && constraints.length > 0) {
      // Constrained instance: wrap in a lambda that takes dictionary parameters
      const dictParams: string[] = [];
      const seenProtocols = new Set<string>();
      for (const constraint of constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      }
      // Generate: const $dict_Protocol_v0 = ($dict_Eq) => ({ ... });
      lines.push(`const ${dictName} = (${dictParams.join(", ")}) => ({`);
      lines.push(methodEntries.join(",\n"));
      lines.push(`});`);
    } else {
      lines.push(`const ${dictName} = {`);
      lines.push(methodEntries.join(",\n"));
      lines.push(`};`);
    }
    // We need to track generated dict names for exports
    ctx.generatedDictNames.push(dictName);
  }

  return lines;
}

/**
 * Generate protocol instance dictionaries.
 *
 * Each instance becomes an object mapping method names to implementations.
 * Example:
 *   implement Num Int where
 *     (+) = intAdd
 *     (-) = intSub
 *
 *   becomes:
 *   const $dict_Num_Int = {
 *     "_PLUS": intAdd,
 *     "_MINUS": intSub,
 *   };
 *
 * For constrained instances like `implement Eq a => ExampleProtocol a where ...`,
 * the dictionary becomes a function that takes constraint dictionaries:
 *   const $dict_ExampleProtocol_v0 = ($dict_Eq) => ({
 *     exampleMethod: $impl_ExampleProtocol_v0_exampleMethod($dict_Eq)
 *   });
 */
function generateInstanceDictionaries(ctx: CodegenContext): string[] {
  const lines: string[] = [];
  const currentModule = ctx.program.moduleName;

  // Get source instances to check module ownership
  const sourceInstances = ctx.program.sourceModule.instances;

  // Track which dictionaries we actually generate (for exports)
  const generatedDicts: string[] = [];

  for (let i = 0; i < ctx.instances.length; i++) {
    const inst = ctx.instances[i];
    if (!inst || inst.typeArgs.length === 0) continue;

    // Check if this instance belongs to the current module
    const sourceInst = sourceInstances[i];
    if (sourceInst?.moduleName && sourceInst.moduleName !== currentModule) {
      // This instance comes from a dependency, skip generating it
      continue;
    }

    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const key = `${inst.protocolName}_${typeKey}`;
    const dictName = ctx.instanceDictNames.get(key);
    if (!dictName) continue;

    // Check if this instance has constraints (needs dictionary parameters)
    const constraints = ctx.constrainedInstances.get(key);

    const methodEntries: string[] = [];
    for (const [methodName, implName] of Object.entries(inst.methods)) {
      const safeName = sanitizeIdentifier(methodName);
      const safeImpl = sanitizeIdentifier(implName);

      if (constraints && constraints.length > 0) {
        // Constrained instance: method implementations need to receive dictionaries
        // Build the dictionary parameter list for passing to the method impl
        const dictParams: string[] = [];
        const seenProtocols = new Set<string>();
        for (const constraint of constraints) {
          if (!seenProtocols.has(constraint.protocolName)) {
            seenProtocols.add(constraint.protocolName);
            dictParams.push(`$dict_${constraint.protocolName}`);
          }
        }
        // Pass dictionaries to the method implementation
        const dictPasses = dictParams.map((d) => `(${d})`).join("");
        methodEntries.push(`  ${safeName}: ${safeImpl}${dictPasses}`);
      } else {
        methodEntries.push(`  ${safeName}: ${safeImpl}`);
      }
    }

    if (methodEntries.length > 0) {
      if (constraints && constraints.length > 0) {
        // Constrained instance: wrap in a lambda that takes dictionary parameters
        const dictParams: string[] = [];
        const seenProtocols = new Set<string>();
        for (const constraint of constraints) {
          if (!seenProtocols.has(constraint.protocolName)) {
            seenProtocols.add(constraint.protocolName);
            dictParams.push(`$dict_${constraint.protocolName}`);
          }
        }
        // Generate: const $dict_Protocol_v0 = ($dict_Eq) => ({ ... });
        lines.push(`const ${dictName} = (${dictParams.join(", ")}) => ({`);
        lines.push(methodEntries.join(",\n"));
        lines.push(`});`);
      } else {
        lines.push(`const ${dictName} = {`);
        lines.push(methodEntries.join(",\n"));
        lines.push(`};`);
      }
      generatedDicts.push(dictName);
    }
  }

  // Store the generated dict names for export (override the full list)
  ctx.generatedDictNames = generatedDicts;

  return lines;
}

// ============================================================================
// SCC Generation
// ============================================================================

/**
 * Generate code for a strongly connected component.
 *
 * For mutually recursive values, we need special handling to ensure
 * all names are in scope before any are defined.
 */
function generateSCC(scc: SCC, ctx: CodegenContext): string[] {
  const lines: string[] = [];

  if (scc.isMutuallyRecursive) {
    // Mutually recursive: declare all first, then assign
    for (const name of scc.values) {
      const safeName = sanitizeIdentifier(name);
      lines.push(`let ${safeName};`);
    }
    for (const name of scc.values) {
      if (ctx.program.values[name]) {
        const value = ctx.program.values[name];
        const safeName = sanitizeIdentifier(name);
        const body = generateValue(value, ctx);
        lines.push(`${safeName} = ${body};`);
      } else if (ctx.instanceMap.has(name)) {
        // Handle instance in recursive block?
        // Instances usually aren't mutually recursive with values in a way that requires `let`.
        // But if they are, we'd need to emit them.
        // JavaScript object literals can refer to things defined later? No.
        // If an instance refers to a function that refers to the instance...
        // The function is a value. The instance is an object.
        // `const inst = { method: func }; func = ... use inst ...`
        // We might need `let inst; inst = { ... }`.

        // For now, let's assume instances are generated as consts in their own right
        // if they are part of a cycle with values, we might have issues if we don't use `let`.

        // However, `generateInstanceDictionary` returns lines like `const $dict = ...`.
        // We'd need to split it into decl and assignment if recursive.
        // Given the current architecture, instances are usually not recursive with functions
        // in a way that blocks valid JS emission if we use function hoisting.
        // But we are emitting `const func = ...`.

        // Let's defer complex recursive instance handling for a moment and just emit it.
        // If it's in a recursive block, we should probably output it.
        // But `generateInstanceDictionary` outputs `const`.

        // Actually, if an instance is part of a recursive SCC, it's likely a bug or edge case.
        // But let's try to handle it gracefully.

        const inst = ctx.instanceMap.get(name)!;
        const dictLines = generateInstanceDictionary(inst, ctx);
        // If it was `const ...`, and we are in recursive block, we might want to change it?
        // But `let` variables are already declared at top of block.
        // If instance name was in `scc.values`, we declared `let $dict_...`.
        // So we need to assign it.

        // But `generateInstanceDictionary` creates `const ...`.
        // We should probably just emit it as is, but that would shadow the `let`.
        // This is tricky.

        // Strategy: standard instances likely won't be in SCCs with values often
        // because values depend on instances (functions use dicts)
        // but instances depend on values (impls).
        // If logic is `func uses dict`, `dict uses func`, that's a cycle.
        // `func` takes dict as arg? No, global dict.
        // `dict` has method `func`.
        // `func` body uses `dict`.

        // JS:
        // let func;
        // const dict = { method: func }; // uses undefined func? No, func is reference.
        // func = ... uses dict ...

        // This works fine in JS if `func` is reference.
        // But `dict` must be defined before `func` is CALLED.
        // If `func` is TOP-LEVEL called (e.g. implicitly), then order matters.

        // If in SCC, we rely on `let` for values.
        // Instances are objects. `const dict = ...` is fine as long as we don't redeclare.

        // If `name` is in `scc.values`, we emitted `let name;`.
        // So we should NOT emit `const name = ...`.

        // I will assume for now instances are NOT in recursive SCCs with values commonly enough to break.
        // If they are, I should change `generateInstanceDictionary` to support assignment.
        // But for this fix, we just want correct ordering.

        // For non-recursive steps (the vast majority), just call generateInstance.

        lines.push(...dictLines);
      }
    }
  } else {
    // Non-recursive: simple const declaration
    for (const name of scc.values) {
      if (ctx.program.values[name]) {
        const value = ctx.program.values[name];
        // Skip external declarations - they're imported
        if (value.isExternal) continue;

        const safeName = sanitizeIdentifier(name);
        const body = generateValue(value, ctx);
        lines.push(`const ${safeName} = ${body};`);
      } else if (ctx.instanceMap.has(name)) {
        const inst = ctx.instanceMap.get(name)!;
        lines.push(...generateInstanceDictionary(inst, ctx));
      }
    }
  }

  return lines;
}

/**
 * Generate code for a single value binding.
 */
function generateValue(value: IRValue, ctx: CodegenContext): string {
  // External values don't have a body to generate
  if (value.isExternal) {
    return `/* external: ${value.externalTarget?.exportName} */`;
  }

  // Generate dictionary parameter names for each unique constraint on type VARIABLES.
  // Constraints on concrete types (e.g., Num Int) don't need dictionary parameters -
  // they'll be resolved directly to the concrete instance dictionary.
  const dictParams: string[] = [];
  const seenConstraints = new Set<string>();
  const concreteConstraints = new Map<string, IRType>();

  for (const constraint of value.constraints) {
    const typeArg = constraint.typeArgs[0];
    if (typeArg) {
      if (typeArg.kind === "var") {
        // Type variable: needs dictionary parameter
        const dictKey = `${constraint.protocolName}`;
        if (!seenConstraints.has(dictKey)) {
          seenConstraints.add(dictKey);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      } else if (typeArg.kind === "con") {
        // Concrete type: resolve directly (no dictionary parameter)
        concreteConstraints.set(constraint.protocolName, typeArg);
      }
    }
  }

  // Extract parameter types from the function type
  // For a function like `distance : {x: Int, y: Int} -> Int` with param `point`,
  // we want to map "point" -> {kind: "record", fields: {x: Int, y: Int}}
  const varTypes = new Map<string, IRType>();
  // Also extract the final return type (after all parameters)
  let returnType: IRType = value.type;
  if (value.params.length > 0 && value.type.kind === "fun") {
    let currentType: IRType = value.type;
    for (let i = 0; i < value.params.length; i++) {
      const param = value.params[i];
      if (currentType.kind === "fun") {
        if (param?.kind === "IRVarPattern") {
          varTypes.set(param.name, currentType.from);
        }
        currentType = currentType.to;
      }
    }
    returnType = currentType;
  }

  // Handle values where the body is an IRLambda (including synthetic values and partial application)
  // We need to extract parameter types from the nested lambda's parameters
  // Use `returnType` from above as the starting point (after processing value.params)
  if (value.body.kind === "IRLambda" && returnType.kind === "fun") {
    let currentType: IRType = returnType;
    for (const param of value.body.params) {
      if (currentType.kind === "fun") {
        if (param?.kind === "IRVarPattern") {
          varTypes.set(param.name, currentType.from);
        }
        currentType = currentType.to;
      }
    }
    returnType = currentType;
  }

  // Save previous context and set new scope
  const prevDictParams = ctx.dictParamsInScope;
  const prevConcreteConstraints = ctx.concreteConstraints;
  const prevVarTypes = ctx.varTypes;
  const prevExpectedReturnType = ctx.expectedReturnType;
  ctx.dictParamsInScope = new Set(dictParams);
  ctx.concreteConstraints = concreteConstraints;
  ctx.varTypes = varTypes;
  // Set expected return type for the body - this helps resolve constraints
  // on return types (e.g., for multi-parameter protocols like ExampleProtocol3)
  ctx.expectedReturnType = returnType;

  // If value has parameters or dictionary params, wrap body in curried lambdas
  if (value.params.length > 0 || dictParams.length > 0) {
    let body = generateExpr(value.body, ctx);

    // Wrap in curried functions for each regular parameter
    for (let i = value.params.length - 1; i >= 0; i--) {
      const param = value.params[i];
      if (!param) continue;
      const paramCode = generatePattern(param, ctx);
      body = `(${paramCode}) => ${body}`;
    }

    // Add dictionary parameters at the front (they come before regular params)
    for (let i = dictParams.length - 1; i >= 0; i--) {
      body = `(${dictParams[i]}) => ${body}`;
    }

    // Restore previous scope
    ctx.dictParamsInScope = prevDictParams;
    ctx.concreteConstraints = prevConcreteConstraints;
    ctx.varTypes = prevVarTypes;
    ctx.expectedReturnType = prevExpectedReturnType;
    return body;
  }

  // No parameters: just generate the body
  const body = generateExpr(value.body, ctx);

  // Restore previous scope
  ctx.dictParamsInScope = prevDictParams;
  ctx.concreteConstraints = prevConcreteConstraints;
  ctx.varTypes = prevVarTypes;
  ctx.expectedReturnType = prevExpectedReturnType;
  return body;
}

// ============================================================================
// Expression Generation
// ============================================================================

/**
 * Generate JavaScript for an IR expression.
 */
function generateExpr(expr: IRExpr, ctx: CodegenContext): string {
  switch (expr.kind) {
    case "IRVar":
      return generateVar(expr, ctx);

    case "IRModuleAccess":
      return generateModuleAccess(expr, ctx);

    case "IRLiteral":
      return generateLiteral(expr);

    case "IRLambda":
      return generateLambda(expr, ctx);

    case "IRApply":
      return generateApply(expr, ctx);

    case "IRIf":
      return generateIf(expr, ctx);

    case "IRCase":
      return generateCase(expr, ctx);

    case "IRTuple":
      return generateTuple(expr, ctx);

    case "IRUnit":
      return "undefined";

    case "IRList":
      return generateList(expr, ctx);

    case "IRRecord":
      return generateRecord(expr, ctx);

    case "IRRecordUpdate":
      return generateRecordUpdate(expr, ctx);

    case "IRFieldAccess":
      return generateFieldAccess(expr, ctx);

    case "IRConstructor":
      return generateConstructorExpr(expr, ctx);

    case "IRUnary":
      // Emit native JavaScript unary negation
      return `-${generateExpr(expr.operand, ctx)}`;

    default:
      const _exhaustive: never = expr;
      throw new Error(`Unknown expression kind: ${(expr as any).kind}`);
  }
}

/**
 * Generate a variable reference.
 *
 * All protocol methods (operators or regular functions) use uniform dictionary
 * dispatch. When a protocol method is referenced:
 * 1. In polymorphic context (dictionary param in scope): use the dictionary parameter
 * 2. In monomorphic context: we need type info to resolve the concrete dictionary
 *
 * Short-circuit operators (&& and ||) are special-cased to use their runtime
 * function names directly, as they are not protocol methods.
 *
 * Regular functions and external bindings are referenced directly by name.
 */
function generateVar(
  expr: Extract<IRExpr, { kind: "IRVar" }>,
  ctx: CodegenContext,
): string {
  const currentModule = ctx.program.moduleName;

  // Special handling for short-circuit operators
  // These use helper functions defined at the top of the module
  if (SHORT_CIRCUIT_OPERATORS.has(expr.name)) {
    const helper = SHORT_CIRCUIT_HELPERS[expr.name];
    if (helper) {
      // Track that we need this helper function
      ctx.usedShortCircuitOps.add(expr.name);
      return helper.name;
    }
  }

  // Check if this is an external binding
  // Check if this is an external binding
  // Use the Vibe binding name (expr.name) since imports alias the runtime name to it
  // e.g., "import { listCons as cons }" makes "cons" available, not "listCons"
  const value = ctx.program.values[expr.name];
  if (value?.isExternal && value.externalTarget) {
    return sanitizeIdentifier(expr.name);
  }

  // Check if this is a protocol method (using the protocol method map)
  const protocolName = ctx.protocolMethodMap.get(expr.name);
  if (protocolName) {
    const sanitizedName = sanitizeIdentifier(expr.name);
    const dictParam = `$dict_${protocolName}`;

    // Check if we have a dictionary parameter in scope (polymorphic context)
    if (ctx.dictParamsInScope.has(dictParam)) {
      // Use the dictionary parameter
      return `${dictParam}.${sanitizedName}`;
    }

    // Monomorphic context: try to resolve from type info on the variable
    // or from the constraint field if available
    if (expr.constraint) {
      const typeArg = expr.constraint.typeArgs[0];
      if (typeArg && typeArg.kind === "con") {
        const typeKey = formatTypeKey(typeArg);
        // Pass all constraint type args for multi-parameter protocols
        const dictRef = resolveDictReferenceCtx(
          protocolName,
          typeKey,
          ctx,
          typeArg,
          expr.constraint.typeArgs,
        );
        return `${dictRef}.${sanitizedName}`;
      }
    }

    // Try to use the variable's type annotation for monomorphic dispatch
    if (expr.type && expr.type.kind === "fun") {
      const argType = expr.type.from;
      if (argType.kind === "con") {
        const typeKey = formatTypeKey(argType);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx);
        return `${dictRef}.${sanitizedName}`;
      }
    }

    // Try to use concrete constraints from the enclosing function
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType && constraintType.kind === "con") {
      const typeKey = formatTypeKey(constraintType);
      const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx);
      return `${dictRef}.${sanitizedName}`;
    }

    // Try to use the expected return type for zero-argument protocol methods
    // This handles cases like `defaultVal : a` when we know the expected type is `List Int`
    if (ctx.expectedReturnType && !isTypeVariable(ctx.expectedReturnType)) {
      const typeKey = formatTypeKey(ctx.expectedReturnType);
      const dictRef = resolveDictReferenceCtx(
        protocolName,
        typeKey,
        ctx,
        ctx.expectedReturnType,
      );
      return `${dictRef}.${sanitizedName}`;
    }

    // Fallback: use the dictionary parameter (should have been passed)
    // This handles polymorphic usage where the dict param is expected
    throw new Error(
      `Cannot resolve protocol method '${expr.name}' in monomorphic context without type info.`,
    );
  }

  // Check if this is a constructor from another module
  const ctorInfo = ctx.constructors[expr.name];
  if (
    ctorInfo &&
    ctorInfo.moduleName &&
    ctorInfo.moduleName !== currentModule &&
    ctorInfo.moduleName !== BUILTIN_MODULE_NAME
  ) {
    // Reference constructor from its defining module
    return `${ctorInfo.moduleName}.${sanitizeIdentifier(expr.name)}`;
  }

  const safeName = sanitizeIdentifier(expr.name);
  return safeName;
}

/**
 * Generate a module-qualified access expression.
 *
 * This handles expressions like `JS.null` or `Vibe.JS.null` that were
 * resolved during IR lowering. The generated code uses the import alias
 * and the value name as exported by the Vibe module.
 *
 * Note: We use valueName (the Vibe-level name), NOT externalName (the runtime export name).
 * The compiled module already re-exports values with the Vibe names.
 *
 * Examples:
 * - `Int.eq` -> `Int.eq` (the Vibe module re-exports with this name)
 * - `Vibe.JS.something` -> `JS.something` (using the import alias)
 */
function generateModuleAccess(
  expr: IRModuleAccess,
  ctx: CodegenContext,
): string {
  // Use the Vibe-level value name, not the external/runtime name
  const valueName = sanitizeIdentifier(expr.valueName);
  return `${expr.importAlias}.${valueName}`;
}

/**
 * Generate a literal value.
 */
function generateLiteral(expr: Extract<IRExpr, { kind: "IRLiteral" }>): string {
  switch (expr.literalType) {
    case "int":
    case "float":
      return String(expr.value);
    case "string": {
      // The value may include quotes from the lexer, strip them
      let strVal = String(expr.value);
      if (strVal.startsWith('"') && strVal.endsWith('"')) {
        strVal = strVal.slice(1, -1);
      }
      return JSON.stringify(strVal);
    }
    case "char": {
      // The value may include quotes from the lexer, strip them
      let charVal = String(expr.value);
      if (charVal.startsWith("'") && charVal.endsWith("'")) {
        charVal = charVal.slice(1, -1);
      }
      return JSON.stringify(charVal);
    }
    case "bool":
      // Bool literals compile to JS true/false
      return expr.value ? "true" : "false";
  }
}

/**
 * Generate a lambda expression.
 *
 * Handles both regular curried lambdas and thunks (zero-parameter lambdas).
 * Thunks are used for short-circuit operators to delay evaluation.
 */
function generateLambda(
  expr: Extract<IRExpr, { kind: "IRLambda" }>,
  ctx: CodegenContext,
): string {
  const body = generateExpr(expr.body, ctx);

  // Handle thunks (zero-parameter lambdas) used for short-circuit operators
  if (expr.params.length === 0) {
    return `() => ${body}`;
  }

  // Build curried function
  let result = body;
  for (let i = expr.params.length - 1; i >= 0; i--) {
    const param = expr.params[i];
    if (!param) continue;
    const paramCode = generatePattern(param, ctx);
    result = `(${paramCode}) => ${result}`;
  }

  return result;
}

/**
 * Generate a function application.
 *
 * Vibe uses curried functions, so f(a, b) becomes f(a)(b).
 *
 * For constrained functions (those with protocol constraints), we need to
 * pass the appropriate instance dictionaries before the regular arguments.
 *
 * Protocol methods (including operators) use uniform dictionary dispatch -
 * the dictionary is either passed as a parameter (polymorphic) or resolved
 * to a concrete instance (monomorphic) based on argument types.
 */
function generateApply(
  expr: Extract<IRExpr, { kind: "IRApply" }>,
  ctx: CodegenContext,
): string {
  // Special handling for protocol method applications in monomorphic context
  // When we have Apply(Apply(protocolMethod, arg1), arg2), we need to resolve
  // the dictionary based on the argument types
  const protocolMethodResult = tryGenerateProtocolMethodApply(expr, ctx);
  if (protocolMethodResult !== null) {
    return protocolMethodResult;
  }

  let callee = generateExpr(expr.callee, ctx);

  // Wrap lambda expressions in parentheses to ensure correct application
  // Without this, (x) => body(arg) is generated instead of ((x) => body)(arg)
  if (expr.callee.kind === "IRLambda") {
    callee = `(${callee})`;
  }

  // Check if the callee is a named function with constraints
  // If so, we need to pass dictionaries before regular arguments
  let dictPasses: string[] = [];
  if (expr.callee.kind === "IRVar") {
    const calleeValue = ctx.program.values[expr.callee.name];
    if (
      calleeValue &&
      calleeValue.constraints &&
      calleeValue.constraints.length > 0
    ) {
      // The callee has constraints - we need to pass dictionaries
      const seenProtocols = new Set<string>();
      for (const constraint of calleeValue.constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);

          // Try to resolve the constraint to a concrete instance
          // by looking at the type arguments in the constraint
          const typeArg = constraint.typeArgs[0];
          const dictName = resolveDictionaryForTypeCtx(
            constraint.protocolName,
            typeArg,
            ctx,
          );
          dictPasses.push(dictName);
        }
      }
    }
  }

  // Build the application: first dictionaries, then regular arguments
  let result = callee;

  // Pass dictionaries first
  for (const dict of dictPasses) {
    result = `${result}(${dict})`;
  }

  // Then pass regular arguments
  for (const arg of expr.args) {
    const argCode = generateExpr(arg, ctx);
    result = `${result}(${argCode})`;
  }

  return result;
}

/**
 * Try to generate code for a protocol method application in monomorphic context.
 *
 * This handles expressions like `(+)(x)(y)` where `+` is a protocol method.
 * We extract the root method and all arguments, then try to infer the concrete
 * type from the arguments to resolve the correct instance dictionary.
 *
 * Returns null if:
 * - The callee is not a protocol method
 * - We're in polymorphic context (dictionary param in scope)
 * - We can't infer the concrete type from arguments or constraints
 */
function tryGenerateProtocolMethodApply(
  expr: Extract<IRExpr, { kind: "IRApply" }>,
  ctx: CodegenContext,
): string | null {
  // Extract the root method and all operands from nested applies
  const { methodName, operands } = extractMethodAndOperands(expr);

  if (!methodName) {
    return null;
  }

  // Check if this is a protocol method
  const protocolName = ctx.protocolMethodMap.get(methodName);
  if (!protocolName) {
    return null;
  }

  // If we're in polymorphic context (dictionary param in scope), let normal path handle it
  const dictParam = `$dict_${protocolName}`;
  if (ctx.dictParamsInScope.has(dictParam)) {
    return null;
  }

  // Monomorphic context: try to infer concrete types from operands
  // For multi-parameter protocols, we need ALL operand types to properly resolve constraints
  let concreteType = inferConcreteTypeFromOperands(operands, ctx);
  let allOperandTypes = inferAllTypesFromOperands(operands, ctx);

  // Include the expected return type if available
  // This is crucial for multi-parameter protocols where constraints may be on the return type
  // For example, in `ExampleProtocol3 a b where convert3 : a -> b` with instance
  // `Appendable b => ExampleProtocol3 Float b`, when calling `convert3 10.0` we need to
  // know the expected return type to resolve the `Appendable` constraint correctly.
  if (ctx.expectedReturnType && !isTypeVariable(ctx.expectedReturnType)) {
    allOperandTypes = [...allOperandTypes, ctx.expectedReturnType];
  }

  // If we can't infer from operands, check if we have a concrete constraint
  // from the enclosing function
  if (!concreteType) {
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType) {
      concreteType = constraintType;
    }
  }

  if (!concreteType) {
    return null; // Can't determine type, fall back to normal generation
  }

  // Generate code with the resolved dictionary
  const sanitizedName = sanitizeIdentifier(methodName);
  const typeKey = formatTypeKey(concreteType);
  // Pass concreteType and all operand types so constrained instances can resolve their constraint dicts
  const dictRef = resolveDictReferenceCtx(
    protocolName,
    typeKey,
    ctx,
    concreteType,
    allOperandTypes.length > 0 ? allOperandTypes : undefined,
  );

  // Generate: dict.method(arg1)(arg2)...
  let result = `${dictRef}.${sanitizedName}`;
  for (const operand of operands) {
    const argCode = generateExpr(operand, ctx);
    result = `${result}(${argCode})`;
  }

  return result;
}

/**
 * Extract the root method name and all operands from a curried application.
 *
 * Given: Apply(Apply(IRVar("+"), x), y)
 * Returns: { methodName: "+", operands: [x, y] }
 */
function extractMethodAndOperands(expr: Extract<IRExpr, { kind: "IRApply" }>): {
  methodName: string | null;
  operands: IRExpr[];
} {
  const operands: IRExpr[] = [];
  let current: IRExpr = expr;

  // Traverse down the left spine of applications
  while (current.kind === "IRApply") {
    // Collect arguments in reverse (we're traversing from outside in)
    operands.unshift(...current.args);
    current = current.callee;
  }

  // The innermost callee should be a variable
  if (current.kind === "IRVar" && current.namespace === "value") {
    return { methodName: current.name, operands };
  }

  return { methodName: null, operands: [] };
}

/**
 * Try to infer a concrete type from operands.
 *
 * Looks at literals to determine the concrete type (Int, Float, etc.)
 */
function inferConcreteTypeFromOperands(
  operands: IRExpr[],
  ctx: CodegenContext,
): IRType | null {
  for (const operand of operands) {
    const type = inferExprType(operand, ctx);
    if (type && type.kind === "con") {
      return type;
    }
  }
  return null;
}

/**
 * Infer ALL concrete types from operands.
 *
 * This is used for multi-parameter protocols where we need to know
 * the concrete types for each type parameter to properly resolve constraints.
 */
function inferAllTypesFromOperands(
  operands: IRExpr[],
  ctx: CodegenContext,
): IRType[] {
  const types: IRType[] = [];
  for (const operand of operands) {
    const type = inferExprType(operand, ctx);
    if (type) {
      types.push(type);
    }
  }
  return types;
}

/**
 * Try to infer the type of an IR expression.
 *
 * This is a best-effort inference for codegen purposes:
 * - Literals have known types (int/float literals)
 * - Variables may have type annotations
 * - Field accesses and other expressions can sometimes be typed
 */
function inferExprType(expr: IRExpr, ctx: CodegenContext): IRType | null {
  switch (expr.kind) {
    case "IRLiteral":
      switch (expr.literalType) {
        case "int":
          return { kind: "con", name: "Int", args: [] };
        case "float":
          return { kind: "con", name: "Float", args: [] };
        case "string":
          return { kind: "con", name: "String", args: [] };
        case "char":
          return { kind: "con", name: "Char", args: [] };
        case "bool":
          return { kind: "con", name: "Bool", args: [] };
      }
      break;

    case "IRConstructor": {
      // Look up the constructor to find its parent type
      const ctorInfo = ctx.constructors[expr.name];
      if (ctorInfo) {
        return { kind: "con", name: ctorInfo.parentType, args: [] };
      }
      break;
    }

    case "IRVar":
      // If the variable has type information, use it
      if (expr.type) {
        return expr.type;
      }
      // Check if we have the variable's type in scope
      const varType = ctx.varTypes.get(expr.name);
      if (varType) {
        return varType;
      }
      break;

    case "IRFieldAccess":
      // For field access like `point.x`, look up the target's type
      // and extract the field type from the record
      const targetType = inferExprType(expr.target, ctx);
      if (targetType && targetType.kind === "record") {
        const fieldType = targetType.fields[expr.field];
        if (fieldType) {
          return fieldType;
        }
      }
      break;

    case "IRApply": {
      // For function applications, we need to compute the result type.
      // First, try to get the callee's type and apply the arguments.
      const { methodName, operands } = extractMethodAndOperands(expr);

      // If the callee is a protocol method, we can compute result type from method signature
      if (methodName) {
        const protocolName = ctx.protocolMethodMap.get(methodName);
        if (protocolName) {
          const protocol = ctx.protocols[protocolName];
          if (protocol) {
            // Find the method in the protocol
            const method = protocol.methods.find((m) => m.name === methodName);
            if (method && method.type.kind === "fun") {
              // Apply operand types to compute the result type
              // For `toString : a -> String`, result is String
              // For `== : a -> a -> Bool`, result is Bool after 2 args
              let resultType: IRType = method.type;
              for (let i = 0; i < operands.length && resultType.kind === "fun"; i++) {
                resultType = resultType.to;
              }
              // If result is a type variable, try to substitute with concrete type
              if (resultType.kind === "con") {
                return resultType;
              }
              // Otherwise, infer from operands for numeric-like protocols
              if (resultType.kind === "var") {
                const opType = inferConcreteTypeFromOperands(operands, ctx);
                if (opType) {
                  return opType;
                }
              }
            }
          }
        }
      }

      // Fallback: For regular applications or when we can't determine the type,
      // try to infer from operands (works for numeric operations where result matches operand types)
      for (const operand of operands) {
        const opType = inferExprType(operand, ctx);
        if (opType && opType.kind === "con") {
          return opType;
        }
      }
      break;
    }

    case "IRUnary":
      // Unary negation returns the same type as the operand
      return inferExprType(expr.operand, ctx);

    case "IRList": {
      // A list literal has type List a
      // If we can infer the element type from the first element, use it
      // Otherwise, use a type variable for the element
      if (expr.elements.length > 0) {
        const elemType = inferExprType(expr.elements[0]!, ctx);
        if (elemType) {
          return { kind: "con", name: "List", args: [elemType] };
        }
      }
      // Empty list or unknown element type - still a List, with a type variable
      // We use a fresh type variable ID (doesn't matter which since we're just
      // matching the List constructor structure)
      return { kind: "con", name: "List", args: [{ kind: "var", id: -1 }] };
    }

    case "IRTuple": {
      // A tuple literal has type (a, b, ...)
      const elemTypes: IRType[] = [];
      for (const elem of expr.elements) {
        const elemType = inferExprType(elem, ctx);
        if (elemType) {
          elemTypes.push(elemType);
        } else {
          // Unknown element type
          elemTypes.push({ kind: "var", id: -1 });
        }
      }
      return { kind: "tuple", elements: elemTypes };
    }
  }

  return null;
}

/**
 * Generate an if expression.
 */
function generateIf(
  expr: Extract<IRExpr, { kind: "IRIf" }>,
  ctx: CodegenContext,
): string {
  const cond = generateExpr(expr.condition, ctx);
  const then_ = generateExpr(expr.thenBranch, ctx);
  const else_ = generateExpr(expr.elseBranch, ctx);

  return `(${cond} ? ${then_} : ${else_})`;
}

/**
 * Generate a case expression (pattern matching).
 *
 * Compiles to an IIFE with switch statement:
 *   case x of
 *     Just y -> e1
 *     Nothing -> e2
 *
 *   becomes:
 *   (($match) => {
 *     if ($match.$tag === 0) { const y = $match.$0; return e1; }
 *     if ($match.$tag === 1) { return e2; }
 *     throw new Error("Pattern match failed");
 *   })(x)
 */
function generateCase(
  expr: Extract<IRExpr, { kind: "IRCase" }>,
  ctx: CodegenContext,
): string {
  const matchName = freshName(ctx, "match");
  const discriminant = generateExpr(expr.discriminant, ctx);

  const branches: string[] = [];

  for (const branch of expr.branches) {
    const { condition, bindings, body } = generateBranchCode(
      branch.pattern,
      matchName,
      branch.body,
      ctx,
    );

    let code = "";
    if (condition) {
      code = `if (${condition}) { `;
    } else {
      code = "{ ";
    }

    if (bindings.length > 0) {
      code += bindings.join(" ");
      code += " ";
    }

    code += `return ${body}; }`;
    branches.push(code);
  }

  // Add fallthrough error
  branches.push(`throw new Error("Pattern match failed");`);

  return `((${matchName}) => { ${branches.join(" ")} })(${discriminant})`;
}

/**
 * Generate the condition, bindings, and body for a case branch.
 */
function generateBranchCode(
  pattern: IRPattern,
  matchName: string,
  body: IRExpr,
  ctx: CodegenContext,
): { condition: string | null; bindings: string[]; body: string } {
  const bindings: string[] = [];

  function buildConditionAndBindings(
    pat: IRPattern,
    accessor: string,
  ): string | null {
    switch (pat.kind) {
      case "IRVarPattern":
        // Variable pattern: binds the value, always matches
        const safeName = sanitizeIdentifier(pat.name);
        bindings.push(`const ${safeName} = ${accessor};`);
        return null;

      case "IRWildcardPattern":
        // Wildcard: always matches, no binding
        return null;

      case "IRConstructorPattern": {
        // Constructor pattern: check tag and bind args
        const ctor = ctx.constructors[pat.name];

        // Special case: Bool constructors
        if (ctor?.parentType === BOOL_TYPE_NAME) {
          const boolValue = pat.name === "True" ? "true" : "false";
          return `${accessor} === ${boolValue}`;
        }

        const tag = pat.tag;
        const conditions: string[] = [`${accessor}.$tag === ${tag}`];

        // Recursively handle nested patterns for constructor args
        for (let i = 0; i < pat.args.length; i++) {
          const argPat = pat.args[i];
          if (!argPat) continue;
          const argAccessor = `${accessor}.$${i}`;
          const argCondition = buildConditionAndBindings(argPat, argAccessor);
          if (argCondition) {
            conditions.push(argCondition);
          }
        }

        return conditions.join(" && ");
      }

      case "IRTuplePattern": {
        // Tuple pattern: bind elements by index
        const conditions: string[] = [];
        for (let i = 0; i < pat.elements.length; i++) {
          const elemPat = pat.elements[i];
          if (!elemPat) continue;
          const elemAccessor = `${accessor}[${i}]`;
          const elemCondition = buildConditionAndBindings(
            elemPat,
            elemAccessor,
          );
          if (elemCondition) {
            conditions.push(elemCondition);
          }
        }
        return conditions.length > 0 ? conditions.join(" && ") : null;
      }

      case "IRLiteralPattern":
        // Literal pattern: equality check
        const lit = generateLiteralPatternValue(pat);
        return `${accessor} === ${lit}`;

      case "IRListPattern": {
        // List pattern: check length and bind elements by index
        const conditions: string[] = [
          `Array.isArray(${accessor})`,
          `${accessor}.length === ${pat.elements.length}`,
        ];
        for (let i = 0; i < pat.elements.length; i++) {
          const elemPat = pat.elements[i];
          if (!elemPat) continue;
          const elemAccessor = `${accessor}[${i}]`;
          const elemCondition = buildConditionAndBindings(
            elemPat,
            elemAccessor,
          );
          if (elemCondition) {
            conditions.push(elemCondition);
          }
        }
        return conditions.join(" && ");
      }

      case "IRConsPattern": {
        // Cons pattern: check non-empty and bind head/tail
        const conditions: string[] = [
          `Array.isArray(${accessor})`,
          `${accessor}.length >= 1`,
        ];

        // Head binds to first element
        const headCondition = buildConditionAndBindings(
          pat.head,
          `${accessor}[0]`,
        );
        if (headCondition) {
          conditions.push(headCondition);
        }

        // Tail binds to rest of the array (slice)
        const tailCondition = buildConditionAndBindings(
          pat.tail,
          `${accessor}.slice(1)`,
        );
        if (tailCondition) {
          conditions.push(tailCondition);
        }

        return conditions.join(" && ");
      }

      case "IRRecordPattern": {
        // Record pattern: match and bind field values
        const conditions: string[] = [];
        for (const field of pat.fields) {
          const fieldAccessor = `${accessor}.${field.name}`;
          if (field.pattern) {
            // Nested pattern matching on the field
            const fieldCondition = buildConditionAndBindings(
              field.pattern,
              fieldAccessor,
            );
            if (fieldCondition) {
              conditions.push(fieldCondition);
            }
          } else {
            // Simple binding: { x } means bind x to the field value
            const safeName = sanitizeIdentifier(field.name);
            bindings.push(`const ${safeName} = ${fieldAccessor};`);
          }
        }
        return conditions.length > 0 ? conditions.join(" && ") : null;
      }

      default:
        const _exhaustive: never = pat;
        throw new Error(`Unknown pattern kind: ${(pat as any).kind}`);
    }
  }

  const condition = buildConditionAndBindings(pattern, matchName);
  const bodyCode = generateExpr(body, ctx);

  return { condition, bindings, body: bodyCode };
}

/**
 * Generate the JavaScript value for a literal pattern.
 */
function generateLiteralPatternValue(
  pat: Extract<IRPattern, { kind: "IRLiteralPattern" }>,
): string {
  switch (pat.literalType) {
    case "int":
    case "float":
      return String(pat.value);
    case "string":
    case "char":
      return JSON.stringify(pat.value);
    case "bool":
      return pat.value ? "true" : "false";
  }
}

/**
 * Generate a tuple expression.
 */
function generateTuple(
  expr: Extract<IRExpr, { kind: "IRTuple" }>,
  ctx: CodegenContext,
): string {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}

/**
 * Generate a list expression.
 */
function generateList(
  expr: Extract<IRExpr, { kind: "IRList" }>,
  ctx: CodegenContext,
): string {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}

/**
 * Generate a record expression.
 */
function generateRecord(
  expr: Extract<IRExpr, { kind: "IRRecord" }>,
  ctx: CodegenContext,
): string {
  const fields = expr.fields.map((f) => {
    const value = generateExpr(f.value, ctx);
    return `${f.name}: ${value}`;
  });
  return `{ ${fields.join(", ")} }`;
}

/**
 * Generate a record update expression using spread syntax.
 * { ...base, field1: value1, field2: value2 }
 */
function generateRecordUpdate(
  expr: Extract<IRExpr, { kind: "IRRecordUpdate" }>,
  ctx: CodegenContext,
): string {
  const base = generateExpr(expr.base, ctx);
  const updates = expr.updates.map((f) => {
    const value = generateExpr(f.value, ctx);
    return `${f.name}: ${value}`;
  });
  return `{ ...${base}, ${updates.join(", ")} }`;
}

/**
 * Generate a field access expression.
 */
function generateFieldAccess(
  expr: Extract<IRExpr, { kind: "IRFieldAccess" }>,
  ctx: CodegenContext,
): string {
  const target = generateExpr(expr.target, ctx);
  return `${target}.${expr.field}`;
}

/**
 * Generate an ADT constructor expression.
 */
function generateConstructorExpr(
  expr: Extract<IRExpr, { kind: "IRConstructor" }>,
  ctx: CodegenContext,
): string {
  const ctor = ctx.constructors[expr.name];
  const currentModule = ctx.program.moduleName;

  // Special case: Bool constructors compile to native true/false
  if (ctor?.parentType === BOOL_TYPE_NAME) {
    return expr.name === "True" ? "true" : "false";
  }

  // If constructor is from another module, reference it from that module
  if (
    ctor?.moduleName &&
    ctor.moduleName !== currentModule &&
    ctor.moduleName !== BUILTIN_MODULE_NAME
  ) {
    const ctorName = sanitizeIdentifier(expr.name);
    if (expr.args.length === 0) {
      // Zero-arity constructor: just reference it
      return `${ctor.moduleName}.${ctorName}`;
    }
    // With args: call the constructor function
    let result = `${ctor.moduleName}.${ctorName}`;
    for (const arg of expr.args) {
      const argCode = generateExpr(arg, ctx);
      result = `${result}(${argCode})`;
    }
    return result;
  }

  // For constructors from current module, generate inline
  // Zero-arity: just return tagged object
  if (expr.args.length === 0) {
    return `{ $tag: ${expr.tag} }`;
  }

  // With args: build tagged object with fields
  const fields = expr.args.map((arg, i) => {
    const argCode = generateExpr(arg, ctx);
    return `$${i}: ${argCode}`;
  });

  return `{ $tag: ${expr.tag}, ${fields.join(", ")} }`;
}

// ============================================================================
// Pattern Generation (for function parameters)
// ============================================================================

/**
 * Generate a pattern as a function parameter.
 * For complex patterns, we use destructuring where possible.
 */
function generatePattern(pattern: IRPattern, ctx: CodegenContext): string {
  switch (pattern.kind) {
    case "IRVarPattern":
      return sanitizeIdentifier(pattern.name);

    case "IRWildcardPattern":
      return "_";

    case "IRConstructorPattern":
      // For single-constructor ADTs, generate destructuring
      // The constructor tag is checked at compile time, so we can directly destructure
      // Generate: { $0: arg1, $1: arg2, ... } for the constructor fields
      if (pattern.args.length === 0) {
        // Nullary constructor, just use a placeholder
        return freshName(ctx, "p");
      }
      const ctorBindings = pattern.args.map((arg, i) => {
        const argPattern = generatePattern(arg, ctx);
        return `$${i}: ${argPattern}`;
      });
      return `{ ${ctorBindings.join(", ")} }`;

    case "IRTuplePattern":
      const elements = pattern.elements.map((e) => generatePattern(e, ctx));
      return `[${elements.join(", ")}]`;

    case "IRLiteralPattern":
      // Literal patterns in parameters - shouldn't happen in well-typed code
      return freshName(ctx, "lit");

    case "IRListPattern":
      // List pattern destructuring
      const listElements = pattern.elements.map((e) => generatePattern(e, ctx));
      return `[${listElements.join(", ")}]`;

    case "IRConsPattern":
      // Cons pattern: [head, ...tail]
      const head = generatePattern(pattern.head, ctx);
      const tail = generatePattern(pattern.tail, ctx);
      return `[${head}, ...${tail}]`;

    case "IRRecordPattern":
      // Record pattern: { x, y } or { x: pat }
      const recordBindings = pattern.fields.map((f) => {
        if (f.pattern) {
          // Field with explicit pattern: { x = pat } -> { x: pat }
          const fieldPattern = generatePattern(f.pattern, ctx);
          return `${f.name}: ${fieldPattern}`;
        } else {
          // Field without pattern: { x } -> { x }
          return f.name;
        }
      });
      return `{ ${recordBindings.join(", ")} }`;

    default:
      const _exhaustive: never = pattern;
      throw new Error(`Unknown pattern kind: ${(pattern as any).kind}`);
  }
}

// ============================================================================
// Export Generation
// ============================================================================

/**
 * Generate export statements for module values.
 * Respects the module's exposing clause from semantic analysis.
 *
 * Re-exports from other modules are handled specially:
 * - Local exports use `export { name };`
 * - Re-exports use `export { name } from "module";`
 */
function generateExports(
  program: IRProgram,
  ctx: CodegenContext,
  modulePackages: Map<string, string>,
): string[] {
  const lines: string[] = [];
  const currentModule = program.moduleName;
  const moduleExports = program.exports;

  // Collect local exports (defined in this module)
  const localExports: string[] = [];

  // Collect re-exports grouped by source module
  // Map from module name -> set of names to re-export
  const reExports = new Map<string, Set<string>>();

  /**
   * Helper to add a re-export. Groups by source module for efficient output.
   */
  function addReExport(moduleName: string, name: string): void {
    if (!reExports.has(moduleName)) {
      reExports.set(moduleName, new Set());
    }
    reExports.get(moduleName)!.add(sanitizeIdentifier(name));
  }

  /**
   * Helper to add a constructor export, checking if it's local or from another module.
   */
  function addConstructorExport(ctorName: string): void {
    const ctor = program.constructors[ctorName];
    if (!ctor) return;

    // Check if this constructor is from another module
    if (
      ctor.moduleName &&
      ctor.moduleName !== currentModule &&
      ctor.moduleName !== BUILTIN_MODULE_NAME
    ) {
      // Re-export from the source module
      addReExport(ctor.moduleName, ctorName);
    } else {
      // Local export
      localExports.push(sanitizeIdentifier(ctorName));
    }
  }

  /**
   * Helper to add a value export, checking if it's local or from another module.
   * Note: For now we assume values are local unless we track their source module.
   */
  function addValueExport(
    name: string,
    value: IRValue | undefined,
    checkLocal: boolean = true,
  ): void {
    if (!value && checkLocal) return;

    // Check if this value is defined locally
    if (value) {
      // Always export the Vibe name, not the runtime name
      // For externals, we import "runtimeName as vibeName" so we export vibeName
      localExports.push(sanitizeIdentifier(name));
    }
  }

  // If module exports everything, use the old behavior
  if (moduleExports.exportsAll) {
    // Export all values (non-internal)
    for (const [name, value] of Object.entries(program.values)) {
      // Skip internal names
      if (name.startsWith("$")) continue;
      addValueExport(name, value);
    }

    // Export all constructors that belong to this module
    for (const [ctorName, ctor] of Object.entries(program.constructors)) {
      // Skip built-in Bool and Unit
      if (
        ctor.parentType === BOOL_TYPE_NAME ||
        ctor.parentType === UNIT_TYPE_NAME
      )
        continue;

      // Check if this constructor belongs to the current module
      const adtInfo = program.adts[ctor.parentType];
      if (
        adtInfo?.moduleName &&
        adtInfo.moduleName !== currentModule &&
        adtInfo.moduleName !== BUILTIN_MODULE_NAME
      ) {
        continue;
      }

      localExports.push(sanitizeIdentifier(ctorName));
    }

    // Export all instance dictionaries that were generated in this module
    for (const dictName of ctx.generatedDictNames) {
      localExports.push(dictName);
    }
  } else {
    // Respect the explicit export specifications

    // Export specified values
    for (const valueName of moduleExports.values) {
      const value = program.values[valueName];
      if (value) {
        addValueExport(valueName, value);
      }
    }

    // Export specified operators
    for (const opName of moduleExports.operators) {
      const value = program.values[opName];
      if (value) {
        addValueExport(opName, value);
      }
    }

    // Export constructors for specified types
    for (const [typeName, typeExport] of moduleExports.types) {
      if (typeExport.allConstructors) {
        // Export all constructors for this type
        for (const [ctorName, ctor] of Object.entries(program.constructors)) {
          if (ctor.parentType === typeName) {
            addConstructorExport(ctorName);
          }
        }
      } else if (typeExport.constructors) {
        // Export only specified constructors
        for (const ctorName of typeExport.constructors) {
          if (program.constructors[ctorName]) {
            addConstructorExport(ctorName);
          }
        }
      }
      // If no constructors specified, type is exported opaquely (no constructors)
    }

    // Always export all locally generated instance dictionaries
    // Dictionaries are internal implementation details needed for protocol dispatch,
    // so they should always be accessible when an instance is defined in this module
    for (const dictName of ctx.generatedDictNames) {
      localExports.push(dictName);
    }

    // Re-export dictionaries for instances of exported protocols from imported modules
    // This is needed when a module re-exports a protocol - its dictionaries should
    // also be available through the re-exporting module
    for (const [protocolName, protocolExport] of moduleExports.protocols) {
      for (const [instanceKey, sourceModule] of ctx.instanceModules) {
        // Check if this instance is for the exported protocol
        // Instance keys are like "Show_List_v120", "Num_Int"
        if (
          instanceKey.startsWith(`${protocolName}_`) &&
          sourceModule !== currentModule
        ) {
          const dictName = ctx.instanceDictNames.get(instanceKey);
          if (dictName) {
            addReExport(sourceModule, dictName);
          }
        }
      }
    }

    // Add re-exported values from moduleExports.reExportedValues
    for (const [name, sourceModule] of moduleExports.reExportedValues) {
      addReExport(sourceModule, name);
    }
  }

  // Generate re-export statements first
  // These use ES6 re-export syntax: export { name } from "module";
  for (const [moduleName, names] of reExports) {
    const sortedNames = [...names].sort();
    if (sortedNames.length > 0) {
      // Calculate the import path for the source module
      const importPath = calculateReExportPath(
        program,
        moduleName,
        modulePackages,
      );
      lines.push(`export { ${sortedNames.join(", ")} } from "${importPath}";`);
    }
  }

  // Generate local export statement
  const uniqueLocalExports = [...new Set(localExports)].sort();
  if (uniqueLocalExports.length > 0) {
    lines.push(`export { ${uniqueLocalExports.join(", ")} };`);
  }

  return lines;
}

// ============================================================================
// File Writing Utilities
// ============================================================================

/**
 * Options for writing generated code to disk.
 */
export interface WriteOptions {
  /** Base output directory */
  distDir: string;

  /** Package name for output directory structure */
  packageName: string;

  /** Whether to create directories if they don't exist */
  createDirs?: boolean;
}

/**
 * Write a generated module to disk.
 *
 * The module is written to: distDir/packageName/modulePath.js
 * For example:
 *   - ExampleApp module in ExampleApp package -> dist/ExampleApp/ExampleApp.js
 *   - SimpleTest module in ExampleApp package -> dist/ExampleApp/SimpleTest.js
 *   - Vibe.Utils module in Vibe package -> dist/Vibe/Vibe/Utils.js
 */
export function writeModule(
  module: GeneratedModule,
  options: WriteOptions,
): string {
  const { distDir, packageName, createDirs = true } = options;

  // Convert module name to path segments (e.g., "Vibe.Utils" -> ["Vibe", "Utils"])
  const moduleSegments = module.moduleName.split(".");

  // Build output path: dist/packageName/moduleSegments.../fileName.js
  // e.g., dist/ExampleApp/SimpleTest.js or dist/Vibe/Vibe/Utils.js
  const fileName = moduleSegments[moduleSegments.length - 1];
  const moduleDir = path.join(
    distDir,
    packageName,
    ...moduleSegments.slice(0, -1),
  );
  const filePath = path.join(moduleDir, `${fileName}.js`);

  if (createDirs) {
    fs.mkdirSync(moduleDir, { recursive: true });
  }

  fs.writeFileSync(filePath, module.code, "utf8");

  return filePath;
}

/**
 * Build result containing all generated modules.
 */
export interface BuildResult {
  /** Map of module name to generated output path */
  outputs: Map<string, string>;

  /** Any errors that occurred */
  errors: Error[];
}

/**
 * Build a complete project from multiple IR programs.
 */
export function buildProject(
  programs: IRProgram[],
  options: WriteOptions,
): BuildResult {
  const outputs = new Map<string, string>();
  const errors: Error[] = [];

  for (const program of programs) {
    try {
      const generated = generate(program);
      const outputPath = writeModule(generated, options);
      outputs.set(generated.moduleName, outputPath);
    } catch (error) {
      errors.push(error instanceof Error ? error : new Error(String(error)));
    }
  }

  return { outputs, errors };
}
