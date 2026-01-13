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
import { sanitizeOperator } from "@vibe/syntax";

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

  /** Map from external module path to imported bindings */
  externalBindings: Map<string, Set<string>>;

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
  };

  // Build protocol method map from protocol definitions
  // This maps method names to their protocol names for uniform dictionary dispatch
  for (const [protocolName, protocol] of Object.entries(program.protocols)) {
    for (const method of protocol.methods) {
      ctx.protocolMethodMap.set(method.name, protocolName);
    }
  }

  // Collect external bindings by module
  for (const value of Object.values(program.values)) {
    if (value.isExternal && value.externalTarget) {
      const { modulePath, exportName } = value.externalTarget;
      if (!ctx.externalBindings.has(modulePath)) {
        ctx.externalBindings.set(modulePath, new Set());
      }
      ctx.externalBindings.get(modulePath)!.add(exportName);
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
 * Format a type as a key string for instance lookup.
 */
function formatTypeKey(type: IRType | undefined): string {
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
 * Resolve a dictionary reference for a protocol and concrete type.
 *
 * Returns the fully qualified dictionary name (e.g., "$dict_Num_Int" for local,
 * "Vibe.$dict_Num_Int" for imported from Vibe module).
 *
 * Throws an error if the dictionary cannot be found - this indicates a compiler bug
 * since all instances should be known at compile time.
 */
function resolveDictReference(
  protocolName: string,
  typeKey: string,
  ctx: CodegenContext
): string {
  const key = `${protocolName}_${typeKey}`;

  // Check if it's defined locally in this module
  if (ctx.localInstanceKeys.has(key)) {
    return `$dict_${key}`;
  }

  // Check if it's in the known instances (from imports)
  const sourceModule = ctx.instanceModules.get(key);
  if (sourceModule) {
    // Use the source module's import alias for the dictionary reference
    // The import alias is determined by how the module was imported
    const importAlias = getImportAliasForModule(sourceModule, ctx);
    return `${importAlias}.$dict_${key}`;
  }

  // This is a compiler error - we should never reach here if the semantics
  // phase correctly validated that all protocol constraints are satisfiable
  throw new Error(
    `Codegen error: No instance found for ${protocolName} ${typeKey}. ` +
      `This indicates a bug in the compiler - the semantics phase should have ` +
      `verified that this instance exists.`
  );
}

/**
 * Get the import alias for a module name.
 *
 * Looks up how the module was imported and returns the alias.
 * For example, if "Vibe.JS" was imported as "JS", returns "JS".
 * If no alias is found, returns the last segment of the module name.
 */
function getImportAliasForModule(
  moduleName: string,
  ctx: CodegenContext
): string {
  // Check the import aliases in the program
  for (const alias of ctx.program.importAliases) {
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

  /** External imports required */
  imports: Map<string, Set<string>>;

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
  options: GenerateOptions = {}
): GeneratedModule {
  const ctx = createCodegenContext(program);
  const { modulePackages = new Map() } = options;
  const lines: string[] = [];

  // 1. Generate imports for external modules
  const importLines = generateImports(ctx);
  if (importLines.length > 0) {
    lines.push(...importLines);
    lines.push("");
  }

  // 2. Generate imports for dependency modules
  const depImportLines = generateDependencyImports(program, modulePackages);
  if (depImportLines.length > 0) {
    lines.push(...depImportLines);
    lines.push("");
  }

  // 3. Generate ADT constructor functions
  const ctorLines = generateConstructors(ctx);
  if (ctorLines.length > 0) {
    lines.push("// ADT Constructors");
    lines.push(...ctorLines);
    lines.push("");
  }

  // 4. Generate protocol instance dictionaries
  const dictLines = generateInstanceDictionaries(ctx);
  if (dictLines.length > 0) {
    lines.push("// Protocol Instance Dictionaries");
    lines.push(...dictLines);
    lines.push("");
  }

  // 5. Generate values in dependency order
  lines.push("// Values");
  for (const scc of program.dependencyOrder) {
    const sccLines = generateSCC(scc, ctx);
    lines.push(...sccLines);
  }

  // 6. Generate exports
  const exportLines = generateExports(program, ctx);
  if (exportLines.length > 0) {
    lines.push("");
    lines.push(...exportLines);
  }

  return {
    moduleName: program.moduleName ?? "Main",
    packageName: program.packageName ?? program.moduleName ?? "Main",
    code: lines.join("\n"),
    imports: ctx.externalBindings,
    exports: Object.keys(program.values).filter(
      (name) => !name.startsWith("$")
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
  const lines: string[] = [];

  for (const [modulePath, bindings] of ctx.externalBindings) {
    const bindingList = Array.from(bindings).sort();
    if (bindingList.length > 0) {
      // Use relative path for @vibe/runtime - it will be bundled
      const importPath =
        modulePath === "@vibe/runtime" ? "@vibe/runtime" : modulePath;
      lines.push(`import { ${bindingList.join(", ")} } from "${importPath}";`);
    }
  }

  return lines;
}

/**
 * Generate import statements for dependency modules.
 *
 * The import path is calculated based on:
 * - Current module's package and path depth
 * - Imported module's package and path
 *
 * Examples:
 * - ExampleApp/SimpleTest.js importing Vibe/Vibe.js -> ../Vibe/Vibe.js
 * - ExampleApp/Sub/Module.js importing Vibe/Vibe.js -> ../../Vibe/Vibe.js
 * - ExampleApp/SimpleTest.js importing ExampleApp/ExampleApp.js -> ./ExampleApp.js
 */
function generateDependencyImports(
  program: IRProgram,
  modulePackages: Map<string, string>
): string[] {
  const lines: string[] = [];

  // Get imports from source program
  const imports = program.sourceProgram.imports || [];
  const currentModule = program.moduleName || "Main";
  const currentPackage = program.packageName || currentModule;
  const importedModules = new Set<string>();

  // Calculate the depth of the current module within its package
  // e.g., "SimpleTest" -> 0, "Sub.Module" -> 1
  const currentDepth = currentModule.split(".").length - 1;

  for (const imp of imports) {
    const moduleName = imp.moduleName;
    importedModules.add(moduleName);

    // Get the imported module's package
    const importedPackage = modulePackages.get(moduleName) || moduleName;

    // Calculate relative import path
    const importPath = calculateImportPath(
      currentPackage,
      currentDepth,
      importedPackage,
      moduleName
    );

    if (imp.exposing?.kind === "All") {
      // import * as ModuleName from "..."
      // Use just the base module name (last segment) as the alias
      const alias = moduleName.split(".").pop() || moduleName;
      lines.push(`import * as ${alias} from "${importPath}";`);
    } else if (imp.exposing?.kind === "Explicit") {
      // Extract names from the export specs
      const names = imp.exposing.exports
        .map((spec) => {
          switch (spec.kind) {
            case "ExportValue":
            case "ExportTypeAll":
              return sanitizeIdentifier(spec.name);
            case "ExportOperator":
              return sanitizeOperator(spec.operator);
            case "ExportTypeSome":
              // For ExportTypeSome, we import the type constructor and the specific members
              return sanitizeIdentifier(spec.name);
            default:
              return null;
          }
        })
        .filter((name): name is string => name !== null);

      if (names.length > 0) {
        // import { specific, names } from "..."
        lines.push(`import { ${names.join(", ")} } from "${importPath}";`);
      } else {
        // No explicit imports, import everything (exposing all)
        const alias = moduleName.split(".").pop() || moduleName;
        lines.push(`import * as ${alias} from "${importPath}";`);
      }
    } else {
      // Default: import everything (exposing all)
      const alias = moduleName.split(".").pop() || moduleName;
      lines.push(`import * as ${alias} from "${importPath}";`);
    }
  }

  // Auto-import Vibe if it's a dependency (injected by module resolver)
  // but not explicitly imported
  if (currentModule !== "Vibe" && !importedModules.has("Vibe")) {
    // Check if we have any ADTs/constructors from Vibe
    const hasVibeDeps = Object.values(program.adts).some(
      (adt) => adt.moduleName === "Vibe"
    );
    if (hasVibeDeps) {
      const vibePackage = modulePackages.get("Vibe") || "Vibe";
      const importPath = calculateImportPath(
        currentPackage,
        currentDepth,
        vibePackage,
        "Vibe"
      );
      lines.push(`import * as Vibe from "${importPath}";`);
    }
  }

  return lines;
}

/**
 * Calculate the relative import path between two modules.
 *
 * @param currentPackage - Package of the importing module
 * @param currentDepth - Depth of importing module within its package (0 for top-level)
 * @param importedPackage - Package of the imported module
 * @param importedModule - Full module name of the imported module
 */
function calculateImportPath(
  currentPackage: string,
  currentDepth: number,
  importedPackage: string,
  importedModule: string
): string {
  // Convert module name to path segments
  const moduleSegments = importedModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath =
    moduleSegments.length > 1
      ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js`
      : `${fileName}.js`;

  if (currentPackage === importedPackage) {
    // Same package - use relative path within package
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    // Different package - go up to dist, then into imported package
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${importedPackage}/${modulePath}`;
  }
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
    if (parentType === "Bool") continue;

    // Skip Unit - we use undefined
    if (parentType === "Unit") continue;

    // Check if this ADT belongs to the current module
    const adtInfo = ctx.program.adts[parentType];
    if (
      adtInfo?.moduleName &&
      adtInfo.moduleName !== currentModule &&
      adtInfo.moduleName !== "__builtin__"
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

    const methodEntries: string[] = [];
    for (const [methodName, implName] of Object.entries(inst.methods)) {
      const safeName = sanitizeIdentifier(methodName);
      const safeImpl = sanitizeIdentifier(implName);
      methodEntries.push(`  ${safeName}: ${safeImpl}`);
    }

    if (methodEntries.length > 0) {
      lines.push(`const ${dictName} = {`);
      lines.push(methodEntries.join(",\n"));
      lines.push(`};`);
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
    for (const valueName of scc.values) {
      const safeName = sanitizeIdentifier(valueName);
      lines.push(`let ${safeName};`);
    }
    for (const valueName of scc.values) {
      const value = ctx.program.values[valueName];
      if (!value) continue;
      const safeName = sanitizeIdentifier(valueName);
      const body = generateValue(value, ctx);
      lines.push(`${safeName} = ${body};`);
    }
  } else {
    // Non-recursive: simple const declaration
    for (const valueName of scc.values) {
      const value = ctx.program.values[valueName];
      if (!value) continue;

      // Skip external declarations - they're imported
      if (value.isExternal) continue;

      const safeName = sanitizeIdentifier(valueName);
      const body = generateValue(value, ctx);
      lines.push(`const ${safeName} = ${body};`);
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
  }

  // Save previous context and set new scope
  const prevDictParams = ctx.dictParamsInScope;
  const prevConcreteConstraints = ctx.concreteConstraints;
  const prevVarTypes = ctx.varTypes;
  ctx.dictParamsInScope = new Set(dictParams);
  ctx.concreteConstraints = concreteConstraints;
  ctx.varTypes = varTypes;

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
    return body;
  }

  // No parameters: just generate the body
  const body = generateExpr(value.body, ctx);

  // Restore previous scope
  ctx.dictParamsInScope = prevDictParams;
  ctx.concreteConstraints = prevConcreteConstraints;
  ctx.varTypes = prevVarTypes;
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
 * Regular functions and external bindings are referenced directly by name.
 */
function generateVar(
  expr: Extract<IRExpr, { kind: "IRVar" }>,
  ctx: CodegenContext
): string {
  const currentModule = ctx.program.moduleName;

  // Check if this is an external binding
  const value = ctx.program.values[expr.name];
  if (value?.isExternal && value.externalTarget) {
    return sanitizeIdentifier(value.externalTarget.exportName);
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
        const dictRef = resolveDictReference(protocolName, typeKey, ctx);
        return `${dictRef}.${sanitizedName}`;
      }
    }

    // Try to use the variable's type annotation for monomorphic dispatch
    if (expr.type && expr.type.kind === "fun") {
      const argType = expr.type.from;
      if (argType.kind === "con") {
        const typeKey = formatTypeKey(argType);
        const dictRef = resolveDictReference(protocolName, typeKey, ctx);
        return `${dictRef}.${sanitizedName}`;
      }
    }

    // Try to use concrete constraints from the enclosing function
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType && constraintType.kind === "con") {
      const typeKey = formatTypeKey(constraintType);
      const dictRef = resolveDictReference(protocolName, typeKey, ctx);
      return `${dictRef}.${sanitizedName}`;
    }

    // Fallback: use the dictionary parameter (should have been passed)
    // This handles polymorphic usage where the dict param is expected
    return `${dictParam}.${sanitizedName}`;
  }

  // Check if this is a constructor from another module
  const ctorInfo = ctx.constructors[expr.name];
  if (
    ctorInfo &&
    ctorInfo.moduleName &&
    ctorInfo.moduleName !== currentModule &&
    ctorInfo.moduleName !== "__builtin__"
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
 * and the external name if available.
 *
 * Examples:
 * - `JS.null` with external name `null_` -> `JS.null_`
 * - `Vibe.JS.something` -> `JS.something` (using the import alias)
 */
function generateModuleAccess(
  expr: IRModuleAccess,
  ctx: CodegenContext
): string {
  // Use the external name if available, otherwise use the value name
  const valueName = sanitizeIdentifier(expr.externalName || expr.valueName);
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
 */
function generateLambda(
  expr: Extract<IRExpr, { kind: "IRLambda" }>,
  ctx: CodegenContext
): string {
  const body = generateExpr(expr.body, ctx);

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
  ctx: CodegenContext
): string {
  // Special handling for protocol method applications in monomorphic context
  // When we have Apply(Apply(protocolMethod, arg1), arg2), we need to resolve
  // the dictionary based on the argument types
  const protocolMethodResult = tryGenerateProtocolMethodApply(expr, ctx);
  if (protocolMethodResult !== null) {
    return protocolMethodResult;
  }

  const callee = generateExpr(expr.callee, ctx);

  // Check if the callee is a named function with constraints
  // If so, we need to pass dictionaries before regular arguments
  let dictPasses: string[] = [];
  if (expr.callee.kind === "IRVar") {
    const calleeValue = ctx.program.values[expr.callee.name];
    if (calleeValue && calleeValue.constraints.length > 0) {
      // The callee has constraints - we need to pass dictionaries
      const seenProtocols = new Set<string>();
      for (const constraint of calleeValue.constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);

          // Try to resolve the constraint to a concrete instance
          // by looking at the type arguments in the constraint
          const typeArg = constraint.typeArgs[0];
          const dictName = resolveDictionaryForType(
            constraint.protocolName,
            typeArg,
            ctx
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
  ctx: CodegenContext
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

  // Monomorphic context: try to infer concrete type from operands
  let concreteType = inferConcreteTypeFromOperands(operands, ctx);

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
  const dictRef = resolveDictReference(protocolName, typeKey, ctx);

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
  ctx: CodegenContext
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

    case "IRApply":
      // For applications like `* point.x point.x`, we can infer the result type
      // by looking at the operands. For most numeric operations, the result type
      // matches the operand types.
      const { operands } = extractMethodAndOperands(expr);
      for (const operand of operands) {
        const opType = inferExprType(operand, ctx);
        if (opType && opType.kind === "con") {
          // For numeric operations, the result type matches the operand type
          return opType;
        }
      }
      break;
  }

  return null;
}

/**
 * Resolve a protocol constraint to a dictionary reference.
 *
 * If the type is concrete (e.g., Int), we look up the instance dictionary.
 * If the type is a type variable, we pass through $dict_Protocol (polymorphic).
 */
function resolveDictionaryForType(
  protocolName: string,
  type: IRType | undefined,
  ctx: CodegenContext
): string {
  if (!type) {
    // Unknown type - use polymorphic pass-through
    return `$dict_${protocolName}`;
  }

  // If it's a concrete type, look up the instance dictionary
  if (type.kind === "con") {
    const typeKey = formatTypeKey(type);
    return resolveDictReference(protocolName, typeKey, ctx);
  }

  // Type variable - pass through the polymorphic dictionary
  return `$dict_${protocolName}`;
}

/**
 * Generate an if expression.
 */
function generateIf(
  expr: Extract<IRExpr, { kind: "IRIf" }>,
  ctx: CodegenContext
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
  ctx: CodegenContext
): string {
  const matchName = freshName(ctx, "match");
  const discriminant = generateExpr(expr.discriminant, ctx);

  const branches: string[] = [];

  for (const branch of expr.branches) {
    const { condition, bindings, body } = generateBranchCode(
      branch.pattern,
      matchName,
      branch.body,
      ctx
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
  ctx: CodegenContext
): { condition: string | null; bindings: string[]; body: string } {
  const bindings: string[] = [];

  function buildConditionAndBindings(
    pat: IRPattern,
    accessor: string
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
        if (ctor?.parentType === "Bool") {
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
            elemAccessor
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
            elemAccessor
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
          `${accessor}[0]`
        );
        if (headCondition) {
          conditions.push(headCondition);
        }

        // Tail binds to rest of the array (slice)
        const tailCondition = buildConditionAndBindings(
          pat.tail,
          `${accessor}.slice(1)`
        );
        if (tailCondition) {
          conditions.push(tailCondition);
        }

        return conditions.join(" && ");
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
  pat: Extract<IRPattern, { kind: "IRLiteralPattern" }>
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
  ctx: CodegenContext
): string {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}

/**
 * Generate a list expression.
 */
function generateList(
  expr: Extract<IRExpr, { kind: "IRList" }>,
  ctx: CodegenContext
): string {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}

/**
 * Generate a record expression.
 */
function generateRecord(
  expr: Extract<IRExpr, { kind: "IRRecord" }>,
  ctx: CodegenContext
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
  ctx: CodegenContext
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
  ctx: CodegenContext
): string {
  const target = generateExpr(expr.target, ctx);
  return `${target}.${expr.field}`;
}

/**
 * Generate an ADT constructor expression.
 */
function generateConstructorExpr(
  expr: Extract<IRExpr, { kind: "IRConstructor" }>,
  ctx: CodegenContext
): string {
  const ctor = ctx.constructors[expr.name];
  const currentModule = ctx.program.moduleName;

  // Special case: Bool constructors compile to native true/false
  if (ctor?.parentType === "Bool") {
    return expr.name === "True" ? "true" : "false";
  }

  // If constructor is from another module, reference it from that module
  if (
    ctor?.moduleName &&
    ctor.moduleName !== currentModule &&
    ctor.moduleName !== "__builtin__"
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
      // Constructor patterns in parameters need destructuring
      // For now, use a placeholder and handle in body
      return freshName(ctx, "p");

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
function generateExports(program: IRProgram, ctx: CodegenContext): string[] {
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
      ctor.moduleName !== "__builtin__"
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
    checkLocal: boolean = true
  ): void {
    if (!value && checkLocal) return;

    // Check if this value is defined locally
    if (value) {
      if (value.isExternal && value.externalTarget) {
        localExports.push(sanitizeIdentifier(value.externalTarget.exportName));
      } else {
        localExports.push(sanitizeIdentifier(name));
      }
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
      if (ctor.parentType === "Bool" || ctor.parentType === "Unit") continue;

      // Check if this constructor belongs to the current module
      const adtInfo = program.adts[ctor.parentType];
      if (
        adtInfo?.moduleName &&
        adtInfo.moduleName !== currentModule &&
        adtInfo.moduleName !== "__builtin__"
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

    // Export instance dictionaries for specified protocols
    for (const [protocolName, protocolExport] of moduleExports.protocols) {
      // Export dictionaries for instances of this protocol defined in this module
      for (const dictName of ctx.generatedDictNames) {
        // Dictionary names are like $dict_Num_Int, $dict_Eq_Float
        if (dictName.startsWith(`$dict_${protocolName}_`)) {
          localExports.push(dictName);
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
      const importPath = calculateReExportPath(program, moduleName);
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

/**
 * Calculate the import path for re-exporting from another module.
 * This mirrors the logic in generateDependencyImports.
 */
function calculateReExportPath(
  program: IRProgram,
  targetModule: string
): string {
  const currentModule = program.moduleName || "Main";
  const currentPackage = program.packageName || currentModule;

  // Calculate the depth of the current module within its package
  const currentDepth = currentModule.split(".").length - 1;

  // Find the import for this module to get its package info
  const imports = program.sourceProgram.imports || [];
  let targetPackage = targetModule; // Default: assume module name is package name

  for (const imp of imports) {
    if (imp.moduleName === targetModule) {
      // Could extract package info here if available
      break;
    }
  }

  // Convert module name to path segments
  const moduleSegments = targetModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath =
    moduleSegments.length > 1
      ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js`
      : `${fileName}.js`;

  if (currentPackage === targetPackage) {
    // Same package - use relative path within package
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    // Different package - go up to dist, then into target package
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${targetPackage}/${modulePath}`;
  }
}

// ============================================================================
// Identifier Sanitization
// ============================================================================

/**
 * Sanitize an identifier for JavaScript.
 *
 * Vibe allows operator-like identifiers that need to be converted:
 * - (+) -> _PLUS
 * - (&&) -> _AND_AND
 * - etc.
 */
function sanitizeIdentifier(name: string): string {
  // Handle parenthesized operators
  if (name.startsWith("(") && name.endsWith(")")) {
    name = name.slice(1, -1);
  }

  // Check for operator characters
  if (/^[+\-*/<>=!&|^%:.]+$/.test(name)) {
    return operatorToIdentifier(name);
  }

  // Check for reserved words
  if (RESERVED_WORDS.has(name)) {
    return `$${name}`;
  }

  return name;
}

/**
 * Convert an operator to a valid JavaScript identifier.
 */
function operatorToIdentifier(op: string): string {
  const chars: string[] = [];
  for (const c of op) {
    const mapped = OPERATOR_CHAR_MAP[c];
    if (mapped) {
      chars.push(mapped);
    } else {
      chars.push(`_${c.charCodeAt(0)}_`);
    }
  }
  return `_${chars.join("")}`;
}

/**
 * Map of operator characters to identifier-safe names.
 */
const OPERATOR_CHAR_MAP: Record<string, string> = {
  "+": "PLUS",
  "-": "MINUS",
  "*": "STAR",
  "/": "SLASH",
  "<": "LT",
  ">": "GT",
  "=": "EQ",
  "!": "BANG",
  "&": "AND",
  "|": "OR",
  "^": "CARET",
  "%": "PERCENT",
  ":": "COLON",
  ".": "DOT",
};

/**
 * JavaScript reserved words that need to be escaped.
 */
const RESERVED_WORDS = new Set([
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "null",
  "return",
  "static",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "undefined",
  "var",
  "void",
  "while",
  "with",
  "yield",
]);

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
  options: WriteOptions
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
    ...moduleSegments.slice(0, -1)
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
  options: WriteOptions
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
