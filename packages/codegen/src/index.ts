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
  SCC,
} from "@vibe/ir";

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

  /** Counter for generating unique names */
  uniqueCounter: number;
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
    uniqueCounter: 0,
  };

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

  // Generate instance dictionary names
  for (const inst of program.instances) {
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const key = `${inst.protocolName}_${typeKey}`;
    ctx.instanceDictNames.set(key, `$dict_${key}`);
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
  const exportLines = generateExports(program);
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
    } else if (
      imp.exposing?.kind === "Explicit" &&
      imp.exposing.names.length > 0
    ) {
      // import { specific, names } from "..."
      const names = imp.exposing.names.map((name) => sanitizeIdentifier(name));
      lines.push(`import { ${names.join(", ")} } from "${importPath}";`);
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
    const dictName = ctx.instanceDictNames.get(
      `${inst.protocolName}_${typeKey}`
    );
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
    }
  }

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

  // If value has parameters, wrap body in curried lambdas
  if (value.params.length > 0) {
    let body = generateExpr(value.body, ctx);

    // Wrap in curried functions for each parameter
    for (let i = value.params.length - 1; i >= 0; i--) {
      const param = value.params[i];
      if (!param) continue;
      const paramCode = generatePattern(param, ctx);
      body = `(${paramCode}) => ${body}`;
    }

    return body;
  }

  // No parameters: just generate the body
  return generateExpr(value.body, ctx);
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

  // Check if this is a protocol operator that needs resolution
  //
  // LIMITATION: Currently operators always resolve to their Int/default implementations.
  // Proper type-directed resolution would require:
  // 1. Passing inferred types from semantic analysis through IR to codegen
  // 2. Looking up the appropriate protocol implementation based on operand types
  // 3. Supporting protocol dictionaries for polymorphic usage
  //
  // For now, numeric operators use Int implementations except:
  // - Division (/) uses floatDiv for consistency with mathematical semantics
  // - String operations (++) use append
  //
  // This works for most code but will give incorrect results for Float arithmetic
  // operations other than division. Users needing Float arithmetic should use
  // explicit float operations like floatAdd, floatSub, etc.
  const operatorImpl = OPERATOR_IMPLEMENTATIONS[expr.name];
  if (operatorImpl) {
    // Check if this operator's implementation is available in scope
    const implValue = ctx.program.values[operatorImpl];
    if (implValue?.isExternal) {
      return sanitizeIdentifier(operatorImpl);
    }
    // Otherwise, reference it from Vibe module
    return `Vibe.${sanitizeIdentifier(operatorImpl)}`;
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
 * Default implementations for protocol operators.
 * Maps operator names to their Int/default implementations.
 * This is a temporary solution until proper type-directed resolution.
 */
const OPERATOR_IMPLEMENTATIONS: Record<string, string> = {
  "+": "intAdd",
  "-": "intSub",
  "*": "intMul",
  "/": "floatDiv", // Division defaults to Float
  "//": "intDiv",
  "%": "intMod",
  "==": "intEq",
  "/=": "intNeq",
  "<": "intLt",
  "<=": "intLte",
  ">": "intGt",
  ">=": "intGte",
  "&&": "and",
  "||": "or",
  "++": "append",
  "::": "cons",
  "|>": "pipeForward",
  "<|": "pipeBackward",
  ">>": "composeForward",
  "<<": "composeBackward",
};

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
 */
function generateApply(
  expr: Extract<IRExpr, { kind: "IRApply" }>,
  ctx: CodegenContext
): string {
  const callee = generateExpr(expr.callee, ctx);

  // Apply arguments one at a time (curried)
  let result = callee;
  for (const arg of expr.args) {
    const argCode = generateExpr(arg, ctx);
    result = `${result}(${argCode})`;
  }

  return result;
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
 */
function generateExports(program: IRProgram): string[] {
  const lines: string[] = [];
  const currentModule = program.moduleName;

  // Collect all exportable names
  const exports: string[] = [];

  // Export values (non-internal)
  for (const [name, value] of Object.entries(program.values)) {
    // Skip internal names
    if (name.startsWith("$")) continue;

    // For external declarations, export the external name
    if (value.isExternal && value.externalTarget) {
      exports.push(sanitizeIdentifier(value.externalTarget.exportName));
    } else {
      exports.push(sanitizeIdentifier(name));
    }
  }

  // Export constructors that belong to this module
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

    exports.push(sanitizeIdentifier(ctorName));
  }

  // Remove duplicates and sort
  const uniqueExports = [...new Set(exports)].sort();

  if (uniqueExports.length > 0) {
    lines.push(`export { ${uniqueExports.join(", ")} };`);
  }

  return lines;
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
