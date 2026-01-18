import type {
  Program,
  ValueDeclaration,
  TypeAnnotationDeclaration,
  ExternalDeclaration,
  TypeDeclaration,
  TypeAliasDeclaration,
  OpaqueTypeDeclaration,
  ConstructorVariant,
  TypeExpr,
  Span,
  ImportDeclaration,
  ModuleDeclaration,
  Exposing,
  ExportSpec,
  Expr,
  Pattern,
  ProtocolDeclaration,
  ImplementationDeclaration,
  Constraint as ASTConstraint,
  InfixDeclaration,
  OperatorInfo,
  OperatorRegistry,
} from "@vibe/syntax";
import { BUILTIN_MODULE_NAME } from "@vibe/syntax";

// Re-export errors for external consumers
export { SemanticError, ImplementingProtocolError } from "./errors";
import { SemanticError, ImplementingProtocolError } from "./errors";

// Re-export types for external consumers
export type {
  Type,
  TypeVar,
  TypeCon,
  TypeFun,
  TypeTuple,
  TypeRecord,
  TypeScheme,
  Constraint,
  Scope,
  Substitution,
  ConstructorInfo,
  ADTInfo,
  TypeAliasInfo,
  RecordInfo,
  RecordFieldInfo,
  OpaqueTypeInfo,
  ProtocolInfo,
  ProtocolMethodInfo,
  DefaultMethodImpl,
  InstanceInfo,
  ValueInfo,
  ExportInfo,
  SemanticModule,
  AnalyzeOptions,
  ConstraintContext,
  LookupResult,
  InstanceLookupResult,
  InstantiationResult,
  TypeVarContext,
  AnnotationResult,
  TypeValidationError,
} from "./types";

import type {
  Type,
  TypeVar,
  TypeCon,
  TypeScheme,
  Constraint,
  Scope,
  Substitution,
  ConstructorInfo,
  ADTInfo,
  TypeAliasInfo,
  RecordInfo,
  RecordFieldInfo,
  OpaqueTypeInfo,
  ProtocolInfo,
  ProtocolMethodInfo,
  DefaultMethodImpl,
  InstanceInfo,
  ValueInfo,
  ExportInfo,
  SemanticModule,
  AnalyzeOptions,
  ConstraintContext,
  LookupResult,
  InstanceLookupResult,
  TypeValidationError,
} from "./types";

// Import type utilities
import {
  freshType,
  listType,
  fn,
  fnChain,
  typesEqual,
  applySubstitution,
  applySubstitutionToConstraints,
  getFreeTypeVars,
  getFreeTypeVarsInScope,
  formatType,
  createConstraintContext,
  addConstraint,
  flattenFunctionParams,
  collectTypeVarIds,
  collectTypeVarIdsOrdered,
  applyVarSubstitution,
  applyTypeSubstitution,
} from "./utils";

// Import builtins
import {
  BUILTIN_CONSTRUCTORS,
  INFIX_TYPES,
  BUILTIN_SPAN,
  BUILTIN_OPERATOR_FIXITY,
  initializeBuiltinADTs,
  initializeBuiltinOpaqueTypes,
} from "./builtins";

/**
 * Create a Lambda expression from pattern arguments and a body expression.
 * This is shared between protocol default methods, implementation methods,
 * and implementing clause generation to ensure consistent behavior.
 *
 * @param args - Pattern arguments for the lambda
 * @param body - Body expression
 * @param span - Source location for error reporting
 * @returns A Lambda expression, or the body directly if args is empty
 */
function makeLambda(args: Pattern[], body: Expr, span: Span): Expr {
  if (args.length === 0) {
    return body;
  }
  return {
    kind: "Lambda",
    args,
    body,
    span,
  };
}

/**
 * Module-level constraint context for collecting protocol constraints during type inference.
 * This is reset at the start of analyzing each top-level value declaration,
 * and the collected constraints are attached during generalization.
 *
 * Using a module-level context avoids threading a context parameter through
 * all the recursive analysis functions.
 */
let currentConstraintContext: ConstraintContext = createConstraintContext();

/**
 * Module-level instance registry for validating concrete constraints.
 * This is set at the start of analyzing a module and used during generalization
 * to validate that constraints on concrete types have matching instances.
 */
let currentInstanceRegistry: InstanceInfo[] = [];

/**
 * Set the instance registry for constraint validation.
 */
function setInstanceRegistry(instances: InstanceInfo[]): void {
  currentInstanceRegistry = instances;
}

/**
 * Reset the constraint context for a new value analysis.
 */
function resetConstraintContext(): void {
  currentConstraintContext = createConstraintContext();
}

/**
 * Get the currently collected constraints.
 */
function getCollectedConstraints(): Constraint[] {
  return currentConstraintContext.constraints;
}

/**
 * Eagerly validate pending constraints after unification.
 *
 * This function is called after each application in the Apply case to detect
 * type mismatches early. When a protocol method's return type (a type variable)
 * gets unified with a function type due to over-application, this catches the
 * error immediately rather than waiting until generalization.
 *
 * The key insight: if a type variable from a protocol constraint gets unified
 * to a concrete type structure (function, tuple, etc.) that no instance could
 * ever match, we should report it as a type mismatch rather than a missing
 * instance error.
 *
 * @param substitution - Current type substitution
 * @param span - Source location for error reporting
 * @param expectedNonFunctionType - If provided, the type that was expected to not be a function
 */
function validateConstraintsEagerly(
  substitution: Substitution,
  span: Span,
  expectedNonFunctionType?: Type,
): void {
  const constraints = getCollectedConstraints();
  if (constraints.length === 0) return;

  for (const c of constraints) {
    // Apply substitution to get the current resolved type args
    const resolvedTypeArgs = c.typeArgs.map((t) =>
      applySubstitution(t, substitution),
    );

    // Check if any type arg has become a concrete non-variable type
    // (meaning it's no longer polymorphic and can be validated now)
    const hasConcreteNonVarArg = resolvedTypeArgs.some((t) => {
      // A type is "concrete" for validation if it has a known shape
      // that isn't just a type variable
      if (t.kind === "var") return false;
      if (t.kind === "fun") return true; // Function types are concrete structures
      if (t.kind === "tuple") return true;
      if (t.kind === "record") return true;
      // For type constructors, check if they have any unresolved type variables
      if (t.kind === "con") {
        // If all args are resolved (no free vars), it's fully concrete
        // Otherwise, it's partially concrete but may still need validation
        // For now, we focus on function types as those are the most common source
        // of over-application errors
        return false;
      }
      return false;
    });

    if (!hasConcreteNonVarArg) continue;

    // Check if this constraint involves a function type that can't be satisfied
    const resolvedConstraint: Constraint = {
      protocolName: c.protocolName,
      typeArgs: resolvedTypeArgs,
    };

    const satisfiability = checkConstraintSatisfiability(
      resolvedConstraint,
      currentInstanceRegistry,
    );

    if (!satisfiability.possible) {
      // Find which type arg is the problematic function type
      for (const typeArg of resolvedTypeArgs) {
        if (typeArg.kind === "fun") {
          // This is the key: produce a type mismatch error, not an instance error
          // The real problem is that the user is trying to apply a non-function value
          // to more arguments than its type allows.
          //
          // When `convert3 10.0 []` is written with convert3 : a -> b,
          // the `b` gets unified with `List t -> result` because we're trying
          // to apply (convert3 10.0) to another argument [].
          // But the actual instance returns List Int (not a function).
          //
          // We format the error to show what type was expected vs what we tried
          // to use it as (a function type).
          throw new SemanticError(
            `Type mismatch: cannot unify '${formatType(
              typeArg.from,
            )}' with '${formatType(typeArg)}'`,
            span,
          );
        }
      }
      // Fallback: generic type mismatch for non-function cases
      const typeArgsStr = resolvedTypeArgs.map((t) => formatType(t)).join(", ");
      throw new SemanticError(
        `Type mismatch: expression cannot be used as a function (constraint '${c.protocolName}' on '${typeArgsStr}' cannot be satisfied)`,
        span,
      );
    }
  }
}

/**
 * Scope maintains a symbol table mapping names to their type schemes.
 * Type schemes enable let-polymorphism: bindings can be polymorphic,
 * and each use site gets a fresh instantiation of the type.
 *
 * NOTE: This local alias is used since Scope is also imported from types.ts
 * but we use it differently in places here. This will be cleaned up in a future refactor.
 */

/**
 * Primitive type constants are no longer defined here.
 * All types including Int, Float, String, Char, Bool, Unit come from the prelude as ADTs.
 *
 * The semantics phase resolves these types by looking them up in the ADT registry
 * which is populated from the prelude.
 */

// ===== Algebraic Data Type (ADT) Registry =====
// The ADT registry tracks user-defined types and their constructors.
// This enables proper type checking and exhaustiveness analysis.

/**
 * Compute the expected module name from a file path and source directory.
 *
 * For example:
 * - filePath: /project/src/Main.vibe, srcDir: /project/src -> "Main"
 * - filePath: /project/src/Data/List.vibe, srcDir: /project/src -> "Data.List"
 *
 * @param filePath - Absolute path to the source file
 * @param srcDir - Absolute path to the source directory
 * @returns Expected module name
 */
function computeExpectedModuleName(filePath: string, srcDir: string): string {
  // Normalize paths to handle different separators
  const normalizedFilePath = filePath.replace(/\\/g, "/");
  const normalizedSrcDir = srcDir.replace(/\\/g, "/").replace(/\/$/, "");

  // Get relative path from srcDir
  if (!normalizedFilePath.startsWith(normalizedSrcDir + "/")) {
    // Fallback: extract from file name only
    const fileName = normalizedFilePath.split("/").pop() ?? "";
    return fileName.replace(/\.vibe$/, "");
  }

  // Remove srcDir prefix and .vibe extension
  const relativePath = normalizedFilePath
    .slice(normalizedSrcDir.length + 1) // +1 for the trailing slash
    .replace(/\.vibe$/, "");

  // Convert path separators to dots for module name
  return relativePath.replace(/\//g, ".");
}

/**
 * Check for type name collision when importing.
 * Returns true if there's a conflict (same name, different module).
 */
function checkTypeCollision(
  name: string,
  importingFrom: string,
  existing: { moduleName?: string } | undefined,
  span: Span,
  kind: "type" | "type alias" | "protocol",
): void {
  if (!existing) return;

  // If the existing definition is from a different module, that's a conflict
  // (unless it's a builtin which has no module)
  if (
    existing.moduleName !== undefined &&
    existing.moduleName !== importingFrom
  ) {
    throw new SemanticError(
      `${
        kind === "type"
          ? "Type"
          : kind === "type alias"
            ? "Type alias"
            : "Protocol"
      } '${name}' conflicts with ${kind} from module '${
        existing.moduleName
      }'. ` + `Consider using qualified imports or aliasing one of them.`,
      span,
    );
  }
}

// ============================================================================
// Strongly Connected Components (SCC) for Mutual Recursion
// ============================================================================

/**
 * Collect all free variable references in an expression.
 * These are variables that are not bound within the expression itself.
 */
function collectFreeVars(
  expr: Expr,
  bound: Set<string> = new Set(),
): Set<string> {
  const free = new Set<string>();

  function visit(e: Expr, localBound: Set<string>): void {
    switch (e.kind) {
      case "Var":
        if (!localBound.has(e.name)) {
          free.add(e.name);
        }
        break;
      case "Number":
      case "String":
      case "Char":
      case "Unit":
        break;
      case "Paren":
        visit(e.expression, localBound);
        break;
      case "Tuple":
        e.elements.forEach((el) => visit(el, localBound));
        break;
      case "List":
        e.elements.forEach((el) => visit(el, localBound));
        break;
      case "ListRange":
        visit(e.start, localBound);
        visit(e.end, localBound);
        break;
      case "Record":
        e.fields.forEach((f) => visit(f.value, localBound));
        break;
      case "RecordUpdate":
        if (!localBound.has(e.base)) {
          free.add(e.base);
        }
        e.fields.forEach((f) => visit(f.value, localBound));
        break;
      case "FieldAccess":
        visit(e.target, localBound);
        break;
      case "Apply":
        visit(e.callee, localBound);
        e.args.forEach((arg) => visit(arg, localBound));
        break;
      case "Infix":
        visit(e.left, localBound);
        visit(e.right, localBound);
        if (!localBound.has(e.operator)) {
          free.add(e.operator);
        }
        break;
      case "Unary":
        visit(e.operand, localBound);
        break;
      case "Lambda": {
        const lambdaBound = new Set(localBound);
        e.args.forEach((p) => collectPatternVars(p, lambdaBound));
        visit(e.body, lambdaBound);
        break;
      }
      case "LetIn": {
        const letBound = new Set(localBound);
        for (const binding of e.bindings) {
          // Collect from binding body with current scope
          visit(binding.body, letBound);
          // Add binding name to scope for subsequent bindings and main body
          letBound.add(binding.name);
          binding.args.forEach((p) => collectPatternVars(p, letBound));
        }
        visit(e.body, letBound);
        break;
      }
      case "If":
        visit(e.condition, localBound);
        visit(e.thenBranch, localBound);
        visit(e.elseBranch, localBound);
        break;
      case "Case":
        visit(e.discriminant, localBound);
        for (const branch of e.branches) {
          const branchBound = new Set(localBound);
          collectPatternVars(branch.pattern, branchBound);
          visit(branch.body, branchBound);
        }
        break;
    }
  }

  visit(expr, bound);
  return free;
}

/**
 * Collect all variables bound by a pattern into the given set.
 */
function collectPatternVars(pattern: Pattern, bound: Set<string>): void {
  switch (pattern.kind) {
    case "VarPattern":
      bound.add(pattern.name);
      break;
    case "WildcardPattern":
      break;
    case "ConstructorPattern":
      pattern.args.forEach((p) => collectPatternVars(p, bound));
      break;
    case "TuplePattern":
      pattern.elements.forEach((p) => collectPatternVars(p, bound));
      break;
    case "ListPattern":
      pattern.elements.forEach((p) => collectPatternVars(p, bound));
      break;
    case "ConsPattern":
      collectPatternVars(pattern.head, bound);
      collectPatternVars(pattern.tail, bound);
      break;
  }
}

/**
 * Build a dependency graph for value declarations.
 * Returns a map from each name to the set of names it references.
 */
function buildDependencyGraph(
  values: Record<string, ValueInfo>,
): Map<string, Set<string>> {
  const graph = new Map<string, Set<string>>();
  const valueNames = new Set(Object.keys(values));

  for (const [name, info] of Object.entries(values)) {
    const deps = new Set<string>();

    if (info.declaration.kind === "ValueDeclaration") {
      // Collect free variables from the body
      const bound = new Set<string>();
      info.declaration.args.forEach((p) => collectPatternVars(p, bound));
      const freeVars = collectFreeVars(info.declaration.body, bound);

      // Filter to only include names that are defined in this module
      for (const v of freeVars) {
        if (valueNames.has(v) && v !== name) {
          deps.add(v);
        }
      }
    }

    graph.set(name, deps);
  }

  return graph;
}

/**
 * Compute strongly connected components using Tarjan's algorithm.
 * Returns SCCs in reverse topological order (dependencies before dependents).
 */
function computeSCCs(graph: Map<string, Set<string>>): string[][] {
  const index = new Map<string, number>();
  const lowlink = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: string[] = [];
  const sccs: string[][] = [];
  let currentIndex = 0;

  function strongConnect(v: string): void {
    index.set(v, currentIndex);
    lowlink.set(v, currentIndex);
    currentIndex++;
    stack.push(v);
    onStack.add(v);

    const deps = graph.get(v) ?? new Set();
    for (const w of deps) {
      if (!index.has(w)) {
        strongConnect(w);
        lowlink.set(v, Math.min(lowlink.get(v)!, lowlink.get(w)!));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v)!, index.get(w)!));
      }
    }

    if (lowlink.get(v) === index.get(v)) {
      const scc: string[] = [];
      let w: string;
      do {
        w = stack.pop()!;
        onStack.delete(w);
        scc.push(w);
      } while (w !== v);
      sccs.push(scc);
    }
  }

  for (const v of graph.keys()) {
    if (!index.has(v)) {
      strongConnect(v);
    }
  }

  // SCCs are returned in reverse topological order
  return sccs;
}

/**
 * Built-in operator type signatures are now EMPTY.
 *
 * All operators must be defined via protocols and implementations
 * in user code or the prelude. This enables a clean separation where
 * the compiler handles syntax/semantics while the prelude provides
 * standard operators.
 *
 * Example prelude definitions:
 *   protocol Num a where
 *     (+) : a -> a -> a
 *     (-) : a -> a -> a
 *     (*) : a -> a -> a
 *
 *   implement Num Int where
 *     (+) = intAdd
 *     (-) = intSub
 *     (*) = intMul
 */

/**
 * Helper function to check if an item is exported from a module.
 * Returns true if the item is exported, false otherwise.
 */
function isExportedFromModule(
  depModule: SemanticModule,
  itemName: string,
  itemKind:
    | "value"
    | "operator"
    | "type"
    | "constructor"
    | "protocol"
    | "method",
): boolean {
  const exports = depModule.exports;

  // If module exports everything, all items are available
  if (exports.exportsAll) {
    return true;
  }

  switch (itemKind) {
    case "value":
      return exports.values.has(itemName);
    case "operator":
      return exports.operators.has(itemName) || exports.values.has(itemName);
    case "type":
      return exports.types.has(itemName);
    case "constructor": {
      // Check if any type exports this constructor
      for (const [, typeExport] of exports.types) {
        if (typeExport.constructors?.has(itemName)) {
          return true;
        }
      }
      return false;
    }
    case "protocol":
      return exports.protocols.has(itemName);
    case "method": {
      // Check if any protocol exports this method
      for (const [, protocolExport] of exports.protocols) {
        if (protocolExport.methods?.has(itemName)) {
          return true;
        }
      }
      return false;
    }
  }
}

/**
 * Import a single export specification from a dependency module.
 */
function importExportSpec(
  spec: ExportSpec,
  depModule: SemanticModule,
  imp: ImportDeclaration,
  globalScope: Scope,
  substitution: Substitution,
  constructors: Record<string, ConstructorInfo>,
  constructorTypes: Record<string, TypeScheme>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  protocols: Record<string, ProtocolInfo>,
  operators: OperatorRegistry,
  importedValues: Map<string, string>,
  typeSchemes: Record<string, TypeScheme>,
): void {
  switch (spec.kind) {
    case "ExportValue": {
      const name = spec.name;

      // First check if it's actually exported
      if (!isExportedFromModule(depModule, name, "value")) {
        // Check if it's a type, type alias, opaque type, or protocol
        if (
          isExportedFromModule(depModule, name, "type") &&
          depModule.adts[name]
        ) {
          // Import the ADT without constructors
          const depADT = depModule.adts[name]!;
          checkTypeCollision(
            name,
            imp.moduleName,
            adts[name],
            imp.span,
            "type",
          );
          adts[name] = depADT;
          return;
        }
        if (depModule.typeAliases[name]) {
          checkTypeCollision(
            name,
            imp.moduleName,
            typeAliases[name],
            imp.span,
            "type alias",
          );
          typeAliases[name] = depModule.typeAliases[name]!;
          return;
        }
        if (depModule.opaqueTypes[name]) {
          opaqueTypes[name] = depModule.opaqueTypes[name]!;
          return;
        }
        if (
          isExportedFromModule(depModule, name, "protocol") &&
          depModule.protocols[name]
        ) {
          // Import the protocol without methods
          checkTypeCollision(
            name,
            imp.moduleName,
            protocols[name],
            imp.span,
            "protocol",
          );
          protocols[name] = depModule.protocols[name]!;
          return;
        }
        // If nothing matched, the item is not exported
        throw new SemanticError(
          `Cannot import '${name}' from module '${imp.moduleName}' - it is not exported`,
          spec.span,
        );
      }

      // Import value
      const depValue = depModule.values[name];
      if (depValue && depValue.type) {
        const importedType = depValue.type;
        const scheme = generalize(importedType, globalScope, substitution);
        globalScope.symbols.set(name, scheme);
        // Track for re-export support
        importedValues.set(name, imp.moduleName);
      }

      // Also check for constructors (might be re-exported)
      if (isExportedFromModule(depModule, name, "constructor")) {
        const depConstructor = depModule.constructors[name];
        if (depConstructor) {
          constructors[name] = depConstructor;
          const ctorScheme = depModule.constructorTypes[name];
          if (ctorScheme) {
            globalScope.symbols.set(name, ctorScheme);
            constructorTypes[name] = ctorScheme;
          }
        }
      }
      break;
    }

    case "ExportOperator": {
      const op = spec.operator;

      // Check if operator is exported
      if (!isExportedFromModule(depModule, op, "operator")) {
        throw new SemanticError(
          `Cannot import operator '${op}' from module '${imp.moduleName}' - it is not exported`,
          spec.span,
        );
      }

      // Import operator value if it exists
      const depValue = depModule.values[op];
      if (depValue && depValue.type) {
        const importedType = depValue.type;
        const scheme = generalize(importedType, globalScope, substitution);
        globalScope.symbols.set(op, scheme);
      }

      // Import operator info if it exists
      const opInfo = depModule.operators.get(op);
      if (opInfo) {
        operators.set(op, opInfo);
      }
      break;
    }

    case "ExportTypeAll": {
      const name = spec.name;

      // Check if it's an ADT
      const depADT = depModule.adts[name];
      if (depADT) {
        if (!isExportedFromModule(depModule, name, "type")) {
          throw new SemanticError(
            `Cannot import type '${name}(..)' from module '${imp.moduleName}' - it is not exported`,
            spec.span,
          );
        }

        checkTypeCollision(name, imp.moduleName, adts[name], imp.span, "type");
        adts[name] = depADT;

        // Import all constructors
        for (const ctorName of depADT.constructors) {
          const ctor = depModule.constructors[ctorName];
          if (ctor) {
            constructors[ctorName] = ctor;
            const ctorScheme = depModule.constructorTypes[ctorName];
            if (ctorScheme) {
              globalScope.symbols.set(ctorName, ctorScheme);
              constructorTypes[ctorName] = ctorScheme;
            }
          }
        }
        return;
      }

      // Check if it's a protocol
      const depProtocol = depModule.protocols[name];
      if (depProtocol) {
        if (!isExportedFromModule(depModule, name, "protocol")) {
          throw new SemanticError(
            `Cannot import protocol '${name}(..)' from module '${imp.moduleName}' - it is not exported`,
            spec.span,
          );
        }

        checkTypeCollision(
          name,
          imp.moduleName,
          protocols[name],
          imp.span,
          "protocol",
        );
        protocols[name] = depProtocol;

        // Add all protocol methods to scope
        const methodSchemes = addProtocolMethodsToScope(
          depProtocol,
          globalScope,
        );
        // Store in typeSchemes for LSP hover/completion
        for (const [methodName, scheme] of methodSchemes) {
          typeSchemes[methodName] = scheme;
        }
        return;
      }

      throw new SemanticError(
        `Cannot import '${name}(..)' from module '${imp.moduleName}' - it is not a type or protocol`,
        spec.span,
      );
    }

    case "ExportTypeSome": {
      const name = spec.name;
      const members = spec.members;

      // Check if it's an ADT with specific constructors
      const depADT = depModule.adts[name];
      if (depADT) {
        if (!isExportedFromModule(depModule, name, "type")) {
          throw new SemanticError(
            `Cannot import type '${name}' from module '${imp.moduleName}' - it is not exported`,
            spec.span,
          );
        }

        checkTypeCollision(name, imp.moduleName, adts[name], imp.span, "type");
        adts[name] = depADT;

        // Import specific constructors
        for (const ctorName of members) {
          if (!depADT.constructors.includes(ctorName)) {
            throw new SemanticError(
              `Constructor '${ctorName}' is not defined in type '${name}'`,
              spec.span,
            );
          }
          const ctor = depModule.constructors[ctorName];
          if (ctor) {
            constructors[ctorName] = ctor;
            const ctorScheme = depModule.constructorTypes[ctorName];
            if (ctorScheme) {
              globalScope.symbols.set(ctorName, ctorScheme);
              constructorTypes[ctorName] = ctorScheme;
            }
          }
        }
        return;
      }

      // Check if it's a protocol with specific methods
      const depProtocol = depModule.protocols[name];
      if (depProtocol) {
        if (!isExportedFromModule(depModule, name, "protocol")) {
          throw new SemanticError(
            `Cannot import protocol '${name}' from module '${imp.moduleName}' - it is not exported`,
            spec.span,
          );
        }

        checkTypeCollision(
          name,
          imp.moduleName,
          protocols[name],
          imp.span,
          "protocol",
        );
        protocols[name] = depProtocol;

        // Add specific protocol methods to scope
        for (const methodName of members) {
          const methodInfo = depProtocol.methods.get(methodName);
          if (!methodInfo) {
            throw new SemanticError(
              `Method '${methodName}' is not defined in protocol '${name}'`,
              spec.span,
            );
          }

          // Create constrained type scheme for the method
          const constraintTypeVar: Type = freshType();
          const methodType = methodInfo.type;
          const scheme: TypeScheme = {
            vars: new Set([
              constraintTypeVar.kind === "var" ? constraintTypeVar.id : -1,
            ]),
            constraints: [
              { protocolName: name, typeArgs: [constraintTypeVar] },
            ],
            type: methodType,
          };
          globalScope.symbols.set(methodName, scheme);
        }
        return;
      }

      throw new SemanticError(
        `Cannot import '${name}(...)' from module '${imp.moduleName}' - it is not a type or protocol`,
        spec.span,
      );
    }
  }
}

export function analyze(
  program: Program,
  options: AnalyzeOptions = {},
): SemanticModule {
  const { fileContext } = options;

  // ===== Module Declaration Validation =====
  // Every Vibe file MUST have a module declaration as the first statement (enforced by parser).
  // When fileContext is provided, validate that the declared module name matches the file's path.
  if (fileContext) {
    const { filePath, srcDir } = fileContext;

    // Compute expected module name from file path
    const expectedModuleName = computeExpectedModuleName(filePath, srcDir);

    const declaredModuleName = program.module.name;
    if (declaredModuleName !== expectedModuleName) {
      throw new SemanticError(
        `Module name '${declaredModuleName}' does not match file path.\n` +
          `Expected: module ${expectedModuleName} exposing (..)\n` +
          `File path: ${filePath}`,
        program.module.span,
      );
    }
  }

  const values: Record<string, ValueInfo> = {};
  const annotations: Record<string, TypeAnnotationDeclaration> = {};
  const types: Record<string, Type> = {};
  const typeSchemes: Record<string, TypeScheme> = {};
  const substitution: Substitution = new Map();

  // Extract the current module name for qualified naming
  const currentModuleName = program.module?.name;

  // ===== ADT and Type Alias Registries =====
  // These track user-defined types for constructor validation and exhaustiveness checking.
  const adts: Record<string, ADTInfo> = {};
  const constructors: Record<string, ConstructorInfo> = {};
  const constructorTypes: Record<string, TypeScheme> = {};
  const typeAliases: Record<string, TypeAliasInfo> = {};
  const records: Record<string, RecordInfo> = {};

  // ===== Opaque Type Registry =====
  // These track opaque types for JS interop (no pattern matching or record updates allowed)
  const opaqueTypes: Record<string, OpaqueTypeInfo> = {};

  // ===== Protocol and Instance Registries =====
  // These track type class definitions and their implementations.
  const protocols: Record<string, ProtocolInfo> = {};
  const instances: InstanceInfo[] = [];
  // Track instances defined in THIS module (not imported) for validation.
  // We only need to validate local instances since imported instances were
  // already validated when their defining module was analyzed.
  const localInstances: InstanceInfo[] = [];

  // ===== Operator Registries =====
  // These track custom operator precedence and associativity declarations.
  const operators: OperatorRegistry = new Map();
  const infixDeclarations: InfixDeclaration[] = [];

  // ===== Imported Values Registry =====
  // Track values imported from other modules for re-export support.
  // Maps value name -> source module name.
  const importedValues = new Map<string, string>();

  // ===== Initialize Builtin Types =====
  // These primitive types are built into the compiler and automatically available.
  initializeBuiltinADTs(adts, constructors, constructorTypes);
  initializeBuiltinOpaqueTypes(opaqueTypes);

  const { dependencies = new Map() } = options;

  // Build the effective imports list
  const imports: ImportDeclaration[] = program.imports ?? [];

  validateImports(imports);

  const globalScope: Scope = { symbols: new Map() };

  // Add built-in Bool constructors (True/False) to global scope
  globalScope.symbols.set("True", constructorTypes["True"]!);
  globalScope.symbols.set("False", constructorTypes["False"]!);

  // Seed built-in operators as functions for prefix/infix symmetry.
  // Built-in operators are monomorphic (not polymorphic).
  // Short-circuit operators (&& and ||) are now included here.
  for (const [op, ty] of Object.entries(INFIX_TYPES)) {
    globalScope.symbols.set(op, { vars: new Set(), constraints: [], type: ty });
  }

  // Seed built-in operator fixities (precedence and associativity).
  // These are for short-circuit operators that are built into the compiler.
  for (const [op, fixity] of Object.entries(BUILTIN_OPERATOR_FIXITY)) {
    operators.set(op, fixity);
  }

  // Merge types from imported dependency modules
  // This replaces the previous approach of seeding placeholder types for imports.
  // Now we use actual types from pre-analyzed dependency modules.
  for (const imp of imports) {
    const depModule = dependencies.get(imp.moduleName);
    if (!depModule) {
      // If dependency not provided, seed with placeholder (for backward compatibility)
      if (imp.alias) {
        globalScope.symbols.set(imp.alias, {
          vars: new Set(),
          constraints: [],
          type: freshType(),
        });
      }
      continue;
    }

    // IMPORTANT: Always import protocols and instances from dependencies
    // Protocols and instances are "global" - they don't respect exposing clauses
    // This matches Haskell's behavior where type class instances are always visible
    for (const [name, protocol] of Object.entries(depModule.protocols) as [
      string,
      ProtocolInfo,
    ][]) {
      protocols[name] = protocol;

      // Also add protocol methods to global scope
      // Protocol methods are polymorphic functions with constraints
      const methodSchemes = addProtocolMethodsToScope(protocol, globalScope);
      // Store in typeSchemes for LSP hover/completion
      for (const [methodName, scheme] of methodSchemes) {
        typeSchemes[methodName] = scheme;
      }
    }

    for (const instance of depModule.instances) {
      instances.push(instance);
    }

    // Handle import alias (e.g., `import Html as H`)
    if (imp.alias) {
      // Register the alias in scope for qualified name access (e.g., H.div).
      // The type is a placeholder since module namespaces aren't first-class values -
      // actual qualified access is resolved specially in tryResolveModuleFieldAccess.
      globalScope.symbols.set(imp.alias, {
        vars: new Set(),
        constraints: [],
        type: freshType(), // Placeholder: module namespaces resolved via tryResolveModuleFieldAccess
      });
    }

    // Handle unaliased imports (e.g., `import Vibe.JS`)
    // Register the module path so it can be accessed via qualified names like Vibe.JS.null
    if (!imp.alias) {
      // Extract the first component of the module name (e.g., "Vibe" from "Vibe.JS")
      const moduleParts = imp.moduleName.split(".");
      const rootModule = moduleParts[0]!;

      // Check if we haven't already registered this module root
      if (!globalScope.symbols.has(rootModule)) {
        globalScope.symbols.set(rootModule, {
          vars: new Set(),
          constraints: [],
          type: freshType(), // Placeholder: module namespaces resolved via tryResolveModuleFieldAccess
        });
      }
    }

    // Handle explicit exposing with new ExportSpec format
    if (imp.exposing?.kind === "Explicit") {
      for (const spec of imp.exposing.exports) {
        importExportSpec(
          spec,
          depModule,
          imp,
          globalScope,
          substitution,
          constructors,
          constructorTypes,
          adts,
          typeAliases,
          opaqueTypes,
          protocols,
          operators,
          importedValues,
          typeSchemes,
        );
      }
    }

    // Handle exposing all (e.g., `import Html exposing (..)`)
    if (imp.exposing?.kind === "All") {
      // Import all exported values
      for (const [name, depValue] of Object.entries(depModule.values) as [
        string,
        ValueInfo,
      ][]) {
        if (depValue.type && isExportedFromModule(depModule, name, "value")) {
          const importedType = depValue.type;
          const scheme = generalize(importedType, globalScope, substitution);
          globalScope.symbols.set(name, scheme);
          // Track for re-export support
          importedValues.set(name, imp.moduleName);
        }
      }

      // Import all exported constructors
      for (const [name, ctor] of Object.entries(depModule.constructors) as [
        string,
        ConstructorInfo,
      ][]) {
        if (isExportedFromModule(depModule, name, "constructor")) {
          constructors[name] = ctor;
          // Also import the type scheme so the constructor can be used as a value
          const ctorScheme = depModule.constructorTypes[name];
          if (ctorScheme) {
            globalScope.symbols.set(name, ctorScheme);
            constructorTypes[name] = ctorScheme;
          }
        }
      }

      // Import all exported ADTs
      for (const [name, adt] of Object.entries(depModule.adts) as [
        string,
        ADTInfo,
      ][]) {
        if (isExportedFromModule(depModule, name, "type")) {
          // Check for collision with existing ADT from different module
          checkTypeCollision(
            name,
            imp.moduleName,
            adts[name],
            imp.span,
            "type",
          );
          adts[name] = adt;
        }
      }

      // Import all exported type aliases
      for (const [name, alias] of Object.entries(depModule.typeAliases) as [
        string,
        TypeAliasInfo,
      ][]) {
        if (isExportedFromModule(depModule, name, "type")) {
          // Check for collision with existing type alias from different module
          checkTypeCollision(
            name,
            imp.moduleName,
            typeAliases[name],
            imp.span,
            "type alias",
          );
          typeAliases[name] = alias;
        }
      }

      // Import all exported opaque types
      for (const [name, opaque] of Object.entries(depModule.opaqueTypes) as [
        string,
        OpaqueTypeInfo,
      ][]) {
        if (isExportedFromModule(depModule, name, "type")) {
          // Check for collision with existing opaque type from different module
          // Allow builtin types to be shadowed by imports (e.g., prelude can re-export Int)
          // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
          if (
            Object.hasOwn(opaqueTypes, name) &&
            opaqueTypes[name]!.moduleName !== BUILTIN_MODULE_NAME &&
            opaqueTypes[name]!.moduleName !== imp.moduleName
          ) {
            throw new SemanticError(
              `Opaque type '${name}' conflicts with opaque type from module '${
                opaqueTypes[name]!.moduleName
              }'. ` + `Consider using a different name or qualified imports.`,
              imp.span,
            );
          }
          opaqueTypes[name] = opaque;
        }
      }

      // Import all exported protocols
      for (const [name, protocol] of Object.entries(depModule.protocols) as [
        string,
        ProtocolInfo,
      ][]) {
        if (isExportedFromModule(depModule, name, "protocol")) {
          // Check for collision with existing protocol from different module
          checkTypeCollision(
            name,
            imp.moduleName,
            protocols[name],
            imp.span,
            "protocol",
          );
          protocols[name] = protocol;

          // Add protocol methods to scope so they can be called directly
          const methodSchemes = addProtocolMethodsToScope(
            protocol,
            globalScope,
          );
          // Store in typeSchemes for LSP hover/completion
          for (const [methodName, scheme] of methodSchemes) {
            typeSchemes[methodName] = scheme;
          }
        }
      }

      // Import all instances (always imported regardless of exports)
      for (const instance of depModule.instances) {
        instances.push(instance);
      }

      // Import exported operator declarations
      for (const [op, info] of depModule.operators) {
        if (isExportedFromModule(depModule, op, "operator")) {
          operators.set(op, info);
          // Track for re-export support (operators are also values)
          importedValues.set(op, imp.moduleName);
        }
      }
    }
  }

  // ===== PASS 0: Register infix declarations =====
  // We process infix declarations first so operator precedence is known during parsing.
  // Note: This pass validates declarations but the parser pre-processing handles actual precedence.
  for (const decl of program.declarations) {
    if (decl.kind === "InfixDeclaration") {
      registerInfixDeclaration(decl, operators, infixDeclarations);
      continue;
    }
  }

  // ===== PASS 1a: Register type declarations (ADTs only) =====
  // We register ADTs first so type aliases can reference them.
  for (const decl of program.declarations) {
    if (decl.kind === "TypeDeclaration") {
      registerTypeDeclaration(
        decl,
        adts,
        records,
        constructors,
        constructorTypes,
        typeAliases,
        opaqueTypes,
        globalScope,
        currentModuleName,
      );
      continue;
    }
  }

  // ===== PASS 1a2: Register opaque type declarations =====
  // Opaque types are registered before type aliases since aliases may reference them.
  for (const decl of program.declarations) {
    if (decl.kind === "OpaqueTypeDeclaration") {
      registerOpaqueType(decl, opaqueTypes, currentModuleName);
      continue;
    }
  }

  // ===== PASS 1b: Register type aliases =====
  // Type aliases are registered after ADTs so they can reference them.
  // Aliases are also registered without validation first so they can reference each other.
  const typeAliasDecls: TypeAliasDeclaration[] = [];
  for (const decl of program.declarations) {
    if (decl.kind === "TypeAliasDeclaration") {
      registerTypeAliasWithoutValidation(decl, typeAliases, currentModuleName);
      typeAliasDecls.push(decl);
      continue;
    }
  }

  // ===== PASS 1c: Validate type alias references =====
  // Now that all ADTs and aliases are registered, validate that type references exist.
  for (const decl of typeAliasDecls) {
    validateTypeAliasReferences(decl, adts, typeAliases, opaqueTypes, records);
  }

  // ===== PASS 1d: Register protocols =====
  for (const decl of program.declarations) {
    if (decl.kind === "ProtocolDeclaration") {
      registerProtocol(
        decl,
        protocols,
        adts,
        typeAliases,
        globalScope,
        constructors,
        opaqueTypes,
        substitution,
        currentModuleName,
        imports,
        dependencies,
      );
      continue;
    }
  }

  // ===== PASS 1e: Process 'implementing' clauses =====
  // For type declarations with 'implementing', validate all protocols have all defaults
  // and generate synthetic implement blocks.
  for (const decl of program.declarations) {
    if (decl.kind === "TypeDeclaration" && decl.implementing) {
      processImplementingClause(
        decl,
        protocols,
        instances,
        localInstances,
        adts,
        typeAliases,
        currentModuleName,
      );
    }
  }

  // ===== PASS 1.5: Register implementation declarations =====
  // Implementations are registered after protocols but before value inference
  // so that we can resolve constraints during type checking.
  for (const decl of program.declarations) {
    if (decl.kind === "ImplementationDeclaration") {
      registerImplementation(
        decl,
        instances,
        localInstances,
        protocols,
        adts,
        typeAliases,
        currentModuleName,
      );
      continue;
    }
  }

  // ===== PASS 2: Register value declarations =====
  for (const decl of program.declarations) {
    if (
      decl.kind === "ValueDeclaration" ||
      decl.kind === "ExternalDeclaration"
    ) {
      registerValue(values, decl);
      continue;
    }

    if (decl.kind === "TypeAnnotationDeclaration") {
      // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
      if (Object.hasOwn(annotations, decl.name)) {
        throw new SemanticError(
          `Duplicate type annotation for '${decl.name}'`,
          decl.span,
        );
      }
      annotations[decl.name] = decl;
    }
  }

  for (const [name, ann] of Object.entries(annotations)) {
    // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
    if (!Object.hasOwn(values, name)) {
      throw new SemanticError(
        `Type annotation for '${name}' has no matching definition`,
        ann.span,
      );
    }
    const value = values[name]!;
    if (value.declaration.kind === "ExternalDeclaration") {
      throw new SemanticError(
        `External declaration '${name}' already includes a type annotation`,
        ann.span,
      );
    }
    value.annotation = ann.annotation;
  }

  // Seed global names to enable recursion.
  // We initially seed with monomorphic schemes (empty quantifier set)
  // and will generalize them after full inference.
  for (const [name, info] of Object.entries(values)) {
    const annotationExpr =
      info.annotation ??
      (info.declaration.kind === "ExternalDeclaration"
        ? info.declaration.annotation
        : undefined);

    // Arity validation is not needed here - the type system will verify
    // that the RHS expression's type matches the annotation during unification.

    // Use typeFromAnnotationWithConstraints to extract both type and constraints
    let annotationType: Type | undefined;
    let annotatedConstraints: Constraint[] | undefined;

    if (annotationExpr) {
      // Validate that all type references in the annotation are defined.
      // For value/external declarations, type variables are implicitly defined,
      // so we collect them first to treat as "defined params".
      const typeVars = collectTypeVariables(annotationExpr);
      const validationErrors = validateTypeExpr(
        annotationExpr,
        typeVars,
        adts,
        typeAliases,
        info.declaration.span,
        opaqueTypes,
        records,
      );

      if (validationErrors.length > 0) {
        const err = validationErrors[0]!;
        const message = err.suggestion
          ? `${err.message}. ${err.suggestion}`
          : err.message;
        throw new SemanticError(message, err.span);
      }

      const result = typeFromAnnotationWithConstraints(
        annotationExpr,
        new Map(),
        adts,
        typeAliases,
        protocols,
        records,
      );
      annotationType = result.type;
      annotatedConstraints =
        result.constraints.length > 0 ? result.constraints : undefined;

      // Store annotated constraints in the value info for later merging
      if (annotatedConstraints) {
        info.annotatedConstraints = annotatedConstraints;
      }
    }

    const seeded =
      annotationType ?? seedValueType(info.declaration, adts, typeAliases);
    // Seed with a monomorphic scheme (no quantified variables yet)
    declareSymbol(
      globalScope,
      name,
      { vars: new Set(), constraints: [], type: seeded },
      info.declaration.span,
    );
    types[name] = seeded;
  }

  // Set the instance registry for constraint validation during generalization.
  // This allows validateConcreteConstraints to check if concrete type constraints
  // have matching instances.
  setInstanceRegistry(instances);

  // ===== PASS 2.0b: Concretize polymorphic instance type args =====
  // Before value inference, analyze each instance's method bodies to determine
  // if polymorphic type args can be concretized. This is important for multi-parameter
  // protocols where the return type is constrained (e.g., `implement Appendable a => Proto Float a`
  // where the method body forces `a` to be `List Int`).
  concretizeInstanceTypeArgs(
    localInstances,
    instances,
    protocols,
    globalScope,
    substitution,
    constructors,
    adts,
    typeAliases,
    opaqueTypes,
    imports,
    dependencies,
  );

  // Build dependency graph and compute SCCs for proper mutual recursion handling.
  // SCCs are returned in reverse topological order, so we process dependencies first.
  const depGraph = buildDependencyGraph(values);
  const sccs = computeSCCs(depGraph);

  // Process each SCC:
  // - For single-element SCCs (non-recursive or self-recursive): infer and generalize immediately
  // - For multi-element SCCs (mutually recursive): infer all together, then generalize all together
  for (const scc of sccs) {
    // First, handle any external declarations in this SCC (they don't participate in mutual recursion)
    const externals: string[] = [];
    const valueDecls: string[] = [];

    for (const name of scc) {
      const info = values[name]!;
      if (info.declaration.kind === "ExternalDeclaration") {
        externals.push(name);
      } else {
        valueDecls.push(name);
      }
    }

    // Process externals first
    for (const name of externals) {
      const info = values[name]!;
      if (info.declaration.kind === "ExternalDeclaration") {
        // Use typeFromAnnotationWithConstraints for external declarations too
        const result = typeFromAnnotationWithConstraints(
          info.declaration.annotation,
          new Map(),
          adts,
          typeAliases,
          protocols,
          records,
        );
        info.type = result.type;

        // Store annotated constraints if any
        if (result.constraints.length > 0) {
          info.annotatedConstraints = result.constraints;
        }

        // External declarations use their annotated constraints directly
        const scheme: TypeScheme = {
          vars: new Set<number>(),
          constraints: result.constraints,
          type: info.type,
        };
        globalScope.symbols.set(info.declaration.name, scheme);
        // Store the type scheme for external declarations too (needed by IR lowering)
        typeSchemes[name] = scheme;
      }
    }

    // Process value declarations in this SCC together
    if (valueDecls.length > 0) {
      // Reset constraint context for this SCC
      // All values in an SCC share constraints (they're mutually recursive)
      resetConstraintContext();

      // Infer all declarations in the SCC
      const inferredTypes: Map<string, Type> = new Map();

      for (const name of valueDecls) {
        const info = values[name]!;
        if (info.declaration.kind !== "ValueDeclaration") continue;

        const declaredType = types[name]!;
        // Use the pre-computed annotated type (already extracted with constraints during seeding)
        const annotationType = info.annotation
          ? typeFromAnnotation(info.annotation, new Map(), adts, typeAliases, records)
          : undefined;

        const inferred = analyzeValueDeclaration(
          info.declaration,
          globalScope,
          substitution,
          declaredType,
          annotationType,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );

        inferredTypes.set(name, inferred);
        types[name] = inferred;
        info.type = inferred;
      }

      // After inferring all in the SCC, generalize all together
      // This ensures mutually recursive functions get proper polymorphic types
      for (const name of valueDecls) {
        const info = values[name]!;
        const inferred = inferredTypes.get(name)!;
        const generalizedScheme = generalizeWithAnnotatedConstraints(
          inferred,
          { symbols: new Map(), parent: globalScope.parent },
          substitution,
          info.annotatedConstraints,
          info.declaration.span,
        );
        globalScope.symbols.set(name, generalizedScheme);
        // Store the type scheme with constraints for dictionary-passing
        typeSchemes[name] = generalizedScheme;
      }
    }
  }

  // ===== PASS 2.5: Validate implementation method expressions =====
  // Now that all values are registered, validate that method implementations
  // reference defined identifiers. Only validate localInstances since imported
  // instances were already validated in their defining module.
  validateImplementationMethodExpressions(
    localInstances,
    globalScope,
    constructors,
    imports,
    dependencies,
  );

  // ===== PASS 2.5b: Validate implementation method types =====
  // Validate that each method implementation's type matches the protocol's
  // declared method signature (with type parameters substituted).
  validateImplementationMethodTypes(
    localInstances,
    instances,
    protocols,
    globalScope,
    substitution,
    constructors,
    adts,
    typeAliases,
    opaqueTypes,
    imports,
    dependencies,
  );

  // ===== PASS 2.5c: Validate instance constraint satisfiability =====
  // For polymorphic instances with constraints (e.g., `implement Eq a => ExampleProtocol a`),
  // validate that the constraint protocols have instances that could satisfy the constraint.
  // This catches cases where a constraint references a protocol with no instances.
  validateInstanceConstraintSatisfiability(
    localInstances,
    instances,
    protocols,
    adts,
  );

  // ===== PASS 2.5d: Validate concrete constraint instances exist =====
  // After type inference, check that any protocol constraints on concrete types
  // (e.g., `Eq A` when calling a function that requires `Eq a`) have corresponding
  // instance declarations. This provides early error detection for missing instances.
  validateConcreteConstraintInstances(
    values,
    instances,
    protocols,
    substitution,
  );

  // ===== PASS 2.6: Validate protocol default implementations =====
  // Validate that default implementations in protocols reference defined identifiers.
  // This is done after all values are registered so forward references work.
  validateProtocolDefaultImplementations(
    protocols,
    globalScope,
    constructors,
    imports,
    dependencies,
    currentModuleName,
  );

  // Compute export information for this module
  const exports = computeModuleExports(
    program.module,
    values,
    adts,
    constructors,
    typeAliases,
    opaqueTypes,
    protocols,
    operators,
    importedValues,
  );

  return {
    values,
    annotations,
    module: program.module,
    imports,
    types,
    typeSchemes,
    adts,
    constructors,
    constructorTypes,
    typeAliases,
    records,
    opaqueTypes,
    protocols,
    instances,
    operators,
    infixDeclarations,
    exports,
  };
}

function registerValue(
  values: Record<string, ValueInfo>,
  decl: ValueDeclaration | ExternalDeclaration,
) {
  // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
  if (Object.hasOwn(values, decl.name)) {
    throw new SemanticError(
      `Duplicate definition for '${decl.name}'`,
      decl.span,
    );
  }
  values[decl.name] = {
    declaration: decl,
    annotation:
      decl.kind === "ExternalDeclaration" ? decl.annotation : undefined,
    externalTarget:
      decl.kind === "ExternalDeclaration" ? decl.target : undefined,
  };
}

/**
 * Register an Algebraic Data Type declaration.
 *
 * This function:
 * 1. Validates the type name is not already defined
 * 2. Validates constructor names are unique within the module
 * 3. Validates type parameters are unique
 * 4. Registers the ADT in the adts registry
 * 5. Registers each constructor in the constructors registry
 * 6. Seeds constructors as polymorphic values in the global scope
 *
 * For example, `type Maybe a = Just a | Nothing` registers:
 * - ADT: Maybe with param [a] and constructors [Just, Nothing]
 * - Constructor Just: arity 1, parent Maybe
 * - Constructor Nothing: arity 0, parent Maybe
 * - Global scope: Just : forall a. a -> Maybe a
 * - Global scope: Nothing : forall a. Maybe a
 */
function registerTypeDeclaration(
  decl: TypeDeclaration,
  adts: Record<string, ADTInfo>,
  records: Record<string, RecordInfo>,
  constructors: Record<string, ConstructorInfo>,
  constructorTypes: Record<string, TypeScheme>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  globalScope: Scope,
  moduleName?: string,
) {
  // Check for duplicate type name
  const existingADT = adts[decl.name];
  if (existingADT) {
    // If it's from a different module, provide a more helpful error message
    if (existingADT.moduleName && existingADT.moduleName !== moduleName) {
      throw new SemanticError(
        `Type '${decl.name}' conflicts with type from module '${existingADT.moduleName}'. ` +
          `Consider using a different name or qualified imports.`,
        decl.span,
      );
    }
    throw new SemanticError(
      `Duplicate type declaration for '${decl.name}'`,
      decl.span,
    );
  }

  // Validate type parameters are unique
  const paramSet = new Set<string>();
  for (const param of decl.params) {
    if (paramSet.has(param)) {
      throw new SemanticError(
        `Duplicate type parameter '${param}' in type '${decl.name}'`,
        decl.span,
      );
    }
    paramSet.add(param);
  }

  // Validate at least one constructor (for ADTs only)
  // Record types don't have constructors
  if (!decl.constructors || decl.constructors.length === 0) {
    // If we have record fields, this is a record type - register it separately
    if (decl.recordFields) {
      registerRecordTypeDeclaration(
        decl,
        records,
        adts,
        typeAliases,
        opaqueTypes,
        moduleName,
      );
      return;
    }
    throw new SemanticError(
      `Type '${decl.name}' must have at least one constructor`,
      decl.span,
    );
  }

  // Register the ADT
  const constructorNames = decl.constructors.map((c) => c.name);
  adts[decl.name] = {
    name: decl.name,
    moduleName,
    params: decl.params,
    constructors: constructorNames,
    span: decl.span,
  };

  // Create fresh type variables for each type parameter
  // These will be used to construct the polymorphic type scheme for constructors
  const paramTypeVars: Map<string, TypeVar> = new Map();
  for (const param of decl.params) {
    paramTypeVars.set(param, freshType());
  }

  // Build the result type: TypeName param1 param2 ...
  // For Maybe a, this is: { kind: "con", name: "Maybe", args: [TypeVar(a)] }
  const resultType: TypeCon = {
    kind: "con",
    name: decl.name,
    args: decl.params.map((p) => paramTypeVars.get(p)!),
  };

  // Register each constructor
  for (const ctor of decl.constructors) {
    // Check for duplicate constructor name defined within the SAME module
    // Allow shadowing constructors imported from other modules
    const existingCtor = constructors[ctor.name];
    if (existingCtor && existingCtor.moduleName === moduleName) {
      throw new SemanticError(
        `Duplicate constructor '${ctor.name}' (constructor names must be unique within a module)`,
        ctor.span,
      );
    }

    // Note: We allow shadowing built-in constructors (like True/False)
    // The type checker will disambiguate based on context

    // Register constructor info
    constructors[ctor.name] = {
      arity: ctor.args.length,
      argTypes: ctor.args,
      parentType: decl.name,
      parentParams: decl.params,
      moduleName,
      span: ctor.span,
    };

    // Build the constructor's type and register it in global scope
    // For Just: a -> Maybe a
    // For Nothing: Maybe a
    const ctorType = buildConstructorType(ctor, resultType, paramTypeVars);

    // Generalize over all type parameters to make it polymorphic
    const quantifiedVars = new Set<number>();
    for (const tv of paramTypeVars.values()) {
      quantifiedVars.add(tv.id);
    }

    const ctorScheme: TypeScheme = {
      vars: quantifiedVars,
      constraints: [],
      type: ctorType,
    };

    // Register constructor as a polymorphic value in global scope
    globalScope.symbols.set(ctor.name, ctorScheme);

    // Also store in constructorTypes for export
    constructorTypes[ctor.name] = ctorScheme;
  }
}

/**
 * Check if a TypeExpr contains a RecordType anywhere in its structure.
 * Used to reject floating record types in type annotations.
 */
function containsRecordType(expr: TypeExpr): boolean {
  switch (expr.kind) {
    case "RecordType":
      return true;
    case "TypeRef":
      return expr.args.some(containsRecordType);
    case "FunctionType":
      return containsRecordType(expr.from) || containsRecordType(expr.to);
    case "TupleType":
      return expr.elements.some(containsRecordType);
    case "QualifiedType":
      return containsRecordType(expr.type);
  }
}

/**
 * Register a record type declaration.
 *
 * Record types are named types with fields, defined using:
 * `type Person = { name : String, age : Int }`
 *
 * Unlike ADTs:
 * - Record types don't have constructors
 * - Fields are accessed using dot notation
 * - Records can be used in pattern matching
 */
function registerRecordTypeDeclaration(
  decl: TypeDeclaration,
  records: Record<string, RecordInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  moduleName?: string,
): void {
  // Check for duplicate type name across all type namespaces
  if (records[decl.name]) {
    throw new SemanticError(
      `Duplicate record type declaration for '${decl.name}'`,
      decl.span,
    );
  }
  if (adts[decl.name]) {
    throw new SemanticError(
      `Record type '${decl.name}' conflicts with ADT '${decl.name}'`,
      decl.span,
    );
  }
  if (typeAliases[decl.name]) {
    throw new SemanticError(
      `Record type '${decl.name}' conflicts with type alias '${decl.name}'`,
      decl.span,
    );
  }
  if (opaqueTypes[decl.name]) {
    throw new SemanticError(
      `Record type '${decl.name}' conflicts with opaque type '${decl.name}'`,
      decl.span,
    );
  }

  // Validate no mixing of constructors and record fields
  if (decl.constructors && decl.constructors.length > 0) {
    throw new SemanticError(
      `Type '${decl.name}' cannot have both constructors and record fields. ` +
        `Use 'type' for ADTs or record types separately.`,
      decl.span,
    );
  }

  // Validate record fields exist - but allow empty records (like unit types)
  if (!decl.recordFields) {
    throw new SemanticError(
      `Record type '${decl.name}' is missing field definitions`,
      decl.span,
    );
  }

  // Check for duplicate field names
  const fieldNames = new Set<string>();
  for (const field of decl.recordFields) {
    if (fieldNames.has(field.name)) {
      throw new SemanticError(
        `Duplicate field '${field.name}' in record type '${decl.name}'`,
        field.span,
      );
    }
    fieldNames.add(field.name);
  }

  // Create temporary type variables for type params (for field type resolution)
  const paramTypeVars: Map<string, TypeVar> = new Map();
  for (const param of decl.params) {
    paramTypeVars.set(param, freshType());
  }

  // Validate field types - reject floating record types in field definitions
  for (const field of decl.recordFields) {
    if (containsRecordType(field.type)) {
      throw new SemanticError(
        `Record types cannot be used directly in type annotations. ` +
          `Define a named record type using 'type RecordName = { ... }' and reference it by name.`,
        field.span,
      );
    }
  }

  // Build field info - store the AST TypeExpr for proper type parameter resolution
  const fields: RecordFieldInfo[] = decl.recordFields.map((field) => ({
    name: field.name,
    typeExpr: field.type, // Store the AST TypeExpr for later resolution
    span: field.span,
  }));

  // Register the record type
  records[decl.name] = {
    name: decl.name,
    moduleName,
    params: decl.params,
    fields,
    span: decl.span,
  };
}

/**
 * Build the type for a constructor.
 *
 * For a constructor like `Just a` in `type Maybe a`:
 * - Arguments: [a]
 * - Result: Maybe a
 * - Type: a -> Maybe a
 *
 * For a constructor like `Nothing` in `type Maybe a`:
 * - Arguments: []
 * - Result: Maybe a
 * - Type: Maybe a (no function arrow)
 *
 * For a constructor like `Node a (Tree a) (Tree a)` in `type Tree a`:
 * - Arguments: [a, Tree a, Tree a]
 * - Result: Tree a
 * - Type: a -> Tree a -> Tree a -> Tree a
 */
function buildConstructorType(
  ctor: ConstructorVariant,
  resultType: TypeCon,
  paramTypeVars: Map<string, TypeVar>,
): Type {
  if (ctor.args.length === 0) {
    // Nullary constructor: just return the result type
    return resultType;
  }

  // Convert each argument TypeExpr to internal Type
  const argTypes: Type[] = ctor.args.map((argExpr) =>
    constructorArgToType(argExpr, paramTypeVars),
  );

  // Build function type: arg1 -> arg2 -> ... -> ResultType
  return fnChain(argTypes, resultType);
}

/**
 * Convert a constructor argument TypeExpr to an internal Type.
 *
 * This handles:
 * - Type variables (lowercase, e.g., "a") -> look up in paramTypeVars
 * - Type constructors (uppercase, e.g., "List a") -> build TypeCon
 * - Recursive types (e.g., "Tree a" in Tree definition) -> build TypeCon
 */
function constructorArgToType(
  expr: TypeExpr,
  paramTypeVars: Map<string, TypeVar>,
): Type {
  switch (expr.kind) {
    case "TypeRef": {
      // Check if it's a type variable (lowercase, single letter typically)
      const typeVar = paramTypeVars.get(expr.name);
      if (typeVar && expr.args.length === 0) {
        return typeVar;
      }

      // Otherwise it's a type constructor
      // Recursively convert arguments
      const args = expr.args.map((arg) =>
        constructorArgToType(arg, paramTypeVars),
      );
      return {
        kind: "con",
        name: expr.name,
        args,
      };
    }
    case "FunctionType": {
      return {
        kind: "fun",
        from: constructorArgToType(expr.from, paramTypeVars),
        to: constructorArgToType(expr.to, paramTypeVars),
      };
    }
    case "TupleType": {
      return {
        kind: "tuple",
        elements: expr.elements.map((el) =>
          constructorArgToType(el, paramTypeVars),
        ),
      };
    }
    case "RecordType": {
      // Convert record type annotation to internal record type
      const sortedFields = [...expr.fields].sort((a, b) =>
        a.name.localeCompare(b.name),
      );
      const fields: Record<string, Type> = {};
      for (const field of sortedFields) {
        fields[field.name] = constructorArgToType(field.type, paramTypeVars);
      }
      return {
        kind: "record",
        fields,
      };
    }
    case "QualifiedType": {
      // For constructor arguments, we don't support qualified types
      // (constraints only make sense at the top level of function signatures)
      throw new SemanticError(
        "Constructor arguments cannot have constraints",
        expr.span,
      );
    }
  }
}

/**
 * Collect all type variable names from a type expression.
 * Type variables are lowercase identifiers (e.g., 'a', 'b', 'elem').
 * This is used to determine which names are implicitly defined type parameters
 * in a type annotation.
 */
function collectTypeVariables(expr: TypeExpr): Set<string> {
  const vars = new Set<string>();

  function collect(e: TypeExpr): void {
    switch (e.kind) {
      case "TypeRef": {
        const name = e.name;
        // Type variables are lowercase identifiers
        if (name.length > 0 && name[0] === name[0]!.toLowerCase()) {
          vars.add(name);
        }
        // Also collect from type arguments
        for (const arg of e.args) {
          collect(arg);
        }
        break;
      }
      case "FunctionType":
        collect(e.from);
        collect(e.to);
        break;
      case "TupleType":
        for (const el of e.elements) {
          collect(el);
        }
        break;
      case "RecordType":
        for (const field of e.fields) {
          collect(field.type);
        }
        break;
      case "QualifiedType":
        collect(e.type);
        // Also collect from constraints
        for (const constraint of e.constraints) {
          for (const arg of constraint.typeArgs) {
            collect(arg);
          }
        }
        break;
    }
  }

  collect(expr);
  return vars;
}

/**
 * Validate that all type references in a type expression are defined.
 * Returns an array of validation errors found.
 *
 * @param expr - The type expression to validate
 * @param definedParams - Set of type parameters that are in scope (e.g., from the alias declaration)
 * @param adts - Registry of defined ADT types
 * @param typeAliases - Registry of defined type aliases
 * @param parentSpan - Span to use for error reporting if the expression has no span
 * @param opaqueTypes - Registry of opaque types (optional)
 */
function validateTypeExpr(
  expr: TypeExpr,
  definedParams: Set<string>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  parentSpan: Span,
  opaqueTypes: Record<string, OpaqueTypeInfo> = {},
  records: Record<string, RecordInfo> = {},
): TypeValidationError[] {
  const errors: TypeValidationError[] = [];

  function findCaseSuggestion(name: string): string | undefined {
    const nameLower = name.toLowerCase();
    // Check ADTs - look for case-insensitive match to suggest correct casing
    for (const adtName of Object.keys(adts)) {
      if (adtName.toLowerCase() === nameLower && adtName !== name) {
        return adtName;
      }
    }
    // Check type aliases
    for (const aliasName of Object.keys(typeAliases)) {
      if (aliasName.toLowerCase() === nameLower && aliasName !== name) {
        return aliasName;
      }
    }
    // Check opaque types
    for (const opaqueName of Object.keys(opaqueTypes)) {
      if (opaqueName.toLowerCase() === nameLower && opaqueName !== name) {
        return opaqueName;
      }
    }
    // Check record types
    for (const recordName of Object.keys(records)) {
      if (recordName.toLowerCase() === nameLower && recordName !== name) {
        return recordName;
      }
    }
    return undefined;
  }

  function validate(e: TypeExpr): void {
    switch (e.kind) {
      case "TypeRef": {
        const name = e.name;
        const isLowercase = name.charAt(0) === name.charAt(0).toLowerCase();

        // Check if it's a defined type parameter
        if (definedParams.has(name)) {
          // Valid type parameter reference
          // But validate any type arguments (type params shouldn't have args)
          if (e.args.length > 0) {
            errors.push({
              message: `Type parameter '${name}' cannot take type arguments`,
              span: e.span ?? parentSpan,
            });
          }
          return;
        }

        // Check if it's a defined ADT (exact match required)
        const adtByName = adts[name];
        if (adtByName) {
          // Valid ADT reference - validate args
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }

        // Check if it's "List" which is handled specially by the compiler
        if (name === "List") {
          // List is a built-in type constructor, validate its argument
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }

        // Check if it's a type alias
        if (typeAliases[name]) {
          // Valid type alias reference - validate args
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }

        // Check if it's an opaque type
        if (opaqueTypes[name]) {
          // Valid opaque type reference - validate args
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }

        // Check if it's a named record type
        if (records[name]) {
          // Valid record type reference - validate args
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }

        // Not found - check for case mismatch suggestions
        const suggestion = findCaseSuggestion(name);

        // Check if this could be a type variable (single lowercase letter)
        if (isLowercase && name.length === 1) {
          // Single lowercase letter that's not in definedParams
          // This is an undefined type variable
          errors.push({
            message: `Type variable '${name}' is not defined in this context`,
            span: e.span ?? parentSpan,
            suggestion: `Add '${name}' as a type parameter to the type alias`,
          });
          return;
        }

        // Multi-character lowercase or unknown uppercase - undefined type
        if (suggestion) {
          errors.push({
            message: `Type '${name}' is not defined`,
            span: e.span ?? parentSpan,
            suggestion: `Did you mean '${suggestion}'?`,
          });
        } else {
          errors.push({
            message: `Type '${name}' is not defined`,
            span: e.span ?? parentSpan,
          });
        }

        // Still validate args to catch any nested errors
        for (const arg of e.args) {
          validate(arg);
        }
        break;
      }
      case "FunctionType": {
        validate(e.from);
        validate(e.to);
        break;
      }
      case "TupleType": {
        for (const el of e.elements) {
          validate(el);
        }
        break;
      }
      case "RecordType": {
        for (const field of e.fields) {
          validate(field.type);
        }
        break;
      }
      case "QualifiedType": {
        validate(e.type);
        break;
      }
    }
  }

  validate(expr);
  return errors;
}

/**
 * Register a type alias declaration without validating type references.
 * This allows aliases to be registered before validation, enabling
 * mutual references between aliases.
 */
function registerTypeAliasWithoutValidation(
  decl: TypeAliasDeclaration,
  typeAliases: Record<string, TypeAliasInfo>,
  moduleName?: string,
) {
  // Check for duplicate alias name
  const existingAlias = typeAliases[decl.name];
  if (existingAlias) {
    // If it's from a different module, provide a more helpful error message
    if (existingAlias.moduleName && existingAlias.moduleName !== moduleName) {
      throw new SemanticError(
        `Type alias '${decl.name}' conflicts with type alias from module '${existingAlias.moduleName}'. ` +
          `Consider using a different name or qualified imports.`,
        decl.span,
      );
    }
    throw new SemanticError(`Duplicate type alias '${decl.name}'`, decl.span);
  }

  // Validate type parameters are unique
  const paramSet = new Set<string>();
  for (const param of decl.params) {
    if (paramSet.has(param)) {
      throw new SemanticError(
        `Duplicate type parameter '${param}' in type alias '${decl.name}'`,
        decl.span,
      );
    }
    paramSet.add(param);
  }

  // Register the alias (validation happens later)
  typeAliases[decl.name] = {
    name: decl.name,
    moduleName,
    params: decl.params,
    value: decl.value,
    span: decl.span,
  };
}

/**
 * Validate that all type references in a type alias are defined.
 * Called after all ADTs and aliases are registered.
 */
function validateTypeAliasReferences(
  decl: TypeAliasDeclaration,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo> = {},
  records: Record<string, RecordInfo> = {},
) {
  // Reject bare record types in type alias declarations
  // Record types must be defined using `type Name = { ... }` syntax
  if (decl.value.kind === "RecordType") {
    throw new SemanticError(
      `Type alias '${decl.name}' cannot directly define a record type. ` +
        `Use 'type ${decl.name} = { ... }' instead of 'type alias'.`,
      decl.value.span,
    );
  }

  const paramSet = new Set<string>(decl.params);

  const validationErrors = validateTypeExpr(
    decl.value,
    paramSet,
    adts,
    typeAliases,
    decl.span,
    opaqueTypes,
    records,
  );

  if (validationErrors.length > 0) {
    // Report the first error (could be extended to report all)
    const err = validationErrors[0]!;
    const message = err.suggestion
      ? `${err.message}. ${err.suggestion}`
      : err.message;
    throw new SemanticError(message, err.span);
  }
}

/**
 * Register an opaque type declaration.
 *
 * Opaque types are abstract types that hide their implementation.
 * They are useful for JS interop where the actual type is unknown.
 * Pattern matching and record updates are not allowed on opaque types.
 *
 * Example: `type Promise a` creates an opaque type that takes one parameter.
 */
function registerOpaqueType(
  decl: OpaqueTypeDeclaration,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  moduleName?: string,
) {
  // Check for duplicate type name
  // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
  if (Object.hasOwn(opaqueTypes, decl.name)) {
    const existing = opaqueTypes[decl.name]!;
    if (existing.moduleName && existing.moduleName !== moduleName) {
      throw new SemanticError(
        `Opaque type '${decl.name}' conflicts with opaque type from module '${existing.moduleName}'. ` +
          `Consider using a different name or qualified imports.`,
        decl.span,
      );
    }
    throw new SemanticError(
      `Duplicate opaque type declaration for '${decl.name}'`,
      decl.span,
    );
  }

  // Validate type parameters are unique
  const paramSet = new Set<string>();
  for (const param of decl.params) {
    if (paramSet.has(param)) {
      throw new SemanticError(
        `Duplicate type parameter '${param}' in opaque type '${decl.name}'`,
        decl.span,
      );
    }
    paramSet.add(param);
  }

  // Register the opaque type
  opaqueTypes[decl.name] = {
    name: decl.name,
    moduleName,
    params: decl.params,
    span: decl.span,
  };
}

/**
 * Register a type alias declaration.
 *
 * Type aliases don't introduce new constructors, they just create
 * a new name for an existing type expression.
 *
 * For example, `type alias UserId = number` allows using "UserId"
 * anywhere "number" is expected.
 *
 * @deprecated Use registerTypeAliasWithoutValidation and validateTypeAliasReferences instead
 */
function registerTypeAlias(
  decl: TypeAliasDeclaration,
  typeAliases: Record<string, TypeAliasInfo>,
  adts: Record<string, ADTInfo>,
  moduleName?: string,
) {
  registerTypeAliasWithoutValidation(decl, typeAliases, moduleName);
  validateTypeAliasReferences(decl, adts, typeAliases, {}, {});
}

/**
 * Register an infix declaration in the operator registry.
 * Validates that there are no duplicate declarations for the same operator.
 */
function registerInfixDeclaration(
  decl: InfixDeclaration,
  operators: OperatorRegistry,
  infixDeclarations: InfixDeclaration[],
) {
  // Check for duplicate operator declaration
  if (operators.has(decl.operator)) {
    throw new SemanticError(
      `Duplicate infix declaration for operator '${decl.operator}'`,
      decl.span,
    );
  }

  // Convert fixity to associativity
  const associativity: "left" | "right" | "none" =
    decl.fixity === "infixl"
      ? "left"
      : decl.fixity === "infixr"
        ? "right"
        : "none";

  // Validate precedence range (0-9)
  if (decl.precedence < 0 || decl.precedence > 9) {
    throw new SemanticError(
      `Precedence must be between 0 and 9, got ${decl.precedence}`,
      decl.span,
    );
  }

  // Register the operator
  operators.set(decl.operator, {
    precedence: decl.precedence,
    associativity,
  });

  // Store the declaration for later reference
  infixDeclarations.push(decl);
}

/**
 * Add protocol methods to a scope as polymorphic functions with constraints.
 * This is called when:
 * 1. Registering a new protocol
 * 2. Importing protocols from a dependency module
 */
function addProtocolMethodsToScope(protocol: ProtocolInfo, scope: Scope) {
  // Create SHARED type variable context for ALL protocol parameters
  const sharedTypeVarCtx = new Map<string, TypeVar>();
  for (const param of protocol.params) {
    sharedTypeVarCtx.set(param, freshType());
  }

  // Create the constraint for this protocol
  const protocolConstraint: Constraint = {
    protocolName: protocol.name,
    typeArgs: protocol.params.map((p) => sharedTypeVarCtx.get(p)!),
  };

  // Get the set of quantified variable IDs
  const quantifiedVars = new Set<number>();
  for (const tv of sharedTypeVarCtx.values()) {
    quantifiedVars.add(tv.id);
  }

  // Return the method schemes for optional storage in module.typeSchemes
  const methodSchemes = new Map<string, TypeScheme>();

  // Add each method to scope
  for (const [methodName, methodInfo] of protocol.methods) {
    // Only add if not already defined (don't override explicit definitions)
    if (!scope.symbols.has(methodName)) {
      // Refresh the type with new consistent type variable IDs
      const refreshedType = refreshType(methodInfo.type, sharedTypeVarCtx);

      const scheme: TypeScheme = {
        vars: new Set(quantifiedVars),
        constraints: [protocolConstraint],
        type: refreshedType,
      };
      scope.symbols.set(methodName, scheme);
      methodSchemes.set(methodName, scheme);
    }
  }

  return methodSchemes;
}

/**
 * Refresh type variables in a type using a name-to-TypeVar mapping.
 * This replaces type variables whose ID matches any in the original context
 * with new fresh variables from the provided mapping.
 *
 * Since we don't have the original names, we need to track which var IDs
 * correspond to which parameter names based on the order they appear in the type.
 */
function refreshType(type: Type, newVarMap: Map<string, TypeVar>): Type {
  // Build a mapping from old var IDs to new var IDs
  // The types in protocol methods were created with their own fresh vars,
  // but we need to map them to our consistent vars.
  // We do this by collecting all var IDs in the type and mapping them
  // to the new vars in order of the protocol params.

  const oldVarIds = collectTypeVarIds(type);
  const newVars = Array.from(newVarMap.values());

  // Create substitution from old IDs to new TypeVars
  const varSubst = new Map<number, Type>();
  const oldVarArray = Array.from(oldVarIds);
  for (let i = 0; i < Math.min(oldVarArray.length, newVars.length); i++) {
    varSubst.set(oldVarArray[i]!, newVars[i]!);
  }

  return applyVarSubstitution(type, varSubst);
}

/**
 * Helper function to replace fresh type variables in an inferred type
 * with protocol type parameters where appropriate.
 * This is used when inferring types from default implementations.
 */
function substituteProtocolVars(
  type: Type,
  protocolVarCtx: Map<string, TypeVar>,
): Type {
  // For now, just return the type as-is
  // In a more sophisticated implementation, we could try to unify
  // the inferred type variables with the protocol's type parameters
  // based on how they're used in the default implementation
  return type;
}

/**
 * Register a protocol declaration in the protocol registry.
 * Also adds protocol methods to the global scope as polymorphic functions with constraints.
 */
function registerProtocol(
  decl: ProtocolDeclaration,
  protocols: Record<string, ProtocolInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  globalScope: Scope,
  constructors: Record<string, ConstructorInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  substitution: Substitution,
  moduleName?: string,
  imports: ImportDeclaration[] = [],
  dependencies: Map<string, SemanticModule> = new Map(),
) {
  // Check for duplicate protocol name
  const existingProtocol = protocols[decl.name];
  if (existingProtocol) {
    // If it's from a different module, provide a more helpful error message
    if (
      existingProtocol.moduleName &&
      existingProtocol.moduleName !== moduleName
    ) {
      throw new SemanticError(
        `Protocol '${decl.name}' conflicts with protocol from module '${existingProtocol.moduleName}'. ` +
          `Consider using a different name or qualified imports.`,
        decl.span,
      );
    }
    throw new SemanticError(`Duplicate protocol '${decl.name}'`, decl.span);
  }

  // Validate type parameters are unique
  const paramSet = new Set<string>();
  for (const param of decl.params) {
    if (paramSet.has(param)) {
      throw new SemanticError(
        `Duplicate type parameter '${param}' in protocol '${decl.name}'`,
        decl.span,
      );
    }
    paramSet.add(param);
  }

  // Validate at least one method
  if (decl.methods.length === 0) {
    throw new SemanticError(
      `Protocol '${decl.name}' must have at least one method`,
      decl.span,
    );
  }

  // Create SHARED type variable context for ALL protocol parameters
  // This ensures all methods use the same type variable IDs for protocol params
  const sharedTypeVarCtx = new Map<string, TypeVar>();
  for (const param of decl.params) {
    sharedTypeVarCtx.set(param, freshType());
  }

  // Create the constraint for this protocol
  const protocolConstraint: Constraint = {
    protocolName: decl.name,
    typeArgs: decl.params.map((p) => sharedTypeVarCtx.get(p)!),
  };

  // Get the set of quantified variable IDs
  const quantifiedVars = new Set<number>();
  for (const tv of sharedTypeVarCtx.values()) {
    quantifiedVars.add(tv.id);
  }

  // Convert method type expressions to internal types
  const methods = new Map<string, ProtocolMethodInfo>();
  const methodNames = new Set<string>();

  for (const method of decl.methods) {
    // Check for duplicate method names
    if (methodNames.has(method.name)) {
      throw new SemanticError(
        `Duplicate method '${method.name}' in protocol '${decl.name}'`,
        method.span,
      );
    }
    methodNames.add(method.name);

    let methodType: Type;

    if (method.type) {
      // Has explicit type annotation - convert from AST to internal representation
      // Use the shared type variable context so all methods use same var IDs
      methodType = typeFromAnnotation(
        method.type,
        sharedTypeVarCtx,
        adts,
        typeAliases,
      );
    } else if (method.defaultImpl) {
      // No explicit type annotation, but has default implementation
      // Infer the type from the default implementation by analyzing it as a lambda
      const lambdaExpr = makeLambda(
        method.defaultImpl.args,
        method.defaultImpl.body,
        method.span,
      );

      // Create a temporary scope for type inference
      const tempScope: Scope = { parent: globalScope, symbols: new Map() };

      // Infer the type of the lambda
      const inferredType = analyzeExpr(
        lambdaExpr,
        tempScope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );

      // Apply substitutions to get the final type
      methodType = applySubstitution(inferredType, substitution);

      // Replace any fresh type variables with the protocol's type parameters where appropriate
      // This ensures the inferred type uses the protocol's type variables
      methodType = substituteProtocolVars(methodType, sharedTypeVarCtx);
    } else {
      // Neither type annotation nor default implementation
      throw new SemanticError(
        `Protocol method '${method.name}' must have either a type annotation or a default implementation`,
        method.span,
      );
    }

    // Store method info with optional default implementation
    const methodInfo: ProtocolMethodInfo = {
      type: methodType,
      span: method.span,
    };

    // If there's a default implementation, store it
    if (method.defaultImpl) {
      methodInfo.defaultImpl = {
        args: method.defaultImpl.args,
        body: method.defaultImpl.body,
      };
    }

    methods.set(method.name, methodInfo);

    // Add method to global scope as a polymorphic function with constraint
    // Only add if not already defined (don't override explicit definitions)
    if (!globalScope.symbols.has(method.name)) {
      // Combine superclass constraints with the protocol's own constraint
      const allConstraints: Constraint[] = [
        protocolConstraint,
        ...decl.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((ta) =>
            typeFromAnnotation(ta, sharedTypeVarCtx, adts, typeAliases),
          ),
        })),
      ];
      const scheme: TypeScheme = {
        vars: new Set(quantifiedVars), // Copy to avoid sharing
        constraints: allConstraints,
        type: methodType,
      };
      globalScope.symbols.set(method.name, scheme);
    }
  }

  // Convert superclass constraints from AST to internal representation
  const superclassConstraints: Constraint[] = decl.constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((ta) =>
      typeFromAnnotation(ta, sharedTypeVarCtx, adts, typeAliases),
    ),
  }));

  // Register the protocol
  protocols[decl.name] = {
    name: decl.name,
    moduleName,
    params: decl.params,
    superclassConstraints,
    methods,
    span: decl.span,
  };
}

/**
 * Process an 'implementing' clause on a type declaration.
 * Validates that all referenced protocols have all methods with defaults,
 * and creates synthetic implement blocks.
 */
function processImplementingClause(
  decl: TypeDeclaration,
  protocols: Record<string, ProtocolInfo>,
  instances: InstanceInfo[],
  localInstances: InstanceInfo[],
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  moduleName?: string,
) {
  if (!decl.implementing) return;

  for (const protocolName of decl.implementing) {
    const protocol = protocols[protocolName];

    // Check that the protocol exists
    if (!protocol) {
      throw new SemanticError(
        `Unknown protocol '${protocolName}' in 'implementing' clause`,
        decl.span,
      );
    }

    // Collect methods with and without defaults
    const methodsWithoutDefaults: string[] = [];
    const methodsWithDefaults: string[] = [];

    for (const [methodName, methodInfo] of protocol.methods) {
      if (methodInfo.defaultImpl) {
        methodsWithDefaults.push(methodName);
      } else {
        methodsWithoutDefaults.push(methodName);
      }
    }

    // If any method lacks a default, throw an error
    if (methodsWithoutDefaults.length > 0) {
      throw new ImplementingProtocolError(
        decl.name,
        protocolName,
        methodsWithoutDefaults,
        methodsWithDefaults,
        decl.span,
      );
    }

    // All methods have defaults - create a synthetic implement block
    // Build type arguments for the type being implemented
    // For a type like `type Pair a b = ...`, the type arg would be `Pair a b`
    const typeArgs: Type[] = [
      {
        kind: "con",
        name: decl.name,
        args: decl.params.map(
          (param): TypeVar => ({
            kind: "var",
            id: freshType().id,
          }),
        ),
      },
    ];

    // Create method implementations from defaults
    const methods = new Map<string, Expr>();
    for (const [methodName, methodInfo] of protocol.methods) {
      if (methodInfo.defaultImpl) {
        // Create a lambda from the default implementation
        const defaultLambda = makeLambda(
          methodInfo.defaultImpl.args,
          methodInfo.defaultImpl.body,
          methodInfo.span,
        );
        methods.set(methodName, defaultLambda);
      }
    }

    // Create a temporary instance info for overlap checking
    const newInstance: InstanceInfo = {
      protocolName,
      moduleName,
      typeArgs,
      constraints: [], // Synthetic implementations have no extra constraints
      methods,
      explicitMethods: new Set(),
      span: decl.span,
    };

    // Check for overlapping instances
    for (const existing of instances) {
      if (existing.protocolName !== protocolName) continue;

      // Check if instances overlap, considering constraints
      if (instancesOverlap(existing, newInstance, instances)) {
        throw new SemanticError(
          `Overlapping implementation for protocol '${protocolName}' (from 'implementing' clause)`,
          decl.span,
        );
      }
    }

    // Register in both global and local instance lists
    instances.push(newInstance);
    localInstances.push(newInstance);
  }
}

/**
 * Register an implementation declaration in the instance registry.
 */
function registerImplementation(
  decl: ImplementationDeclaration,
  instances: InstanceInfo[],
  localInstances: InstanceInfo[],
  protocols: Record<string, ProtocolInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  moduleName?: string,
) {
  // Check that the protocol exists
  const protocol = protocols[decl.protocolName];
  if (!protocol) {
    throw new SemanticError(
      `Unknown protocol '${decl.protocolName}'`,
      decl.span,
    );
  }

  // Validate number of type arguments matches protocol parameters
  if (decl.typeArgs.length !== protocol.params.length) {
    throw new SemanticError(
      `Protocol '${decl.protocolName}' expects ${protocol.params.length} type argument(s), but got ${decl.typeArgs.length}`,
      decl.span,
    );
  }

  // Create a type variable context for converting type expressions
  const typeVarCtx = new Map<string, TypeVar>();

  // Convert type arguments from AST to internal representation
  const typeArgs: Type[] = [];
  for (const typeArg of decl.typeArgs) {
    typeArgs.push(typeFromAnnotation(typeArg, typeVarCtx, adts, typeAliases));
  }

  // Convert constraints
  const constraints: Constraint[] = [];
  for (const astConstraint of decl.constraints) {
    const constraintTypeArgs: Type[] = [];
    for (const typeArg of astConstraint.typeArgs) {
      constraintTypeArgs.push(
        typeFromAnnotation(typeArg, typeVarCtx, adts, typeAliases),
      );
    }
    constraints.push({
      protocolName: astConstraint.protocolName,
      typeArgs: constraintTypeArgs,
    });
  }

  // Validate that all required methods (those without defaults) are implemented
  const implementedMethods = new Set(decl.methods.map((m) => m.name));
  const allMethods = new Set(protocol.methods.keys());

  // Check that methods without defaults are implemented
  for (const methodName of allMethods) {
    const methodInfo = protocol.methods.get(methodName)!;
    if (!implementedMethods.has(methodName) && !methodInfo.defaultImpl) {
      throw new SemanticError(
        `Instance is missing implementation for method '${methodName}'`,
        decl.span,
      );
    }
  }

  // Check for extra methods that aren't in the protocol
  for (const implemented of implementedMethods) {
    if (!allMethods.has(implemented)) {
      throw new SemanticError(
        `Method '${implemented}' is not part of protocol '${decl.protocolName}'`,
        decl.span,
      );
    }
  }

  // Convert method implementations to a map
  // For methods not explicitly implemented, use the default implementation from the protocol
  const methods = new Map<string, Expr>();
  const explicitMethods = new Set<string>();
  for (const method of decl.methods) {
    // If the method has inline pattern arguments, wrap in a lambda
    // e.g., `toString a = showA a` becomes `\a -> showA a`
    if (method.args && method.args.length > 0) {
      const lambda = makeLambda(
        method.args,
        method.implementation,
        method.span,
      );
      methods.set(method.name, lambda);
    } else {
      methods.set(method.name, method.implementation);
    }
    explicitMethods.add(method.name);
  }

  // For methods with defaults that weren't explicitly implemented,
  // create a lambda expression from the default implementation
  for (const [methodName, methodInfo] of protocol.methods) {
    if (!methods.has(methodName) && methodInfo.defaultImpl) {
      // Create a lambda from the default implementation
      // The lambda wraps the args and body from the default
      const defaultLambda = makeLambda(
        methodInfo.defaultImpl.args,
        methodInfo.defaultImpl.body,
        methodInfo.span,
      );
      methods.set(methodName, defaultLambda);
    }
  }

  // Create the instance info
  const instanceInfo: InstanceInfo = {
    protocolName: decl.protocolName,
    moduleName,
    typeArgs,
    constraints,
    methods,
    explicitMethods,
    span: decl.span,
  };

  // Check for overlapping instances
  // An implementation overlaps if another implementation exists for the same protocol and type
  // AND their constraints don't prevent overlap
  for (const existing of instances) {
    if (existing.protocolName !== decl.protocolName) continue;

    // Check if instances overlap, considering constraints
    if (instancesOverlap(existing, instanceInfo, instances)) {
      throw new SemanticError(
        `Overlapping implementation for protocol '${decl.protocolName}'`,
        decl.span,
      );
    }
  }

  // Register the instance in both the global list and local list
  // The global list includes imported instances for overlap checking
  // The local list is used for validation (only validate methods in this module)
  instances.push(instanceInfo);
  localInstances.push(instanceInfo);
}

/**
 * Concretize polymorphic instance type args by analyzing method bodies.
 *
 * When an implementation body forces a polymorphic type arg to be concrete
 * (e.g., `convert3 _ = [1]` forces `a` to be `List Int`), we update the
 * instance's typeArgs so that later generalization produces concrete types
 * instead of leaving quantified variables with constraints.
 *
 * This fixes hover showing `ExampleProtocol3 Float t402 => t402` instead of `List Int`.
 */
function concretizeInstanceTypeArgs(
  localInstances: InstanceInfo[],
  allInstances: InstanceInfo[],
  protocols: Record<string, ProtocolInfo>,
  globalScope: Scope,
  substitution: Substitution,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
): void {
  for (const instance of localInstances) {
    const protocol = protocols[instance.protocolName];
    if (!protocol) continue;

    // Only process instances that have type variables in their typeArgs
    const hasTypeVars = instance.typeArgs.some((t) => t.kind === "var");
    if (!hasTypeVars) continue;

    // Analyze each explicitly implemented method to infer concrete types
    for (const methodName of instance.explicitMethods) {
      const methodExpr = instance.methods.get(methodName);
      const protocolMethodInfo = protocol.methods.get(methodName);

      if (!methodExpr || !protocolMethodInfo) continue;

      // Get the expected type from the protocol, substituting type parameters
      const expectedType = substituteTypeParams(
        protocolMethodInfo.type,
        protocol.params,
        instance.typeArgs,
      );

      // Create a fresh substitution for inference
      const inferSubstitution: Substitution = new Map(substitution);

      // Infer the type of the implementation expression
      const tempScope: Scope = { symbols: new Map(), parent: globalScope };
      try {
        const inferredType = analyzeExpr(
          methodExpr,
          tempScope,
          inferSubstitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );

        // Unify to get concrete types
        unify(inferredType, expectedType, methodExpr.span, inferSubstitution);

        // Apply the inference results back to the instance's typeArgs
        for (let i = 0; i < instance.typeArgs.length; i++) {
          const typeArg = instance.typeArgs[i]!;
          const resolved = applySubstitution(typeArg, inferSubstitution);
          // Only update if we got a more concrete type (not still a bare type variable)
          if (resolved.kind !== "var" && typeArg.kind === "var") {
            instance.typeArgs[i] = resolved;
          }
        }

        // Update constraints' type args and remove fully-satisfied constraints
        // When a constraint's type arg becomes concrete (e.g., Appendable a -> Appendable (List Int)),
        // and there's a matching instance, the constraint is satisfied and can be removed.
        const constraintsToRemove: Constraint[] = [];
        for (const constraint of instance.constraints) {
          let allConcrete = true;
          for (let i = 0; i < constraint.typeArgs.length; i++) {
            const typeArg = constraint.typeArgs[i]!;
            const resolved = applySubstitution(typeArg, inferSubstitution);
            if (resolved.kind !== "var" && typeArg.kind === "var") {
              constraint.typeArgs[i] = resolved;
            }
            if (constraint.typeArgs[i]!.kind === "var") {
              allConcrete = false;
            }
          }

          // If all type args are now concrete, check if the constraint is satisfied
          if (allConcrete) {
            const isSatisfied = findInstanceForConstraint(
              constraint.protocolName,
              constraint.typeArgs,
              allInstances,
            );
            if (isSatisfied) {
              constraintsToRemove.push(constraint);
            }
          }
        }

        // Remove satisfied constraints from the instance
        for (const toRemove of constraintsToRemove) {
          const idx = instance.constraints.indexOf(toRemove);
          if (idx !== -1) {
            instance.constraints.splice(idx, 1);
          }
        }
      } catch {
        // If analysis fails, continue - validation will catch errors later
      }
    }
  }
}

/**
 * Check if there's an instance that satisfies a constraint with the given type args.
 */
function findInstanceForConstraint(
  protocolName: string,
  typeArgs: Type[],
  instances: InstanceInfo[],
): boolean {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName) continue;
    if (inst.typeArgs.length !== typeArgs.length) continue;

    // Check if all type args match
    let allMatch = true;
    for (let i = 0; i < typeArgs.length; i++) {
      if (!instanceTypeMatches(inst.typeArgs[i]!, typeArgs[i]!)) {
        allMatch = false;
        break;
      }
    }
    if (allMatch) return true;
  }
  return false;
}

/**
 * Validate that all method implementations in protocol instances have types
 * that match the protocol's declared method signatures.
 *
 * For example, if Show declares `toString : a -> String`, and we're implementing
 * `Show A`, then the implementation must have type `A -> String`, not `String`.
 */
function validateImplementationMethodTypes(
  localInstances: InstanceInfo[],
  allInstances: InstanceInfo[],
  protocols: Record<string, ProtocolInfo>,
  globalScope: Scope,
  substitution: Substitution,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
): void {
  for (const instance of localInstances) {
    const protocol = protocols[instance.protocolName];
    if (!protocol) continue;

    // Build a substitution from protocol type params to instance type args
    // e.g., for `implement Show A`, map protocol param 'a' to concrete type 'A'
    const paramSubstitution = new Map<number, Type>();
    const paramNameToId = new Map<string, number>();

    // First, create type variables for each protocol parameter
    for (let i = 0; i < protocol.params.length; i++) {
      const paramName = protocol.params[i]!;
      const typeArg = instance.typeArgs[i];
      if (typeArg) {
        // Create a fresh type variable ID for the parameter
        const paramVar = freshType();
        paramNameToId.set(paramName, paramVar.id);
        // Map this type variable to the concrete instance type
        paramSubstitution.set(paramVar.id, typeArg);
      }
    }

    // Validate each explicitly implemented method
    for (const methodName of instance.explicitMethods) {
      const methodExpr = instance.methods.get(methodName);
      const protocolMethodInfo = protocol.methods.get(methodName);

      if (!methodExpr || !protocolMethodInfo) continue;

      // Get the expected type from the protocol, substituting type parameters
      const expectedType = substituteTypeParams(
        protocolMethodInfo.type,
        protocol.params,
        instance.typeArgs,
      );

      // Create a fresh substitution for inference
      const inferSubstitution: Substitution = new Map(substitution);

      // Infer the type of the implementation expression
      const tempScope: Scope = { symbols: new Map(), parent: globalScope };
      const inferredType = analyzeExpr(
        methodExpr,
        tempScope,
        inferSubstitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );

      // Try to unify the inferred type with the expected type
      try {
        unify(inferredType, expectedType, methodExpr.span, inferSubstitution);
      } catch (e) {
        if (e instanceof SemanticError) {
          throw new SemanticError(
            `Implementation of '${methodName}' for '${
              instance.protocolName
            }' has type '${formatType(
              applySubstitution(inferredType, inferSubstitution),
            )}' but protocol expects '${formatType(expectedType)}'`,
            methodExpr.span,
          );
        }
        throw e;
      }

      // After successful unification, check that any constraints on the instance
      // are satisfied when type variables are unified with concrete types.
      // For example, if we have `implement Appendable a => Proto Float a`
      // and the method implementation returns Int, then a=Int must satisfy Appendable.
      for (const constraint of instance.constraints) {
        for (const constraintTypeArg of constraint.typeArgs) {
          // Apply the substitution to see what the constraint type arg resolves to
          const resolvedType = applySubstitution(
            constraintTypeArg,
            inferSubstitution,
          );

          // If it resolved to a concrete type, check that an instance exists
          if (resolvedType.kind === "con") {
            const hasInstance = findInstanceForTypeInternal(
              constraint.protocolName,
              resolvedType,
              allInstances,
            );
            if (!hasInstance) {
              throw new SemanticError(
                `Implementation of '${methodName}' for '${instance.protocolName}' ` +
                  `requires '${constraint.protocolName}' constraint on type parameter, ` +
                  `but the implementation uses type '${formatType(
                    resolvedType,
                  )}' ` +
                  `which does not implement '${constraint.protocolName}'`,
                methodExpr.span,
              );
            }
          }
        }
      }

      // IMPORTANT: Concretize the instance's type args based on inference results.
      // When the implementation body forces a polymorphic type arg to be concrete
      // (e.g., `convert3 _ = [1]` forces `a` to be `List Int`), we update the
      // instance's typeArgs so that later generalization produces concrete types
      // instead of leaving quantified variables with constraints.
      // This fixes hover showing `ExampleProtocol3 Float t402 => t402` instead of `List Int`.
      for (let i = 0; i < instance.typeArgs.length; i++) {
        const typeArg = instance.typeArgs[i]!;
        const resolved = applySubstitution(typeArg, inferSubstitution);
        // Only update if we got a more concrete type (not still a bare type variable)
        if (resolved.kind !== "var" && typeArg.kind === "var") {
          instance.typeArgs[i] = resolved;
        }
      }

      // Also update constraints' type args to reflect any concretization
      for (const constraint of instance.constraints) {
        for (let i = 0; i < constraint.typeArgs.length; i++) {
          const typeArg = constraint.typeArgs[i]!;
          const resolved = applySubstitution(typeArg, inferSubstitution);
          if (resolved.kind !== "var" && typeArg.kind === "var") {
            constraint.typeArgs[i] = resolved;
          }
        }
      }
    }
  }
}

/**
 * Substitute protocol type variables with concrete types.
 * Used to specialize a method type for a particular instance.
 *
 * The strategy is to collect all unique type variable IDs from the method type
 * and map them to the instance's type arguments by the order they first appear.
 * This works because the protocol method types use consistent type variable IDs
 * across all methods for the same protocol parameter.
 */
function substituteTypeParams(
  type: Type,
  params: string[],
  typeArgs: Type[],
): Type {
  // Collect all unique type variable IDs from the method type in order
  const varIds: number[] = [];
  collectTypeVarIdsOrdered(type, varIds, new Set());

  // Build a substitution map from type variable IDs to concrete types
  // We assume the first N unique type vars correspond to the N protocol params
  const substitution = new Map<number, Type>();
  for (let i = 0; i < Math.min(varIds.length, typeArgs.length); i++) {
    substitution.set(varIds[i]!, typeArgs[i]!);
  }

  return applyTypeSubstitution(type, substitution);
}

/**
 * Validate that all identifiers referenced in implementation method expressions
 * are defined in the current scope. This ensures we catch undefined function
 * references like `intAdd` at compile time rather than runtime.
 *
 * This is called after all value declarations have been processed, so the
 * globalScope contains all defined symbols.
 *
 * Note: Only validates explicitly provided method implementations, not default
 * implementations inherited from protocols (those are validated during protocol
 * registration in a scope that includes other protocol methods).
 */
function validateImplementationMethodExpressions(
  instances: InstanceInfo[],
  globalScope: Scope,
  constructors: Record<string, ConstructorInfo>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
): void {
  for (const instance of instances) {
    // Only validate methods that were explicitly provided in the implement block
    // Default implementations from protocols are validated during protocol registration
    for (const methodName of instance.explicitMethods) {
      const methodExpr = instance.methods.get(methodName);
      if (methodExpr) {
        validateExpressionIdentifiers(
          methodExpr,
          globalScope,
          constructors,
          instance.protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
    }
  }
}

/**
 * Validate that constraints on polymorphic instances are satisfiable.
 *
 * For each instance with constraints (e.g., `implement Eq a => ExampleProtocol a`),
 * we check that:
 * 1. The constraint protocol exists
 *
 * Note: We don't require instances to exist at this point because they may be
 * defined in other modules that are imported at the call site. The actual
 * satisfiability check happens during code generation when we know the concrete types.
 */
function validateInstanceConstraintSatisfiability(
  localInstances: InstanceInfo[],
  allInstances: InstanceInfo[],
  protocols: Record<string, ProtocolInfo>,
  _adts: Record<string, ADTInfo>,
): void {
  for (const instance of localInstances) {
    // Skip instances without constraints
    if (instance.constraints.length === 0) continue;

    for (const constraint of instance.constraints) {
      // Validate the constraint protocol exists
      const constraintProtocol = protocols[constraint.protocolName];
      if (!constraintProtocol) {
        throw new SemanticError(
          `Instance constraint references unknown protocol '${constraint.protocolName}'`,
          instance.span,
        );
      }

      // Note: We intentionally don't check if instances exist because they may be
      // defined in other modules. The check happens at code generation time when
      // the instance is used with concrete types.
    }
  }
}

/**
 * Validate that concrete-type constraints have corresponding instances.
 *
 * When a polymorphic function with constraints (e.g., `Eq a => a -> Bool`) is
 * called with a concrete type, we need to verify that the concrete type has
 * an instance of the required protocol.
 *
 * For example, if we have:
 *   implement Eq a => ExampleProtocol a where exampleMethod var = var == var
 *   main = exampleMethod A  -- where A is a type without Eq instance
 *
 * This function detects that `Eq A` is required but no such instance exists.
 */
function validateConcreteConstraintInstances(
  values: Record<string, ValueInfo>,
  instances: InstanceInfo[],
  protocols: Record<string, ProtocolInfo>,
  substitution: Substitution,
): void {
  for (const [valueName, valueInfo] of Object.entries(values)) {
    // Skip synthetic values and values without inferred constraints
    if (valueName.startsWith("$")) continue;

    // For now, we validate by checking that for each constraint on a concrete type
    // in the value info's collectedConstraints (if available), an instance exists
    if (valueInfo.collectedConstraints) {
      for (const constraint of valueInfo.collectedConstraints) {
        // Apply substitution to get the resolved constraint types
        const resolvedTypeArgs = constraint.typeArgs.map((t) =>
          applySubstitution(t, substitution),
        );

        // Check if any type argument is a concrete type (not a type variable)
        for (const typeArg of resolvedTypeArgs) {
          if (typeArg.kind === "con") {
            // This is a constraint on a concrete type - validate instance exists
            const hasInstance = findInstanceForTypeInternal(
              constraint.protocolName,
              typeArg,
              instances,
            );

            if (!hasInstance) {
              const span = valueInfo.span ?? valueInfo.declaration.span;
              throw new SemanticError(
                `No instance of '${
                  constraint.protocolName
                }' for type '${formatType(typeArg)}'. ` +
                  `Add an implementation: implement ${
                    constraint.protocolName
                  } ${formatType(typeArg)} where ...`,
                span,
              );
            }
          }
        }
      }
    }
  }
}

/**
 * Check if an instance type pattern matches a concrete type.
 *
 * Type variables in the instance type match anything (including other type variables).
 * This is used for instance lookup where `implement Protocol (List a)` should match
 * when looking for `Protocol (List Int)` or `Protocol (List t121)`.
 */
function instanceTypeMatches(instType: Type, concreteType: Type): boolean {
  // Type variable in instance matches anything
  if (instType.kind === "var") {
    return true;
  }

  // Both must be the same kind
  if (instType.kind !== concreteType.kind) {
    return false;
  }

  if (instType.kind === "con" && concreteType.kind === "con") {
    if (instType.name !== concreteType.name) {
      return false;
    }
    if (instType.args.length !== concreteType.args.length) {
      return false;
    }
    for (let i = 0; i < instType.args.length; i++) {
      if (!instanceTypeMatches(instType.args[i]!, concreteType.args[i]!)) {
        return false;
      }
    }
    return true;
  }

  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return (
      instanceTypeMatches(instType.from, concreteType.from) &&
      instanceTypeMatches(instType.to, concreteType.to)
    );
  }

  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0; i < instType.elements.length; i++) {
      if (
        !instanceTypeMatches(instType.elements[i]!, concreteType.elements[i]!)
      ) {
        return false;
      }
    }
    return true;
  }

  if (instType.kind === "record" && concreteType.kind === "record") {
    const instKeys = Object.keys(instType.fields);
    const concreteKeys = Object.keys(concreteType.fields);
    if (instKeys.length !== concreteKeys.length) {
      return false;
    }
    for (const key of instKeys) {
      if (!(key in concreteType.fields)) {
        return false;
      }
      if (
        !instanceTypeMatches(instType.fields[key]!, concreteType.fields[key]!)
      ) {
        return false;
      }
    }
    return true;
  }

  // Same kind but not handled - use equality
  return typesEqual(instType, concreteType);
}

/**
 * Check if a constraint can possibly be satisfied, even if it contains type variables.
 * This is a more permissive check than validateConstraintSatisfiable - it returns
 * possible=true if there COULD be a valid instance, and possible=false if the
 * constraint can NEVER be satisfied.
 *
 * The key insight is that certain type shapes (like function types) may not have
 * any instances for certain protocols. For example, there's typically no
 * `Appendable (a -> b)` instance, so `Appendable (List t -> r)` can never be satisfied.
 *
 * @param constraint - The constraint to check
 * @param instances - Available protocol instances
 * @returns {possible: boolean} - Whether the constraint could possibly be satisfied
 */
function checkConstraintSatisfiability(
  constraint: Constraint,
  instances: InstanceInfo[],
): { possible: boolean } {
  // If all type args are fully concrete, do a full validation
  const hasFreeVars = constraint.typeArgs.some((t) => {
    const freeVars = getFreeTypeVars(t, new Map());
    return freeVars.size > 0;
  });

  if (!hasFreeVars) {
    // All concrete - do full validation
    const result = validateConstraintSatisfiable(constraint, instances);
    return { possible: result.found };
  }

  // Has free type variables - check if the type shape could match any instance
  // For each type arg, check if its "shape" (outermost constructor or function)
  // could possibly match an instance
  for (const typeArg of constraint.typeArgs) {
    // Get the outermost shape of the type
    const shape = getTypeShape(typeArg);

    if (shape === "fun") {
      // This is a function type. Check if there's ANY instance for function types.
      // If not, this constraint can never be satisfied.
      const hasFunctionInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName) return false;
        // Check if any type arg of the instance is a function type or type variable
        // A type variable could match a function, so that counts
        return inst.typeArgs.some((t) => t.kind === "fun" || t.kind === "var");
      });

      if (!hasFunctionInstance) {
        // No instance could possibly match a function type for this protocol
        return { possible: false };
      }
    }

    if (shape === "tuple") {
      // Similar check for tuple types
      const hasTupleInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName) return false;
        return inst.typeArgs.some(
          (t) => t.kind === "tuple" || t.kind === "var",
        );
      });

      if (!hasTupleInstance) {
        return { possible: false };
      }
    }

    if (shape === "record") {
      // Similar check for record types
      const hasRecordInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName) return false;
        return inst.typeArgs.some(
          (t) => t.kind === "record" || t.kind === "var",
        );
      });

      if (!hasRecordInstance) {
        return { possible: false };
      }
    }

    // For concrete types (con) with a specific name, check if there could be a matching instance
    if (shape === "con" && typeArg.kind === "con") {
      const hasMatchingInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName) return false;
        return inst.typeArgs.some(
          (t) =>
            t.kind === "var" || (t.kind === "con" && t.name === typeArg.name),
        );
      });

      if (!hasMatchingInstance) {
        return { possible: false };
      }
    }
  }

  // Could not rule out satisfiability - assume it's possible
  return { possible: true };
}

/**
 * Get the "shape" of a type - its outermost constructor kind.
 * This is used for quick checks of whether a type could possibly match an instance.
 */
function getTypeShape(type: Type): "var" | "con" | "fun" | "tuple" | "record" {
  switch (type.kind) {
    case "var":
      return "var";
    case "con":
      return "con";
    case "fun":
      return "fun";
    case "tuple":
      return "tuple";
    case "record":
      return "record";
    default:
      return "con"; // Default fallback
  }
}

/**
 * Validate that a constraint (with all its type arguments) is satisfiable.
 * This handles multi-parameter protocols correctly by:
 * 1. Matching all type arguments against instance type arguments
 * 2. Building a substitution for type variables in the instance
 * 3. Checking that instance constraints are satisfied with the substituted types
 *
 * @param constraint - The full constraint to validate (protocol name + all type args)
 * @param instances - Available protocol instances
 * @returns Information about whether the constraint is satisfiable
 */
function validateConstraintSatisfiable(
  constraint: Constraint,
  instances: InstanceInfo[],
): InstanceLookupResult {
  let firstUnsatisfiedConstraint: {
    constraint: string;
    forType: string;
  } | null = null;

  for (const inst of instances) {
    if (inst.protocolName !== constraint.protocolName) continue;

    // Must have same number of type arguments
    if (inst.typeArgs.length !== constraint.typeArgs.length) continue;

    // Try to match all type arguments and build a substitution
    // The substitution maps instance type variables to concrete types from the constraint
    const instSubstitution = new Map<number, Type>();
    let allArgsMatch = true;

    for (let i = 0; i < inst.typeArgs.length; i++) {
      const instArg = inst.typeArgs[i]!;
      const constraintArg = constraint.typeArgs[i]!;

      if (!matchTypeArgForInstance(instArg, constraintArg, instSubstitution)) {
        allArgsMatch = false;
        break;
      }
    }

    if (!allArgsMatch) continue;

    // All type args matched! Now check if instance constraints are satisfied
    if (inst.constraints.length === 0) {
      return { found: true }; // No constraints, instance matches
    }

    // Check each constraint on the instance
    let allConstraintsSatisfied = true;
    for (const instConstraint of inst.constraints) {
      // Apply the substitution to the constraint's type args
      // e.g., if instance has `Appendable a` and we matched a -> List Int,
      // we need to check if `Appendable (List Int)` holds
      const substitutedTypeArgs = instConstraint.typeArgs.map((t) =>
        applySubstitution(t, instSubstitution),
      );

      const substitutedConstraint: Constraint = {
        protocolName: instConstraint.protocolName,
        typeArgs: substitutedTypeArgs,
      };

      // Check if constraint can potentially be satisfied
      // Even with free type variables, some type shapes (like function types)
      // might never have an instance for certain protocols
      const canBeSatisfied = checkConstraintSatisfiability(
        substitutedConstraint,
        instances,
      );

      if (!canBeSatisfied.possible) {
        allConstraintsSatisfied = false;
        if (!firstUnsatisfiedConstraint) {
          firstUnsatisfiedConstraint = {
            constraint: instConstraint.protocolName,
            forType: substitutedTypeArgs.map((t) => formatType(t)).join(", "),
          };
        }
        break;
      }
    }

    if (allConstraintsSatisfied) {
      return { found: true };
    }
  }

  // No matching instance found
  if (firstUnsatisfiedConstraint) {
    return {
      found: false,
      reason: "unsatisfied-constraint",
      constraint: firstUnsatisfiedConstraint.constraint,
      forType: firstUnsatisfiedConstraint.forType,
    };
  }

  return { found: false, reason: "no-instance" };
}

/**
 * Try to match an instance type argument against a constraint type argument.
 * Builds up a substitution mapping instance type variables to concrete types.
 *
 * Returns true if the match succeeds, false otherwise.
 */
function matchTypeArgForInstance(
  instArg: Type,
  constraintArg: Type,
  substitution: Map<number, Type>,
): boolean {
  // If instance arg is a type variable, it can match anything
  // Record the mapping for constraint validation
  if (instArg.kind === "var") {
    const existing = substitution.get(instArg.id);
    if (existing) {
      // Already mapped - must be equal
      return typesEqual(existing, constraintArg);
    }
    substitution.set(instArg.id, constraintArg);
    return true;
  }

  // If constraint arg is a type variable, this is a polymorphic constraint
  // We can't fully resolve it, but the match can succeed
  if (constraintArg.kind === "var") {
    // For polymorphic constraints, we allow the match but can't fully validate
    return true;
  }

  // Both must be same kind
  if (instArg.kind !== constraintArg.kind) return false;

  if (instArg.kind === "con" && constraintArg.kind === "con") {
    if (instArg.name !== constraintArg.name) return false;
    if (instArg.args.length !== constraintArg.args.length) return false;
    for (let i = 0; i < instArg.args.length; i++) {
      if (
        !matchTypeArgForInstance(
          instArg.args[i]!,
          constraintArg.args[i]!,
          substitution,
        )
      ) {
        return false;
      }
    }
    return true;
  }

  if (instArg.kind === "fun" && constraintArg.kind === "fun") {
    return (
      matchTypeArgForInstance(instArg.from, constraintArg.from, substitution) &&
      matchTypeArgForInstance(instArg.to, constraintArg.to, substitution)
    );
  }

  return typesEqual(instArg, constraintArg);
}

/**
 * Find an instance for a given protocol and concrete type.
 * Returns detailed information about whether a matching instance exists
 * and why it might not match.
 */
function findInstanceForTypeWithReason(
  protocolName: string,
  concreteType: TypeCon,
  instances: InstanceInfo[],
): InstanceLookupResult {
  let hasPolymorphicInstance = false;
  let firstUnsatisfiedConstraint: {
    constraint: string;
    forType: string;
  } | null = null;

  for (const inst of instances) {
    if (inst.protocolName !== protocolName) continue;

    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg) continue;

    // Exact match: instance for the concrete type
    if (
      instTypeArg.kind === "con" &&
      instTypeArg.name === concreteType.name &&
      instTypeArg.args.length === concreteType.args.length
    ) {
      // Check if type arguments match (for parameterized types like List Int)
      // Use instanceTypeMatches which allows type variables in the instance to match anything
      let argsMatch = true;
      for (let i = 0; i < instTypeArg.args.length; i++) {
        if (!instanceTypeMatches(instTypeArg.args[i]!, concreteType.args[i]!)) {
          argsMatch = false;
          break;
        }
      }
      if (argsMatch) {
        return { found: true };
      }
    }

    // Polymorphic match: instance with type variable that could match
    // BUT we need to check if the instance's constraints are satisfiable
    if (instTypeArg.kind === "var") {
      hasPolymorphicInstance = true;
      // This is a polymorphic instance - check if constraints are satisfiable
      if (inst.constraints.length === 0) {
        return { found: true }; // No constraints, always matches
      }

      // Check if all constraints can be satisfied for the concrete type
      let allConstraintsSatisfied = true;
      for (const constraint of inst.constraints) {
        // For each constraint like `Eq a`, check if `Eq concreteType` exists
        const constraintResult = findInstanceForTypeWithReason(
          constraint.protocolName,
          concreteType,
          instances,
        );
        if (!constraintResult.found) {
          allConstraintsSatisfied = false;
          if (!firstUnsatisfiedConstraint) {
            firstUnsatisfiedConstraint = {
              constraint: constraint.protocolName,
              forType: formatType(concreteType),
            };
          }
          break;
        }
      }

      if (allConstraintsSatisfied) {
        return { found: true };
      }
    }
  }

  // If we found a polymorphic instance but constraints weren't satisfied
  if (hasPolymorphicInstance && firstUnsatisfiedConstraint) {
    return {
      found: false,
      reason: "unsatisfied-constraint",
      constraint: firstUnsatisfiedConstraint.constraint,
      forType: firstUnsatisfiedConstraint.forType,
    };
  }

  return { found: false, reason: "no-instance" };
}

/**
 * Simple boolean version for backward compatibility
 */
function findInstanceForTypeInternal(
  protocolName: string,
  concreteType: TypeCon,
  instances: InstanceInfo[],
): boolean {
  return findInstanceForTypeWithReason(protocolName, concreteType, instances)
    .found;
}

/**
 * Validate that all identifiers referenced in protocol default implementation
 * expressions are defined in the current scope. This ensures we catch undefined
 * function references at compile time.
 *
 * This is called after all value declarations have been processed, so the
 * globalScope contains all defined symbols.
 */
function validateProtocolDefaultImplementations(
  protocols: Record<string, ProtocolInfo>,
  globalScope: Scope,
  constructors: Record<string, ConstructorInfo>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
  currentModuleName?: string,
): void {
  for (const [protocolName, protocol] of Object.entries(protocols)) {
    // Only validate protocols defined in this module
    if (protocol.moduleName !== currentModuleName) continue;

    for (const [methodName, methodInfo] of protocol.methods) {
      if (methodInfo.defaultImpl) {
        // Create a scope with the method parameters bound
        const methodScope: Scope = { symbols: new Map(), parent: globalScope };
        for (const arg of methodInfo.defaultImpl.args) {
          bindPatternNames(arg, methodScope);
        }

        // Validate the body expression
        validateExpressionIdentifiers(
          methodInfo.defaultImpl.body,
          methodScope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
    }
  }
}

/**
 * Recursively validate that all identifier references in an expression exist
 * in the given scope. Throws SemanticError for undefined identifiers.
 *
 * @param expr The expression to validate
 * @param scope The scope to look up identifiers in
 * @param constructors Known constructors (for distinguishing value vs constructor refs)
 * @param protocolName Protocol name for error context
 * @param methodName Method name for error context
 * @param imports Import declarations for module resolution
 * @param dependencies Dependency modules for module-qualified access validation
 */
function validateExpressionIdentifiers(
  expr: Expr,
  scope: Scope,
  constructors: Record<string, ConstructorInfo>,
  protocolName: string,
  methodName: string,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
): void {
  switch (expr.kind) {
    case "Var": {
      const name = expr.name;
      // Check if it's a constructor (constructors are always valid)
      if (constructors[name]) {
        return;
      }
      // Check if it's defined in scope
      if (!symbolExists(scope, name)) {
        throw new SemanticError(
          `Undefined name '${name}' in implementation of '${methodName}' for protocol '${protocolName}'`,
          expr.span,
        );
      }
      return;
    }
    case "Lambda": {
      // Create a child scope with the lambda arguments
      const childScope: Scope = { symbols: new Map(), parent: scope };
      for (const arg of expr.args) {
        bindPatternNames(arg, childScope);
      }
      validateExpressionIdentifiers(
        expr.body,
        childScope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "Apply": {
      validateExpressionIdentifiers(
        expr.callee,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      for (const arg of expr.args) {
        validateExpressionIdentifiers(
          arg,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "If": {
      validateExpressionIdentifiers(
        expr.condition,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      validateExpressionIdentifiers(
        expr.thenBranch,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      validateExpressionIdentifiers(
        expr.elseBranch,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "LetIn": {
      // Create a child scope for let bindings
      const childScope: Scope = { symbols: new Map(), parent: scope };
      for (const binding of expr.bindings) {
        // First validate the binding body in the parent scope
        validateExpressionIdentifiers(
          binding.body,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
        // Then add the binding name to the child scope
        childScope.symbols.set(binding.name, {
          vars: new Set(),
          constraints: [],
          type: { kind: "var", id: -1 }, // Placeholder type
        });
      }
      validateExpressionIdentifiers(
        expr.body,
        childScope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "Case": {
      validateExpressionIdentifiers(
        expr.discriminant,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      for (const branch of expr.branches) {
        // Create a child scope with pattern bindings
        const branchScope: Scope = { symbols: new Map(), parent: scope };
        bindPatternNames(branch.pattern, branchScope);
        validateExpressionIdentifiers(
          branch.body,
          branchScope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "Infix": {
      // For infix expressions, we need to check if the operator is defined
      // Operators like +, -, etc. are protocol methods and should be in scope
      if (!symbolExists(scope, expr.operator)) {
        throw new SemanticError(
          `Undefined operator '${expr.operator}' in implementation of '${methodName}' for protocol '${protocolName}'`,
          expr.span,
        );
      }
      validateExpressionIdentifiers(
        expr.left,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      validateExpressionIdentifiers(
        expr.right,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "Unary": {
      validateExpressionIdentifiers(
        expr.operand,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "Paren": {
      validateExpressionIdentifiers(
        expr.expression,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "Tuple": {
      for (const element of expr.elements) {
        validateExpressionIdentifiers(
          element,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "List": {
      for (const element of expr.elements) {
        validateExpressionIdentifiers(
          element,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "ListRange": {
      validateExpressionIdentifiers(
        expr.start,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      validateExpressionIdentifiers(
        expr.end,
        scope,
        constructors,
        protocolName,
        methodName,
        imports,
        dependencies,
      );
      return;
    }
    case "Record": {
      for (const field of expr.fields) {
        validateExpressionIdentifiers(
          field.value,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "RecordUpdate": {
      // Check that the base record exists
      if (!symbolExists(scope, expr.base)) {
        throw new SemanticError(
          `Undefined name '${expr.base}' in implementation of '${methodName}' for protocol '${protocolName}'`,
          expr.span,
        );
      }
      for (const field of expr.fields) {
        validateExpressionIdentifiers(
          field.value,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "FieldAccess": {
      // For field access (e.g., Int.add), validate module-qualified access
      // by checking that the module exists and exports the field
      const resolved = validateModuleFieldAccess(
        expr,
        imports,
        dependencies,
        protocolName,
        methodName,
      );
      if (!resolved) {
        // Not a module access, validate the target expression normally
        validateExpressionIdentifiers(
          expr.target,
          scope,
          constructors,
          protocolName,
          methodName,
          imports,
          dependencies,
        );
      }
      return;
    }
    case "Number":
    case "String":
    case "Char":
    case "Unit":
      // Literals don't reference identifiers
      return;
  }
}

/**
 * Validate a module-qualified field access (e.g., Int.add).
 * Returns true if this was a valid module access, false if not a module access.
 * Throws SemanticError if it looks like a module access but is invalid.
 */
function validateModuleFieldAccess(
  expr: Extract<Expr, { kind: "FieldAccess" }>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
  protocolName: string,
  methodName: string,
): boolean {
  // Collect the chain of field accesses to reconstruct the module path
  const parts: string[] = [];
  let current: Expr = expr;

  // Traverse backwards through FieldAccess expressions
  while (current.kind === "FieldAccess") {
    parts.unshift(current.field);
    current = current.target;
  }

  // The base should be a Var to be a module reference
  if (current.kind !== "Var") {
    return false;
  }

  // The base name (e.g., "Int" from "Int.add")
  const baseName = current.name;
  parts.unshift(baseName);

  // Try to find a matching import for the module path
  for (const imp of imports) {
    // Check for module alias match (e.g., "import Vibe.Int as Int" with "Int.add")
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = dependencies.get(imp.moduleName);
      if (!depModule) {
        throw new SemanticError(
          `Module '${imp.moduleName}' (aliased as '${imp.alias}') not found in implementation of '${methodName}' for protocol '${protocolName}'`,
          expr.span,
        );
      }

      // Get the field name (e.g., "add" from ["Int", "add"])
      const fieldName = parts[1]!;

      // Check if it's an exported value
      const valueInfo = depModule.values[fieldName];
      if (valueInfo) {
        // Check if it's exported
        if (!isExportedFromModule(depModule, fieldName, "value")) {
          throw new SemanticError(
            `'${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`,
            expr.span,
          );
        }
        return true;
      }

      // Check if it's a constructor
      const ctorInfo = depModule.constructors[fieldName];
      if (ctorInfo) {
        if (!isExportedFromModule(depModule, fieldName, "constructor")) {
          throw new SemanticError(
            `Constructor '${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`,
            expr.span,
          );
        }
        return true;
      }

      // Field not found in the module
      throw new SemanticError(
        `'${fieldName}' is not defined in module '${imp.moduleName}' (aliased as '${imp.alias}') in implementation of '${methodName}' for protocol '${protocolName}'`,
        expr.span,
      );
    }

    // Check for unaliased import match (e.g., "import Vibe.JS" with "Vibe.JS.null")
    const importParts = imp.moduleName.split(".");
    if (!imp.alias && importParts.length <= parts.length - 1) {
      let matches = true;
      for (let i = 0; i < importParts.length; i++) {
        if (importParts[i] !== parts[i]) {
          matches = false;
          break;
        }
      }

      if (matches) {
        const depModule = dependencies.get(imp.moduleName);
        if (!depModule) {
          throw new SemanticError(
            `Module '${imp.moduleName}' not found in implementation of '${methodName}' for protocol '${protocolName}'`,
            expr.span,
          );
        }

        // Get the remaining parts after the module name
        const fieldParts = parts.slice(importParts.length);

        if (fieldParts.length === 1) {
          const fieldName = fieldParts[0]!;

          // Check if it's an exported value
          const valueInfo = depModule.values[fieldName];
          if (valueInfo) {
            if (!isExportedFromModule(depModule, fieldName, "value")) {
              throw new SemanticError(
                `'${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`,
                expr.span,
              );
            }
            return true;
          }

          // Check if it's a constructor
          const ctorInfo = depModule.constructors[fieldName];
          if (ctorInfo) {
            if (!isExportedFromModule(depModule, fieldName, "constructor")) {
              throw new SemanticError(
                `Constructor '${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`,
                expr.span,
              );
            }
            return true;
          }

          // Field not found in the module
          throw new SemanticError(
            `'${fieldName}' is not defined in module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`,
            expr.span,
          );
        }
      }
    }
  }

  // Not a recognized module access pattern
  return false;
}

/**
 * Check if a symbol exists in the scope hierarchy (without throwing).
 */
function symbolExists(scope: Scope, name: string): boolean {
  if (scope.symbols.has(name)) {
    return true;
  }
  if (scope.parent) {
    return symbolExists(scope.parent, name);
  }
  return false;
}

/**
 * Bind pattern variable names into a scope (for lambda args, case branches, etc.)
 * This is a simple version that just adds placeholder type schemes.
 */
function bindPatternNames(pattern: Pattern, scope: Scope): void {
  switch (pattern.kind) {
    case "VarPattern":
      scope.symbols.set(pattern.name, {
        vars: new Set(),
        constraints: [],
        type: { kind: "var", id: -1 }, // Placeholder
      });
      return;
    case "WildcardPattern":
      return;
    case "ConstructorPattern":
      for (const arg of pattern.args) {
        bindPatternNames(arg, scope);
      }
      return;
    case "TuplePattern":
      for (const element of pattern.elements) {
        bindPatternNames(element, scope);
      }
      return;
    case "ListPattern":
      for (const element of pattern.elements) {
        bindPatternNames(element, scope);
      }
      return;
    case "ConsPattern":
      bindPatternNames(pattern.head, scope);
      bindPatternNames(pattern.tail, scope);
      return;
    case "RecordPattern":
      for (const field of pattern.fields) {
        if (field.pattern) {
          bindPatternNames(field.pattern, scope);
        }
      }
      return;
  }
}

/**
 * Check if two protocol implementations truly overlap.
 *
 * Two implementations overlap only if there exists some concrete type that
 * could match both implementations simultaneously. This means we must consider
 * constraints: if implementation A has constraint `Eq a` and implementation B
 * is for `List a`, they only overlap if `List a` can satisfy `Eq`.
 *
 * @param inst1 - First implementation to compare
 * @param inst2 - Second implementation to compare
 * @param instances - All known instances (for constraint satisfaction checking)
 * @returns true if the implementations could apply to the same concrete type
 */
function instancesOverlap(
  inst1: InstanceInfo,
  inst2: InstanceInfo,
  instances: InstanceInfo[],
): boolean {
  // First, check if the type arguments structurally overlap
  if (!typesOverlap(inst1.typeArgs, inst2.typeArgs)) {
    return false;
  }

  // If types structurally overlap, check if constraints allow actual overlap
  // We need to check both directions:
  // 1. Can inst1's type satisfy inst2's constraints?
  // 2. Can inst2's type satisfy inst1's constraints?

  // For each pair of corresponding type arguments, check constraint compatibility
  for (let i = 0; i < inst1.typeArgs.length; i++) {
    const type1 = inst1.typeArgs[i]!;
    const type2 = inst2.typeArgs[i]!;

    // Case 1: type1 is a variable with constraints, type2 is concrete
    // Check if type2 satisfies all of type1's constraints
    if (type1.kind === "var" && type2.kind === "con") {
      const constraintsForVar = inst1.constraints.filter((c) =>
        c.typeArgs.some((t) => t.kind === "var" && t.id === type1.id),
      );

      for (const constraint of constraintsForVar) {
        // Substitute the concrete type for the type variable in the constraint
        const substitutedType = substituteTypeInConstraint(
          constraint,
          type1.id,
          type2,
        );
        if (
          !canSatisfyConstraint(
            substitutedType,
            constraint.protocolName,
            instances,
          )
        ) {
          // The concrete type cannot satisfy the constraint, so no overlap
          return false;
        }
      }
    }

    // Case 2: type2 is a variable with constraints, type1 is concrete
    // Check if type1 satisfies all of type2's constraints
    if (type2.kind === "var" && type1.kind === "con") {
      const constraintsForVar = inst2.constraints.filter((c) =>
        c.typeArgs.some((t) => t.kind === "var" && t.id === type2.id),
      );

      for (const constraint of constraintsForVar) {
        // Substitute the concrete type for the type variable in the constraint
        const substitutedType = substituteTypeInConstraint(
          constraint,
          type2.id,
          type1,
        );
        if (
          !canSatisfyConstraint(
            substitutedType,
            constraint.protocolName,
            instances,
          )
        ) {
          // The concrete type cannot satisfy the constraint, so no overlap
          return false;
        }
      }
    }

    // Case 3: Both are type variables with constraints
    // This is more complex - we'd need to check if there's any type that could
    // satisfy both sets of constraints. For now, we conservatively assume overlap
    // unless the constraints are mutually exclusive (which is hard to determine).
    // This is the same behavior as before - we assume they could overlap.
  }

  return true;
}

/**
 * Substitute a type variable with a concrete type in a constraint's type arguments.
 * Returns the resulting concrete type after substitution.
 */
function substituteTypeInConstraint(
  constraint: Constraint,
  varId: number,
  replacement: Type,
): Type {
  // Find the type argument that contains the variable and substitute
  for (const typeArg of constraint.typeArgs) {
    if (typeArg.kind === "var" && typeArg.id === varId) {
      return replacement;
    }
    if (typeArg.kind === "con") {
      // Handle nested type constructors like `List a`
      const substitutedArgs = typeArg.args.map((arg) =>
        arg.kind === "var" && arg.id === varId ? replacement : arg,
      );
      return { kind: "con", name: typeArg.name, args: substitutedArgs };
    }
  }
  return constraint.typeArgs[0] || replacement;
}

/**
 * Check if a concrete type can satisfy a protocol constraint.
 * This checks if there's an instance of the protocol for the given type.
 */
function canSatisfyConstraint(
  concreteType: Type,
  protocolName: string,
  instances: InstanceInfo[],
): boolean {
  if (concreteType.kind !== "con") {
    // If it's still a type variable, we can't determine satisfaction
    // Conservative: assume it could be satisfied
    return true;
  }

  // Look for an instance of the protocol for this concrete type
  return findInstanceForTypeInternal(protocolName, concreteType, instances);
}

/**
 * Check if two lists of types overlap (for instance overlap detection).
 * Returns true if the types could potentially unify.
 */
function typesOverlap(types1: Type[], types2: Type[]): boolean {
  if (types1.length !== types2.length) return false;

  for (let i = 0; i < types1.length; i++) {
    if (!typeOverlaps(types1[i]!, types2[i]!)) {
      return false;
    }
  }

  return true;
}

/**
 * Check if two types overlap (could potentially unify).
 * This is a conservative check - we reject if they're definitely the same.
 */
function typeOverlaps(type1: Type, type2: Type): boolean {
  // Type variables can unify with anything
  if (type1.kind === "var" || type2.kind === "var") return true;

  // Type constructors overlap if they have the same name
  if (type1.kind === "con" && type2.kind === "con") {
    if (type1.name !== type2.name) return false;
    return typesOverlap(type1.args, type2.args);
  }

  // Functions overlap if both domain and codomain overlap
  if (type1.kind === "fun" && type2.kind === "fun") {
    return (
      typeOverlaps(type1.from, type2.from) && typeOverlaps(type1.to, type2.to)
    );
  }

  // Tuples overlap if all elements overlap
  if (type1.kind === "tuple" && type2.kind === "tuple") {
    return typesOverlap(type1.elements, type2.elements);
  }

  // Records overlap if all fields overlap (simplified check)
  if (type1.kind === "record" && type2.kind === "record") {
    const fields1 = Object.keys(type1.fields);
    const fields2 = Object.keys(type2.fields);
    if (fields1.length !== fields2.length) return false;

    for (const field of fields1) {
      if (!type2.fields[field]) return false;
      if (!typeOverlaps(type1.fields[field]!, type2.fields[field]!)) {
        return false;
      }
    }
    return true;
  }

  // Different kinds don't overlap
  return false;
}

/**
 * Compute export information for a module based on its exposing clause.
 * This validates that all exported items exist and builds the ExportInfo structure.
 *
 * @param importedValues - Map from value name to source module name, for values imported
 *                         from other modules that can be re-exported.
 */
function computeModuleExports(
  moduleDecl: ModuleDeclaration | undefined,
  values: Record<string, ValueInfo>,
  adts: Record<string, ADTInfo>,
  constructors: Record<string, ConstructorInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  protocols: Record<string, ProtocolInfo>,
  operators: OperatorRegistry,
  importedValues: Map<string, string> = new Map(),
): ExportInfo {
  // Default: empty exports
  const exports: ExportInfo = {
    values: new Set(),
    operators: new Set(),
    types: new Map(),
    protocols: new Map(),
    exportsAll: false,
    reExportedValues: new Map(),
  };

  if (!moduleDecl || !moduleDecl.exposing) {
    // No exposing clause means export nothing by default
    return exports;
  }

  const exposing = moduleDecl.exposing;

  // Handle "exposing (..)" - export everything
  if (exposing.kind === "All") {
    exports.exportsAll = true;

    // Export all values
    for (const name of Object.keys(values)) {
      exports.values.add(name);
    }

    // Export all operators
    for (const op of operators.keys()) {
      exports.operators.add(op);
    }

    // Export all ADTs with all constructors
    for (const [name, adt] of Object.entries(adts)) {
      exports.types.set(name, {
        allConstructors: true,
        constructors: new Set(adt.constructors),
      });
    }

    // Export all type aliases
    for (const name of Object.keys(typeAliases)) {
      exports.types.set(name, { allConstructors: false });
    }

    // Export all opaque types
    for (const name of Object.keys(opaqueTypes)) {
      exports.types.set(name, { allConstructors: false });
    }

    // Export all protocols with all methods
    for (const [name, protocol] of Object.entries(protocols)) {
      exports.protocols.set(name, {
        allMethods: true,
        methods: new Set(protocol.methods.keys()),
      });
    }

    return exports;
  }

  // Handle explicit exports
  for (const spec of exposing.exports) {
    switch (spec.kind) {
      case "ExportValue": {
        const name = spec.name;

        // Check if it's a value/function defined locally
        if (values[name]) {
          exports.values.add(name);
          continue;
        }

        // Check if it's an imported value that can be re-exported
        if (importedValues.has(name)) {
          exports.reExportedValues.set(name, importedValues.get(name)!);
          continue;
        }

        // Check if it's a type (ADT, alias, or opaque) exported without constructors
        if (adts[name]) {
          exports.types.set(name, {
            allConstructors: false,
            constructors: new Set(),
          });
          continue;
        }
        if (typeAliases[name]) {
          exports.types.set(name, { allConstructors: false });
          continue;
        }
        if (opaqueTypes[name]) {
          exports.types.set(name, { allConstructors: false });
          continue;
        }

        // Check if it's a protocol exported without methods
        if (protocols[name]) {
          exports.protocols.set(name, {
            allMethods: false,
            methods: new Set(),
          });
          continue;
        }

        throw new SemanticError(
          `Module exposes '${name}' which is not defined`,
          spec.span,
        );
      }

      case "ExportOperator": {
        const op = spec.operator;

        // Check if operator is defined (either as a value or in operator registry)
        if (!values[op] && !operators.has(op) && !importedValues.has(op)) {
          throw new SemanticError(
            `Module exposes operator '${op}' which is not defined`,
            spec.span,
          );
        }

        exports.operators.add(op);
        // Also add to values since operators are functions
        if (values[op]) {
          exports.values.add(op);
        } else if (importedValues.has(op)) {
          exports.reExportedValues.set(op, importedValues.get(op)!);
        }
        break;
      }

      case "ExportTypeAll": {
        const name = spec.name;

        // Check if it's an ADT
        if (adts[name]) {
          const adt = adts[name]!;
          exports.types.set(name, {
            allConstructors: true,
            constructors: new Set(adt.constructors),
          });
          continue;
        }

        // Check if it's a protocol
        if (protocols[name]) {
          const protocol = protocols[name]!;
          exports.protocols.set(name, {
            allMethods: true,
            methods: new Set(protocol.methods.keys()),
          });
          // Protocol methods are NOT exported as standalone values
          // They are only accessible through instance dictionaries
          continue;
        }

        // Type alias or opaque type with (..) is an error
        if (typeAliases[name]) {
          throw new SemanticError(
            `Type alias '${name}' cannot use (..) syntax - type aliases have no constructors`,
            spec.span,
          );
        }
        if (opaqueTypes[name]) {
          throw new SemanticError(
            `Opaque type '${name}' cannot use (..) syntax - opaque types have no constructors`,
            spec.span,
          );
        }

        throw new SemanticError(
          `Module exposes '${name}(..)' but '${name}' is not a type or protocol`,
          spec.span,
        );
      }

      case "ExportTypeSome": {
        const name = spec.name;
        const members = spec.members;

        // Check if it's an ADT with specific constructors
        if (adts[name]) {
          const adt = adts[name]!;
          const exportedCtors = new Set<string>();

          for (const memberName of members) {
            if (!adt.constructors.includes(memberName)) {
              throw new SemanticError(
                `Constructor '${memberName}' is not defined in type '${name}'`,
                spec.span,
              );
            }
            exportedCtors.add(memberName);
          }

          exports.types.set(name, {
            allConstructors: false,
            constructors: exportedCtors,
          });
          continue;
        }

        // Check if it's a protocol with specific methods
        if (protocols[name]) {
          const protocol = protocols[name]!;
          const exportedMethods = new Set<string>();

          for (const memberName of members) {
            if (!protocol.methods.has(memberName)) {
              throw new SemanticError(
                `Method '${memberName}' is not defined in protocol '${name}'`,
                spec.span,
              );
            }
            exportedMethods.add(memberName);
            // Also export the method name as a value
            exports.values.add(memberName);
          }

          exports.protocols.set(name, {
            allMethods: false,
            methods: exportedMethods,
          });
          continue;
        }

        throw new SemanticError(
          `Module exposes '${name}(...)' but '${name}' is not a type or protocol`,
          spec.span,
        );
      }
    }
  }

  return exports;
}

function validateImports(imports: ImportDeclaration[]) {
  const byModule = new Map<string, ImportDeclaration>();
  const byAlias = new Map<string, ImportDeclaration>();

  for (const imp of imports) {
    const duplicateModule = byModule.get(imp.moduleName);
    if (duplicateModule) {
      throw new SemanticError(
        `Duplicate import of module '${imp.moduleName}'`,
        imp.span,
      );
    }
    byModule.set(imp.moduleName, imp);

    if (imp.alias) {
      const duplicateAlias = byAlias.get(imp.alias);
      if (duplicateAlias) {
        throw new SemanticError(
          `Duplicate import alias '${imp.alias}'`,
          imp.span,
        );
      }
      byAlias.set(imp.alias, imp);
    }
  }
}

function analyzeValueDeclaration(
  decl: ValueDeclaration,
  globalScope: Scope,
  substitution: Substitution,
  declaredType: Type,
  annotationType: Type | undefined,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  imports: ImportDeclaration[] = [],
  dependencies: Map<string, SemanticModule> = new Map(),
): Type {
  // Validate function parameter patterns (single-constructor ADTs, tuples, records)
  validateFunctionParamPatterns(decl.args, constructors, adts);

  const paramTypes = annotationType
    ? extractAnnotationParams(annotationType, decl.args.length, decl.span)
    : decl.args.map(() => freshType());
  const returnType = annotationType
    ? extractAnnotationReturn(annotationType, decl.args.length)
    : freshType();
  const expected = fnChain(paramTypes, returnType);

  unify(expected, declaredType, decl.span, substitution);

  const fnScope: Scope = { parent: globalScope, symbols: new Map() };
  bindPatterns(
    fnScope,
    decl.args,
    paramTypes,
    substitution,
    constructors,
    adts,
  );

  const bodyType = analyzeExpr(
    decl.body,
    fnScope,
    substitution,
    globalScope,
    constructors,
    adts,
    typeAliases,
    opaqueTypes,
    imports,
    dependencies,
  );
  unify(bodyType, returnType, decl.body.span, substitution);

  return applySubstitution(expected, substitution);
}

/**
 * Try to resolve a module-qualified field access (e.g., Vibe.JS.null)
 * Returns the type of the accessed symbol if it's a module access, or null if not.
 */
function tryResolveModuleFieldAccess(
  expr: Extract<Expr, { kind: "FieldAccess" }>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
): Type | null {
  // Collect the chain of field accesses to reconstruct the module path
  const parts: string[] = [];
  let current: Expr = expr;

  // Traverse backwards through FieldAccess expressions
  while (current.kind === "FieldAccess") {
    parts.unshift(current.field);
    current = current.target;
  }

  // The base should be a Var to be a module reference
  if (current.kind !== "Var") {
    return null;
  }

  // The base name (e.g., "Vibe" from "Vibe.JS.null" or "JS" from "JS.null" when using alias)
  const baseName = current.name;
  parts.unshift(baseName);

  // Now we have the full path like ["Vibe", "JS", "null"] or ["JS", "null"] for alias access
  // Try to find a matching import for the module path
  // For "Vibe.JS.null", we look for:
  // 1. import Vibe.JS (with field null)
  // 2. import Vibe (with field JS.null)
  // For "JS.null" with "import Vibe.JS as JS", we look for the alias match

  for (const imp of imports) {
    const importParts = imp.moduleName.split(".");

    // Check for module alias match first (e.g., "import Vibe.JS as JS" with "JS.null")
    // If the base name matches the alias and we have remaining parts, resolve from that module
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = dependencies.get(imp.moduleName);
      if (!depModule) {
        continue;
      }

      // Get the remaining parts after the alias (e.g., ["null"] from ["JS", "null"])
      const fieldParts = parts.slice(1);

      // Look up the value in the module
      if (fieldParts.length === 1) {
        const field = fieldParts[0]!;
        // Check if it's a value - type may be on valueInfo.type or in depModule.types
        const valueInfo = depModule.values[field];
        if (valueInfo) {
          const valueType = valueInfo.type || depModule.types[field];
          if (valueType) {
            return valueType;
          }
        }
        // Check if it's a constructor
        const ctorScheme = depModule.constructorTypes[field];
        if (ctorScheme) {
          return ctorScheme.type;
        }
      }
      // For nested field accesses like Alias.value.field, we return null here
      // and let the recursive FieldAccess analysis in analyzeExpr handle it.
    }

    // Check if this import could match our path (non-alias case)
    // If import is "Vibe.JS" and we access "Vibe.JS.null", it matches
    if (importParts.length <= parts.length - 1) {
      let matches = true;
      for (let i = 0; i < importParts.length; i++) {
        if (importParts[i] !== parts[i]) {
          matches = false;
          break;
        }
      }

      if (matches) {
        const depModule = dependencies.get(imp.moduleName);
        if (!depModule) {
          return null;
        }

        // Get the remaining parts after the module name
        const fieldParts = parts.slice(importParts.length);

        // Look up the value in the module
        if (fieldParts.length === 1) {
          const field = fieldParts[0]!;
          // Check if it's a value - type may be on valueInfo.type or in depModule.types
          const valueInfo = depModule.values[field];
          if (valueInfo) {
            const valueType = valueInfo.type || depModule.types[field];
            if (valueType) {
              return valueType;
            }
          }
          // Check if it's a constructor
          const ctorScheme = depModule.constructorTypes[field];
          if (ctorScheme) {
            return ctorScheme.type;
          }
        }
        // For nested field accesses like Vibe.JS.value.field, we return null here
        // and let the recursive FieldAccess analysis in analyzeExpr handle it.
      }
    }
  }

  return null;
}

function analyzeExpr(
  expr: Expr,
  scope: Scope,
  substitution: Substitution,
  globalScope: Scope,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo> = {},
  imports: ImportDeclaration[] = [],
  dependencies: Map<string, SemanticModule> = new Map(),
): Type {
  switch (expr.kind) {
    case "Var": {
      // Look up the symbol and collect any protocol constraints
      const { type: resolved, constraints } = lookupSymbolWithConstraints(
        scope,
        expr.name,
        expr.span,
        substitution,
      );
      // Add any constraints from the symbol to the current context
      for (const constraint of constraints) {
        addConstraint(currentConstraintContext, constraint);
      }
      return applySubstitution(resolved, substitution);
    }
    case "Number": {
      // Number literals are treated as Int or Float based on decimal point
      const hasDecimal = expr.value.includes(".");
      const typeName = hasDecimal ? "Float" : "Int";
      const opaque = opaqueTypes[typeName];
      if (!opaque && !adts[typeName]) {
        throw new SemanticError(
          `Type '${typeName}' not found. Make sure the prelude is imported.`,
          expr.span,
        );
      }
      return { kind: "con", name: typeName, args: [] };
    }
    case "String": {
      const opaque = opaqueTypes["String"];
      if (!opaque && !adts["String"]) {
        throw new SemanticError(
          "Type 'String' not found. Make sure the prelude is imported.",
          expr.span,
        );
      }
      return { kind: "con", name: "String", args: [] };
    }
    case "Char": {
      const opaque = opaqueTypes["Char"];
      if (!opaque && !adts["Char"]) {
        throw new SemanticError(
          "Type 'Char' not found. Make sure the prelude is imported.",
          expr.span,
        );
      }
      return { kind: "con", name: "Char", args: [] };
    }
    case "Unit": {
      // () is syntax sugar for the Unit constructor from the prelude
      const opaque = opaqueTypes["Unit"];
      if (!opaque && !adts["Unit"]) {
        throw new SemanticError(
          "Type 'Unit' not found. Make sure the prelude is imported.",
          expr.span,
        );
      }
      return { kind: "con", name: "Unit", args: [] };
    }
    case "Tuple": {
      const elements = expr.elements.map((el) =>
        analyzeExpr(
          el,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        ),
      );
      return { kind: "tuple", elements };
    }
    case "List": {
      if (expr.elements.length === 0) {
        return listType(freshType());
      }
      const first = analyzeExpr(
        expr.elements[0]!,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      for (const el of expr.elements.slice(1)) {
        const elType = analyzeExpr(
          el,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );
        unify(first, elType, el.span, substitution);
      }
      return listType(applySubstitution(first, substitution));
    }
    case "ListRange": {
      const startType = analyzeExpr(
        expr.start,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const endType = analyzeExpr(
        expr.end,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      unify(startType, endType, expr.span, substitution);
      return listType(applySubstitution(startType, substitution));
    }
    case "Record": {
      const fields: Record<string, Type> = {};
      for (const field of expr.fields) {
        // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
        if (Object.hasOwn(fields, field.name)) {
          throw new SemanticError(
            `Duplicate record field '${field.name}'`,
            field.span,
          );
        }
        fields[field.name] = analyzeExpr(
          field.value,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );
      }
      return { kind: "record", fields };
    }
    case "RecordUpdate": {
      const baseType = lookupSymbol(scope, expr.base, expr.span, substitution);
      const concreteBase = applySubstitution(baseType, substitution);
      if (concreteBase.kind !== "record") {
        throw new SemanticError(
          `Cannot update non-record '${expr.base}'`,
          expr.span,
        );
      }
      const updatedFields: Record<string, Type> = { ...concreteBase.fields };
      for (const field of expr.fields) {
        if (!updatedFields[field.name]) {
          throw new SemanticError(
            `Record '${expr.base}' has no field '${field.name}'`,
            field.span,
          );
        }
        const fieldType = analyzeExpr(
          field.value,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );
        unify(updatedFields[field.name]!, fieldType, field.span, substitution);
        updatedFields[field.name] = applySubstitution(
          updatedFields[field.name]!,
          substitution,
        );
      }
      return { kind: "record", fields: updatedFields };
    }
    case "FieldAccess": {
      // First, try to resolve module-qualified access (e.g., Vibe.JS.null)
      const moduleAccess = tryResolveModuleFieldAccess(
        expr,
        imports,
        dependencies,
      );
      if (moduleAccess) {
        return moduleAccess;
      }

      // Otherwise, handle as record field access
      const targetType = analyzeExpr(
        expr.target,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const concrete = applySubstitution(targetType, substitution);
      if (concrete.kind !== "record") {
        throw new SemanticError(
          `Cannot access field '${expr.field}' on non-record value`,
          expr.span,
        );
      }
      const fieldType = concrete.fields[expr.field];
      if (!fieldType) {
        throw new SemanticError(
          `Record has no field '${expr.field}'`,
          expr.span,
        );
      }
      return applySubstitution(fieldType, substitution);
    }
    case "Lambda": {
      const paramTypes = expr.args.map(() => freshType());
      const fnScope: Scope = { parent: scope, symbols: new Map() };
      bindPatterns(
        fnScope,
        expr.args,
        paramTypes,
        substitution,
        constructors,
        adts,
      );
      const bodyType = analyzeExpr(
        expr.body,
        fnScope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      return fnChain(paramTypes, bodyType);
    }
    case "Apply": {
      let calleeType = analyzeExpr(
        expr.callee,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      for (const arg of expr.args) {
        const argType = analyzeExpr(
          arg,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );
        const resultType = freshType();
        unify(calleeType, fn(argType, resultType), expr.span, substitution);
        // Eagerly validate constraints after unification to catch type mismatches
        // where a protocol method's return type gets unified with a function type
        // due to over-application
        validateConstraintsEagerly(substitution, arg.span);
        calleeType = applySubstitution(resultType, substitution);
      }
      return applySubstitution(calleeType, substitution);
    }
    case "If": {
      const condType = analyzeExpr(
        expr.condition,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      // Bool type must be defined in the prelude
      const boolAdt = adts["Bool"];
      if (!boolAdt) {
        throw new SemanticError(
          "Type 'Bool' not found. Make sure the prelude is imported.",
          expr.condition.span,
        );
      }
      const tBool: Type = { kind: "con", name: "Bool", args: [] };
      unify(condType, tBool, expr.condition.span, substitution);
      const thenType = analyzeExpr(
        expr.thenBranch,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const elseType = analyzeExpr(
        expr.elseBranch,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      unify(thenType, elseType, expr.span, substitution);
      return applySubstitution(thenType, substitution);
    }
    case "LetIn": {
      const letScope: Scope = { parent: scope, symbols: new Map() };

      // First pass: seed the scope with monomorphic schemes to enable recursion
      for (const binding of expr.bindings) {
        if (letScope.symbols.has(binding.name)) {
          throw new SemanticError(
            `Duplicate let-binding '${binding.name}'`,
            binding.span,
          );
        }
        const seeded = seedValueType(binding, adts, typeAliases);
        // Seed with monomorphic scheme (empty quantifier set)
        declareSymbol(
          letScope,
          binding.name,
          { vars: new Set(), constraints: [], type: seeded },
          binding.span,
        );
      }

      // Second pass: analyze each binding and generalize its type
      // This is where let-polymorphism happens for local bindings
      for (const binding of expr.bindings) {
        const declared = lookupSymbol(
          letScope,
          binding.name,
          binding.span,
          substitution,
        );
        const inferred = analyzeValueDeclaration(
          binding,
          letScope,
          substitution,
          declared,
          undefined,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
        );
        // Generalize the inferred type for polymorphic let-bindings
        // Note: We generalize with respect to the parent scope, not letScope,
        // to allow quantifying over variables not bound in the parent
        const generalizedScheme = generalize(inferred, scope, substitution);
        letScope.symbols.set(binding.name, generalizedScheme);
      }

      return analyzeExpr(
        expr.body,
        letScope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
    }
    case "Case": {
      const discriminantType = analyzeExpr(
        expr.discriminant,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const branchTypes: Type[] = [];
      let hasWildcard = false;
      let hasTuplePattern = false;
      let hasVarPattern = false;
      const usedConstructors = new Set<string>();

      expr.branches.forEach((branch, index) => {
        const branchScope: Scope = { parent: scope, symbols: new Map() };
        const patternType = bindPattern(
          branch.pattern,
          branchScope,
          substitution,
          new Set(),
          freshType(),
          constructors,
          adts,
        );
        unify(discriminantType, patternType, branch.pattern.span, substitution);
        const bodyType = analyzeExpr(
          branch.body,
          branchScope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases,
          opaqueTypes,
          imports,
          dependencies,
        );
        branchTypes.push(bodyType);

        if (branch.pattern.kind === "WildcardPattern") {
          hasWildcard = true;
          if (index !== expr.branches.length - 1) {
            throw new SemanticError(
              "Wildcard pattern makes following branches unreachable",
              branch.pattern.span,
            );
          }
        }
        if (branch.pattern.kind === "TuplePattern") {
          hasTuplePattern = true;
        }
        if (branch.pattern.kind === "VarPattern") {
          hasVarPattern = true;
        }
        if (branch.pattern.kind === "ConstructorPattern") {
          usedConstructors.add(branch.pattern.name);
          validateConstructorArity(branch.pattern, constructors);
        }
      });

      if (branchTypes.length === 0) {
        throw new SemanticError("Case expression has no branches", expr.span);
      }

      const firstType = branchTypes[0]!;
      for (const bt of branchTypes.slice(1)) {
        unify(firstType, bt, expr.span, substitution);
      }

      // Exhaustiveness checking:
      // - Wildcard patterns are always exhaustive
      // - Variable patterns are always exhaustive (they match anything)
      // - Tuple patterns are exhaustive (tuples have no variants)
      // - Constructor patterns need coverage checking against ADT definition
      if (!hasWildcard && !hasVarPattern && !hasTuplePattern) {
        const coverage = constructorCoverage(
          usedConstructors,
          constructors,
          adts,
          discriminantType,
          substitution,
        );
        if (!coverage.exhaustive) {
          const missingMsg =
            coverage.missing && coverage.missing.length > 0
              ? ` (missing: ${coverage.missing.join(", ")})`
              : "";
          throw new SemanticError(
            `Non-exhaustive case expression${missingMsg}`,
            expr.span,
          );
        }
      }

      return applySubstitution(firstType, substitution);
    }
    case "Infix": {
      const leftType = analyzeExpr(
        expr.left,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const rightType = analyzeExpr(
        expr.right,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const opType = INFIX_TYPES[expr.operator];

      if (opType) {
        const expected = applySubstitution(opType, substitution);
        const params = flattenFunctionParams(expected);
        if (params.length < 2) {
          throw new SemanticError("Invalid operator type", expr.span);
        }
        unify(params[0]!, leftType, expr.left.span, substitution);
        unify(params[1]!, rightType, expr.right.span, substitution);
        return applySubstitution(
          extractAnnotationReturn(expected, 2),
          substitution,
        );
      }

      const callee: Expr = {
        kind: "Var",
        name: expr.operator,
        namespace: "lower",
        span: expr.span,
      };
      const applyExpr: Expr = {
        kind: "Apply",
        callee,
        args: [expr.left, expr.right],
        span: expr.span,
      };
      return analyzeExpr(
        applyExpr,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
    }
    case "Paren":
      return analyzeExpr(
        expr.expression,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
    case "Unary": {
      // Unary negation: only allowed for Int and Float
      const operandType = analyzeExpr(
        expr.operand,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases,
        opaqueTypes,
        imports,
        dependencies,
      );
      const concreteType = applySubstitution(operandType, substitution);

      // Check if the operand is Int or Float
      if (concreteType.kind === "con") {
        if (concreteType.name === "Int" || concreteType.name === "Float") {
          return concreteType;
        }
      }

      // For type variables, we need to defer the check or constrain the type
      // For now, we require the type to be known as Int or Float
      if (concreteType.kind === "var") {
        throw new SemanticError(
          `Unary negation requires a concrete numeric type (Int or Float), but got an unknown type. Add a type annotation to disambiguate.`,
          expr.span,
        );
      }

      throw new SemanticError(
        `Unary negation is only allowed for Int and Float, but got '${formatType(
          concreteType,
        )}'`,
        expr.span,
      );
    }
    default: {
      const _exhaustive: never = expr;
      throw new SemanticError(
        "Unsupported expression",
        (expr as { span: Span }).span,
      );
    }
  }
}

/**
 * Check if a set of constructor patterns exhaustively covers an ADT.
 *
 * This function performs exhaustiveness checking for pattern matching on ADTs.
 * It determines if the provided constructors cover all variants of the matched type.
 *
 * Algorithm:
 * 1. Try to determine the ADT from the discriminant type or used constructors
 * 2. Compare used constructors against all constructors in the ADT
 * 3. Return missing constructors if any
 *
 * @param usedConstructors - Set of constructor names used in patterns
 * @param constructorsRegistry - Global constructor registry
 * @param adts - ADT registry
 * @param discriminantType - Type of the value being matched
 * @param substitution - Current type substitution
 * @returns Object with exhaustive flag and optional missing constructor list
 */
function constructorCoverage(
  usedConstructors: Set<string>,
  constructorsRegistry: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  discriminantType: Type,
  substitution: Substitution,
): { exhaustive: boolean; missing?: string[] } {
  // First, try to determine the ADT from the used constructors
  let adtName: string | undefined;

  for (const ctorName of usedConstructors) {
    const ctorInfo = constructorsRegistry[ctorName];
    if (ctorInfo) {
      if (adtName && adtName !== ctorInfo.parentType) {
        // Mixing constructors from different ADTs - not exhaustive
        return { exhaustive: false, missing: ["(mixed ADT constructors)"] };
      }
      adtName = ctorInfo.parentType;
    }
  }

  // Try to determine ADT from the discriminant type
  const concreteType = applySubstitution(discriminantType, substitution);
  if (concreteType.kind === "con") {
    // Look up ADT by exact type name
    const adt = adts[concreteType.name];
    if (adt && !adtName) {
      adtName = concreteType.name;
    }
  }

  // If we found an ADT, check coverage
  const foundAdt = adtName ? adts[adtName] : undefined;
  if (foundAdt) {
    const allConstructors = new Set(foundAdt.constructors);
    const missing: string[] = [];

    for (const ctor of allConstructors) {
      if (!usedConstructors.has(ctor)) {
        missing.push(ctor);
      }
    }

    if (missing.length === 0) {
      return { exhaustive: true };
    }
    return { exhaustive: false, missing };
  }

  // Fall back to legacy built-in constructor checking for backward compatibility
  const coversBool =
    usedConstructors.has("True") && usedConstructors.has("False");
  const coversMaybe =
    usedConstructors.has("Nothing") && usedConstructors.has("Just");
  const coversResult =
    usedConstructors.has("Ok") && usedConstructors.has("Err");
  if (coversBool || coversMaybe || coversResult) {
    return { exhaustive: true };
  }

  return { exhaustive: false };
}

/**
 * Validate that patterns used in function parameters are allowed.
 *
 * Function parameters can use:
 * - Variable patterns (x)
 * - Wildcard patterns (_)
 * - Tuple patterns ((a, b))
 * - Record patterns ({ x, y })
 * - Constructor patterns ONLY for single-constructor ADTs (e.g., Pair a b)
 *
 * Multi-constructor ADTs (like Maybe, Result) require case expressions.
 */
function validateFunctionParamPatterns(
  patterns: Pattern[],
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
): void {
  for (const pattern of patterns) {
    validateFunctionParamPattern(pattern, constructors, adts);
  }
}

/**
 * Recursively validate a single function parameter pattern.
 */
function validateFunctionParamPattern(
  pattern: Pattern,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
): void {
  switch (pattern.kind) {
    case "VarPattern":
    case "WildcardPattern":
      // Always allowed
      return;

    case "TuplePattern":
      // Allowed, but validate nested patterns
      for (const element of pattern.elements) {
        validateFunctionParamPattern(element, constructors, adts);
      }
      return;

    case "RecordPattern":
      // Allowed, but validate nested patterns
      for (const field of pattern.fields) {
        if (field.pattern) {
          validateFunctionParamPattern(field.pattern, constructors, adts);
        }
      }
      return;

    case "ConstructorPattern": {
      // Only allowed for single-constructor ADTs
      const ctorInfo = constructors[pattern.name];

      if (!ctorInfo) {
        // Unknown constructor - will be caught by other validation
        return;
      }

      const adtInfo = adts[ctorInfo.parentType];

      if (!adtInfo) {
        // Unknown ADT - will be caught by other validation
        return;
      }

      if (adtInfo.constructors.length > 1) {
        const constructorNames = adtInfo.constructors.join(", ");
        throw new SemanticError(
          `Constructor pattern '${pattern.name}' is not allowed in function parameters. ` +
            `The type '${ctorInfo.parentType}' has multiple constructors (${constructorNames}). ` +
            `Use a case expression in the function body instead.`,
          pattern.span,
        );
      }

      // Validate nested patterns
      for (const arg of pattern.args) {
        validateFunctionParamPattern(arg, constructors, adts);
      }
      return;
    }

    case "ListPattern":
    case "ConsPattern":
      // These should have been rejected by the parser, but check anyway
      throw new SemanticError(
        `List patterns are not allowed in function parameters. ` +
          `Use a case expression in the function body instead.`,
        pattern.span,
      );
  }
}

function bindPatterns(
  scope: Scope,
  patterns: Pattern[],
  paramTypes: Type[],
  substitution: Substitution,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
) {
  if (patterns.length !== paramTypes.length) {
    throw new Error("Internal arity mismatch during pattern binding");
  }
  const seen = new Set<string>();
  patterns.forEach((pattern, idx) => {
    const paramType = paramTypes[idx]!;
    bindPattern(
      pattern,
      scope,
      substitution,
      seen,
      paramType,
      constructors,
      adts,
    );
    if (pattern.kind === "VarPattern") {
      // Pattern variables are monomorphic
      scope.symbols.set(pattern.name, {
        vars: new Set(),
        constraints: [],
        type: paramType,
      });
    }
  });
}

/**
 * Bind pattern variables to types in a scope.
 *
 * This function recursively processes a pattern, binding variables to their types
 * and validating that the pattern is well-formed for the expected type.
 *
 * For constructor patterns, it:
 * 1. Validates constructor arity (number of arguments)
 * 2. Determines the constructor's type from the registry
 * 3. Unifies the pattern type with the expected type
 * 4. Recursively binds nested pattern arguments
 */
function bindPattern(
  pattern: Pattern,
  scope: Scope,
  substitution: Substitution,
  seen: Set<string>,
  expected: Type,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
): Type {
  switch (pattern.kind) {
    case "VarPattern": {
      if (seen.has(pattern.name)) {
        throw new SemanticError(
          `Duplicate pattern variable '${pattern.name}'`,
          pattern.span,
        );
      }
      seen.add(pattern.name);
      // Pattern variables are monomorphic (not generalized)
      // This follows the let-polymorphism discipline where only let-bound names are polymorphic
      declareSymbol(
        scope,
        pattern.name,
        { vars: new Set(), constraints: [], type: expected },
        pattern.span,
      );
      return expected;
    }
    case "WildcardPattern":
      return expected;
    case "TuplePattern": {
      const subTypes = pattern.elements.map(() => freshType());
      unify(
        { kind: "tuple", elements: subTypes },
        expected,
        pattern.span,
        substitution,
      );
      pattern.elements.forEach((el, idx) =>
        bindPattern(
          el,
          scope,
          substitution,
          seen,
          subTypes[idx]!,
          constructors,
          adts,
        ),
      );
      return applySubstitution(
        { kind: "tuple", elements: subTypes },
        substitution,
      );
    }
    case "ConstructorPattern": {
      // Validate constructor arity
      validateConstructorArity(pattern, constructors);

      // Look up constructor info to get proper types
      const ctorInfo = constructors[pattern.name];

      if (ctorInfo) {
        // User-defined ADT constructor
        // Create fresh type variables for the ADT's type parameters
        const paramTypeVars: Map<string, TypeVar> = new Map();
        for (const param of ctorInfo.parentParams) {
          paramTypeVars.set(param, freshType());
        }

        // Build the result type: ParentType param1 param2 ...
        const resultType: TypeCon = {
          kind: "con",
          name: ctorInfo.parentType,
          args: ctorInfo.parentParams.map((p) => paramTypeVars.get(p)!),
        };

        // Build types for constructor arguments
        const argTypes: Type[] = ctorInfo.argTypes.map((argExpr) =>
          constructorArgToType(argExpr, paramTypeVars),
        );

        // Unify the result type with the expected type to bind type parameters
        unify(resultType, expected, pattern.span, substitution);

        // Validate we have the right number of argument patterns
        if (pattern.args.length !== argTypes.length) {
          throw new SemanticError(
            `Constructor '${pattern.name}' expects ${argTypes.length} argument(s), got ${pattern.args.length}`,
            pattern.span,
          );
        }

        // Recursively bind pattern arguments
        pattern.args.forEach((arg, idx) => {
          const argType = applySubstitution(argTypes[idx]!, substitution);
          bindPattern(
            arg,
            scope,
            substitution,
            seen,
            argType,
            constructors,
            adts,
          );
        });

        return applySubstitution(resultType, substitution);
      } else {
        // Fall back to legacy behavior for built-in constructors
        const argTypes = pattern.args.map(() => freshType());
        const constructed: Type = fnChain(argTypes, freshType());
        unify(constructed, expected, pattern.span, substitution);
        pattern.args.forEach((arg, idx) =>
          bindPattern(
            arg,
            scope,
            substitution,
            seen,
            argTypes[idx]!,
            constructors,
            adts,
          ),
        );
        return applySubstitution(expected, substitution);
      }
    }
    case "ListPattern": {
      // List pattern: [] or [x, y, z]
      // Expected type should be List a for some a
      const elemType = freshType();
      const lt = listType(elemType);
      unify(lt, expected, pattern.span, substitution);

      // Bind each element pattern to the element type
      pattern.elements.forEach((el) =>
        bindPattern(
          el,
          scope,
          substitution,
          seen,
          applySubstitution(elemType, substitution),
          constructors,
          adts,
        ),
      );
      return applySubstitution(lt, substitution);
    }
    case "ConsPattern": {
      // Cons pattern: head :: tail
      // Expected type should be List a for some a
      const elemType = freshType();
      const lt = listType(elemType);
      unify(lt, expected, pattern.span, substitution);

      // Head pattern binds to element type
      bindPattern(
        pattern.head,
        scope,
        substitution,
        seen,
        applySubstitution(elemType, substitution),
        constructors,
        adts,
      );

      // Tail pattern binds to list type
      bindPattern(
        pattern.tail,
        scope,
        substitution,
        seen,
        applySubstitution(lt, substitution),
        constructors,
        adts,
      );

      return applySubstitution(lt, substitution);
    }
    case "RecordPattern": {
      // Record pattern: { x, y } or { x = pat, y }
      // Expected type should be a record with at least these fields

      // Build record type from field names
      const fieldTypes: Record<string, Type> = {};
      for (const field of pattern.fields) {
        fieldTypes[field.name] = freshType();
      }

      // Create record type and unify with expected
      const recordType: Type = {
        kind: "record",
        fields: fieldTypes,
      };
      unify(recordType, expected, pattern.span, substitution);

      // Bind each field's pattern (or the field name as variable)
      for (const field of pattern.fields) {
        const fieldType = fieldTypes[field.name]!;
        const appliedFieldType = applySubstitution(fieldType, substitution);

        if (field.pattern) {
          // Field has an explicit pattern: { x = pat }
          bindPattern(
            field.pattern,
            scope,
            substitution,
            seen,
            appliedFieldType,
            constructors,
            adts,
          );
        } else {
          // Field without pattern becomes a variable: { x } === { x = x }
          if (seen.has(field.name)) {
            throw new SemanticError(
              `Duplicate pattern variable '${field.name}'`,
              pattern.span,
            );
          }
          seen.add(field.name);
          declareSymbol(
            scope,
            field.name,
            { vars: new Set(), constraints: [], type: appliedFieldType },
            pattern.span,
          );
        }
      }

      return applySubstitution(recordType, substitution);
    }
    default:
      return expected;
  }
}

/**
 * Validate that a constructor pattern has the correct number of arguments.
 *
 * Checks both user-defined constructors (from the registry) and built-in
 * constructors (True, False, Just, Nothing, Ok, Err).
 */
function validateConstructorArity(
  pattern: Extract<Pattern, { kind: "ConstructorPattern" }>,
  constructors: Record<string, ConstructorInfo>,
) {
  // First check user-defined constructors
  const ctorInfo = constructors[pattern.name];
  if (ctorInfo) {
    if (ctorInfo.arity !== pattern.args.length) {
      throw new SemanticError(
        `Constructor '${pattern.name}' expects ${ctorInfo.arity} argument(s), got ${pattern.args.length}`,
        pattern.span,
      );
    }
    return;
  }

  // Fall back to built-in constructors
  const expected = BUILTIN_CONSTRUCTORS[pattern.name];
  if (expected !== undefined && expected !== pattern.args.length) {
    throw new SemanticError(
      `Constructor '${pattern.name}' expects ${expected} argument(s)`,
      pattern.span,
    );
  }
}

/**
 * Declare a symbol in the scope with its type scheme.
 * Type schemes enable polymorphism - the symbol can be instantiated
 * at different types at different use sites.
 */
function declareSymbol(
  scope: Scope,
  name: string,
  scheme: TypeScheme,
  span: Span,
) {
  if (scope.symbols.has(name)) {
    throw new SemanticError(`Duplicate definition for '${name}'`, span);
  }
  scope.symbols.set(name, scheme);
}

/**
 * Look up a symbol in the scope and instantiate its type scheme.
 * Each lookup gets a fresh instantiation, enabling polymorphic usage.
 * Also returns any protocol constraints associated with the symbol.
 *
 * Example:
 *   id : forall a. a -> a
 *   First use: id 42        -> instantiated as number -> number
 *   Second use: id "hello"  -> instantiated as string -> string
 *
 *   (+) : forall a. Num a => a -> a -> a
 *   Use: x + y              -> returns type: a -> a -> a, constraints: [Num a]
 */
function lookupSymbolWithConstraints(
  scope: Scope,
  name: string,
  span: Span,
  substitution: Substitution,
): LookupResult {
  if (scope.symbols.has(name)) {
    const scheme = scope.symbols.get(name)!;
    // Instantiate the scheme to get a fresh type for this use site
    const { type, constraints } = instantiateWithConstraints(
      scheme,
      substitution,
    );
    return { type, constraints };
  }
  if (scope.parent) {
    return lookupSymbolWithConstraints(scope.parent, name, span, substitution);
  }
  throw new SemanticError(`Undefined name '${name}'`, span);
}

/**
 * Look up a symbol in the scope and instantiate its type scheme.
 * Each lookup gets a fresh instantiation, enabling polymorphic usage.
 *
 * Example:
 *   id : forall a. a -> a
 *   First use: id 42        -> instantiated as number -> number
 *   Second use: id "hello"  -> instantiated as string -> string
 */
function lookupSymbol(
  scope: Scope,
  name: string,
  span: Span,
  substitution: Substitution,
): Type {
  return lookupSymbolWithConstraints(scope, name, span, substitution).type;
}

function seedValueType(
  decl: ValueDeclaration | ExternalDeclaration,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
): Type {
  if (decl.kind === "ExternalDeclaration") {
    return typeFromAnnotation(decl.annotation, new Map(), adts, typeAliases);
  }
  const argTypes = decl.args.map(() => freshType());
  const resultType = freshType();
  return fnChain(argTypes, resultType);
}

/**
 * Generalize a type into a type scheme by quantifying over its free type variables.
 * This is the key operation that enables let-polymorphism.
 *
 * The generalization rule:
 * - Take a type τ and a typing environment Γ
 * - Find all type variables in τ that are NOT free in Γ
 * - Quantify over those variables to create a polymorphic type scheme
 *
 * Example:
 *   let id = \x -> x
 *   After inference: id : a -> a where a is a type variable
 *   Generalize: id : forall a. a -> a
 *   Now 'id' can be used at multiple different types
 *
 * Note: We only generalize at let-bindings and top-level declarations.
 * Lambda-bound variables remain monomorphic (this is the "let" in let-polymorphism).
 */
function generalize(
  type: Type,
  scope: Scope,
  substitution: Substitution,
): TypeScheme {
  const typeFreeVars = getFreeTypeVars(type, substitution);
  const scopeFreeVars = getFreeTypeVarsInScope(scope, substitution);

  // Quantify over type variables that appear in the type but not in the scope
  const quantified = new Set<number>();
  for (const v of typeFreeVars) {
    if (!scopeFreeVars.has(v)) {
      quantified.add(v);
    }
  }

  // Get collected constraints and apply substitution
  const rawConstraints = getCollectedConstraints();
  const resolvedConstraints = applySubstitutionToConstraints(
    rawConstraints,
    substitution,
  );

  // Filter constraints to only those involving quantified type variables
  // A constraint like "Num Int" is already satisfied and shouldn't be quantified
  // A constraint like "Num a" where a is quantified should be kept
  const relevantConstraints = resolvedConstraints.filter((c) => {
    // Check if any type arg contains a quantified variable
    return c.typeArgs.some((t) => {
      const freeVars = getFreeTypeVars(t, substitution);
      for (const v of freeVars) {
        if (quantified.has(v)) return true;
      }
      return false;
    });
  });

  // Deduplicate constraints
  const uniqueConstraints: Constraint[] = [];
  for (const c of relevantConstraints) {
    const isDuplicate = uniqueConstraints.some(
      (uc) =>
        uc.protocolName === c.protocolName &&
        uc.typeArgs.length === c.typeArgs.length &&
        uc.typeArgs.every((t, i) => typesEqual(t, c.typeArgs[i]!)),
    );
    if (!isDuplicate) {
      uniqueConstraints.push(c);
    }
  }

  return { vars: quantified, constraints: uniqueConstraints, type };
}

/**
 * Generalize a type into a type scheme, merging user-annotated constraints with inferred ones.
 * This extends the basic generalize() function to support qualified type annotations.
 *
 * The merge strategy is "satisfiable": user-annotated constraints are included in the
 * final type scheme, and we verify they are consistent with what was inferred.
 *
 * Validation rules:
 * 1. All user-annotated constraints must reference only quantified type variables
 * 2. User-annotated constraints are merged with inferred constraints
 * 3. If a constraint is both annotated and inferred, that's fine (no conflict)
 *
 * @param type - The inferred type to generalize
 * @param scope - The current scope (for determining quantifiable variables)
 * @param substitution - Current type substitution
 * @param annotatedConstraints - User-annotated constraints from type annotation
 * @param span - Source location for error reporting
 */
function generalizeWithAnnotatedConstraints(
  type: Type,
  scope: Scope,
  substitution: Substitution,
  annotatedConstraints: Constraint[] | undefined,
  span: Span,
): TypeScheme {
  const typeFreeVars = getFreeTypeVars(type, substitution);
  const scopeFreeVars = getFreeTypeVarsInScope(scope, substitution);

  // Quantify over type variables that appear in the type but not in the scope
  const quantified = new Set<number>();
  for (const v of typeFreeVars) {
    if (!scopeFreeVars.has(v)) {
      quantified.add(v);
    }
  }

  // Get collected (inferred) constraints and apply substitution
  const rawConstraints = getCollectedConstraints();
  const resolvedConstraints = applySubstitutionToConstraints(
    rawConstraints,
    substitution,
  );

  // Validate constraints that are not purely polymorphic.
  // Constraints that ONLY involve quantified type variables are deferred until call time.
  // But constraints that have concrete type arguments (or type structures like function types)
  // should be validated now to catch errors early.
  for (const c of resolvedConstraints) {
    // Apply substitution to all type args
    const resolvedTypeArgs = c.typeArgs.map((t) =>
      applySubstitution(t, substitution),
    );

    // Check if the constraint involves ONLY quantified type variables at the top level
    // If so, skip validation - it will be validated at call sites when instantiated
    const isFullyPolymorphic = resolvedTypeArgs.every((t) => {
      if (t.kind === "var" && quantified.has(t.id)) {
        return true; // This is a quantified type variable
      }
      return false;
    });

    if (isFullyPolymorphic) {
      // All type args are quantified vars - skip validation, defer to call sites
      continue;
    }

    const resolvedConstraint: Constraint = {
      protocolName: c.protocolName,
      typeArgs: resolvedTypeArgs,
    };

    // Validate that an instance exists (or could exist) for this constraint
    const lookupResult = validateConstraintSatisfiable(
      resolvedConstraint,
      currentInstanceRegistry,
    );

    if (!lookupResult.found) {
      // Build a helpful error message
      const typeArgsStr = resolvedTypeArgs.map((t) => formatType(t)).join(", ");

      if (lookupResult.reason === "unsatisfied-constraint") {
        throw new SemanticError(
          `No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` +
            `The instance requires '${lookupResult.constraint}' for '${lookupResult.forType}', ` +
            `but no such instance exists.`,
          span,
        );
      } else {
        throw new SemanticError(
          `No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` +
            `Add an implementation: implement ${c.protocolName} ${typeArgsStr} where ...`,
          span,
        );
      }
    }
  }

  // Filter inferred constraints to only those involving quantified type variables.
  // However, we need to catch ambiguous type variables: constraints with type variables
  // that are NOT quantified (i.e., don't appear in the final type).
  const inferredRelevantConstraints: Constraint[] = [];
  for (const c of resolvedConstraints) {
    const typeArgFreeVars: number[] = [];
    for (const t of c.typeArgs) {
      const freeVars = getFreeTypeVars(t, substitution);
      for (const v of freeVars) {
        typeArgFreeVars.push(v);
      }
    }

    // Check if any free var is quantified
    const hasQuantifiedVar = typeArgFreeVars.some((v) => quantified.has(v));

    // Check if any free var is NOT quantified (ambiguous)
    const hasUnquantifiedVar = typeArgFreeVars.some((v) => !quantified.has(v));

    if (typeArgFreeVars.length === 0) {
      // No type variables - constraint is fully concrete, skip it
      // (it was already validated above)
      continue;
    }

    if (hasUnquantifiedVar && !hasQuantifiedVar) {
      // All type variables are unquantified - this is ambiguous!
      // The constraint references type variables that don't appear in the final type.
      const typeArgsStr = c.typeArgs.map((t) => formatType(t)).join(", ");
      throw new SemanticError(
        `Ambiguous type variable in '${c.protocolName}' constraint. ` +
          `The type '${typeArgsStr}' contains type variable(s) that do not appear ` +
          `in the expression's type, so they cannot be determined. ` +
          `Consider adding a type annotation to make the type concrete.`,
        span,
      );
    }

    // Keep constraints that have at least one quantified variable
    if (hasQuantifiedVar) {
      inferredRelevantConstraints.push(c);
    }
  }

  // Process user-annotated constraints
  let allConstraints = [...inferredRelevantConstraints];

  if (annotatedConstraints && annotatedConstraints.length > 0) {
    // Apply substitution to annotated constraints
    const resolvedAnnotated = applySubstitutionToConstraints(
      annotatedConstraints,
      substitution,
    );

    // Validate each annotated constraint
    for (const c of resolvedAnnotated) {
      // Check that all type args in the constraint reference quantified variables
      for (const typeArg of c.typeArgs) {
        const freeVars = getFreeTypeVars(typeArg, substitution);
        let hasQuantifiedVar = false;
        for (const v of freeVars) {
          if (quantified.has(v)) {
            hasQuantifiedVar = true;
          }
        }
        // If the constraint is on a concrete type (no free vars), it's meaningless
        if (freeVars.size === 0) {
          throw new SemanticError(
            `Constraint '${c.protocolName}' is on a concrete type, which is not allowed in type annotations`,
            span,
          );
        }
        // If the constraint references a type variable not in the function's type,
        // that's likely an error
        if (!hasQuantifiedVar) {
          throw new SemanticError(
            `Constraint '${c.protocolName}' references type variables not used in the function type`,
            span,
          );
        }
      }

      // Add to the constraint list (will be deduplicated below)
      allConstraints.push(c);
    }
  }

  // Deduplicate constraints
  const uniqueConstraints: Constraint[] = [];
  for (const c of allConstraints) {
    const isDuplicate = uniqueConstraints.some(
      (uc) =>
        uc.protocolName === c.protocolName &&
        uc.typeArgs.length === c.typeArgs.length &&
        uc.typeArgs.every((t, i) => typesEqual(t, c.typeArgs[i]!)),
    );
    if (!isDuplicate) {
      uniqueConstraints.push(c);
    }
  }

  // IMPORTANT: Try to resolve constraints against instances to concretize type variables.
  // When a constraint has some concrete type args and some type variables (e.g., ExampleProtocol3 Float t123),
  // and there's a unique matching instance (e.g., ExampleProtocol3 Float (List Int)), we can unify
  // the type variable with the instance's concrete type. This fixes hover showing polymorphic types
  // with constraints instead of concrete resolved types.
  const constraintsToRemove: Constraint[] = [];
  for (const c of uniqueConstraints) {
    // Check if any type arg is a quantified type variable
    const resolvedTypeArgs = c.typeArgs.map((t) =>
      applySubstitution(t, substitution),
    );

    // Find all type variable positions and concrete positions
    const varPositions: number[] = [];
    const concretePositions: number[] = [];
    for (let i = 0; i < resolvedTypeArgs.length; i++) {
      const arg = resolvedTypeArgs[i]!;
      if (arg.kind === "var" && quantified.has(arg.id)) {
        varPositions.push(i);
      } else if (arg.kind !== "var") {
        concretePositions.push(i);
      }
    }

    // If we have at least one concrete type arg and at least one type variable,
    // try to find a unique matching instance
    if (concretePositions.length > 0 && varPositions.length > 0) {
      const matchingInstances = currentInstanceRegistry.filter((inst) => {
        if (inst.protocolName !== c.protocolName) return false;
        if (inst.typeArgs.length !== resolvedTypeArgs.length) return false;

        // Check that all concrete positions match
        for (const pos of concretePositions) {
          const instArg = inst.typeArgs[pos]!;
          const constraintArg = resolvedTypeArgs[pos]!;
          if (!instanceTypeMatches(instArg, constraintArg)) return false;
        }

        // Check that the instance has concrete types for the variable positions
        // (so we can actually resolve the type variable)
        for (const pos of varPositions) {
          const instArg = inst.typeArgs[pos]!;
          // Instance type arg must be concrete (not a bare type variable)
          if (instArg.kind === "var") return false;
        }

        return true;
      });

      // If exactly one matching instance, unify the type variables with instance types
      if (matchingInstances.length === 1) {
        const matchingInst = matchingInstances[0]!;
        for (const pos of varPositions) {
          const typeVar = resolvedTypeArgs[pos]!;
          const instType = matchingInst.typeArgs[pos]!;
          if (typeVar.kind === "var") {
            // Add to substitution to concretize this type variable
            substitution.set(typeVar.id, instType);
            // Remove from quantified set since it's now concrete
            quantified.delete(typeVar.id);
          }
        }
        // Mark this constraint for removal since it's now fully resolved
        constraintsToRemove.push(c);
      }
    }
  }

  // Remove fully resolved constraints
  const finalConstraints = uniqueConstraints.filter(
    (c) => !constraintsToRemove.includes(c),
  );

  // Apply the updated substitution to the type
  const finalType = applySubstitution(type, substitution);

  // Check for ambiguous type variables: constraints on type variables that don't appear
  // in the final type. For example, in `[] == []`, the type is `Bool` but we have a
  // constraint `Eq a` where `a` doesn't appear in `Bool`. This is an error because
  // there's no way to determine what `a` should be.
  const finalTypeFreeVars = getFreeTypeVars(finalType, substitution);
  for (const c of finalConstraints) {
    for (const typeArg of c.typeArgs) {
      // Apply substitution to get the resolved type arg
      const resolvedTypeArg = applySubstitution(typeArg, substitution);
      const constraintFreeVars = getFreeTypeVars(resolvedTypeArg, substitution);
      for (const v of constraintFreeVars) {
        // If this type variable appears in the constraint but NOT in the final type,
        // then it's ambiguous - there's no way to determine what type it should be.
        // This catches cases like `[] == []` where the result is Bool but the
        // constraint is `Eq a` and `a` doesn't appear anywhere in the result type.
        if (!finalTypeFreeVars.has(v)) {
          throw new SemanticError(
            `Ambiguous type variable in '${c.protocolName}' constraint. ` +
              `The type variable does not appear in the expression's type, ` +
              `so it cannot be determined. Consider adding a type annotation ` +
              `to make the type concrete.`,
            span,
          );
        }
      }
    }
  }

  return { vars: quantified, constraints: finalConstraints, type: finalType };
}

/**
 * Result of instantiation including both type and instantiated constraints.
 */
type InstantiationResult = {
  type: Type;
  constraints: Constraint[];
};

/**
 * Instantiate a type scheme by replacing all quantified type variables with fresh ones.
 * Also instantiates the constraints with the same variable mapping.
 * This is the dual of generalization - it's used at each use site of a polymorphic binding.
 *
 * Example:
 *   Given scheme: forall a. Num a => a -> a
 *   First instantiation: Num b => b -> b  (where b is fresh)
 *   Second instantiation: Num c => c -> c (where c is fresh)
 *
 * The instantiation rule:
 * - For each quantified type variable in the scheme
 * - Create a fresh type variable
 * - Substitute it throughout the type AND constraints
 */
function instantiateWithConstraints(
  scheme: TypeScheme,
  substitution: Substitution,
): InstantiationResult {
  // If no variables are quantified, the type is monomorphic - return as-is
  if (scheme.vars.size === 0) {
    return { type: scheme.type, constraints: scheme.constraints };
  }

  // Create fresh type variables for each quantified variable
  const instantiationMap = new Map<number, Type>();
  for (const varId of scheme.vars) {
    instantiationMap.set(varId, freshType());
  }

  // Apply the instantiation to get a fresh copy of the type
  const instantiatedType = instantiateType(
    scheme.type,
    instantiationMap,
    substitution,
  );

  // Also instantiate the constraints with the same mapping
  const instantiatedConstraints = scheme.constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) =>
      instantiateType(t, instantiationMap, substitution),
    ),
  }));

  return { type: instantiatedType, constraints: instantiatedConstraints };
}

/**
 * Instantiate a type scheme by replacing all quantified type variables with fresh ones.
 * This is the dual of generalization - it's used at each use site of a polymorphic binding.
 *
 * Example:
 *   Given scheme: forall a. a -> a
 *   First instantiation: b -> b  (where b is fresh)
 *   Second instantiation: c -> c (where c is fresh)
 *   This allows id to be used at both number and string types independently.
 *
 * The instantiation rule:
 * - For each quantified type variable in the scheme
 * - Create a fresh type variable
 * - Substitute it throughout the type
 */
function instantiate(scheme: TypeScheme, substitution: Substitution): Type {
  return instantiateWithConstraints(scheme, substitution).type;
}

/**
 * Helper function to apply an instantiation mapping to a type.
 * Recursively replaces quantified variables with their fresh instantiations.
 */
function instantiateType(
  type: Type,
  instantiationMap: Map<number, Type>,
  substitution: Substitution,
): Type {
  const concrete = applySubstitution(type, substitution);

  if (concrete.kind === "var") {
    // If this variable is being instantiated, use the fresh variable
    const instantiated = instantiationMap.get(concrete.id);
    if (instantiated) {
      return instantiated;
    }
    return concrete;
  }

  if (concrete.kind === "con") {
    return {
      kind: "con",
      name: concrete.name,
      args: concrete.args.map((arg) =>
        instantiateType(arg, instantiationMap, substitution),
      ),
    };
  }

  if (concrete.kind === "fun") {
    return {
      kind: "fun",
      from: instantiateType(concrete.from, instantiationMap, substitution),
      to: instantiateType(concrete.to, instantiationMap, substitution),
    };
  }

  if (concrete.kind === "tuple") {
    return {
      kind: "tuple",
      elements: concrete.elements.map((el) =>
        instantiateType(el, instantiationMap, substitution),
      ),
    };
  }

  if (concrete.kind === "record") {
    const fields: Record<string, Type> = {};
    for (const [key, fieldType] of Object.entries(concrete.fields)) {
      fields[key] = instantiateType(fieldType, instantiationMap, substitution);
    }
    return { kind: "record", fields };
  }

  return concrete;
}

function validateAnnotationArity(
  annotation: TypeExpr,
  argCount: number,
  span: Span,
  name: string,
) {
  const paramCount = countAnnotationParams(annotation);
  if (paramCount !== argCount) {
    throw new SemanticError(
      `Type annotation for '${name}' does not match its argument count`,
      span,
    );
  }
}

function extractAnnotationParams(
  annotation: Type,
  argCount: number,
  span: Span,
): Type[] {
  // Extract params from annotation. If the annotation has fewer params than
  // argCount, fill in fresh type variables for the rest. The type system
  // will catch any mismatches during unification.
  const params = flattenFunctionParams(annotation);
  const result = params.slice(0, argCount);
  // Fill in fresh types for any missing params
  while (result.length < argCount) {
    result.push(freshType());
  }
  return result;
}

function extractAnnotationReturn(annotation: Type, argCount: number): Type {
  let result: Type = annotation;
  for (let i = 0; i < argCount; i++) {
    if (result.kind !== "fun") {
      return result;
    }
    result = result.to;
  }
  return result;
}

function countAnnotationParams(annotation: TypeExpr): number {
  // Handle qualified types by unwrapping to the underlying type
  if (annotation.kind === "QualifiedType") {
    return countAnnotationParams(annotation.type);
  }
  if (annotation.kind === "FunctionType") {
    return 1 + countAnnotationParams(annotation.to);
  }
  return 0;
}

/**
 * TypeVarContext tracks type variable names to their corresponding TypeVar IDs
 * during annotation parsing. This enables polymorphic type annotations like:
 * - id : a -> a
 * - map : (a -> b) -> List a -> List b
 *
 * Type variables are lowercase identifiers (typically single letters: a, b, c)
 * that should be treated as universally quantified variables.
 */
type TypeVarContext = Map<string, TypeVar>;

/**
 * Result of converting a type annotation to an internal type.
 * Includes both the type and any protocol constraints from qualified types.
 */
type AnnotationResult = {
  /** The converted internal type */
  type: Type;
  /** Protocol constraints extracted from qualified types (e.g., Num a => ...) */
  constraints: Constraint[];
};

/**
 * Check if a type name should be treated as a type variable.
 * Type variables are:
 * - Lowercase identifiers (first character is lowercase)
 * - Typically single letters (a, b, c) but can be longer (result, error)
 *
 * In Vibe, ALL lowercase type names are treated as type variables.
 * Concrete types must be capitalized (Int, String, Bool, etc.).
 * This matches Elm/Haskell convention for type system consistency.
 *
 * Examples:
 * - "a", "b", "c" -> type variables
 * - "error", "value", "result" -> type variables
 * - "Int", "String", "Bool" -> concrete types (capitalized)
 */
function isTypeVariable(name: string): boolean {
  if (name.length === 0) return false;

  const firstChar = name[0]!; // Safe because length > 0
  // Lowercase first character = type variable
  // Uppercase first character = concrete type
  return firstChar === firstChar.toLowerCase();
}

/**
 * Convert a type expression (from source annotation) to an internal type,
 * including any protocol constraints from qualified types.
 *
 * This is the primary function for converting user type annotations.
 * It extracts and validates constraints from `QualifiedType` annotations.
 *
 * Type variables (lowercase identifiers like 'a', 'b') get mapped to fresh TypeVars
 * consistently within the same annotation. For example:
 *   a -> a        maps both 'a' to the same TypeVar
 *   (a -> b) -> a maps 'a' consistently, 'b' to a different TypeVar
 *
 * Also handles type aliases by expanding them to their underlying types.
 *
 * @param annotation - The source type expression to convert
 * @param context - Map tracking type variable names to their TypeVar IDs
 * @param adts - Registry of ADT definitions (for recognizing ADT type names)
 * @param typeAliases - Registry of type aliases (for expansion)
 * @param protocols - Registry of protocol definitions (for constraint validation)
 * @returns Object containing the converted type and extracted constraints
 */
function typeFromAnnotationWithConstraints(
  annotation: TypeExpr,
  context: TypeVarContext = new Map(),
  adts: Record<string, ADTInfo> = {},
  typeAliases: Record<string, TypeAliasInfo> = {},
  protocols: Record<string, ProtocolInfo> = {},
  records: Record<string, RecordInfo> = {},
): AnnotationResult {
  // Handle QualifiedType at the top level to extract constraints
  if (annotation.kind === "QualifiedType") {
    // Convert AST constraints to internal constraints
    const constraints: Constraint[] = [];

    for (const astConstraint of annotation.constraints) {
      // Validate that the protocol exists
      const protocol = protocols[astConstraint.protocolName];
      if (!protocol) {
        throw new SemanticError(
          `Unknown protocol '${astConstraint.protocolName}' in type constraint`,
          astConstraint.span,
        );
      }

      // Validate the number of type arguments matches protocol parameters
      if (astConstraint.typeArgs.length !== protocol.params.length) {
        throw new SemanticError(
          `Protocol '${astConstraint.protocolName}' expects ${protocol.params.length} type argument(s), but constraint has ${astConstraint.typeArgs.length}`,
          astConstraint.span,
        );
      }

      // Convert constraint type arguments
      const constraintTypeArgs: Type[] = [];
      for (const typeArg of astConstraint.typeArgs) {
        constraintTypeArgs.push(
          typeFromAnnotation(typeArg, context, adts, typeAliases, records),
        );
      }

      // Validate that constraint type arguments are type variables
      // (Constraints on concrete types like `Num Int` don't make sense in annotations)
      for (let i = 0; i < constraintTypeArgs.length; i++) {
        const typeArg = constraintTypeArgs[i]!;
        if (typeArg.kind !== "var") {
          throw new SemanticError(
            `Constraint '${astConstraint.protocolName}' must be applied to type variables, not concrete types`,
            astConstraint.span,
          );
        }
      }

      constraints.push({
        protocolName: astConstraint.protocolName,
        typeArgs: constraintTypeArgs,
      });
    }

    // Convert the underlying type (recursively handling nested QualifiedTypes)
    const innerResult = typeFromAnnotationWithConstraints(
      annotation.type,
      context,
      adts,
      typeAliases,
      protocols,
      records,
    );

    // Merge constraints from nested qualified types
    return {
      type: innerResult.type,
      constraints: [...constraints, ...innerResult.constraints],
    };
  }

  // For non-qualified types, delegate to the simpler function
  const type = typeFromAnnotation(annotation, context, adts, typeAliases, records);
  return { type, constraints: [] };
}

/**
 * Convert a type expression (from source annotation) to an internal type.
 * Handles type variables by maintaining a context of variable names to TypeVar IDs.
 *
 * NOTE: This function does NOT extract constraints from QualifiedType annotations.
 * Use typeFromAnnotationWithConstraints() when you need constraint extraction.
 *
 * Type variables (lowercase identifiers like 'a', 'b') get mapped to fresh TypeVars
 * consistently within the same annotation. For example:
 *   a -> a        maps both 'a' to the same TypeVar
 *   (a -> b) -> a maps 'a' consistently, 'b' to a different TypeVar
 *
 * Also handles type aliases by expanding them to their underlying types.
 *
 * @param annotation - The source type expression to convert
 * @param context - Map tracking type variable names to their TypeVar IDs
 * @param adts - Registry of ADT definitions (for recognizing ADT type names)
 * @param typeAliases - Registry of type aliases (for expansion)
 */
function typeFromAnnotation(
  annotation: TypeExpr,
  context: TypeVarContext = new Map(),
  adts: Record<string, ADTInfo> = {},
  typeAliases: Record<string, TypeAliasInfo> = {},
  records: Record<string, RecordInfo> = {},
): Type {
  switch (annotation.kind) {
    case "TypeRef": {
      // Check if this is a type variable (lowercase identifier)
      if (isTypeVariable(annotation.name)) {
        // Type variable: look up or create a fresh TypeVar
        let typeVar = context.get(annotation.name);
        if (!typeVar) {
          typeVar = freshType();
          context.set(annotation.name, typeVar);
        }

        // Type variables cannot have arguments
        if (annotation.args.length > 0) {
          // This is likely an error, but we'll treat it as a type constructor
          // to maintain compatibility
        } else {
          return typeVar;
        }
      }

      // Check if this is a type alias
      const aliasInfo = typeAliases[annotation.name];
      if (aliasInfo) {
        // Expand the type alias
        if (annotation.args.length !== aliasInfo.params.length) {
          // Mismatch in type arguments - just fall through to treat as type constructor
        } else {
          // Convert all argument types first
          const argTypes: Type[] = [];
          for (let i = 0; i < aliasInfo.params.length; i++) {
            const argType = typeFromAnnotation(
              annotation.args[i]!,
              context,
              adts,
              typeAliases,
              records,
            );
            argTypes.push(argType);
          }

          // Create fresh type variables for each alias parameter
          // and build a substitution map to replace them with actual argument types
          const aliasContext: TypeVarContext = new Map(context);
          const substitutionMap = new Map<number, Type>();

          for (let i = 0; i < aliasInfo.params.length; i++) {
            const paramName = aliasInfo.params[i]!;
            const fresh = freshType();
            aliasContext.set(paramName, fresh);
            substitutionMap.set(fresh.id, argTypes[i]!);
          }

          // Convert the alias value with fresh type variables
          const expandedType = typeFromAnnotation(
            aliasInfo.value,
            aliasContext,
            adts,
            typeAliases,
            records,
          );

          // Apply substitution to replace fresh vars with actual argument types
          return applySubstitution(expandedType, substitutionMap);
        }
      }

      // Check if this is a named record type
      const recordInfo = records[annotation.name];
      if (recordInfo) {
        // Named record type: expand to structural TypeRecord
        // Handle type parameters by creating a substitution map
        if (annotation.args.length !== recordInfo.params.length) {
          // Mismatch in type arguments - fall through to treat as type constructor
        } else {
          // Convert all argument types first
          const argTypes: Type[] = [];
          for (let i = 0; i < recordInfo.params.length; i++) {
            const argType = typeFromAnnotation(
              annotation.args[i]!,
              context,
              adts,
              typeAliases,
              records,
            );
            argTypes.push(argType);
          }

          // Create fresh type variables for each record parameter
          // and build a substitution map to replace them with actual argument types
          const recordContext: TypeVarContext = new Map(context);
          const substitutionMap = new Map<number, Type>();

          for (let i = 0; i < recordInfo.params.length; i++) {
            const paramName = recordInfo.params[i]!;
            const fresh = freshType();
            recordContext.set(paramName, fresh);
            substitutionMap.set(fresh.id, argTypes[i]!);
          }

          // Build the structural TypeRecord from the record fields
          const fields: Record<string, Type> = {};
          for (const field of recordInfo.fields) {
            // Convert the field's AST TypeExpr to a Type using the record context
            // This properly handles type parameters (e.g., `a` in `Container a`)
            const fieldType = typeFromAnnotation(
              field.typeExpr,
              recordContext,
              adts,
              typeAliases,
              records,
            );
            // Apply substitution to replace fresh vars with actual argument types
            fields[field.name] = applySubstitution(fieldType, substitutionMap);
          }

          return {
            kind: "record",
            fields,
          };
        }
      }

      // Concrete type constructor (Int, String, List, Maybe, etc.)
      // List is now represented as TypeCon like other ADTs
      return {
        kind: "con",
        name: annotation.name,
        args: annotation.args.map((arg) =>
          typeFromAnnotation(arg, context, adts, typeAliases, records),
        ),
      };
    }
    case "FunctionType": {
      const from = typeFromAnnotation(
        annotation.from,
        context,
        adts,
        typeAliases,
        records,
      );
      const to = typeFromAnnotation(
        annotation.to,
        context,
        adts,
        typeAliases,
        records,
      );
      return fn(from, to);
    }
    case "TupleType": {
      return {
        kind: "tuple",
        elements: annotation.elements.map((el) =>
          typeFromAnnotation(el, context, adts, typeAliases, records),
        ),
      };
    }
    case "RecordType": {
      // Reject floating record types in type annotations
      // Record types must be defined using `type Name = { ... }` and then referenced by name
      throw new SemanticError(
        `Record types cannot be used directly in type annotations. ` +
          `Define a named record type using 'type RecordName = { ... }' and reference it by name.`,
        annotation.span,
      );
    }
    case "QualifiedType": {
      // For the simple typeFromAnnotation function, we extract only the underlying type.
      // Use typeFromAnnotationWithConstraints() to also extract constraints.
      // This maintains backwards compatibility with call sites that only need the type.
      return typeFromAnnotation(
        annotation.type,
        context,
        adts,
        typeAliases,
        records,
      );
    }
  }
}

function occursIn(id: number, type: Type, substitution: Substitution): boolean {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
    return concrete.id === id;
  }
  switch (concrete.kind) {
    case "fun":
      return (
        occursIn(id, concrete.from, substitution) ||
        occursIn(id, concrete.to, substitution)
      );
    case "tuple":
      return concrete.elements.some((t) => occursIn(id, t, substitution));
    case "record":
      return Object.values(concrete.fields).some((t) =>
        occursIn(id, t, substitution),
      );
    case "con":
      return concrete.args.some((t) => occursIn(id, t, substitution));
    default:
      return false;
  }
}

function unify(a: Type, b: Type, span: Span, substitution: Substitution) {
  const left = applySubstitution(a, substitution);
  const right = applySubstitution(b, substitution);

  if (left.kind === "var") {
    if (!typesEqual(left, right)) {
      if (occursIn(left.id, right, substitution)) {
        throw new SemanticError("Recursive type detected", span);
      }
      substitution.set(left.id, right);
    }
    return;
  }

  if (right.kind === "var") {
    return unify(right, left, span, substitution);
  }

  if (left.kind === "con" && right.kind === "con") {
    if (left.name !== right.name || left.args.length !== right.args.length) {
      throw new SemanticError(
        `Type mismatch: cannot unify '${formatType(left)}' with '${formatType(
          right,
        )}'`,
        span,
      );
    }
    left.args.forEach((arg, idx) =>
      unify(arg, right.args[idx]!, span, substitution),
    );
    return;
  }

  if (left.kind === "fun" && right.kind === "fun") {
    unify(left.from, right.from, span, substitution);
    unify(left.to, right.to, span, substitution);
    return;
  }

  if (left.kind === "tuple" && right.kind === "tuple") {
    if (left.elements.length !== right.elements.length) {
      throw new SemanticError("Tuple length mismatch", span);
    }
    left.elements.forEach((el, idx) =>
      unify(el, right.elements[idx]!, span, substitution),
    );
    return;
  }

  if (left.kind === "record" && right.kind === "record") {
    const shared = Object.keys(left.fields).filter(
      (k) => right.fields[k] !== undefined,
    );
    for (const key of shared) {
      unify(left.fields[key]!, right.fields[key]!, span, substitution);
    }
    // Row-typed approximation: allow extra fields on either side.
    return;
  }

  throw new SemanticError(
    `Type mismatch: cannot unify '${formatType(left)}' with '${formatType(
      right,
    )}'`,
    span,
  );
}
