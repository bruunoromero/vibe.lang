import type {
  Program,
  ValueDeclaration,
  TypeAnnotationDeclaration,
  DecoratedDeclaration,
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
  Declaration,
} from "@vibe/syntax";
import { BUILTIN_MODULE_NAME } from "@vibe/syntax";

// Re-export errors for external consumers
export {
  SemanticError,
  ImplementingProtocolError,
  MultipleSemanticErrors,
} from "./errors";
import {
  SemanticError,
  ImplementingProtocolError,
  MultipleSemanticErrors,
} from "./errors";

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
  TypeInferenceContext,
  ProtocolMethodUsage,
} from "./types";

// Re-export classes for external consumers
export { Scope, RegistryManager } from "./types";

// Re-export display formatting functions
export {
  formatTypeForDisplay,
  formatTypeSchemeForDisplay,
  formatConstraintsForDisplay,
  buildNormalizedNames,
} from "./utils";

import type {
  Type,
  TypeVar,
  TypeCon,
  TypeScheme,
  Constraint,
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
  TypeInferenceContext,
  TypeVarContext,
  AnnotationResult,
  ProtocolMethodUsage,
} from "./types";

import { Scope, RegistryManager } from "./types";

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
} from "./builtins";

import { checkExhaustiveness } from "./exhaustiveness";

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
 * Error type for recovery - unifies with anything without cascading errors.
 * Inspired by Rust's ty::Error and TypeScript's ErrorType.
 */
const ERROR_TYPE: Type = { kind: "error" };

/**
 * Invert a TypeVarContext (name → TypeVar) to a paramNames map (TypeVar.id → name).
 * Used to preserve user-given type parameter names from annotations.
 */
function invertContext(context: TypeVarContext): Map<number, string> {
  const result = new Map<number, string>();
  for (const [name, typeVar] of context) {
    result.set(typeVar.id, name);
  }
  return result;
}

/**
 * Resolve paramNames through a substitution so that names track through unification.
 * If TypeVar(150) was named "a" but got unified to TypeVar(200), the result maps 200 → "a".
 * If it unified to a concrete type, the name is dropped (no longer polymorphic).
 */
function resolveParamNames(
  paramNames: Map<number, string>,
  substitution: Substitution,
): Map<number, string> {
  const result = new Map<number, string>();
  for (const [id, name] of paramNames) {
    const resolved = applySubstitution({ kind: "var", id }, substitution);
    if (resolved.kind === "var") {
      result.set(resolved.id, name);
    }
  }
  return result;
}

class SemanticAnalyzer {
  /**
   * Module-level error collector for accumulating errors during analysis.
   * Follows Elm's per-definition isolation: errors from one definition
   * don't prevent analysis of other definitions.
   */
  private currentErrors: SemanticError[] = [];
  /**
   * Set to track already-reported errors by their signature (line:col:message).
   * Prevents duplicate errors when the same undefined name is looked up multiple times.
   */
  private errorSignatures: Set<string> = new Set();
  /**
   * Module-level constraint context for collecting protocol constraints during type inference.
   * This is reset at the start of analyzing each top-level value declaration,
   * and the collected constraints are attached during generalization.
   *
   * Using a module-level context avoids threading a context parameter through
   * all the recursive analysis functions.
   */
  private currentConstraintContext: ConstraintContext =
    createConstraintContext();

  /**
   * Tracks protocol method usages (span + constraint) during type inference.
   * Raw usages have unresolved type vars; they are resolved after each value's
   * body is fully analyzed, then stored in the final SemanticModule.
   */
  private _pendingProtocolUsages: Array<{
    node: Expr;
    constraint: Constraint;
  }> = [];
  private _resolvedProtocolUsages: Map<
    Expr,
    { protocolName: string; typeArgs: Type[] }
  > = new Map();
  private _pendingConstrainedCallUsages: Array<{
    node: Expr;
    constraints: Constraint[];
  }> = [];
  private _resolvedConstrainedCallUsages: Map<
    Expr,
    Array<{ protocolName: string; typeArgs: Type[] }>
  > = new Map();

  /**
   * Registry manager containing all type and value registries.
   * Centralizes access to ADTs, constructors, protocols, etc.
   */
  private _registry: RegistryManager = new RegistryManager();

  /**
   * Global scope containing all top-level definitions.
   */
  private _globalScope: Scope = new Scope();

  /**
   * Type substitution for unification.
   */
  private _substitution: Substitution = new Map();

  /**
   * Dependencies map for looking up pre-analyzed modules.
   */
  private _dependencies: Map<string, SemanticModule> = new Map();

  constructor(
    private program: Program,
    private options: AnalyzeOptions,
  ) {
    this._dependencies = options.dependencies ?? new Map();
  }

  analyze() {
    this.ensureModuleNameConsistency();
    this.initializeBuiltinADTs();
    this.initializeBuiltinOpaqueTypes();
    this.validateImports();
    this.seedBuiltinOperators();
    this.mergeImportedModules();

    // ===== PASS 0: Register infix declarations =====
    // We process infix declarations first so operator precedence is known during parsing.
    // Note: This pass validates declarations but the parser pre-processing handles actual precedence.
    this.registerInfixDeclarations();

    // ===== PASS 1a: Register type declarations (ADTs only) =====
    // We register ADTs first so type aliases can reference them.
    this.registerADTTypeDeclarations();

    // ===== PASS 1a2: Register opaque type declarations =====
    // Opaque types are registered before type aliases since aliases may reference them.
    this.registerOpaqueTypeDeclarations();

    // ===== PASS 1b: Register type aliases =====
    // Type aliases are registered after ADTs so they can reference them.
    // Aliases are also registered without validation first so they can reference each other.
    const typeAliasDecls = this.registerTypeAliasDeclarations();

    // ===== PASS 1c: Validate type alias references =====
    // Now that all ADTs and aliases are registered, validate that type references exist.
    this.validateTypeAliasDeclarations(typeAliasDecls);

    // ===== PASS 1d: Register protocols =====
    this.registerProtocolDeclarations();

    // ===== PASS 1.5: Register implementation declarations =====
    // Implementations are registered after protocols but before value inference
    // so that we can resolve constraints during type checking.
    this.registerImplementationDeclarations();

    // Set the module-level record registry for synthesis functions
    _recordRegistry = this.records;
    _protocolMethodUsages = this._resolvedProtocolUsages;
    _currentModuleName = this.getModuleName();

    // ===== PASS 1.6: Auto-implement Eq for all type declarations =====
    // After explicit implementations are registered, auto-generate Eq for types that:
    // 1. Don't already have an Eq implementation
    // 2. Can implement Eq (no function fields, all fields can implement Eq)
    this.autoImplementProtocols();

    // ===== PASS 2: Register value declarations =====
    this.registerValueDeclarations();

    // ===== PASS 2a+: Validate annotations and seed global names =====
    this.validateAnnotationsAndSeedGlobalNames();

    // ===== PASS 2.0a-2.0b: Infer value declarations =====
    this.inferValueDeclarations();

    // ===== PASS 2.5: Validate implementation method expressions =====
    this.validateImplementationMethodExpressions();

    // ===== PASS 2.5b: Validate implementation method types =====
    this.validateImplementationMethodTypes();

    // ===== PASS 2.5c: Validate instance constraint satisfiability =====
    this.validateInstanceConstraintSatisfiability();

    // ===== PASS 2.5d: Validate concrete constraint instances exist =====
    this.validateConcreteConstraintInstances();

    // ===== PASS 2.6: Validate protocol default implementations =====
    this.validateProtocolDefaultImplementations();

    const result = this.buildSemanticModule();

    if (this.getErrors().length > 0) {
      throw new MultipleSemanticErrors(this.getErrors());
    }

    return result;
  }

  // ===== File Context =====

  getFilePath(): string {
    return this.options.fileContext.filePath;
  }

  getSrcDir(): string {
    return this.options.fileContext.srcDir;
  }

  getDeclarations(): Declaration[] {
    return this.program.declarations;
  }

  getModule(): ModuleDeclaration {
    return this.program.module;
  }

  getModuleName(): string {
    return this.getModule().name;
  }

  // ===== Registry Access =====

  get registry(): RegistryManager {
    return this._registry;
  }

  // ===== Registry Convenience Accessors =====
  // These provide direct access to registry data, allowing functions to use
  // analyzer.adts instead of passing adts as a parameter.

  get adts(): Record<string, ADTInfo> {
    return this._registry.adts;
  }

  get constructors(): Record<string, ConstructorInfo> {
    return this._registry.constructors;
  }

  get constructorTypes(): Record<string, TypeScheme> {
    return this._registry.constructorTypes;
  }

  get typeAliases(): Record<string, TypeAliasInfo> {
    return this._registry.typeAliases;
  }

  get records(): Record<string, RecordInfo> {
    return this._registry.records;
  }

  get opaqueTypes(): Record<string, OpaqueTypeInfo> {
    return this._registry.opaqueTypes;
  }

  get protocols(): Record<string, ProtocolInfo> {
    return this._registry.protocols;
  }

  get instances(): InstanceInfo[] {
    return this._registry.instances;
  }

  get operators(): OperatorRegistry {
    return this._registry.operators;
  }

  get values(): Record<string, ValueInfo> {
    return this._registry.values;
  }

  get typeSchemes(): Record<string, TypeScheme> {
    return this._registry.typeSchemes;
  }

  get types(): Record<string, Type> {
    return this._registry.types;
  }

  get annotations(): Record<string, TypeAnnotationDeclaration> {
    return this._registry.annotations;
  }

  get localInstances(): InstanceInfo[] {
    return this._registry.localInstances;
  }

  get infixDeclarations(): InfixDeclaration[] {
    return this._registry.infixDeclarations;
  }

  get localProtocolMethods(): Set<string> {
    return this._registry.localProtocolMethods;
  }

  get importedValues(): Map<string, string> {
    return this._registry.importedValues;
  }

  // ===== Scope Management =====

  get globalScope(): Scope {
    return this._globalScope;
  }

  /** Create a child scope from the global scope */
  createScope(): Scope {
    return this._globalScope.child();
  }

  // ===== Substitution Access =====

  get substitution(): Substitution {
    return this._substitution;
  }

  // ===== Import/Dependency Access =====

  get imports(): ImportDeclaration[] {
    return this.program.imports;
  }

  get dependencies(): Map<string, SemanticModule> {
    return this._dependencies;
  }

  // ===== Error Management =====

  /**
   * Get the currently collected errors.
   */
  getErrors(): SemanticError[] {
    return this.currentErrors;
  }

  /**
   * Add an error to the current error collector.
   * Uses deduplication by location: same file:line:col won't be added twice.
   * This prevents duplicate errors from different code paths at the same location.
   */
  addError(message: string, span: Span): void {
    // Dedupe by location only - prevents duplicates from different validation paths
    const signature = `${this.getFilePath()}:${span.start.line}:${span.start.column}`;
    if (!this.errorSignatures.has(signature)) {
      this.errorSignatures.add(signature);
      this.currentErrors.push(
        new SemanticError(message, span, this.getFilePath()),
      );
    }
  }

  /**
   * Reset the error collector for a new module analysis.
   */
  resetErrors(): void {
    this.currentErrors = [];
    this.errorSignatures = new Set();
  }

  // ===== Constraint Management =====

  /**
   * Get the currently collected constraints.
   */
  getCollectedConstraints(): Constraint[] {
    return this.currentConstraintContext.constraints;
  }

  /**
   * Reset the constraint context for a new value analysis.
   */
  resetConstraintContext(): void {
    this.currentConstraintContext = createConstraintContext();
    this._pendingProtocolUsages = [];
    this._pendingConstrainedCallUsages = [];
  }

  getConstraintContext(): ConstraintContext {
    return this.currentConstraintContext;
  }

  // ===== Type Inference Helpers =====

  /**
   * Analyze an expression using the analyzer's stored registries.
   * This is a convenience method that reduces parameter passing.
   *
   * @param expr - Expression to analyze
   * @param ctx - Type inference context with scope, substitution, and optional expected type
   * @returns The inferred type of the expression
   */
  analyzeExpr(exprArg: Expr, contextArg: TypeInferenceContext): Type {
    // Access registry properties from analyzer
    const { constructors, adts, typeAliases, opaqueTypes, records } = this;
    const { imports, dependencies } = this;

    // Mutable loop variables for tail-call optimised cases (LetIn, Paren).
    // All other cases return immediately inside the switch.
    let expr: Expr = exprArg;
    let scope: Scope = contextArg.scope;
    let substitution: Substitution = contextArg.substitution;
    let expectedType: Type | null = contextArg.expectedType ?? null;

    // eslint-disable-next-line no-constant-condition
    while (true) {
      switch (expr.kind) {
        case "Var": {
          // Look up the symbol and collect any protocol constraints
          const { type: resolved, constraints } =
            this.lookupSymbolWithConstraints(
              scope,
              expr.name,
              expr.span,
              substitution,
            );
          // Add any constraints from the symbol to the current context
          for (const constraint of constraints) {
            addConstraint(this.getConstraintContext(), constraint);
            this._pendingProtocolUsages.push({ node: expr, constraint });
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
              this.getFilePath(),
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
              this.getFilePath(),
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
              this.getFilePath(),
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
              this.getFilePath(),
            );
          }
          return { kind: "con", name: "Unit", args: [] };
        }
        case "Tuple": {
          const elements = expr.elements.map((el) =>
            this.analyzeExpr(el, { scope, substitution }),
          );
          return { kind: "tuple", elements };
        }
        case "List": {
          if (expr.elements.length === 0) {
            return listType(freshType());
          }
          const first = this.analyzeExpr(expr.elements[0]!, {
            scope,
            substitution,
            expectedType: null,
          });
          for (const el of expr.elements.slice(1)) {
            const elType = this.analyzeExpr(el, { scope, substitution });
            this.unify(first, elType, el.span, substitution);
          }
          return listType(applySubstitution(first, substitution));
        }
        case "ListRange": {
          const startType = this.analyzeExpr(expr.start, {
            scope,
            substitution,
          });
          const endType = this.analyzeExpr(expr.end, { scope, substitution });
          this.unify(startType, endType, expr.span, substitution);
          return listType(applySubstitution(startType, substitution));
        }
        case "Record": {
          const fields: Record<string, Type> = {};
          for (const field of expr.fields) {
            if (Object.hasOwn(fields, field.name)) {
              throw new SemanticError(
                `Duplicate record field '${field.name}'`,
                field.span,
                this.getFilePath(),
              );
            }
            fields[field.name] = this.analyzeExpr(field.value, {
              scope,
              substitution,
            });
          }

          const literalFieldNames = new Set(Object.keys(fields));

          // Try to resolve the record literal to a named record type.
          // 1. If expectedType is a TypeCon for a known record, use it directly.
          // 2. Otherwise, search all records in scope for a unique field-name match.
          let targetRecord: RecordInfo | undefined;

          if (expectedType) {
            const expected = applySubstitution(expectedType, substitution);
            if (expected.kind === "con") {
              const rec = this.resolveRecordInfo(expected.name);
              if (rec) {
                targetRecord = rec;
              }
            }
          }

          if (!targetRecord) {
            const candidates = this.findRecordsByFieldNames(literalFieldNames);
            if (candidates.length === 0) {
              const shape = Object.entries(fields)
                .map(([k, v]) => `${k} : ${formatType(v)}`)
                .join(", ");
              throw new SemanticError(
                `No record type in scope has shape { ${shape} }. ` +
                  `Define a named record type with these fields.`,
                expr.span,
                this.getFilePath(),
              );
            }
            if (candidates.length > 1) {
              const names = candidates.map((r) => r.name).join(", ");
              throw new SemanticError(
                `Ambiguous record literal \u2014 multiple types in scope match this shape: ${names}. ` +
                  `Add a type annotation to disambiguate.`,
                expr.span,
                this.getFilePath(),
              );
            }
            targetRecord = candidates[0]!;
          }

          // Validate: literal fields must match record type fields exactly
          const recordFieldNames = new Set(
            targetRecord.fields.map((f) => f.name),
          );
          for (const fname of literalFieldNames) {
            if (!recordFieldNames.has(fname)) {
              throw new SemanticError(
                `Field '${fname}' is not a field of record type '${targetRecord.name}'`,
                expr.fields.find((f) => f.name === fname)!.span,
                this.getFilePath(),
              );
            }
          }
          for (const fname of recordFieldNames) {
            if (!literalFieldNames.has(fname)) {
              throw new SemanticError(
                `Missing field '${fname}' for record type '${targetRecord.name}'`,
                expr.span,
                this.getFilePath(),
              );
            }
          }

          // Create fresh type vars for record params and unify field types
          const paramVars: TypeVar[] = targetRecord.params.map(() =>
            freshType(),
          );
          const resolveCtx = new Map<string, TypeVar>();
          targetRecord.params.forEach((p, i) => {
            resolveCtx.set(p, paramVars[i]!);
          });

          for (const fieldInfo of targetRecord.fields) {
            const declaredFieldType = this.typeFromAnnotation(
              fieldInfo.typeExpr,
              resolveCtx,
            );
            this.unify(
              fields[fieldInfo.name]!,
              declaredFieldType,
              expr.fields.find((f) => f.name === fieldInfo.name)!.span,
              substitution,
            );
          }

          const resolvedArgs = paramVars.map((v) =>
            applySubstitution(v, substitution),
          );
          return { kind: "con", name: targetRecord.name, args: resolvedArgs };
        }
        case "RecordUpdate": {
          const baseType = this.lookupSymbol(
            scope,
            expr.base,
            expr.span,
            substitution,
          );
          const concreteBase = applySubstitution(baseType, substitution);

          if (concreteBase.kind === "con") {
            const recordInfo = this.resolveRecordInfo(concreteBase.name);
            if (recordInfo) {
              // Named record update — resolve fields via RecordInfo
              const paramVars: TypeVar[] = recordInfo.params.map(() =>
                freshType(),
              );
              const resolveCtx = new Map<string, TypeVar>();
              recordInfo.params.forEach((p, i) => {
                resolveCtx.set(p, paramVars[i]!);
              });
              // Unify param vars with the concrete type args
              paramVars.forEach((v, i) => {
                if (concreteBase.args[i]) {
                  this.unify(v, concreteBase.args[i]!, expr.span, substitution);
                }
              });

              const recordFieldNames = new Set(
                recordInfo.fields.map((f) => f.name),
              );
              for (const field of expr.fields) {
                if (!recordFieldNames.has(field.name)) {
                  throw new SemanticError(
                    `Record '${concreteBase.name}' has no field '${field.name}'`,
                    field.span,
                    this.getFilePath(),
                  );
                }
                const fieldInfo = recordInfo.fields.find(
                  (f) => f.name === field.name,
                )!;
                const declaredFieldType = this.typeFromAnnotation(
                  fieldInfo.typeExpr,
                  resolveCtx,
                );
                const fieldType = this.analyzeExpr(field.value, {
                  scope,
                  substitution,
                });
                this.unify(
                  fieldType,
                  declaredFieldType,
                  field.span,
                  substitution,
                );
              }

              const resolvedArgs = paramVars.map((v) =>
                applySubstitution(v, substitution),
              );
              return {
                kind: "con",
                name: concreteBase.name,
                args: resolvedArgs,
              };
            }
          }

          if (concreteBase.kind !== "record") {
            throw new SemanticError(
              `Cannot update non-record '${expr.base}'`,
              expr.span,
              this.getFilePath(),
            );
          }
          const updatedFields: Record<string, Type> = {
            ...concreteBase.fields,
          };
          for (const field of expr.fields) {
            if (!updatedFields[field.name]) {
              throw new SemanticError(
                `Record '${expr.base}' has no field '${field.name}'`,
                field.span,
                this.getFilePath(),
              );
            }
            const fieldType = this.analyzeExpr(field.value, {
              scope,
              substitution,
            });
            this.unify(
              updatedFields[field.name]!,
              fieldType,
              field.span,
              substitution,
            );
            updatedFields[field.name] = applySubstitution(
              updatedFields[field.name]!,
              substitution,
            );
          }
          return { kind: "record", fields: updatedFields };
        }
        case "FieldAccess": {
          // Explicit alias because TypeScript won't narrow a reassigned `let` variable.
          const fieldExpr = expr as Extract<Expr, { kind: "FieldAccess" }>;
          // First, try to resolve module-qualified access (e.g., Vibe.JS.null)
          const moduleAccess = tryResolveModuleFieldAccess(
            fieldExpr,
            imports,
            dependencies,
            substitution,
          );
          if (moduleAccess) {
            // Record constrained call usages for dictionary passing in codegen
            if (moduleAccess.constraints.length > 0) {
              this._pendingConstrainedCallUsages.push({
                node: fieldExpr,
                constraints: moduleAccess.constraints,
              });
            }
            return moduleAccess.type;
          }

          // Otherwise, handle as record field access
          const targetType = this.analyzeExpr(fieldExpr.target, {
            scope,
            substitution,
          });
          const concrete = applySubstitution(targetType, substitution);

          // If target is error type, propagate without additional errors (prevent cascading)
          if (concrete.kind === "error") {
            return ERROR_TYPE;
          }

          let fieldType: Type | undefined;

          if (concrete.kind === "record") {
            fieldType = concrete.fields[fieldExpr.field];
          } else if (concrete.kind === "con") {
            const recordInfo = this.resolveRecordInfo(concrete.name);
            if (recordInfo) {
              const fieldInfo = recordInfo.fields.find(
                (f) => f.name === fieldExpr.field,
              );
              if (fieldInfo) {
                const resolveCtx = new Map<string, TypeVar>();
                const freshVars: TypeVar[] = [];
                recordInfo.params.forEach((p) => {
                  const v: TypeVar = { kind: "var", id: freshType().id };
                  resolveCtx.set(p, v);
                  freshVars.push(v);
                });

                const genericFieldType = this.typeFromAnnotation(
                  fieldInfo.typeExpr,
                  resolveCtx,
                );

                const instSub = new Map<number, Type>();
                freshVars.forEach((v, i) => {
                  instSub.set(v.id, concrete.args[i]!);
                });

                fieldType = applySubstitution(genericFieldType, instSub);
              }
            }
          }

          if (!fieldType) {
            if (
              concrete.kind !== "record" &&
              (concrete.kind !== "con" ||
                !this.resolveRecordInfo(concrete.name))
            ) {
              throw new SemanticError(
                `Cannot access field '${fieldExpr.field}' on non-record value '${formatType(concrete)}'`,
                fieldExpr.span,
                this.getFilePath(),
              );
            }
            throw new SemanticError(
              `Record has no field '${fieldExpr.field}'`,
              fieldExpr.span,
              this.getFilePath(),
            );
          }
          return applySubstitution(fieldType, substitution);
        }
        case "Lambda": {
          const paramTypes = expr.args.map(() => freshType());
          const fnScope = scope.child();
          this.bindPatterns(fnScope, expr.args, paramTypes, substitution);
          const bodyType = this.analyzeExpr(expr.body, {
            scope: fnScope,
            substitution,
          });
          return fnChain(paramTypes, bodyType);
        }
        case "Apply": {
          let calleeType = this.analyzeExpr(expr.callee, {
            scope,
            substitution,
          });
          for (const arg of expr.args) {
            const argType = this.analyzeExpr(arg, { scope, substitution });
            const resultType = freshType();
            this.unify(
              calleeType,
              fn(argType, resultType),
              expr.span,
              substitution,
            );
            // Eagerly validate constraints after unification to catch type mismatches
            // where a protocol method's return type gets unified with a function type
            // due to over-application
            this.validateConstraintsEagerly(substitution, arg.span);
            calleeType = applySubstitution(resultType, substitution);
          }
          return applySubstitution(calleeType, substitution);
        }
        case "If": {
          const condType = this.analyzeExpr(expr.condition, {
            scope,
            substitution,
          });
          // Bool type must be defined in the prelude
          const boolAdt = adts["Bool"];
          if (!boolAdt) {
            throw new SemanticError(
              "Type 'Bool' not found. Make sure the prelude is imported.",
              expr.condition.span,
              this.getFilePath(),
            );
          }
          const tBool: Type = { kind: "con", name: "Bool", args: [] };
          this.unify(condType, tBool, expr.condition.span, substitution);
          const thenType = this.analyzeExpr(expr.thenBranch, {
            scope,
            substitution,
          });
          const elseType = this.analyzeExpr(expr.elseBranch, {
            scope,
            substitution,
          });
          this.unify(thenType, elseType, expr.span, substitution);
          return applySubstitution(thenType, substitution);
        }
        case "LetIn": {
          const letScope = scope.child();

          // First pass: seed the scope with monomorphic schemes to enable recursion
          for (const binding of expr.bindings) {
            if (letScope.symbols.has(binding.name)) {
              throw new SemanticError(
                `Duplicate let-binding '${binding.name}'`,
                binding.span,
                this.getFilePath(),
              );
            }
            const seeded = this.seedValueType(binding);
            // Seed with monomorphic scheme (empty quantifier set)
            this.declareSymbol(
              letScope,
              binding.name,
              { vars: new Set(), constraints: [], type: seeded },
              binding.span,
            );
          }

          // Second pass: analyze each binding and generalize its type
          // This is where let-polymorphism happens for local bindings
          for (const binding of expr.bindings) {
            const declared = this.lookupSymbol(
              letScope,
              binding.name,
              binding.span,
              substitution,
            );
            const inferred = this.analyzeValueDeclaration(
              binding,
              letScope,
              substitution,
              declared,
            );
            // Generalize the inferred type for polymorphic let-bindings
            // Note: We generalize with respect to the parent scope, not letScope,
            // to allow quantifying over variables not bound in the parent
            const generalizedScheme = this.generalize(
              inferred,
              scope,
              substitution,
            );
            letScope.symbols.set(binding.name, generalizedScheme);
          }

          // Tail-call: loop back into the while with the body expression
          expr = expr.body;
          scope = letScope;
          // expectedType intentionally carried through unchanged
          continue;
        }
        case "Case": {
          // Explicit alias because TypeScript won't narrow a reassigned `let` variable.
          const caseExpr = expr as Extract<Expr, { kind: "Case" }>;
          const discriminantType = this.analyzeExpr(caseExpr.discriminant, {
            scope,
            substitution,
          });
          const branchTypes: Type[] = [];

          caseExpr.branches.forEach((branch, index) => {
            const branchScope = new Scope(scope);
            const patternType = this.bindPattern(
              branch.pattern,
              branchScope,
              new Set(),
              freshType(),
              substitution,
            );
            this.unify(
              discriminantType,
              patternType,
              branch.pattern.span,
              substitution,
            );
            const bodyType = this.analyzeExpr(branch.body, {
              scope: branchScope,
              substitution,
              expectedType,
            });
            branchTypes.push(bodyType);

            if (branch.pattern.kind === "WildcardPattern") {
              if (index !== caseExpr.branches.length - 1) {
                throw new SemanticError(
                  "Wildcard pattern makes following branches unreachable",
                  branch.pattern.span,
                  this.getFilePath(),
                );
              }
            }
            if (branch.pattern.kind === "ConstructorPattern") {
              this.validateConstructorArity(branch.pattern);
            }
          });

          if (branchTypes.length === 0) {
            throw new SemanticError(
              "Case expression has no branches",
              caseExpr.span,
              this.getFilePath(),
            );
          }

          const firstType = branchTypes[0]!;
          for (const bt of branchTypes.slice(1)) {
            this.unify(firstType, bt, caseExpr.span, substitution);
          }

          // Exhaustiveness checking
          const patterns = caseExpr.branches.map((b) => b.pattern);
          const result = checkExhaustiveness(
            patterns,
            discriminantType,
            adts,
            constructors,
            (name) =>
              resolveQualifiedConstructor(
                name,
                constructors,
                adts,
                imports,
                dependencies,
              ) || undefined,
          );

          if (!result.exhaustive) {
            throw new SemanticError(
              `Non-exhaustive case expression (missing: ${result.missing})`,
              caseExpr.span,
              this.getFilePath(),
            );
          }

          return applySubstitution(firstType, substitution);
        }
        case "Infix": {
          // Check if this is a builtin operator with known type
          const opType = INFIX_TYPES[expr.operator];

          if (opType) {
            // Builtin operator: analyze operands and unify with known operator type
            const leftType = this.analyzeExpr(expr.left, {
              scope,
              substitution,
            });
            const rightType = this.analyzeExpr(expr.right, {
              scope,
              substitution,
            });
            const expected = applySubstitution(opType, substitution);
            const params = flattenFunctionParams(expected);
            if (params.length < 2) {
              throw new SemanticError(
                "Invalid operator type",
                expr.span,
                this.getFilePath(),
              );
            }
            this.unify(params[0]!, leftType, expr.left.span, substitution);
            this.unify(params[1]!, rightType, expr.right.span, substitution);
            return applySubstitution(
              extractAnnotationReturn(expected, 2),
              substitution,
            );
          }

          // User-defined operator: convert to function application
          // IMPORTANT: Don't analyze operands here - let the Apply handling do it
          // to ensure proper constraint collection and unification
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

          // Track pending usages before recursive call so we can re-key
          // any constraint recorded against the synthetic Var callee to the
          // original Infix node (which is what IR lowering will see).
          const beforeLen = this._pendingProtocolUsages.length;
          const result = this.analyzeExpr(applyExpr, {
            scope,
            substitution,
            expectedType,
          });
          for (let i = beforeLen; i < this._pendingProtocolUsages.length; i++) {
            if (this._pendingProtocolUsages[i]!.node === callee) {
              this._pendingProtocolUsages[i]!.node = expr;
            }
          }
          return result;
        }
        case "Paren":
          expr = expr.expression;
          // expectedType and scope are unchanged — loop continues
          continue;
        case "Unary": {
          // Unary negation: only allowed for Int and Float
          const operandType = this.analyzeExpr(expr.operand, {
            scope,
            substitution,
          });
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
              this.getFilePath(),
            );
          }

          throw new SemanticError(
            `Unary negation is only allowed for Int and Float, but got '${formatType(
              concreteType,
            )}'`,
            expr.span,
            this.getFilePath(),
          );
        }
        default: {
          const _exhaustive: never = expr;
          throw new SemanticError(
            "Unsupported expression",
            (expr as { span: Span }).span,
            this.getFilePath(),
          );
        }
      }
    } // end while (true)
  }

  seedValueType(decl: ValueDeclaration | DecoratedDeclaration): Type {
    if (decl.kind === "DecoratedDeclaration") {
      return this.typeFromAnnotation(decl.annotation, new Map());
    }
    const argTypes = decl.args.map(() => freshType());
    const resultType = freshType();
    return fnChain(argTypes, resultType);
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
   */
  typeFromAnnotation(
    annotation: TypeExpr,
    context: TypeVarContext = new Map(),
  ): Type {
    // Access registries from analyzer
    const { adts, typeAliases, records, imports, dependencies } = this;

    // Helper to resolve type names (qualified or unqualified)
    function resolve(name: string) {
      return resolveQualifiedType(
        name,
        adts,
        typeAliases,
        {}, // Opaque types not passed but usually in adts/aliases? wait, typeFromAnnotation signature doesn't take opaque types
        records,
        imports,
        dependencies,
      );
    }

    // Need to add opaque types to typeFromAnnotation signature if we want to resolve them properly
    // For now, assuming adts covers most? No, opaque types are separate.
    // I will check resolveQualifiedType definition below.

    switch (annotation.kind) {
      case "TypeRef": {
        // Check if this is a type variable (lowercase identifier)
        if (isTypeVariable(annotation.name)) {
          // ... (existing logic)
          // Check local context
          let typeVar = context.get(annotation.name);
          if (!typeVar) {
            typeVar = freshType();
            context.set(annotation.name, typeVar);
          }
          if (annotation.args.length > 0) {
            // Treating as type constructor if it has args - fall through?
            // Existing logic returns var immediately.
            // If 'a' has args, it's invalid syntax usually, but parser allows?
            // Actually, if it has args it shouldn't be a var.
            // But isTypeVariable check is just lowercase.
            // 'list' is lowercase but is type alias/con?
            // Vibe types must be Uppercase.
            // So lowercase implies var.
            // If dot present? 'a.b' is not lowercase?
            // 'R.Result' starts with Upper.
          } else {
            return typeVar;
          }
        }

        // Try to resolve reference (handles qualified names and aliases including imported ones)
        const resolved = resolve(annotation.name);

        if (resolved && resolved.kind === "alias") {
          const aliasInfo = resolved.info as TypeAliasInfo;
          // Expand the type alias
          if (annotation.args.length !== aliasInfo.params.length) {
            // Mismatch - fall back to constructor
          } else {
            // Convert all argument types first
            const argTypes: Type[] = [];
            for (let i = 0; i < aliasInfo.params.length; i++) {
              const argType = this.typeFromAnnotation(
                annotation.args[i]!,
                context,
              );
              argTypes.push(argType);
            }

            // Create fresh type variables and substitution
            const aliasContext: TypeVarContext = new Map(context);
            const substitutionMap = new Map<number, Type>();

            for (let i = 0; i < aliasInfo.params.length; i++) {
              const paramName = aliasInfo.params[i]!;
              const fresh = freshType();
              aliasContext.set(paramName, fresh);
              substitutionMap.set(fresh.id, argTypes[i]!);
            }

            const expandedType = this.typeFromAnnotation(
              aliasInfo.value,
              aliasContext,
            );

            return applySubstitution(expandedType, substitutionMap);
          }
        }

        if (resolved && resolved.kind === "record") {
          const recordInfo = resolved.info as RecordInfo;
          if (annotation.args.length === recordInfo.params.length) {
            const argTypes: Type[] = [];
            for (let i = 0; i < recordInfo.params.length; i++) {
              argTypes.push(
                this.typeFromAnnotation(annotation.args[i]!, context),
              );
            }
            return {
              kind: "con",
              name: recordInfo.name,
              args: argTypes,
            };
          }
        }

        // If resolved to ADT or Opaque, use the CANONICAL name
        const canonicalName = resolved ? resolved.name : annotation.name;

        // Concrete type constructor
        return {
          kind: "con",
          name: canonicalName,
          args: annotation.args.map((arg) =>
            this.typeFromAnnotation(arg, context),
          ),
        };
      }
      case "FunctionType": {
        const from = this.typeFromAnnotation(annotation.from, context);
        const to = this.typeFromAnnotation(annotation.to, context);
        return fn(from, to);
      }
      case "TupleType": {
        return {
          kind: "tuple",
          elements: annotation.elements.map((el) =>
            this.typeFromAnnotation(el, context),
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
          this.getFilePath(),
        );
      }
      case "QualifiedType": {
        // For the simple typeFromAnnotation function, we extract only the underlying type.
        // Use typeFromAnnotationWithConstraints() to also extract constraints.
        // This maintains backwards compatibility with call sites that only need the type.
        return this.typeFromAnnotation(annotation.type, context);
      }
    }
  }

  /**
   * Validate that a constructor pattern has the correct number of arguments.
   *
   * Checks both user-defined constructors (from the registry) and built-in
   * constructors (True, False, Just, Nothing, Ok, Err).
   */
  validateConstructorArity(
    pattern: Extract<Pattern, { kind: "ConstructorPattern" }>,
  ) {
    // First check user-defined constructors
    const ctorInfo = this.constructors[pattern.name];
    if (ctorInfo) {
      if (ctorInfo.arity !== pattern.args.length) {
        throw new SemanticError(
          `Constructor '${pattern.name}' expects ${ctorInfo.arity} argument(s), got ${pattern.args.length}`,
          pattern.span,
          this.getFilePath(),
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
        this.getFilePath(),
      );
    }
  }

  bindPatterns(
    scope: Scope,
    patterns: Pattern[],
    paramTypes: Type[],
    substitution: Substitution = this.substitution,
  ) {
    if (patterns.length !== paramTypes.length) {
      throw new Error("Internal arity mismatch during pattern binding");
    }
    const seen = new Set<string>();
    patterns.forEach((pattern, idx) => {
      const paramType = paramTypes[idx]!;
      this.bindPattern(pattern, scope, seen, paramType, substitution);
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
  bindPattern(
    pattern: Pattern,
    scope: Scope,
    seen = new Set(),
    expected: Type,
    substitution: Substitution = this.substitution,
  ): Type {
    const { constructors, adts, imports, dependencies } = this;

    // Helper for recursive calls
    const bind = (p: Pattern, exp: Type) =>
      this.bindPattern(p, scope, seen, exp);

    switch (pattern.kind) {
      case "VarPattern": {
        if (seen.has(pattern.name)) {
          throw new SemanticError(
            `Duplicate pattern variable '${pattern.name}'`,
            pattern.span,
            this.getFilePath(),
          );
        }
        seen.add(pattern.name);
        // Pattern variables are monomorphic (not generalized)
        // This follows the let-polymorphism discipline where only let-bound names are polymorphic
        this.declareSymbol(
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
        this.unify(
          { kind: "tuple", elements: subTypes },
          expected,
          pattern.span,
          substitution,
        );
        pattern.elements.forEach((el, idx) => bind(el, subTypes[idx]!));
        return applySubstitution(
          { kind: "tuple", elements: subTypes },
          substitution,
        );
      }
      case "ConstructorPattern": {
        // Validate constructor arity
        this.validateConstructorArity(pattern);

        // Look up constructor info to get proper types (supports qualified names)
        const resolved = resolveQualifiedConstructor(
          pattern.name,
          constructors,
          adts,
          imports,
          dependencies,
        );
        const ctorInfo = resolved ? resolved.info : undefined;

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
            this.constructorArgToType(argExpr, paramTypeVars),
          );

          // Unify the result type with the expected type to bind type parameters
          this.unify(resultType, expected, pattern.span, substitution);

          // Validate we have the right number of argument patterns
          if (pattern.args.length !== argTypes.length) {
            throw new SemanticError(
              `Constructor '${pattern.name}' expects ${argTypes.length} argument(s), got ${pattern.args.length}`,
              pattern.span,
              this.getFilePath(),
            );
          }

          // Recursively bind pattern arguments
          pattern.args.forEach((arg, idx) => {
            const argType = applySubstitution(argTypes[idx]!, substitution);
            bind(arg, argType);
          });

          return applySubstitution(resultType, substitution);
        } else {
          // Fall back to legacy behavior for built-in constructors
          const argTypes = pattern.args.map(() => freshType());
          const constructed: Type = fnChain(argTypes, freshType());
          this.unify(constructed, expected, pattern.span, substitution);
          pattern.args.forEach((arg, idx) => bind(arg, argTypes[idx]!));
          return applySubstitution(expected, substitution);
        }
      }
      case "ListPattern": {
        // List pattern: [] or [x, y, z]
        // Expected type should be List a for some a
        const elemType = freshType();
        const lt = listType(elemType);
        this.unify(lt, expected, pattern.span, substitution);

        // Bind each element pattern to the element type
        pattern.elements.forEach((el) =>
          bind(el, applySubstitution(elemType, substitution)),
        );
        return applySubstitution(lt, substitution);
      }
      case "ConsPattern": {
        // Cons pattern: head :: tail
        // Expected type should be List a for some a
        const elemType = freshType();
        const lt = listType(elemType);
        this.unify(lt, expected, pattern.span, substitution);

        // Head pattern binds to element type
        bind(pattern.head, applySubstitution(elemType, substitution));

        // Tail pattern binds to list type
        bind(pattern.tail, applySubstitution(lt, substitution));

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
        this.unify(recordType, expected, pattern.span, substitution);

        // Bind each field's pattern (or the field name as variable)
        for (const field of pattern.fields) {
          const fieldType = fieldTypes[field.name]!;
          const appliedFieldType = applySubstitution(fieldType, substitution);

          if (field.pattern) {
            // Field has an explicit pattern: { x = pat }
            bind(field.pattern, appliedFieldType);
          } else {
            // Field without pattern becomes a variable: { x } === { x = x }
            if (seen.has(field.name)) {
              throw new SemanticError(
                `Duplicate pattern variable '${field.name}'`,
                pattern.span,
                this.getFilePath(),
              );
            }
            seen.add(field.name);
            this.declareSymbol(
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
   * Convert a constructor argument TypeExpr to an internal Type.
   *
   * This handles:
   * - Type variables (lowercase, e.g., "a") -> look up in paramTypeVars
   * - Type constructors (uppercase, e.g., "List a") -> build TypeCon
   * - Recursive types (e.g., "Tree a" in Tree definition) -> build TypeCon
   */
  constructorArgToType(
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
          this.constructorArgToType(arg, paramTypeVars),
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
          from: this.constructorArgToType(expr.from, paramTypeVars),
          to: this.constructorArgToType(expr.to, paramTypeVars),
        };
      }
      case "TupleType": {
        return {
          kind: "tuple",
          elements: expr.elements.map((el) =>
            this.constructorArgToType(el, paramTypeVars),
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
          fields[field.name] = this.constructorArgToType(
            field.type,
            paramTypeVars,
          );
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
          this.getFilePath(),
        );
      }
    }
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
  validateFunctionParamPatterns(patterns: Pattern[]): void {
    for (const pattern of patterns) {
      this.validateFunctionParamPattern(pattern);
    }
  }

  /**
   * Recursively validate a single function parameter pattern.
   */
  validateFunctionParamPattern(pattern: Pattern): void {
    const { constructors, adts } = this;

    switch (pattern.kind) {
      case "VarPattern":
      case "WildcardPattern":
        // Always allowed
        return;

      case "TuplePattern":
        // Allowed, but validate nested patterns
        for (const element of pattern.elements) {
          this.validateFunctionParamPattern(element);
        }
        return;

      case "RecordPattern":
        // Allowed, but validate nested patterns
        for (const field of pattern.fields) {
          if (field.pattern) {
            this.validateFunctionParamPattern(field.pattern);
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
            this.getFilePath(),
          );
        }

        // Validate nested patterns
        for (const arg of pattern.args) {
          this.validateFunctionParamPattern(arg);
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
          this.getFilePath(),
        );
    }
  }

  /**
   * Look up a RecordInfo by name from local records and imported dependencies.
   */
  resolveRecordInfo(name: string): RecordInfo | undefined {
    if (this.records[name]) return this.records[name];
    for (const [, dep] of this.dependencies) {
      if (dep.records[name]) return dep.records[name];
    }
    return undefined;
  }

  /**
   * Find all record types (local + imported) whose field name set exactly
   * matches the given set. Used to resolve un-annotated record literals.
   */
  findRecordsByFieldNames(fieldNames: Set<string>): RecordInfo[] {
    const results: RecordInfo[] = [];
    const seen = new Set<string>();

    const check = (rec: RecordInfo) => {
      if (seen.has(rec.name)) return;
      seen.add(rec.name);
      const recFieldNames = new Set(rec.fields.map((f) => f.name));
      if (
        recFieldNames.size === fieldNames.size &&
        [...fieldNames].every((f) => recFieldNames.has(f))
      ) {
        results.push(rec);
      }
    };

    for (const rec of Object.values(this.records)) {
      check(rec);
    }

    return results;
  }

  analyzeValueDeclaration(
    decl: ValueDeclaration,
    scope: Scope,
    substitution: Substitution,
    declaredType: Type,
    annotationType?: Type,
  ): Type {
    // Validate function parameter patterns (single-constructor ADTs, tuples, records)
    this.validateFunctionParamPatterns(decl.args);

    const paramTypes = annotationType
      ? extractAnnotationParams(annotationType, decl.args.length, decl.span)
      : decl.args.map(() => freshType());
    const returnType = annotationType
      ? extractAnnotationReturn(annotationType, decl.args.length)
      : freshType();
    const expected = fnChain(paramTypes, returnType);

    this.unify(expected, declaredType, decl.span, substitution);

    const fnScope = scope.child();
    this.bindPatterns(fnScope, decl.args, paramTypes, substitution);

    const bodyType = this.analyzeExpr(decl.body, {
      scope: fnScope,
      substitution,
      expectedType: returnType,
    });
    this.unify(bodyType, returnType, decl.body.span, substitution);

    return applySubstitution(expected, substitution);
  }

  /**
   * Unify two types, updating the substitution.
   * Uses the analyzer's stored substitution.
   *
   * @param a - First type
   * @param b - Second type
   * @param span - Source location for error reporting
   */
  unify(
    a: Type,
    b: Type,
    span: Span,
    substitution: Substitution = this.substitution,
  ): void {
    const left = applySubstitution(a, substitution);
    const right = applySubstitution(b, substitution);

    // Error types unify with anything - prevents cascading errors
    if (left.kind === "error" || right.kind === "error") {
      return;
    }

    if (left.kind === "var") {
      if (!typesEqual(left, right)) {
        if (occursIn(left.id, right, substitution)) {
          throw new SemanticError(
            "Recursive type detected",
            span,
            this.getFilePath(),
          );
        }
        substitution.set(left.id, right);
      }
      return;
    }

    if (right.kind === "var") {
      return this.unify(right, left, span, substitution);
    }

    if (left.kind === "con" && right.kind === "con") {
      if (left.name !== right.name || left.args.length !== right.args.length) {
        throw new SemanticError(
          `Type mismatch: cannot unify '${formatType(left)}' with '${formatType(
            right,
          )}'`,
          span,
          this.getFilePath(),
        );
      }
      left.args.forEach((arg, idx) =>
        this.unify(arg, right.args[idx]!, span, substitution),
      );
      return;
    }

    if (left.kind === "fun" && right.kind === "fun") {
      this.unify(left.from, right.from, span, substitution);
      this.unify(left.to, right.to, span, substitution);
      return;
    }

    if (left.kind === "tuple" && right.kind === "tuple") {
      if (left.elements.length !== right.elements.length) {
        throw new SemanticError(
          "Tuple length mismatch",
          span,
          this.getFilePath(),
        );
      }
      left.elements.forEach((el, idx) =>
        this.unify(el, right.elements[idx]!, span, substitution),
      );
      return;
    }

    if (left.kind === "record" && right.kind === "record") {
      const shared = Object.keys(left.fields).filter(
        (k) => right.fields[k] !== undefined,
      );
      for (const key of shared) {
        this.unify(left.fields[key]!, right.fields[key]!, span, substitution);
      }
      // Row-typed approximation: allow extra fields on either side.
      return;
    }

    throw new SemanticError(
      `Type mismatch: cannot unify '${formatType(left)}' with '${formatType(
        right,
      )}'`,
      span,
      this.getFilePath(),
    );
  }

  /**
   * Import a single export specification from a dependency module.
   */
  importExportSpec(
    spec: ExportSpec,
    depModule: SemanticModule,
    imp: ImportDeclaration,
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
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.adts[name],
              imp.span,
              "type",
            );
            this.adts[name] = depADT;
            return;
          }
          if (depModule.typeAliases[name]) {
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.typeAliases[name],
              imp.span,
              "type alias",
            );
            this.typeAliases[name] = depModule.typeAliases[name]!;
            return;
          }
          if (depModule.opaqueTypes[name]) {
            this.opaqueTypes[name] = depModule.opaqueTypes[name]!;
            return;
          }
          if (depModule.records[name]) {
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.records[name],
              imp.span,
              "record type",
            );
            this.records[name] = depModule.records[name]!;
            return;
          }
          if (
            isExportedFromModule(depModule, name, "protocol") &&
            depModule.protocols[name]
          ) {
            // Import the protocol without methods
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.protocols[name],
              imp.span,
              "protocol",
            );
            this.protocols[name] = depModule.protocols[name]!;
            return;
          }
          // If nothing matched, the item is not exported
          throw new SemanticError(
            `Cannot import '${name}' from module '${imp.moduleName}' - it is not exported`,
            spec.span,
            this.getFilePath(),
          );
        }

        // Import value
        const depValue = depModule.values[name];
        if (depValue && depValue.type) {
          const importedType = depValue.type;
          const scheme = this.generalize(
            importedType,
            this.globalScope,
            this.substitution,
          );
          this.globalScope.define(name, scheme);
          // Track for re-export support
          this.importedValues.set(name, imp.moduleName);
        }

        // Also check for constructors (might be re-exported)
        if (isExportedFromModule(depModule, name, "constructor")) {
          const depConstructor = depModule.constructors[name];
          if (depConstructor) {
            this.constructors[name] = depConstructor;
            const ctorScheme = depModule.constructorTypes[name];
            if (ctorScheme) {
              this.globalScope.define(name, ctorScheme);
              this.constructorTypes[name] = ctorScheme;
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
            this.getFilePath(),
          );
        }

        // Import operator value if it exists
        const depValue = depModule.values[op];
        if (depValue && depValue.type) {
          const importedType = depValue.type;
          const scheme = this.generalize(
            importedType,
            this.globalScope,
            this.substitution,
          );
          this.globalScope.define(op, scheme);
        }

        // Import operator info if it exists
        const opInfo = depModule.operators.get(op);
        if (opInfo) {
          this.operators.set(op, opInfo);
          // Track as imported so we can reject fixity redefinitions
          this.importedValues.set(op, imp.moduleName);
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
              this.getFilePath(),
            );
          }

          this.checkTypeCollision(
            name,
            imp.moduleName,
            this.adts[name],
            imp.span,
            "type",
          );
          this.adts[name] = depADT;

          // Import all constructors
          for (const ctorName of depADT.constructors) {
            const ctor = depModule.constructors[ctorName];
            if (ctor) {
              this.constructors[ctorName] = ctor;
              const ctorScheme = depModule.constructorTypes[ctorName];
              if (ctorScheme) {
                this.globalScope.define(ctorName, ctorScheme);
                this.constructorTypes[ctorName] = ctorScheme;
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
              this.getFilePath(),
            );
          }

          this.checkTypeCollision(
            name,
            imp.moduleName,
            this.protocols[name],
            imp.span,
            "protocol",
          );
          this.protocols[name] = depProtocol;

          // Add all protocol methods to scope
          const methodSchemes = addProtocolMethodsToScope(
            depProtocol,
            this.globalScope,
          );
          // Store in typeSchemes for LSP hover/completion
          for (const [methodName, scheme] of methodSchemes) {
            this.typeSchemes[methodName] = scheme;
          }
          return;
        }

        // Check if it's a record type - records cannot use (..) syntax since they have no constructors
        const depRecord = depModule.records[name];
        if (depRecord) {
          throw new SemanticError(
            `Record type '${name}' cannot use (..) syntax - records have no constructors. Use '${name}' instead`,
            spec.span,
            this.getFilePath(),
          );
        }

        throw new SemanticError(
          `Cannot import '${name}(..)' from module '${imp.moduleName}' - it is not a type or protocol`,
          spec.span,
          this.getFilePath(),
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
              this.getFilePath(),
            );
          }

          this.checkTypeCollision(
            name,
            imp.moduleName,
            this.adts[name],
            imp.span,
            "type",
          );
          this.adts[name] = depADT;

          // Import specific constructors
          for (const ctorName of members) {
            if (!depADT.constructors.includes(ctorName)) {
              throw new SemanticError(
                `Constructor '${ctorName}' is not defined in type '${name}'`,
                spec.span,
                this.getFilePath(),
              );
            }
            const ctor = depModule.constructors[ctorName];
            if (ctor) {
              this.constructors[ctorName] = ctor;
              const ctorScheme = depModule.constructorTypes[ctorName];
              if (ctorScheme) {
                this.globalScope.define(ctorName, ctorScheme);
                this.constructorTypes[ctorName] = ctorScheme;
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
              this.getFilePath(),
            );
          }

          this.checkTypeCollision(
            name,
            imp.moduleName,
            this.protocols[name],
            imp.span,
            "protocol",
          );
          this.protocols[name] = depProtocol;

          // Add specific protocol methods to scope
          for (const methodName of members) {
            const methodInfo = depProtocol.methods.get(methodName);
            if (!methodInfo) {
              throw new SemanticError(
                `Method '${methodName}' is not defined in protocol '${name}'`,
                spec.span,
                this.getFilePath(),
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
            this.globalScope.define(methodName, scheme);
          }
          return;
        }

        throw new SemanticError(
          `Cannot import '${name}(...)' from module '${imp.moduleName}' - it is not a type or protocol`,
          spec.span,
          this.getFilePath(),
        );
      }
    }
  }

  registerValue(decl: ValueDeclaration | DecoratedDeclaration) {
    // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
    if (Object.hasOwn(this.values, decl.name)) {
      throw new SemanticError(
        `Duplicate definition for '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }
    const hasBuiltinAnnotation = decl.kind === "DecoratedDeclaration";

    // Extract external target for @external decorator
    let externalTarget:
      | { modulePath: string; exportName: string; span: Span }
      | undefined;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
      externalTarget = {
        modulePath: decl.args[0]!,
        exportName: decl.args[1]!,
        span: decl.span,
      };
    }

    this.values[decl.name] = {
      declaration: decl,
      annotation: hasBuiltinAnnotation ? decl.annotation : undefined,
      externalTarget,
    };
  }

  // ===== Factory Methods =====

  /**
   * Create a child scope from a given parent scope.
   * If no parent is provided, uses the global scope.
   */
  createChildScope(parent?: Scope): Scope {
    return (parent ?? this._globalScope).child();
  }

  // ===== Module Analysis Methods =====

  /**
   * Compute the expected module name from file path.
   * The module name is derived from the relative path within the source directory.
   */
  private computeExpectedModuleName(): string {
    // Normalize paths to handle different separators
    const normalizedFilePath = this.getFilePath().replace(/\\/g, "/");
    const rawSrcDir = this.getSrcDir();
    const normalizedSrcDir = rawSrcDir.replace(/\\/g, "/").replace(/\/$/, "");

    // If srcDir is empty or the file isn't under srcDir, use filename as module name
    if (
      !normalizedSrcDir ||
      !normalizedFilePath.startsWith(normalizedSrcDir + "/")
    ) {
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

  private ensureModuleNameConsistency() {
    const expectedModuleName = this.computeExpectedModuleName();

    const declaredModuleName = this.program.module.name;
    if (declaredModuleName !== expectedModuleName) {
      throw new SemanticError(
        `Module name '${declaredModuleName}' does not match file path.\n` +
          `Expected: module ${expectedModuleName} [exposing (..)]\n` +
          `File path: ${this.getFilePath()}`,
        this.program.module.span,
        this.getFilePath(),
      );
    }
  }

  private initializeBuiltinADTs(): void {
    // Bool ADT: True | False (still an ADT for pattern matching)
    this.adts["Bool"] = {
      name: "Bool",
      params: [],
      constructors: ["True", "False"],
      constraints: [],
      span: BUILTIN_SPAN,
    };
    this.constructors["True"] = {
      arity: 0,
      argTypes: [],
      parentType: "Bool",
      parentParams: [],
      moduleName: BUILTIN_MODULE_NAME,
      span: BUILTIN_SPAN,
    };
    this.constructorTypes["True"] = {
      vars: new Set(),
      constraints: [],
      type: { kind: "con", name: "Bool", args: [] },
    };
    this.constructors["False"] = {
      arity: 0,
      argTypes: [],
      parentType: "Bool",
      parentParams: [],
      moduleName: BUILTIN_MODULE_NAME,
      span: BUILTIN_SPAN,
    };
    this.constructorTypes["False"] = {
      vars: new Set(),
      constraints: [],
      type: { kind: "con", name: "Bool", args: [] },
    };

    // List ADT: List a (builtin parameterized type for lists)
    // Note: List uses the internal "list" type representation but is exposed as "List" ADT
    this.adts["List"] = {
      name: "List",
      params: ["a"],
      constructors: [], // List constructors are handled specially (via list literals)
      constraints: [],
      span: BUILTIN_SPAN,
    };

    this.globalScope.define("True", this.constructorTypes["True"]!);
    this.globalScope.define("False", this.constructorTypes["False"]!);
  }

  private initializeBuiltinOpaqueTypes(): void {
    // Primitive opaque types - no pattern matching allowed
    this.opaqueTypes["Unit"] = {
      name: "Unit",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN,
    };

    this.opaqueTypes["Int"] = {
      name: "Int",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN,
    };

    this.opaqueTypes["Float"] = {
      name: "Float",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN,
    };

    this.opaqueTypes["String"] = {
      name: "String",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN,
    };

    this.opaqueTypes["Char"] = {
      name: "Char",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN,
    };
  }

  private validateImports(): void {
    const byModule = new Map<string, ImportDeclaration>();
    const byAlias = new Map<string, ImportDeclaration>();

    for (const imp of this.imports) {
      const duplicateModule = byModule.get(imp.moduleName);
      if (duplicateModule) {
        throw new SemanticError(
          `Duplicate import of module '${imp.moduleName}'`,
          imp.span,
          this.getFilePath(),
        );
      }
      byModule.set(imp.moduleName, imp);

      if (imp.alias) {
        const duplicateAlias = byAlias.get(imp.alias);
        if (duplicateAlias) {
          throw new SemanticError(
            `Duplicate import alias '${imp.alias}'`,
            imp.span,
            this.getFilePath(),
          );
        }
        byAlias.set(imp.alias, imp);
      }
    }
  }

  private seedBuiltinOperators(): void {
    // Seed built-in operators as functions for prefix/infix symmetry.
    // Built-in operators are monomorphic (not polymorphic).
    // Short-circuit operators (&& and ||) are now included here.
    for (const [op, ty] of Object.entries(INFIX_TYPES)) {
      this.globalScope.define(op, {
        vars: new Set(),
        constraints: [],
        type: ty,
      });
    }

    // Seed built-in operator fixities (precedence and associativity).
    // These are for short-circuit operators that are built into the compiler.
    for (const [op, fixity] of Object.entries(BUILTIN_OPERATOR_FIXITY)) {
      this.operators.set(op, fixity);
    }
  }

  private mergeImportedModules(): void {
    for (const imp of this.imports) {
      const depModule = this.dependencies.get(imp.moduleName);
      if (!depModule) {
        // If dependency not provided, seed with placeholder (for backward compatibility)
        throw new SemanticError(
          "Unresolved module import",
          imp.span,
          this.getFilePath(),
        );
      }

      // NOTE: Protocols are only imported when explicitly requested:
      // - `exposing (..)` imports all exported protocols
      // - `exposing (ProtocolName(..))` imports that specific protocol
      // - `import Dep` (no exposing clause) does NOT import any protocols
      // This ensures that `import Vibe.Basics exposing (Num)` does NOT bring
      // `Show` into scope, so `implement Show Int` will fail if Show is not imported.
      // Protocols will be imported below in the exposing clause handling.

      // IMPORTANT: Instances are always visible (Haskell's orphan instance semantics).
      // Import ALL instances from the dependency, including transitively imported ones.
      // This matches Haskell's behavior where instances are globally visible once imported.
      // Deduplication happens via the instance's unique (protocolName, moduleName, typeArgs) identity.
      // Skip instances that originate from the current module to prevent false overlap
      // errors when a downstream module's cache carries our own instances back to us.
      const currentModuleName = this.getModuleName();
      for (const instance of depModule.instances) {
        if (instance.moduleName === currentModuleName) continue;
        const isDuplicate = this.instances.some(
          (existing) =>
            existing.protocolName === instance.protocolName &&
            existing.moduleName === instance.moduleName &&
            typeArgsEqual(existing.typeArgs, instance.typeArgs),
        );
        if (!isDuplicate) {
          this.instances.push(instance);
        }
      }

      // Handle import alias (e.g., `import Html as H`)
      if (imp.alias) {
        // Register the alias in scope for qualified name access (e.g., H.div).
        // The type is a placeholder since module namespaces aren't first-class values -
        // actual qualified access is resolved specially in tryResolveModuleFieldAccess.
        this.globalScope.define(imp.alias, {
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
        if (!this.globalScope.has(rootModule)) {
          this.globalScope.define(rootModule, {
            vars: new Set(),
            constraints: [],
            type: freshType(), // Placeholder: module namespaces resolved via tryResolveModuleFieldAccess
          });
        }
      }

      // Handle explicit exposing with new ExportSpec format
      if (imp.exposing?.kind === "Explicit") {
        for (const spec of imp.exposing.exports) {
          this.importExportSpec(spec, depModule, imp);
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
            const scheme = this.generalize(
              importedType,
              this.globalScope,
              this.substitution,
            );
            this.globalScope.define(name, scheme);
            // Track for re-export support
            this.importedValues.set(name, imp.moduleName);
          }
        }

        // Import all re-exported values (values that were imported from other modules and re-exported)
        // These are values that the dependency imported from other modules and then re-exports
        // Their type schemes are stored in depModule.typeSchemes
        for (const [name] of depModule.exports.reExportedValues) {
          // Skip if already imported (defined value takes precedence)
          if (this.globalScope.has(name)) continue;

          // Look up the type scheme from depModule's typeSchemes (where re-exported values are stored)
          const scheme = depModule.typeSchemes[name];
          if (scheme) {
            this.globalScope.define(name, scheme);
            this.importedValues.set(name, imp.moduleName);
          }
        }

        // Import all exported constructors
        for (const [name, ctor] of Object.entries(depModule.constructors) as [
          string,
          ConstructorInfo,
        ][]) {
          if (isExportedFromModule(depModule, name, "constructor")) {
            this.constructors[name] = ctor;
            // Also import the type scheme so the constructor can be used as a value
            const ctorScheme = depModule.constructorTypes[name];
            if (ctorScheme) {
              this.globalScope.define(name, ctorScheme);
              this.constructorTypes[name] = ctorScheme;
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
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.adts[name],
              imp.span,
              "type",
            );
            this.adts[name] = adt;
          }
        }

        // Import all exported type aliases
        for (const [name, alias] of Object.entries(depModule.typeAliases) as [
          string,
          TypeAliasInfo,
        ][]) {
          if (isExportedFromModule(depModule, name, "type")) {
            // Check for collision with existing type alias from different module
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.typeAliases[name],
              imp.span,
              "type alias",
            );
            this.typeAliases[name] = alias;
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
              Object.hasOwn(this.opaqueTypes, name) &&
              this.opaqueTypes[name]!.moduleName !== BUILTIN_MODULE_NAME &&
              this.opaqueTypes[name]!.moduleName !== imp.moduleName
            ) {
              throw new SemanticError(
                `Opaque type '${name}' conflicts with opaque type from module '${
                  this.opaqueTypes[name]!.moduleName
                }'. ` + `Consider using a different name or qualified imports.`,
                imp.span,
                this.getFilePath(),
              );
            }
            this.opaqueTypes[name] = opaque;
          }
        }

        // Import all exported record types
        for (const [name, rec] of Object.entries(depModule.records) as [
          string,
          RecordInfo,
        ][]) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.records[name],
              imp.span,
              "record type",
            );
            this.records[name] = rec;
          }
        }

        // Import all exported protocols
        for (const [name, protocol] of Object.entries(depModule.protocols) as [
          string,
          ProtocolInfo,
        ][]) {
          if (isExportedFromModule(depModule, name, "protocol")) {
            // Check for collision with existing protocol from different module
            this.checkTypeCollision(
              name,
              imp.moduleName,
              this.protocols[name],
              imp.span,
              "protocol",
            );
            this.protocols[name] = protocol;

            // Add protocol methods to scope so they can be called directly
            const methodSchemes = addProtocolMethodsToScope(
              protocol,
              this.globalScope,
            );
            // Store in typeSchemes for LSP hover/completion
            for (const [methodName, scheme] of methodSchemes) {
              this.typeSchemes[methodName] = scheme;
            }
          }
        }

        // Import exported operator declarations
        for (const [op, info] of depModule.operators) {
          if (isExportedFromModule(depModule, op, "operator")) {
            this.operators.set(op, info);
            // Track for re-export support (operators are also values)
            this.importedValues.set(op, imp.moduleName);
          }
        }
      }
    }

    // Global instance visibility: import instances from ALL available dependency
    // modules, not just explicitly imported ones. In Haskell, instances are
    // globally visible once defined anywhere in the program. This ensures that
    // auto-derived instances (e.g., Show using ++ / Appendable) can find the
    // required instances even if the defining module isn't directly imported.
    const importedModuleNames = new Set(
      this.imports.map((imp) => imp.moduleName),
    );
    const thisModuleName = this.getModuleName();
    for (const [depName, depModule] of this.dependencies) {
      if (importedModuleNames.has(depName)) continue;
      for (const instance of depModule.instances) {
        if (instance.moduleName === thisModuleName) continue;
        const isDuplicate = this.instances.some(
          (existing) =>
            existing.protocolName === instance.protocolName &&
            existing.moduleName === instance.moduleName &&
            typeArgsEqual(existing.typeArgs, instance.typeArgs),
        );
        if (!isDuplicate) {
          this.instances.push(instance);
        }
      }
    }
  }

  private registerInfixDeclarations(): void {
    for (const decl of this.program.declarations) {
      if (decl.kind === "InfixDeclaration") {
        this.registerInfixDeclaration(decl);
        continue;
      }
    }
  }

  /**
   * Register an infix declaration in the operator registry.
   * Validates that:
   * 1. There are no duplicate declarations for the same operator
   * 2. The operator is not already imported (fixity is intrinsic and cannot be redefined)
   */
  registerInfixDeclaration(decl: InfixDeclaration) {
    // Check if operator is imported - if so, reject the fixity declaration
    // because fixity is intrinsic and travels with the operator
    if (this.importedValues.has(decl.operator)) {
      const sourceModule = this.importedValues.get(decl.operator)!;
      throw new SemanticError(
        `Cannot declare fixity for imported operator '${decl.operator}' from module '${sourceModule}'. ` +
          `Fixity is an intrinsic property of the operator and cannot be redefined.`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Check for duplicate local operator declaration
    if (this.operators.has(decl.operator)) {
      throw new SemanticError(
        `Duplicate infix declaration for operator '${decl.operator}'`,
        decl.span,
        this.getFilePath(),
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
        this.getFilePath(),
      );
    }

    // Register the operator
    this.operators.set(decl.operator, {
      precedence: decl.precedence,
      associativity,
    });

    // Store the declaration for later reference
    this.infixDeclarations.push(decl);
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
  registerRecordTypeDeclaration(decl: TypeDeclaration): void {
    // Check for duplicate type name across all type namespaces
    if (this.records[decl.name]) {
      throw new SemanticError(
        `Duplicate record type declaration for '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }
    if (this.adts[decl.name]) {
      throw new SemanticError(
        `Record type '${decl.name}' conflicts with ADT '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }
    if (this.typeAliases[decl.name]) {
      throw new SemanticError(
        `Record type '${decl.name}' conflicts with type alias '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }
    if (this.opaqueTypes[decl.name]) {
      throw new SemanticError(
        `Record type '${decl.name}' conflicts with opaque type '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate no mixing of constructors and record fields
    if (decl.constructors && decl.constructors.length > 0) {
      throw new SemanticError(
        `Type '${decl.name}' cannot have both constructors and record fields. ` +
          `Use 'type' for ADTs or record types separately.`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate record fields exist - but allow empty records (like unit types)
    if (!decl.recordFields) {
      throw new SemanticError(
        `Record type '${decl.name}' is missing field definitions`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Check for duplicate field names
    const fieldNames = new Set<string>();
    for (const field of decl.recordFields) {
      if (fieldNames.has(field.name)) {
        throw new SemanticError(
          `Duplicate field '${field.name}' in record type '${decl.name}'`,
          field.span,
          this.getFilePath(),
        );
      }
      fieldNames.add(field.name);
    }

    // Create temporary type variables for type params (for field type resolution)
    const paramTypeVars: Map<string, TypeVar> = new Map();
    for (const param of decl.params) {
      paramTypeVars.set(param, freshType());
    }

    // Resolve constraints (if any)
    const semanticConstraints: Constraint[] = [];
    if (decl.constraints) {
      for (const c of decl.constraints) {
        semanticConstraints.push({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) =>
            this.constructorArgToType(t, paramTypeVars),
          ),
        });
      }
    }

    // Validate field types - reject floating record types in field definitions
    for (const field of decl.recordFields) {
      if (containsRecordType(field.type)) {
        throw new SemanticError(
          `Record types cannot be used directly in type annotations. ` +
            `Define a named record type using 'type RecordName = { ... }' and reference it by name.`,
          field.span,
          this.getFilePath(),
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
    this.records[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      constraints: semanticConstraints,
      fields,
      span: decl.span,
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
  registerTypeDeclaration(decl: TypeDeclaration) {
    // Check for duplicate type name
    const existingADT = this.adts[decl.name];
    if (existingADT) {
      // If it's from a different module, provide a more helpful error message
      if (
        existingADT.moduleName &&
        existingADT.moduleName !== this.getModuleName()
      ) {
        throw new SemanticError(
          `Type '${decl.name}' conflicts with type from module '${existingADT.moduleName}'. ` +
            `Consider using a different name or qualified imports.`,
          decl.span,
          this.getFilePath(),
        );
      }
      throw new SemanticError(
        `Duplicate type declaration for '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate type parameters are unique
    const paramSet = new Set<string>();
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(
          `Duplicate type parameter '${param}' in type '${decl.name}'`,
          decl.span,
          this.getFilePath(),
        );
      }
      paramSet.add(param);
    }

    // Validate at least one constructor (for ADTs only)
    // Record types don't have constructors
    if (!decl.constructors || decl.constructors.length === 0) {
      // If we have record fields, this is a record type - register it separately
      if (decl.recordFields) {
        this.registerRecordTypeDeclaration(decl);
        return;
      }
      throw new SemanticError(
        `Type '${decl.name}' must have at least one constructor`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Create fresh type variables for each type parameter
    // These will be used to construct the polymorphic type scheme for constructors
    const paramTypeVars: Map<string, TypeVar> = new Map();
    for (const param of decl.params) {
      paramTypeVars.set(param, freshType());
    }

    // Resolve constraints (if any)
    const semanticConstraints: Constraint[] = [];
    if (decl.constraints) {
      for (const c of decl.constraints) {
        semanticConstraints.push({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) =>
            this.constructorArgToType(t, paramTypeVars),
          ),
        });
      }
    }

    // Register the ADT
    const constructorNames = decl.constructors.map((c) => c.name);
    this.adts[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      constructors: constructorNames,
      constraints: semanticConstraints,
      span: decl.span,
    };

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
      const existingCtor = this.constructors[ctor.name];
      if (existingCtor && existingCtor.moduleName === this.getModuleName()) {
        throw new SemanticError(
          `Duplicate constructor '${ctor.name}' (constructor names must be unique within a module)`,
          ctor.span,
          this.getFilePath(),
        );
      }

      // Note: We allow shadowing built-in constructors (like True/False)
      // The type checker will disambiguate based on context

      // Register constructor info
      this.constructors[ctor.name] = {
        arity: ctor.args.length,
        argTypes: ctor.args,
        parentType: decl.name,
        parentParams: decl.params,
        moduleName: this.getModuleName(),
        span: ctor.span,
      };

      // Build the constructor's type and register it in global scope
      // For Just: a -> Maybe a
      // For Nothing: Maybe a
      const ctorType = this.buildConstructorType(
        ctor,
        resultType,
        paramTypeVars,
      );

      // Generalize over all type parameters to make it polymorphic
      const quantifiedVars = new Set<number>();
      for (const tv of paramTypeVars.values()) {
        quantifiedVars.add(tv.id);
      }

      const ctorScheme: TypeScheme = {
        vars: quantifiedVars,
        constraints: semanticConstraints,
        type: ctorType,
        paramNames: invertContext(paramTypeVars),
      };

      // Register constructor as a polymorphic value in global scope
      this.globalScope.define(ctor.name, ctorScheme);

      // Also store in constructorTypes for export
      this.constructorTypes[ctor.name] = ctorScheme;
    }
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
  buildConstructorType(
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
      this.constructorArgToType(argExpr, paramTypeVars),
    );

    // Build function type: arg1 -> arg2 -> ... -> ResultType
    return fnChain(argTypes, resultType);
  }

  private registerADTTypeDeclarations(): void {
    for (const decl of this.program.declarations) {
      if (decl.kind === "TypeDeclaration") {
        this.registerTypeDeclaration(decl);
        continue;
      }
    }
  }

  private registerOpaqueTypeDeclarations(): void {
    for (const decl of this.program.declarations) {
      if (decl.kind === "OpaqueTypeDeclaration") {
        this.registerOpaqueType(decl);
        continue;
      }
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
  registerOpaqueType(decl: OpaqueTypeDeclaration) {
    // Check for duplicate type name
    // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
    if (Object.hasOwn(this.opaqueTypes, decl.name)) {
      const existing = this.opaqueTypes[decl.name]!;
      if (existing.moduleName && existing.moduleName !== this.getModuleName()) {
        throw new SemanticError(
          `Opaque type '${decl.name}' conflicts with opaque type from module '${existing.moduleName}'. ` +
            `Consider using a different name or qualified imports.`,
          decl.span,
          this.getFilePath(),
        );
      }
      throw new SemanticError(
        `Duplicate opaque type declaration for '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate type parameters are unique
    const paramSet = new Set<string>();
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(
          `Duplicate type parameter '${param}' in opaque type '${decl.name}'`,
          decl.span,
          this.getFilePath(),
        );
      }
      paramSet.add(param);
    }

    // Register the opaque type
    this.opaqueTypes[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      span: decl.span,
    };
  }

  private registerTypeAliasDeclarations(): TypeAliasDeclaration[] {
    const typeAliasDecls: TypeAliasDeclaration[] = [];
    for (const decl of this.program.declarations) {
      if (decl.kind === "TypeAliasDeclaration") {
        this.registerTypeAliasWithoutValidation(decl);
        typeAliasDecls.push(decl);
        continue;
      }
    }
    return typeAliasDecls;
  }

  /**
   * Register a type alias declaration without validating type references.
   * This allows aliases to be registered before validation, enabling
   * mutual references between aliases.
   */
  registerTypeAliasWithoutValidation(decl: TypeAliasDeclaration) {
    // Check for duplicate alias name
    const existingAlias = this.typeAliases[decl.name];
    if (existingAlias) {
      // If it's from a different module, provide a more helpful error message
      if (
        existingAlias.moduleName &&
        existingAlias.moduleName !== this.getModuleName()
      ) {
        throw new SemanticError(
          `Type alias '${decl.name}' conflicts with type alias from module '${existingAlias.moduleName}'. ` +
            `Consider using a different name or qualified imports.`,
          decl.span,
          this.getFilePath(),
        );
      }
      throw new SemanticError(
        `Duplicate type alias '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate type parameters are unique
    const paramSet = new Set<string>();
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(
          `Duplicate type parameter '${param}' in type alias '${decl.name}'`,
          decl.span,
          this.getFilePath(),
        );
      }
      paramSet.add(param);
    }

    // Register the alias (validation happens later)
    this.typeAliases[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      value: decl.value,
      span: decl.span,
    };
  }

  private validateTypeAliasDeclarations(
    typeAliasDecls: TypeAliasDeclaration[],
  ): void {
    for (const decl of typeAliasDecls) {
      this.validateTypeAliasReferences(decl);
    }
  }

  /**
   * Validate that all type references in a type alias are defined.
   * Called after all ADTs and aliases are registered.
   */
  validateTypeAliasReferences(decl: TypeAliasDeclaration) {
    // Reject bare record types in type alias declarations
    // Record types must be defined using `type Name = { ... }` syntax
    if (decl.value.kind === "RecordType") {
      throw new SemanticError(
        `Type alias '${decl.name}' cannot directly define a record type. ` +
          `Use 'type ${decl.name} = { ... }' instead of 'type alias'.`,
        decl.value.span,
        this.getFilePath(),
      );
    }

    const paramSet = new Set<string>(decl.params);

    const validationErrors = validateTypeExpr(
      this,
      decl.value,
      paramSet,
      decl.span,
    );

    if (validationErrors.length > 0) {
      // Report the first error (could be extended to report all)
      const err = validationErrors[0]!;
      const message = err.suggestion
        ? `${err.message}. ${err.suggestion}`
        : err.message;
      throw new SemanticError(message, err.span, this.getFilePath());
    }
  }

  private registerProtocolDeclarations(): void {
    for (const decl of this.program.declarations) {
      if (decl.kind === "ProtocolDeclaration") {
        this.registerProtocol(decl);
        // Track methods from locally-defined protocols for fixity validation
        for (const method of decl.methods) {
          this.localProtocolMethods.add(method.name);
        }
        continue;
      }
    }
  }

  /**
   * Register a protocol declaration in the protocol registry.
   * Also adds protocol methods to the global scope as polymorphic functions with constraints.
   */
  registerProtocol(decl: ProtocolDeclaration) {
    const { protocols, globalScope, substitution } = this;

    // Check for duplicate protocol name
    const existingProtocol = protocols[decl.name];
    if (existingProtocol) {
      // If it's from a different module, provide a more helpful error message
      if (
        existingProtocol.moduleName &&
        existingProtocol.moduleName !== this.getModuleName()
      ) {
        throw new SemanticError(
          `Protocol '${decl.name}' conflicts with protocol from module '${existingProtocol.moduleName}'. ` +
            `Consider using a different name or qualified imports.`,
          decl.span,
          this.getFilePath(),
        );
      }
      throw new SemanticError(
        `Duplicate protocol '${decl.name}'`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate type parameters are unique
    const paramSet = new Set<string>();
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(
          `Duplicate type parameter '${param}' in protocol '${decl.name}'`,
          decl.span,
          this.getFilePath(),
        );
      }
      paramSet.add(param);
    }

    // Validate at least one method
    if (decl.methods.length === 0) {
      throw new SemanticError(
        `Protocol '${decl.name}' must have at least one method`,
        decl.span,
        this.getFilePath(),
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
          this.getFilePath(),
        );
      }
      methodNames.add(method.name);

      let methodType: Type;

      if (method.type) {
        // Has explicit type annotation - convert from AST to internal representation
        // Use the shared type variable context so all methods use same var IDs
        methodType = this.typeFromAnnotation(method.type, sharedTypeVarCtx);
      } else if (method.defaultImpl) {
        // No explicit type annotation, but has default implementation
        // Infer the type from the default implementation by analyzing it as a lambda
        const lambdaExpr = makeLambda(
          method.defaultImpl.args,
          method.defaultImpl.body,
          method.span,
        );

        // Create a temporary scope for type inference
        const tempScope = this.createChildScope();

        // Infer the type of the lambda
        const inferredType = this.analyzeExpr(lambdaExpr, {
          scope: tempScope,
          substitution,
        });

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
          this.getFilePath(),
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
              this.typeFromAnnotation(ta, sharedTypeVarCtx),
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
        this.typeFromAnnotation(ta, sharedTypeVarCtx),
      ),
    }));

    // Register the protocol
    protocols[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      superclassConstraints,
      methods,
      span: decl.span,
    };
  }

  private registerImplementationDeclarations(): void {
    for (const decl of this.program.declarations) {
      if (decl.kind === "ImplementationDeclaration") {
        this.registerImplementation(decl);
        continue;
      }
    }
  }

  /**
   * Register an implementation declaration in the instance registry.
   */
  registerImplementation(decl: ImplementationDeclaration) {
    // Check that the protocol exists
    const protocol = this.protocols[decl.protocolName];
    if (!protocol) {
      throw new SemanticError(
        `Unknown protocol '${decl.protocolName}'`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate number of type arguments matches protocol parameters
    if (decl.typeArgs.length !== protocol.params.length) {
      throw new SemanticError(
        `Protocol '${decl.protocolName}' expects ${protocol.params.length} type argument(s), but got ${decl.typeArgs.length}`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Create a type variable context for converting type expressions
    const typeVarCtx = new Map<string, TypeVar>();

    // Convert type arguments from AST to internal representation
    const typeArgs: Type[] = [];
    for (const typeArg of decl.typeArgs) {
      typeArgs.push(this.typeFromAnnotation(typeArg, typeVarCtx));
    }

    // Convert constraints
    const constraints: Constraint[] = [];
    for (const astConstraint of decl.constraints) {
      // Validate that the constraint protocol exists
      if (!this.protocols[astConstraint.protocolName]) {
        throw new SemanticError(
          `Unknown protocol '${astConstraint.protocolName}' in constraint`,
          decl.span,
          this.getFilePath(),
        );
      }
      const constraintTypeArgs: Type[] = [];
      for (const typeArg of astConstraint.typeArgs) {
        constraintTypeArgs.push(this.typeFromAnnotation(typeArg, typeVarCtx));
      }
      constraints.push({
        protocolName: astConstraint.protocolName,
        typeArgs: constraintTypeArgs,
      });
    }

    // Validate that all required methods (those without defaults) are implemented
    const implementedMethods = new Set(decl.methods.map((m) => m.name));
    const allMethods = new Set(protocol.methods.keys());

    // Check for auto-derivation request: implement Eq/Show with no methods
    if (
      decl.methods.length === 0 &&
      (decl.protocolName === "Eq" || decl.protocolName === "Show")
    ) {
      // Ensure we have a valid type arg
      if (typeArgs.length === 1 && typeArgs[0]!.kind === "con") {
        const typeName = (typeArgs[0] as TypeCon).name;
        // Find declaration
        const typeDecl = this.getDeclarations().find(
          (d) =>
            (d.kind === "TypeDeclaration" ||
              d.kind === "TypeAliasDeclaration") &&
            d.name === typeName,
        );

        if (typeDecl && typeDecl.kind === "TypeDeclaration") {
          // Generate synthetic instance
          let synthetic: InstanceInfo | undefined;
          if (decl.protocolName === "Eq") {
            synthetic = this.generateEqImplementation(typeDecl, protocol);
          } else {
            synthetic = this.generateShowImplementation(typeDecl);
          }

          // Populate implemented methods from synthetic instance
          if (synthetic) {
            // Add synthetic methods to our declaration
            synthetic.methods.forEach((impl, name) => {
              const methodInfo = protocol.methods.get(name)!;
              // We need to convert from Instance method format back to AST MethodImplementation format?
              // No, registerImplementation uses AST decl.methods to build the instance methods.
              // So we need to push to decl.methods?
              // BUT, we are iterating over decl.methods later.

              // Instead of modifying decl.methods (which is AST), maybe we should just populate the instance directly?
              // But registerImplementation validates that all methods are present in decl.methods.

              // Let's cheat: we already know it's empty. We just need to satisfy the checks.
              // Or better: just use the synthetic instance directly?
              // We can't return early because we need to register global instances etc.

              // Let's push to decl.methods.
              // But MethodImplementation needs AST nodes (Expression). Synthetic outputs runtime function lambdas?
              // Wait, generateEqImplementation returns InstanceInfo which has `methods: Map<string, Expr>`.
              // `Expr` IS the AST node for expression.

              decl.methods.push({
                name: name,
                implementation: impl,
                span: decl.span, // Synthetic span
              });

              // Also update implementedMethods set so validation passes
              implementedMethods.add(name);
            });
          }
        }
      }
    }

    // Check that methods without defaults are implemented
    for (const methodName of allMethods) {
      const methodInfo = protocol.methods.get(methodName)!;
      if (!implementedMethods.has(methodName) && !methodInfo.defaultImpl) {
        throw new SemanticError(
          `Instance is missing implementation for method '${methodName}'`,
          decl.span,
          this.getFilePath(),
        );
      }
    }

    // Check for extra methods that aren't in the protocol
    for (const implemented of implementedMethods) {
      if (!allMethods.has(implemented)) {
        throw new SemanticError(
          `Method '${implemented}' is not part of protocol '${decl.protocolName}'`,
          decl.span,
          this.getFilePath(),
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
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods,
      span: decl.span,
    };

    // Check for overlapping instances
    // An implementation overlaps if another implementation exists for the same protocol and type
    // AND their constraints don't prevent overlap
    for (const existing of this.instances) {
      if (existing.protocolName !== decl.protocolName) continue;

      // Check if instances overlap, considering constraints
      if (instancesOverlap(existing, instanceInfo, this.instances)) {
        throw new SemanticError(
          `Overlapping implementation for protocol '${decl.protocolName}'`,
          decl.span,
          this.getFilePath(),
        );
      }
    }

    // Register the instance in both the global list and local list
    // The global list includes imported instances for overlap checking
    // The local list is used for validation (only validate methods in this module)
    this.instances.push(instanceInfo);
    this.localInstances.push(instanceInfo);
  }

  generateShowImplementation(decl: TypeDeclaration): InstanceInfo {
    // 1. Build type arguments for the instance
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

    // Re-map params
    const paramMap = new Map<string, TypeVar>();
    const headType = typeArgs[0] as TypeCon;
    decl.params.forEach((p, i) => {
      paramMap.set(p, headType.args[i] as TypeVar);
    });

    // 2. Generate constraints
    const constraints: Constraint[] = decl.params.map((p) => ({
      protocolName: "Show",
      typeArgs: [paramMap.get(p)!],
    }));

    // 3. Generate toString implementation
    const methods = new Map<string, Expr>();
    const span = decl.span;
    let body: Expr;

    const str = (s: string): Expr => ({
      kind: "String",
      value: `"${s}"`,
      span,
    });
    const append = (a: Expr, b: Expr): Expr => ({
      kind: "Infix",
      left: a,
      operator: "++",
      right: b,
      span,
    });
    const toStringWithUsage = (val: Expr, valType: Type): Expr => {
      const callee: Expr = {
        kind: "Var",
        name: "toString",
        namespace: "lower",
        span,
      };
      const node: Expr = {
        kind: "Apply",
        callee,
        args: [val],
        span,
      };
      this._resolvedProtocolUsages.set(callee, {
        protocolName: "Show",
        typeArgs: [valType],
      });
      return node;
    };

    if (decl.recordFields) {
      let expr: Expr = str(`${decl.name} { `);

      decl.recordFields.forEach((field, i) => {
        if (i > 0) expr = append(expr, str(", "));

        expr = append(expr, str(`${field.name} = `));

        const fieldAccess: Expr = {
          kind: "FieldAccess",
          target: { kind: "Var", name: "x_impl", namespace: "lower", span },
          field: field.name,
          span,
        };
        const fieldType = this.typeFromAnnotation(field.type, paramMap);
        this.ensureNestedTupleInstances("Show", fieldType);
        expr = append(expr, toStringWithUsage(fieldAccess, fieldType));
      });

      expr = append(expr, str(" }"));
      body = expr;
    } else if (decl.constructors) {
      const branches = decl.constructors.map((ctor) => {
        const args = ctor.args.map((_, i) => `a${i}`);
        const pattern: Pattern = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: args.map((a) => ({ kind: "VarPattern", name: a, span })),
          span,
        };

        let expr: Expr;
        if (args.length === 0) {
          expr = str(`${ctor.name}`);
        } else {
          expr = str(`${ctor.name}(`);
          args.forEach((a, i) => {
            if (i > 0) expr = append(expr, str(", "));
            const argType = this.typeFromAnnotation(ctor.args[i]!, paramMap);
            this.ensureNestedTupleInstances("Show", argType);
            expr = append(
              expr,
              toStringWithUsage(
                { kind: "Var", name: a, namespace: "lower", span },
                argType,
              ),
            );
          });
          expr = append(expr, str(")"));
        }

        return { pattern, body: expr, span };
      });

      body = {
        kind: "Case",
        discriminant: { kind: "Var", name: "x_impl", namespace: "lower", span },
        branches,
        span,
      };
    } else {
      // Should not happen for valid types? Opaque?
      body = str(`${decl.name}`);
    }

    methods.set("toString", {
      kind: "Lambda",
      args: [{ kind: "VarPattern", name: "x_impl", span }],
      body,
      span,
    });

    return {
      protocolName: "Show",
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods: new Set(["toString"]),
      span,
    };
  }

  /**
   * Walk a type and synthesize Eq/Show instances for any nested tuple types.
   * This is needed during auto-derive because the generated method bodies
   * reference inner Eq/Show constraints that require concrete tuple instances.
   */
  private ensureNestedTupleInstances(protocolName: string, type: Type): void {
    if (type.kind === "tuple") {
      for (const elem of type.elements) {
        this.ensureNestedTupleInstances(protocolName, elem);
      }
      if (!findInstanceForTypeInternal(protocolName, type, this.instances)) {
        // Directly generate the synthetic instance instead of using trySynthesizeInstance,
        // because during auto-derive the parent type's instance hasn't been pushed yet,
        // so element validation would fail for self-referential types.
        const instance = generateSyntheticInstance(
          protocolName,
          type,
          this._resolvedProtocolUsages,
        );
        this.instances.push(instance);
      }
    } else if (type.kind === "con") {
      for (const arg of (type as TypeCon).args) {
        this.ensureNestedTupleInstances(protocolName, arg);
      }
    }
  }

  generateEqImplementation(
    decl: TypeDeclaration,
    protocol: ProtocolInfo,
  ): InstanceInfo {
    // 0. Validate fields implement Eq
    const typeParams = new Set(decl.params);

    if (decl.recordFields) {
      for (const field of decl.recordFields) {
        this.validateTypeImplementsEq(
          field.type,
          decl.span,
          typeParams,
          new Set(),
          decl.name,
        );
      }
    }

    if (decl.constructors) {
      for (const ctor of decl.constructors) {
        for (const arg of ctor.args) {
          this.validateTypeImplementsEq(
            arg,
            decl.span,
            typeParams,
            new Set(),
            decl.name,
          );
        }
      }
    }

    // 1. Build type arguments for the instance
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

    // Re-map params
    const paramMap = new Map<string, TypeVar>();
    const headType = typeArgs[0];
    if (headType && headType.kind === "con") {
      const typeCon = headType as TypeCon;
      decl.params.forEach((p, i) => {
        paramMap.set(p, typeCon.args[i] as TypeVar);
      });
    }

    // 2. Generate constraints
    const constraints: Constraint[] = decl.params.map((p) => {
      const tvar = paramMap.get(p);
      if (!tvar) throw new Error("Type variable missing");
      return {
        protocolName: "Eq",
        typeArgs: [tvar],
      };
    });

    // 3. Generate (==) implementation
    const methods = new Map<string, Expr>();
    const xVar = "x_impl";
    const yVar = "y_impl";
    const span = decl.span;
    let body: Expr;

    if (decl.recordFields) {
      const checks: Expr[] = decl.recordFields.map((field) => {
        const node: Expr = {
          kind: "Infix",
          left: {
            kind: "FieldAccess",
            target: { kind: "Var", name: xVar, namespace: "lower", span },
            field: field.name,
            span,
          },
          operator: "==",
          right: {
            kind: "FieldAccess",
            target: { kind: "Var", name: yVar, namespace: "lower", span },
            field: field.name,
            span,
          },
          span,
        };
        const fieldType = this.typeFromAnnotation(field.type, paramMap);
        this.ensureNestedTupleInstances("Eq", fieldType);
        this._resolvedProtocolUsages.set(node, {
          protocolName: "Eq",
          typeArgs: [fieldType],
        });
        return node;
      });

      if (checks.length === 0) {
        body = { kind: "Var", name: "True", namespace: "upper", span };
      } else {
        body = checks.reduce((acc, check) => ({
          kind: "Infix",
          left: acc,
          operator: "&&",
          right: check,
          span,
        }));
      }
    } else if (decl.constructors) {
      const branches: { pattern: Pattern; body: Expr; span: Span }[] = [];
      const hasMultipleConstructors = decl.constructors.length > 1;

      for (const ctor of decl.constructors) {
        const argsX: Pattern[] = ctor.args.map((_, i) => ({
          kind: "VarPattern",
          name: `a_${i}`,
          span,
        }));
        const argsY: Pattern[] = ctor.args.map((_, i) => ({
          kind: "VarPattern",
          name: `b_${i}`,
          span,
        }));

        const patX: Pattern = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: argsX,
          span,
        };
        const patY: Pattern = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: argsY,
          span,
        };

        const pattern: Pattern = {
          kind: "TuplePattern",
          elements: [patX, patY],
          span,
        };

        const checks: Expr[] = ctor.args.map((arg, i) => {
          const node: Expr = {
            kind: "Infix",
            left: { kind: "Var", name: `a_${i}`, namespace: "lower", span },
            operator: "==",
            right: { kind: "Var", name: `b_${i}`, namespace: "lower", span },
            span,
          };
          const argType = this.typeFromAnnotation(arg, paramMap);
          this.ensureNestedTupleInstances("Eq", argType);
          this._resolvedProtocolUsages.set(node, {
            protocolName: "Eq",
            typeArgs: [argType],
          });
          return node;
        });

        let branchBody: Expr;
        if (checks.length === 0) {
          branchBody = { kind: "Var", name: "True", namespace: "upper", span };
        } else {
          branchBody = checks.reduce((acc, check) => ({
            kind: "Infix",
            left: acc,
            operator: "&&",
            right: check,
            operatorInfo: {
              precedence: 3, // Default for &&
              associativity: "right",
            },
            span,
          }));
        }

        branches.push({ pattern, body: branchBody, span });
      }

      if (hasMultipleConstructors || decl.constructors.length === 0) {
        branches.push({
          pattern: { kind: "WildcardPattern", span },
          body: { kind: "Var", name: "False", namespace: "upper", span },
          span,
        });
      }

      body = {
        kind: "Case",
        discriminant: {
          kind: "Tuple",
          elements: [
            { kind: "Var", name: xVar, namespace: "lower", span },
            { kind: "Var", name: yVar, namespace: "lower", span },
          ],
          span,
        },
        branches,
        span,
      };
    } else {
      body = { kind: "Var", name: "True", namespace: "upper", span };
    }

    methods.set("==", {
      kind: "Lambda",
      args: [
        { kind: "VarPattern", name: xVar, span },
        { kind: "VarPattern", name: yVar, span },
      ],
      body,
      span,
    });

    return {
      protocolName: protocol.name,
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods: new Set(["=="]),
      span: decl.span,
    };
  }

  /**
   * Process an 'implementing' clause on a type declaration.
   * Validates that all referenced protocols have all methods with defaults,
   * and creates synthetic implement blocks.
   */
  validateTypeImplementsEq(
    type: TypeExpr,
    declSpan: Span,
    typeParams: Set<string>,
    checkedTypes: Set<string>,
    derivingTypeName?: string,
  ): void {
    const typeKey = JSON.stringify(type);
    if (checkedTypes.has(typeKey)) return;
    checkedTypes.add(typeKey);

    switch (type.kind) {
      case "FunctionType":
        throw new SemanticError(
          `Type mismatch: cannot unify 'Int' with 'Int -> Int'. No instance of Eq for function type.`,
          declSpan,
          this.getFilePath(),
        );
      case "TypeRef": {
        const firstChar = type.name.charAt(0);
        const isTypeVariable =
          firstChar === firstChar.toLowerCase() &&
          firstChar !== firstChar.toUpperCase();

        if (isTypeVariable && typeParams.has(type.name)) return;

        // Self-reference: the type we're currently deriving Eq for
        if (derivingTypeName && type.name === derivingTypeName) return;

        // Convert TypeRef to Type for instance lookup
        const typeForLookup: Type = {
          kind: "con",
          name: type.name,
          args: [],
        };
        if (findInstanceForTypeInternal("Eq", typeForLookup, this.instances)) {
          return;
        }

        if (type.name === "List" && type.args.length === 1) {
          this.validateTypeImplementsEq(
            type.args[0]!,
            declSpan,
            typeParams,
            checkedTypes,
            derivingTypeName,
          );
          return;
        }

        // Check local type declarations
        const localDecl = this.getDeclarations().find(
          (d) =>
            (d.kind === "TypeDeclaration" ||
              d.kind === "TypeAliasDeclaration") &&
            d.name === type.name,
        );

        if (localDecl) {
          if (localDecl.kind === "TypeDeclaration") {
            const hasExplicit = this.getDeclarations().some(
              (d) =>
                d.kind === "ImplementationDeclaration" &&
                d.protocolName === "Eq" &&
                d.typeArgs.length > 0 &&
                d.typeArgs[0]!.kind === "TypeRef" &&
                (d.typeArgs[0]! as any).name === type.name,
            );

            if (hasExplicit) return;

            throw new SemanticError(
              `Type '${type.name}' does not implement 'Eq'. Implicit 'Eq' requires all fields to implement 'Eq'.`,
              declSpan,
              this.getFilePath(),
            );
          }
        }

        type.args.forEach((arg) =>
          this.validateTypeImplementsEq(
            arg,
            declSpan,
            typeParams,
            checkedTypes,
            derivingTypeName,
          ),
        );
        return;
      }

      case "TupleType":
        type.elements.forEach((elem) =>
          this.validateTypeImplementsEq(
            elem,
            declSpan,
            typeParams,
            checkedTypes,
            derivingTypeName,
          ),
        );
        return;
      case "RecordType":
        type.fields.forEach((f) =>
          this.validateTypeImplementsEq(
            f.type,
            declSpan,
            typeParams,
            checkedTypes,
            derivingTypeName,
          ),
        );
        return;
      case "QualifiedType":
        this.validateTypeImplementsEq(
          type.type,
          declSpan,
          typeParams,
          checkedTypes,
          derivingTypeName,
        );
        return;
      default:
        return;
    }
  }

  private autoImplementProtocols(): void {
    for (const decl of this.getDeclarations()) {
      if (decl.kind === "TypeDeclaration") {
        const eqInstance = this.autoImplementProtocolForType("Eq", decl);
        if (eqInstance) {
          this.instances.push(eqInstance);
          this.localInstances.push(eqInstance);
        }

        const showInstance = this.autoImplementProtocolForType("Show", decl);
        if (showInstance) {
          this.instances.push(showInstance);
          this.localInstances.push(showInstance);
        }
      }
    }
  }

  /**
   * Automatically implement Eq for a type declaration if possible.
   * Returns the generated instance, or undefined if Eq cannot be implemented.
   * Only auto-implements for the Vibe/Vibe.Basics Eq protocol, not custom Eq protocols.
   */

  autoImplementProtocolForType(
    protocolName: string,
    decl: TypeDeclaration,
  ): InstanceInfo | undefined {
    // Find the protocol
    const protocol = this.protocols[protocolName];
    if (!protocol) return undefined;

    // Only auto-implement for Vibe/Vibe.Basics protocols
    const isVibe =
      protocol.moduleName === "Vibe" || protocol.moduleName === "Vibe.Basics";
    if (!isVibe) return undefined;

    // Check if type can implement protocol
    if (!this.canDeclImplementProtocol(decl, protocolName)) {
      return undefined;
    }

    // Check for existing explicit implementation
    for (const existing of this.instances) {
      if (existing.protocolName !== protocolName) continue;
      if (existing.typeArgs.length === 0) continue;
      const typeArg = existing.typeArgs[0];
      if (typeArg?.kind === "con" && (typeArg as TypeCon).name === decl.name) {
        // Already has an implementation
        return undefined;
      }
    }

    if (protocolName === "Eq") {
      return this.generateEqImplementation(decl, protocol);
    } else if (protocolName === "Show") {
      return this.generateShowImplementation(decl);
    }

    return undefined;
  }

  private registerValueDeclarations(): void {
    for (const decl of this.getDeclarations()) {
      if (
        decl.kind === "ValueDeclaration" ||
        decl.kind === "DecoratedDeclaration"
      ) {
        // Validate DecoratedDeclaration before registering
        if (decl.kind === "DecoratedDeclaration") {
          this.validateDecoratedDeclaration(decl);
        }
        this.registerValue(decl);
        continue;
      }

      if (decl.kind === "TypeAnnotationDeclaration") {
        // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
        if (Object.hasOwn(this.annotations, decl.name)) {
          throw new SemanticError(
            `Duplicate type annotation for '${decl.name}'`,
            decl.span,
            this.getFilePath(),
          );
        }
        this.annotations[decl.name] = decl;
      }
    }
  }

  /**
   * Check if a TypeDeclaration can automatically implement a protocol.
   * Returns true if all fields/constructor args can implement the protocol.
   */
  canDeclImplementProtocol(
    decl: TypeDeclaration,
    protocolName: string,
  ): boolean {
    const typeParams = new Set(decl.params);

    if (decl.recordFields) {
      for (const field of decl.recordFields) {
        if (
          !this.canTypeImplementProtocol(field.type, protocolName, typeParams)
        ) {
          return false;
        }
      }
    }

    if (decl.constructors) {
      for (const ctor of decl.constructors) {
        for (const arg of ctor.args) {
          if (!this.canTypeImplementProtocol(arg, protocolName, typeParams)) {
            return false;
          }
        }
      }
    }

    return true;
  }

  /**
   * Check if a type expression can implement a protocol.
   *
   * This function determines whether a type can satisfy a protocol constraint either through:
   * 1. An explicit instance implementation
   * 2. Auto-derivation (currently only supported for standard Eq from Vibe/Vibe.Basics)
   * 3. Protocol with all default method implementations
   */
  canTypeImplementProtocol(
    type: TypeExpr,
    protocolName: string,
    typeParams: Set<string>,
    checkedTypes: Set<string> = new Set(),
  ): boolean {
    const typeKey = JSON.stringify(type);
    if (checkedTypes.has(typeKey)) return true;
    checkedTypes.add(typeKey);

    // Helper to check if a type matches an instance's type argument
    const typeMatchesInstanceArg = (
      instanceArg: Type,
      typeName: string,
    ): boolean => {
      return (
        instanceArg.kind === "con" && (instanceArg as TypeCon).name === typeName
      );
    };

    switch (type.kind) {
      case "FunctionType":
        // Functions can only implement protocols if there's an explicit instance
        // Check if there's an instance for function types
        const hasFunctionInstance = this.instances.some(
          (inst) =>
            inst.protocolName === protocolName &&
            inst.typeArgs.length > 0 &&
            inst.typeArgs[0]!.kind === "fun",
        );
        return hasFunctionInstance;

      case "TypeRef": {
        const firstChar = type.name.charAt(0);
        // Type variables (lowercase) are allowed - they get protocol constraint
        if (
          firstChar === firstChar.toLowerCase() &&
          firstChar !== firstChar.toUpperCase()
        ) {
          return true;
        }

        // Global Check: Check if there's already an instance for this type
        const hasInstance = this.instances.some(
          (inst) =>
            inst.protocolName === protocolName &&
            inst.typeArgs.length > 0 &&
            typeMatchesInstanceArg(inst.typeArgs[0]!, type.name),
        );
        if (hasInstance) {
          // Also check type arguments recursively
          return type.args.every((arg) =>
            this.canTypeImplementProtocol(
              arg,
              protocolName,
              typeParams,
              checkedTypes,
            ),
          );
        }

        // Auto-Derive Check: Determine if we can auto-derive this protocol
        const protocol = this.protocols[protocolName];
        if (!protocol) {
          // Protocol not found - can't implement
          return false;
        }

        // Check if all methods have default implementations
        const allMethodsHaveDefaults = Array.from(
          protocol.methods.values(),
        ).every((method) => method.defaultImpl !== undefined);

        // If all methods have defaults, any type can satisfy the protocol
        if (allMethodsHaveDefaults) {
          return true;
        }

        // Check if type is structural and all fields can implement Eq
        // Check local type declarations
        const localDecl = this.getDeclarations().find(
          (d) =>
            (d.kind === "TypeDeclaration" ||
              d.kind === "TypeAliasDeclaration") &&
            d.name === type.name,
        );

        if (localDecl && localDecl.kind === "TypeDeclaration") {
          // Local type will get auto-Eq, check its type args
          return type.args.every((arg) =>
            this.canTypeImplementProtocol(
              arg,
              protocolName,
              typeParams,
              checkedTypes,
            ),
          );
        }

        // For type aliases, check type args
        if (localDecl && localDecl.kind === "TypeAliasDeclaration") {
          return type.args.every((arg) =>
            this.canTypeImplementProtocol(
              arg,
              protocolName,
              typeParams,
              checkedTypes,
            ),
          );
        }

        // Concrete type with no instance, no local declaration, and protocol
        // without all-default methods — can't auto-derive.
        return false;
      }

      case "TupleType":
        // Tuples can implement protocols if all elements can
        return type.elements.every((elem) =>
          this.canTypeImplementProtocol(
            elem,
            protocolName,
            typeParams,
            checkedTypes,
          ),
        );

      case "RecordType":
        // Records can implement protocols if all fields can
        return type.fields.every((f) =>
          this.canTypeImplementProtocol(
            f.type,
            protocolName,
            typeParams,
            checkedTypes,
          ),
        );

      case "QualifiedType":
        return this.canTypeImplementProtocol(
          type.type,
          protocolName,
          typeParams,
          checkedTypes,
        );

      default:
        return false;
    }
  }

  /**
   * Validates that each infix declaration has a corresponding LOCAL function definition.
   *
   * Fixity is an intrinsic property of an operator - it travels with the operator wherever
   * it goes. You cannot separate an operator from its fixity. This means:
   *
   * 1. An infix declaration must define a NEW operator in the current module
   * 2. The operator must have a local definition (not imported)
   * 3. You cannot redefine the fixity of an imported operator
   *
   * Valid local definitions include:
   * - Local value declarations (e.g., `(+) x y = ...`)
   * - External declarations (e.g., `@external ... (+) : ...`)
   * - Protocol methods defined in THIS module
   *
   * Note: Imported operator check is done earlier in registerInfixDeclaration.
   */
  validateInfixDeclarationsHaveDefinitions(): void {
    for (const decl of this.infixDeclarations) {
      const op = decl.operator;

      // Check if operator is defined locally:
      // 1. As a local value/function
      const hasLocalDefinition = Object.hasOwn(this.values, op);
      // 2. As a local protocol method
      const isLocalProtocolMethod = this.localProtocolMethods.has(op);

      if (!hasLocalDefinition && !isLocalProtocolMethod) {
        throw new SemanticError(
          `Infix declaration for operator '${op}' has no corresponding function definition. ` +
            `Define the operator in this module or remove the fixity declaration.`,
          decl.span,
          this.getFilePath(),
        );
      }
    }
  }

  /**
   * Validate a DecoratedDeclaration has correct decorator and argument count.
   * This is where we enforce decorator semantics that were deferred from the parser.
   */
  private validateDecoratedDeclaration(decl: DecoratedDeclaration): void {
    const validDecorators = ["external", "get", "call", "val", "import"];
    if (!validDecorators.includes(decl.decorator)) {
      throw new SemanticError(
        `Unknown decorator '@${decl.decorator}'. Valid decorators are: @external, @get, @call, @val, @import`,
        decl.span,
        this.getFilePath(),
      );
    }

    // Validate argument counts for each decorator type
    switch (decl.decorator) {
      case "external":
        if (decl.args.length !== 2) {
          throw new SemanticError(
            `@external requires exactly 2 string arguments (module path and export name), got ${decl.args.length}`,
            decl.span,
            this.getFilePath(),
          );
        }
        break;
      case "get":
      case "call":
      case "val":
        if (decl.args.length !== 1) {
          throw new SemanticError(
            `@${decl.decorator} requires exactly 1 string argument (property key), got ${decl.args.length}`,
            decl.span,
            this.getFilePath(),
          );
        }
        break;
      case "import":
        if (decl.args.length !== 1) {
          throw new SemanticError(
            `@import requires exactly 1 string argument (module path), got ${decl.args.length}`,
            decl.span,
            this.getFilePath(),
          );
        }
        break;
    }
  }

  /**
   * Check if a declaration is an FFI-style declaration (no Vibe body to infer from).
   */
  private isFFIDeclaration(
    decl: ValueDeclaration | DecoratedDeclaration,
  ): boolean {
    return decl.kind === "DecoratedDeclaration";
  }

  /**
   * Validate @get/@call type structure for DecoratedDeclaration.
   */
  private validateDecoratedPropertyType(
    decl: DecoratedDeclaration,
    type: Type,
  ): void {
    const variant = decl.decorator; // "get", "call", or "val"

    // @val has no structural constraints on the type
    if (variant === "val") {
      return;
    }

    const params = flattenFunctionParams(type);

    if (variant === "get") {
      if (params.length !== 1) {
        throw new SemanticError(
          `@get declaration '${decl.name}' must have type A -> B (exactly one argument), got ${params.length} argument(s)`,
          decl.span,
          this.getFilePath(),
        );
      }
    } else if (variant === "call") {
      if (params.length < 1) {
        throw new SemanticError(
          `@call declaration '${decl.name}' must have at least one argument`,
          decl.span,
          this.getFilePath(),
        );
      }
    }
  }

  private validateAnnotationsAndSeedGlobalNames(): void {
    // ===== PASS 2a: Validate infix declarations have definitions =====
    this.validateInfixDeclarationsHaveDefinitions();

    for (const [name, ann] of Object.entries(this.annotations)) {
      if (!Object.hasOwn(this.values, name)) {
        throw new SemanticError(
          `Type annotation for '${name}' has no matching definition`,
          ann.span,
          this.getFilePath(),
        );
      }
      const value = this.values[name]!;
      if (value.declaration.kind === "DecoratedDeclaration") {
        throw new SemanticError(
          `Decorated declaration '${name}' already includes a type annotation`,
          ann.span,
          this.getFilePath(),
        );
      }
      value.annotation = ann.annotation;
    }

    // Seed global names to enable recursion.
    for (const [name, info] of Object.entries(this.values)) {
      const annotationExpr =
        info.annotation ??
        (info.declaration.kind === "DecoratedDeclaration"
          ? info.declaration.annotation
          : undefined);

      let annotationType: Type | undefined;
      let annotatedConstraints: Constraint[] | undefined;

      if (annotationExpr) {
        const typeVars = collectTypeVariables(annotationExpr);
        const validationErrors = validateTypeExpr(
          this,
          annotationExpr,
          typeVars,
          info.declaration.span,
        );

        if (validationErrors.length > 0) {
          const err = validationErrors[0]!;
          const message = err.suggestion
            ? `${err.message}. ${err.suggestion}`
            : err.message;
          throw new SemanticError(message, err.span, this.getFilePath());
        }

        const result = this.typeFromAnnotationWithConstraints(
          annotationExpr,
          new Map(),
        );
        annotationType = result.type;
        annotatedConstraints =
          result.constraints.length > 0 ? result.constraints : undefined;

        if (annotatedConstraints) {
          info.annotatedConstraints = annotatedConstraints;
        }
        if (result.paramNames.size > 0) {
          info.annotatedParamNames = result.paramNames;
        }
      }

      const seeded = annotationType ?? this.seedValueType(info.declaration);
      this.declareSymbol(
        this.globalScope,
        name,
        { vars: new Set(), constraints: [], type: seeded },
        info.declaration.span,
      );
      this.types[name] = seeded;
    }
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
   */
  typeFromAnnotationWithConstraints(
    annotation: TypeExpr,
    context: TypeVarContext = new Map(),
  ): AnnotationResult {
    // Access registries from analyzer
    const { protocols } = this;

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
            this.getFilePath(),
          );
        }

        // Validate the number of type arguments matches protocol parameters
        if (astConstraint.typeArgs.length !== protocol.params.length) {
          throw new SemanticError(
            `Protocol '${astConstraint.protocolName}' expects ${protocol.params.length} type argument(s), but constraint has ${astConstraint.typeArgs.length}`,
            astConstraint.span,
            this.getFilePath(),
          );
        }

        // Convert constraint type arguments
        const constraintTypeArgs: Type[] = [];
        for (const typeArg of astConstraint.typeArgs) {
          constraintTypeArgs.push(this.typeFromAnnotation(typeArg, context));
        }

        // Validate that constraint type arguments are type variables
        // (Constraints on concrete types like `Num Int` don't make sense in annotations)
        for (let i = 0; i < constraintTypeArgs.length; i++) {
          const typeArg = constraintTypeArgs[i]!;
          if (typeArg.kind !== "var") {
            throw new SemanticError(
              `Constraint '${astConstraint.protocolName}' must be applied to type variables, not concrete types`,
              astConstraint.span,
              this.getFilePath(),
            );
          }
        }

        constraints.push({
          protocolName: astConstraint.protocolName,
          typeArgs: constraintTypeArgs,
        });
      }

      // Convert the underlying type (recursively handling nested QualifiedTypes)
      const innerResult = this.typeFromAnnotationWithConstraints(
        annotation.type,
        context,
      );

      // Merge constraints from nested qualified types
      const paramNames = invertContext(context);
      return {
        type: innerResult.type,
        constraints: [...constraints, ...innerResult.constraints],
        paramNames,
      };
    }

    // For non-qualified types, delegate to the simpler function
    const type = this.typeFromAnnotation(annotation, context);
    const paramNames = invertContext(context);
    return { type, constraints: [], paramNames };
  }

  private inferValueDeclarations(): void {
    // ===== PASS 2.0b: Concretize polymorphic instance type args =====
    this.concretizeInstanceTypeArgs();

    // Build dependency graph and compute SCCs for proper mutual recursion handling.
    const depGraph = buildDependencyGraph(this.values);
    const sccs = computeSCCs(depGraph);

    // Process each SCC
    for (const scc of sccs) {
      const externals: string[] = [];
      const valueDecls: string[] = [];

      for (const name of scc) {
        const info = this.values[name]!;
        if (this.isFFIDeclaration(info.declaration)) {
          externals.push(name);
        } else {
          valueDecls.push(name);
        }
      }

      // Process externals, property, and imported value declarations first
      for (const name of externals) {
        const info = this.values[name]!;
        const decl = info.declaration;
        if (decl.kind === "DecoratedDeclaration") {
          const result = this.typeFromAnnotationWithConstraints(
            decl.annotation,
            new Map(),
          );
          info.type = result.type;

          if (result.constraints.length > 0) {
            info.annotatedConstraints = result.constraints;
          }

          // Validate @get/@call declarations
          if (decl.decorator === "get" || decl.decorator === "call") {
            this.validateDecoratedPropertyType(decl, info.type);
          }

          // Quantify all free type variables so the scheme can be properly
          // instantiated with fresh variables at each use site
          const freeVars = getFreeTypeVars(info.type, this.substitution);
          const scheme: TypeScheme = {
            vars: freeVars,
            constraints: result.constraints,
            type: info.type,
            paramNames:
              result.paramNames.size > 0 ? result.paramNames : undefined,
          };
          this.globalScope.symbols.set(info.declaration.name, scheme);
          this.typeSchemes[name] = scheme;
        }
      }

      // Process value declarations in this SCC together
      if (valueDecls.length > 0) {
        this.resetConstraintContext();

        const inferredTypes: Map<string, Type> = new Map();

        for (const name of valueDecls) {
          const info = this.values[name]!;
          if (info.declaration.kind !== "ValueDeclaration") continue;

          const declaredType = this.types[name]!;
          const annotationType = info.annotation
            ? this.typeFromAnnotation(info.annotation, new Map())
            : undefined;

          const inferred = this.analyzeValueDeclaration(
            info.declaration,
            this.globalScope,
            this.substitution,
            declaredType,
            annotationType,
          );

          inferredTypes.set(name, inferred);
          this.types[name] = inferred;
          info.type = inferred;
        }

        // After inferring all in the SCC, generalize all together
        for (const name of valueDecls) {
          const info = this.values[name]!;
          const inferred = inferredTypes.get(name)!;
          const generalizedScheme = this.generalizeWithAnnotatedConstraints(
            inferred,
            new Scope(),
            this.substitution,
            info.annotatedConstraints,
            info.declaration.span,
          );
          if (info.annotatedParamNames) {
            generalizedScheme.paramNames = resolveParamNames(
              info.annotatedParamNames,
              this.substitution,
            );
          }
          this.globalScope.define(name, generalizedScheme);
          this.typeSchemes[name] = generalizedScheme;
        }

        // Resolve pending protocol method usages with the final substitution.
        // This captures concrete constraint types (e.g., Show (Ref Int)) that
        // are dropped during generalization but needed for dictionary resolution.
        for (const usage of this._pendingProtocolUsages) {
          const resolvedArgs = usage.constraint.typeArgs.map((t) =>
            applySubstitution(t, this.substitution),
          );
          this._resolvedProtocolUsages.set(usage.node, {
            protocolName: usage.constraint.protocolName,
            typeArgs: resolvedArgs,
          });
        }

        // Resolve pending constrained call usages (e.g., Dict.insert with Ord k)
        for (const usage of this._pendingConstrainedCallUsages) {
          const resolved = usage.constraints.map((c) => ({
            protocolName: c.protocolName,
            typeArgs: c.typeArgs.map((t) =>
              applySubstitution(t, this.substitution),
            ),
          }));
          this._resolvedConstrainedCallUsages.set(usage.node, resolved);
        }
      }
    }
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
  generalizeWithAnnotatedConstraints(
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
    const rawConstraints = this.getCollectedConstraints();
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
        this.instances,
      );

      if (!lookupResult.found) {
        // Build a helpful error message
        const typeArgsStr = resolvedTypeArgs
          .map((t) => formatType(t))
          .join(", ");

        if (lookupResult.reason === "unsatisfied-constraint") {
          throw new SemanticError(
            `No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` +
              `The instance requires '${lookupResult.constraint}' for '${lookupResult.forType}', ` +
              `but no such instance exists.`,
            span,
            this.getFilePath(),
          );
        } else {
          throw new SemanticError(
            `No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` +
              `Add an implementation: implement ${c.protocolName} ${typeArgsStr} where ...`,
            span,
            this.getFilePath(),
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
      const hasUnquantifiedVar = typeArgFreeVars.some(
        (v) => !quantified.has(v),
      );

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
          this.getFilePath(),
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
              this.getFilePath(),
            );
          }
          // If the constraint references a type variable not in the function's type,
          // that's likely an error
          if (!hasQuantifiedVar) {
            throw new SemanticError(
              `Constraint '${c.protocolName}' references type variables not used in the function type`,
              span,
              this.getFilePath(),
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
        const matchingInstances = this.instances.filter((inst) => {
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
        const constraintFreeVars = getFreeTypeVars(
          resolvedTypeArg,
          substitution,
        );
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
              this.getFilePath(),
            );
          }
        }
      }
    }

    return { vars: quantified, constraints: finalConstraints, type: finalType };
  }

  private validateImplementationMethodExpressions(): void {
    for (const instance of this.localInstances) {
      // Only validate methods that were explicitly provided in the implement block
      // Default implementations from protocols are validated during protocol registration
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        if (methodExpr) {
          this.validateExpressionIdentifiers(
            methodExpr,
            this.globalScope,
            instance.protocolName,
            methodName,
          );
        }
      }
    }
  }

  /**
   * Recursively validate that all identifier references in an expression exist
   * in the given scope. Throws SemanticError for undefined identifiers.
   */
  validateExpressionIdentifiers(
    expr: Expr,
    scope: Scope,
    protocolName: string,
    methodName: string,
  ): void {
    // Helper for recursive calls
    const validate = (e: Expr, s: Scope = scope) =>
      this.validateExpressionIdentifiers(e, s, protocolName, methodName);

    switch (expr.kind) {
      case "Var": {
        const name = expr.name;
        // Check if it's a constructor (constructors are always valid)
        if (this.constructors[name]) {
          return;
        }
        // Check if it's defined in scope
        if (!symbolExists(scope, name)) {
          this.addError(
            `Undefined name '${name}' in implementation of '${methodName}' for protocol '${protocolName}'`,
            expr.span,
          );
        }
        return;
      }
      case "Lambda": {
        // Create a child scope with the lambda arguments
        const childScope = new Scope(scope);
        for (const arg of expr.args) {
          bindPatternNames(arg, childScope);
        }
        validate(expr.body, childScope);
        return;
      }
      case "Apply": {
        validate(expr.callee);
        for (const arg of expr.args) {
          validate(arg);
        }
        return;
      }
      case "If": {
        validate(expr.condition);
        validate(expr.thenBranch);
        validate(expr.elseBranch);
        return;
      }
      case "LetIn": {
        // Create a child scope for let bindings
        const childScope = new Scope(scope);
        for (const binding of expr.bindings) {
          // First validate the binding body in the parent scope
          validate(binding.body);
          // Then add the binding name to the child scope
          childScope.symbols.set(binding.name, {
            vars: new Set(),
            constraints: [],
            type: { kind: "var", id: -1 }, // Placeholder type
          });
        }
        validate(expr.body, childScope);
        return;
      }
      case "Case": {
        validate(expr.discriminant);
        for (const branch of expr.branches) {
          // Create a child scope with pattern bindings
          const branchScope = new Scope(scope);
          bindPatternNames(branch.pattern, branchScope);
          validate(branch.body, branchScope);
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
            this.getFilePath(),
          );
        }
        validate(expr.left);
        validate(expr.right);
        return;
      }
      case "Unary": {
        validate(expr.operand);
        return;
      }
      case "Paren": {
        validate(expr.expression);
        return;
      }
      case "Tuple": {
        for (const element of expr.elements) {
          validate(element);
        }
        return;
      }
      case "List": {
        for (const element of expr.elements) {
          validate(element);
        }
        return;
      }
      case "ListRange": {
        validate(expr.start);
        validate(expr.end);
        return;
      }
      case "Record": {
        for (const field of expr.fields) {
          validate(field.value);
        }
        return;
      }
      case "RecordUpdate": {
        // Check that the base record exists
        if (!symbolExists(scope, expr.base)) {
          this.addError(
            `Undefined name '${expr.base}' in implementation of '${methodName}' for protocol '${protocolName}'`,
            expr.span,
          );
        }
        for (const field of expr.fields) {
          validate(field.value);
        }
        return;
      }
      case "FieldAccess": {
        // For field access (e.g., Int.add), validate module-qualified access
        // by checking that the module exists and exports the field
        const resolved = this.validateModuleFieldAccess(
          expr,
          protocolName,
          methodName,
        );
        if (!resolved) {
          // Not a module access, validate the target expression normally
          validate(expr.target);
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
  validateModuleFieldAccess(
    expr: Extract<Expr, { kind: "FieldAccess" }>,
    protocolName: string,
    methodName: string,
  ): boolean {
    const { imports, dependencies } = this;

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
            this.getFilePath(),
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
              this.getFilePath(),
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
              this.getFilePath(),
            );
          }
          return true;
        }

        // Field not found in the module
        throw new SemanticError(
          `'${fieldName}' is not defined in module '${imp.moduleName}' (aliased as '${imp.alias}') in implementation of '${methodName}' for protocol '${protocolName}'`,
          expr.span,
          this.getFilePath(),
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
              this.getFilePath(),
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
                  this.getFilePath(),
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
                  this.getFilePath(),
                );
              }
              return true;
            }

            // Field not found in the module
            throw new SemanticError(
              `'${fieldName}' is not defined in module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`,
              expr.span,
              this.getFilePath(),
            );
          }
        }
      }
    }

    // Not a recognized module access pattern
    return false;
  }

  private validateInstanceConstraintSatisfiability(): void {
    for (const instance of this.localInstances) {
      // Skip instances without constraints
      if (instance.constraints.length === 0) continue;

      for (const constraint of instance.constraints) {
        // Validate the constraint protocol exists
        const constraintProtocol = this.protocols[constraint.protocolName];
        if (!constraintProtocol) {
          throw new SemanticError(
            `Instance constraint references unknown protocol '${constraint.protocolName}'`,
            instance.span,
            this.getFilePath(),
          );
        }
      }
    }
  }

  private validateProtocolDefaultImplementations(): void {
    for (const [protocolName, protocol] of Object.entries(this.protocols)) {
      // Only validate protocols defined in this module
      if (protocol.moduleName !== this.getModuleName()) continue;

      for (const [methodName, methodInfo] of protocol.methods) {
        if (methodInfo.defaultImpl) {
          // Create a scope with the method parameters bound
          const methodScope = new Scope(this.globalScope);
          for (const arg of methodInfo.defaultImpl.args) {
            bindPatternNames(arg, methodScope);
          }

          // Validate the body expression
          this.validateExpressionIdentifiers(
            methodInfo.defaultImpl.body,
            methodScope,
            protocolName,
            methodName,
          );
        }
      }
    }
  }

  /**
   * Compute export information for a module based on its exposing clause.
   * This validates that all exported items exist and builds the ExportInfo structure.
   *
   * @param importedValues - Map from value name to source module name, for values imported
   *                         from other modules that can be re-exported.
   */
  private computeModuleExports(): ExportInfo {
    const moduleDecl = this.getModule();
    const {
      values,
      operators,
      adts,
      typeAliases,
      opaqueTypes,
      records,
      protocols,
      importedValues,
    } = this;

    // Default: empty exports
    const exports: ExportInfo = {
      values: new Set(),
      operators: new Set(),
      types: new Map(),
      protocols: new Map(),
      exportsAll: false,
      reExportedValues: new Map(),
    };

    if (!moduleDecl.exposing) {
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

      // Export all record types
      for (const name of Object.keys(records)) {
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
          // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
          if (Object.hasOwn(values, name)) {
            exports.values.add(name);
            continue;
          }

          // Check if it's an imported value that can be re-exported
          if (importedValues.has(name)) {
            exports.reExportedValues.set(name, importedValues.get(name)!);
            continue;
          }

          // Check if it's a type (ADT, alias, or opaque) exported without constructors
          // Use Object.hasOwn to avoid prototype pollution
          if (Object.hasOwn(adts, name)) {
            exports.types.set(name, {
              allConstructors: false,
              constructors: new Set(),
            });
            continue;
          }
          if (Object.hasOwn(typeAliases, name)) {
            exports.types.set(name, { allConstructors: false });
            continue;
          }
          if (Object.hasOwn(opaqueTypes, name)) {
            exports.types.set(name, { allConstructors: false });
            continue;
          }
          if (Object.hasOwn(records, name)) {
            exports.types.set(name, { allConstructors: false });
            continue;
          }

          // Check if it's a protocol exported without methods
          if (Object.hasOwn(protocols, name)) {
            exports.protocols.set(name, {
              allMethods: false,
              methods: new Set(),
            });
            continue;
          }

          throw new SemanticError(
            `Module exposes '${name}' which is not defined`,
            spec.span,
            this.getFilePath(),
          );
        }

        case "ExportOperator": {
          const op = spec.operator;

          // Check if operator is defined (either as a value or in operator registry)
          // Use Object.hasOwn to avoid prototype pollution
          if (
            !Object.hasOwn(values, op) &&
            !operators.has(op) &&
            !importedValues.has(op)
          ) {
            throw new SemanticError(
              `Module exposes operator '${op}' which is not defined`,
              spec.span,
              this.getFilePath(),
            );
          }

          exports.operators.add(op);
          // Also add to values since operators are functions
          if (Object.hasOwn(values, op)) {
            exports.values.add(op);
          } else if (importedValues.has(op)) {
            exports.reExportedValues.set(op, importedValues.get(op)!);
          }
          break;
        }

        case "ExportTypeAll": {
          const name = spec.name;

          // Check if it's an ADT
          // Use Object.hasOwn to avoid prototype pollution
          if (Object.hasOwn(adts, name)) {
            const adt = adts[name]!;
            exports.types.set(name, {
              allConstructors: true,
              constructors: new Set(adt.constructors),
            });
            continue;
          }

          // Check if it's a protocol
          if (Object.hasOwn(protocols, name)) {
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
          if (Object.hasOwn(typeAliases, name)) {
            throw new SemanticError(
              `Type alias '${name}' cannot use (..) syntax - type aliases have no constructors`,
              spec.span,
              this.getFilePath(),
            );
          }
          if (Object.hasOwn(opaqueTypes, name)) {
            throw new SemanticError(
              `Opaque type '${name}' cannot use (..) syntax - opaque types have no constructors`,
              spec.span,
              this.getFilePath(),
            );
          }

          // Record types cannot use (..) syntax since they have no constructors
          if (Object.hasOwn(records, name)) {
            throw new SemanticError(
              `Record type '${name}' cannot use (..) syntax - records have no constructors. Use '${name}' instead`,
              spec.span,
              this.getFilePath(),
            );
          }

          throw new SemanticError(
            `Module exposes '${name}(..)' but '${name}' is not a type or protocol`,
            spec.span,
            this.getFilePath(),
          );
        }

        case "ExportTypeSome": {
          const name = spec.name;
          const members = spec.members;

          // Check if it's an ADT with specific constructors
          // Use Object.hasOwn to avoid prototype pollution
          if (Object.hasOwn(adts, name)) {
            const adt = adts[name]!;
            const exportedCtors = new Set<string>();

            for (const memberName of members) {
              if (!adt.constructors.includes(memberName)) {
                throw new SemanticError(
                  `Constructor '${memberName}' is not defined in type '${name}'`,
                  spec.span,
                  this.getFilePath(),
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
          if (Object.hasOwn(protocols, name)) {
            const protocol = protocols[name]!;
            const exportedMethods = new Set<string>();

            for (const memberName of members) {
              if (!protocol.methods.has(memberName)) {
                throw new SemanticError(
                  `Method '${memberName}' is not defined in protocol '${name}'`,
                  spec.span,
                  this.getFilePath(),
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
            this.getFilePath(),
          );
        }
      }
    }

    return exports;
  }

  private buildSemanticModule(): SemanticModule {
    const exports = this.computeModuleExports();

    // For re-exported values, store their type schemes so downstream importers can access them
    for (const [name] of exports.reExportedValues) {
      const scheme = this.globalScope.lookup(name);
      if (scheme && !this.typeSchemes[name]) {
        this.typeSchemes[name] = scheme;
      }
    }

    return {
      values: this.values,
      annotations: this.annotations,
      module: this.program.module,
      imports: this.imports,
      types: this.types,
      typeSchemes: this.typeSchemes,
      adts: this.adts,
      constructors: this.constructors,
      constructorTypes: this.constructorTypes,
      typeAliases: this.typeAliases,
      records: this.records,
      opaqueTypes: this.opaqueTypes,
      protocols: this.protocols,
      instances: this.instances,
      operators: this.operators,
      infixDeclarations: this.infixDeclarations,
      exports,
      errors: this.getErrors(),
      importedValues: this.importedValues,
      protocolMethodUsages: this._resolvedProtocolUsages,
      constrainedCallUsages: this._resolvedConstrainedCallUsages,
    };
  }

  // ===== Instance Validation Methods =====

  /**
   * Concretize polymorphic instance type arguments.
   *
   * Before value inference, analyze each instance's method bodies to determine
   * if polymorphic type args can be concretized. This is important for multi-parameter
   * protocols where the return type is constrained.
   */
  concretizeInstanceTypeArgs(): void {
    for (const instance of this.localInstances) {
      const protocol = this.protocols[instance.protocolName];
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
        const inferSubstitution: Substitution = new Map(this._substitution);

        // Infer the type of the implementation expression
        const tempScope = this.createChildScope();
        try {
          const inferredType = this.analyzeExpr(methodExpr, {
            scope: tempScope,
            substitution: inferSubstitution,
          });

          // Unify to get concrete types
          this.unify(
            inferredType,
            expectedType,
            methodExpr.span,
            inferSubstitution,
          );

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
                this.instances,
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
   * Validate that all method implementations in protocol instances have types
   * that match the protocol's declared method signatures.
   */
  validateImplementationMethodTypes(): void {
    for (const instance of this.localInstances) {
      const protocol = this.protocols[instance.protocolName];
      if (!protocol) continue;

      // Build a substitution from protocol type params to instance type args
      const paramSubstitution = new Map<number, Type>();
      const paramNameToId = new Map<string, number>();

      // First, create type variables for each protocol parameter
      for (let i = 0; i < protocol.params.length; i++) {
        const paramName = protocol.params[i]!;
        const typeArg = instance.typeArgs[i];
        if (typeArg) {
          const paramVar = freshType();
          paramNameToId.set(paramName, paramVar.id);
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
        const inferSubstitution: Substitution = new Map(this._substitution);

        // Infer the type of the implementation expression
        const tempScope = this.createChildScope();
        let inferredType: Type;

        if (methodExpr.kind === "Lambda" && expectedType.kind === "fun") {
          // Extract parameter types from expected function type
          const expectedParamTypes: Type[] = [];
          let currentType: Type = expectedType;
          while (
            currentType.kind === "fun" &&
            expectedParamTypes.length < methodExpr.args.length
          ) {
            expectedParamTypes.push(currentType.from);
            currentType = currentType.to;
          }

          // Bind parameters with expected types
          this.bindPatterns(
            tempScope,
            methodExpr.args,
            expectedParamTypes,
            inferSubstitution,
          );

          // Analyze the body
          const bodyType = this.analyzeExpr(methodExpr.body, {
            scope: tempScope,
            substitution: inferSubstitution,
          });

          inferredType = fnChain(expectedParamTypes, bodyType);
        } else {
          inferredType = this.analyzeExpr(methodExpr, {
            scope: tempScope,
            substitution: inferSubstitution,
            expectedType,
          });
        }

        // Try to unify the inferred type with the expected type
        try {
          this.unify(
            inferredType,
            expectedType,
            methodExpr.span,
            inferSubstitution,
          );
        } catch (e) {
          if (e instanceof SemanticError) {
            throw new SemanticError(
              `Implementation of '${methodName}' for '${
                instance.protocolName
              }' has type '${formatType(
                applySubstitution(inferredType, inferSubstitution),
              )}' but protocol expects '${formatType(expectedType)}'`,
              methodExpr.span,
              this.getFilePath(),
            );
          }
          throw e;
        }

        // After successful unification, check that any constraints on the instance
        // are satisfied when type variables are unified with concrete types.
        for (const constraint of instance.constraints) {
          for (const constraintTypeArg of constraint.typeArgs) {
            const resolvedType = applySubstitution(
              constraintTypeArg,
              inferSubstitution,
            );

            if (resolvedType.kind === "con") {
              const hasInstance = findInstanceForTypeInternal(
                constraint.protocolName,
                resolvedType,
                this.instances,
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
                  this.getFilePath(),
                );
              }
            }
          }
        }

        // Concretize the instance's type args based on inference results.
        for (let i = 0; i < instance.typeArgs.length; i++) {
          const typeArg = instance.typeArgs[i]!;
          const resolved = applySubstitution(typeArg, inferSubstitution);
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
   * Validate that concrete type constraints have corresponding instance declarations.
   * After type inference, check that any protocol constraints on concrete types
   * have corresponding instance declarations.
   */
  validateConcreteConstraintInstances(): void {
    for (const [valueName, valueInfo] of Object.entries(this.values)) {
      // Skip synthetic values and values without inferred constraints
      if (valueName.startsWith("$")) continue;

      if (valueInfo.collectedConstraints) {
        for (const constraint of valueInfo.collectedConstraints) {
          const resolvedTypeArgs = constraint.typeArgs.map((t) =>
            applySubstitution(t, this._substitution),
          );

          for (const typeArg of resolvedTypeArgs) {
            if (typeArg.kind === "con") {
              const hasInstance = findInstanceForTypeInternal(
                constraint.protocolName,
                typeArg,
                this.instances,
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
                  this.getFilePath(),
                );
              }
            }
          }
        }
      }
    }
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
   */
  validateConstraintsEagerly(substitution: Substitution, span: Span): void {
    const constraints = this.getCollectedConstraints();
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
        this.instances,
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
              this.getFilePath(),
            );
          }
        }
        // Fallback: generic type mismatch for non-function cases
        const typeArgsStr = resolvedTypeArgs
          .map((t) => formatType(t))
          .join(", ");
        throw new SemanticError(
          `Type mismatch: expression cannot be used as a function (constraint '${c.protocolName}' on '${typeArgsStr}' cannot be satisfied)`,
          span,
          this.getFilePath(),
        );
      }
    }
  }

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
  /**
   * Check for type name collision when importing.
   * Returns true if there's a conflict (same name, different module).
   */
  checkTypeCollision(
    name: string,
    importingFrom: string,
    existing: { moduleName?: string } | undefined,
    span: Span,
    kind: "type" | "type alias" | "protocol" | "record type",
  ): void {
    if (!existing) return;

    // Protocols are globally available (like type class instances in Haskell)
    // Allow re-importing the same protocol through different module paths
    // e.g., Eq can be imported via Vibe.Basics or Vibe (which re-exports from Basics)
    if (kind === "protocol") {
      return;
    }

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
              : kind === "record type"
                ? "Record type"
                : "Protocol"
        } '${name}' conflicts with ${kind} from module '${
          existing.moduleName
        }'. ` + `Consider using qualified imports or aliasing one of them.`,
        span,
        this.getFilePath(),
      );
    }
  }

  // ===== Symbol Management Methods =====

  /**
   * Declare a symbol in a scope with duplicate checking.
   * Throws if the symbol already exists in the given scope.
   */
  declareSymbol(
    scope: Scope,
    name: string,
    scheme: TypeScheme,
    span: Span,
  ): void {
    if (scope.has(name)) {
      throw new SemanticError(
        `Duplicate definition for '${name}'`,
        span,
        this.getFilePath(),
      );
    }
    scope.define(name, scheme);
  }

  /**
   * Look up a symbol in the scope and instantiate its type scheme.
   * Returns both the instantiated type and any protocol constraints.
   */
  lookupSymbolWithConstraints(
    scope: Scope,
    name: string,
    span: Span,
    substitution?: Substitution,
  ): LookupResult {
    const sub = substitution ?? this._substitution;
    const scheme = scope.lookup(name);
    if (scheme) {
      const { type, constraints } = instantiateWithConstraints(scheme, sub);
      return { type, constraints };
    }
    this.addError(`Undefined name '${name}'`, span);
    return { type: ERROR_TYPE, constraints: [] };
  }

  /**
   * Look up a symbol and return just its instantiated type.
   */
  lookupSymbol(
    scope: Scope,
    name: string,
    span: Span,
    substitution?: Substitution,
  ): Type {
    return this.lookupSymbolWithConstraints(scope, name, span, substitution)
      .type;
  }

  // ===== Type Generalization Methods =====

  /**
   * Generalize a type into a type scheme by quantifying over its free type variables.
   * This is the key operation that enables let-polymorphism.
   */
  generalize(
    type: Type,
    scope: Scope,
    substitution?: Substitution,
  ): TypeScheme {
    const sub = substitution ?? this._substitution;
    const typeFreeVars = getFreeTypeVars(type, sub);
    const scopeFreeVars = getFreeTypeVarsInScope(scope, sub);

    const quantified = new Set<number>();
    for (const v of typeFreeVars) {
      if (!scopeFreeVars.has(v)) {
        quantified.add(v);
      }
    }

    const rawConstraints = this.getCollectedConstraints();
    const resolvedConstraints = applySubstitutionToConstraints(
      rawConstraints,
      sub,
    );

    const relevantConstraints = resolvedConstraints.filter((c) => {
      return c.typeArgs.some((t) => {
        const freeVars = getFreeTypeVars(t, sub);
        for (const v of freeVars) {
          if (quantified.has(v)) return true;
        }
        return false;
      });
    });

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
}

/**
 * Scope maintains a symbol table mapping names to their type schemes.
 * Type schemes enable let-polymorphism: bindings can be polymorphic,
 * and each use site gets a fresh instantiation of the type.
 *
 * NOTE: This local alias is used since Scope is also imported from types.ts
 * but we use it differently in places here. This will be cleaned up in a future refactor.
 */

// ============================================================================
// Strongly Connected Components (SCC) for Mutual Recursion
// ============================================================================

/**
 * Collect all free variable references in an expression.
 * These are variables that are not bound within the expression itself.
 *
 * Implemented iteratively using an explicit worklist to avoid stack overflow
 * on deeply nested expressions.
 */
function collectFreeVars(
  expr: Expr,
  bound: Set<string> = new Set(),
): Set<string> {
  const free = new Set<string>();
  // Each work item is [expression, bound-variable-set-at-that-point]
  const worklist: Array<[Expr, Set<string>]> = [[expr, bound]];

  while (worklist.length > 0) {
    const [e, localBound] = worklist.pop()!;
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
        worklist.push([e.expression, localBound]);
        break;
      case "Tuple":
        for (const el of e.elements) worklist.push([el, localBound]);
        break;
      case "List":
        for (const el of e.elements) worklist.push([el, localBound]);
        break;
      case "ListRange":
        worklist.push([e.start, localBound]);
        worklist.push([e.end, localBound]);
        break;
      case "Record":
        for (const f of e.fields) worklist.push([f.value, localBound]);
        break;
      case "RecordUpdate":
        if (!localBound.has(e.base)) {
          free.add(e.base);
        }
        for (const f of e.fields) worklist.push([f.value, localBound]);
        break;
      case "FieldAccess":
        worklist.push([e.target, localBound]);
        break;
      case "Apply":
        worklist.push([e.callee, localBound]);
        for (const arg of e.args) worklist.push([arg, localBound]);
        break;
      case "Infix":
        worklist.push([e.left, localBound]);
        worklist.push([e.right, localBound]);
        if (!localBound.has(e.operator)) {
          free.add(e.operator);
        }
        break;
      case "Unary":
        worklist.push([e.operand, localBound]);
        break;
      case "Lambda": {
        const lambdaBound = new Set(localBound);
        for (const p of e.args) collectPatternVars(p, lambdaBound);
        worklist.push([e.body, lambdaBound]);
        break;
      }
      case "LetIn": {
        // Build the scoped bound set: all binding names are added upfront.
        // Let-binding names are always local (never module-level), so this
        // over-approximation is safe for the purpose of dependency analysis.
        const letBound = new Set(localBound);
        for (const binding of e.bindings) {
          letBound.add(binding.name);
          for (const p of binding.args) collectPatternVars(p, letBound);
        }
        for (const binding of e.bindings) {
          worklist.push([binding.body, letBound]);
        }
        worklist.push([e.body, letBound]);
        break;
      }
      case "If":
        worklist.push([e.condition, localBound]);
        worklist.push([e.thenBranch, localBound]);
        worklist.push([e.elseBranch, localBound]);
        break;
      case "Case":
        worklist.push([e.discriminant, localBound]);
        for (const branch of e.branches) {
          const branchBound = new Set(localBound);
          collectPatternVars(branch.pattern, branchBound);
          worklist.push([branch.body, branchBound]);
        }
        break;
    }
  }

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

export function analyze(
  program: Program,
  options: AnalyzeOptions,
): SemanticModule {
  return new SemanticAnalyzer(program, options).analyze();
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
 * @param analyzer - The semantic analyzer (for accessing registries)
 * @param expr - The type expression to validate
 * @param definedParams - Set of type parameters that are in scope (e.g., from the alias declaration)
 * @param parentSpan - Span to use for error reporting if the expression has no span
 */
function validateTypeExpr(
  analyzer: SemanticAnalyzer,
  expr: TypeExpr,
  definedParams: Set<string>,
  parentSpan: Span,
): TypeValidationError[] {
  const { adts, typeAliases, opaqueTypes, records, imports, dependencies } =
    analyzer;
  const errors: TypeValidationError[] = [];

  function resolve(name: string) {
    return resolveQualifiedType(
      name,
      adts,
      typeAliases,
      opaqueTypes,
      records,
      imports,
      dependencies,
    );
  }

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

        // Try to resolve qualified type
        const resolved = resolve(name);
        if (resolved) {
          // Valid qualified reference - validate args
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

  // Build paramNames from the protocol's type variable context
  const paramNames = invertContext(sharedTypeVarCtx);

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
        paramNames,
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
 * Concretize polymorphic instance type args by analyzing method bodies.
 *
 * When an implementation body forces a polymorphic type arg to be concrete
 * (e.g., `convert3 _ = [1]` forces `a` to be `List Int`), we update the
 * instance's typeArgs so that later generalization produces concrete types
 * instead of leaving quantified variables with constraints.
 *
 * This fixes hover showing `ExampleProtocol3 Float t402 => t402` instead of `List Int`.
 */
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

  // Attempt synthetic derivation (Tuple/Record)
  if (typeArgs.length > 0) {
    if (trySynthesizeInstance(protocolName, typeArgs[0]!, instances)) {
      return true;
    }
  }

  return false;
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
      if (
        constraint.protocolName === "Eq" ||
        constraint.protocolName === "Show"
      ) {
        return { possible: true };
      }
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
      if (
        constraint.protocolName === "Eq" ||
        constraint.protocolName === "Show"
      ) {
        return { possible: true };
      }
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

  // Attempt synthetic derivation (Tuple/Record)
  if (constraint.typeArgs.length === 1) {
    if (
      trySynthesizeInstance(
        constraint.protocolName,
        constraint.typeArgs[0]!,
        instances,
      )
    ) {
      return { found: true };
    }
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

  if (instArg.kind === "tuple" && constraintArg.kind === "tuple") {
    if (instArg.elements.length !== constraintArg.elements.length) return false;
    for (let i = 0; i < instArg.elements.length; i++) {
      if (
        !matchTypeArgForInstance(
          instArg.elements[i]!,
          constraintArg.elements[i]!,
          substitution,
        )
      ) {
        return false;
      }
    }
    return true;
  }

  if (instArg.kind === "record" && constraintArg.kind === "record") {
    const instKeys = Object.keys(instArg.fields).sort();
    const constraintKeys = Object.keys(constraintArg.fields).sort();
    if (instKeys.length !== constraintKeys.length) return false;

    for (let i = 0; i < instKeys.length; i++) {
      const key = instKeys[i]!;
      if (key !== constraintKeys[i]) return false;
      if (
        !matchTypeArgForInstance(
          instArg.fields[key]!,
          constraintArg.fields[key]!,
          substitution,
        )
      ) {
        return false;
      }
    }
    return true;
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
  concreteType: Type,
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
    let matchesStructure = false;

    if (instTypeArg.kind === concreteType.kind) {
      if (instTypeArg.kind === "con" && concreteType.kind === "con") {
        matchesStructure =
          instTypeArg.name === concreteType.name &&
          instTypeArg.args.length === concreteType.args.length;
      } else if (
        instTypeArg.kind === "tuple" &&
        concreteType.kind === "tuple"
      ) {
        matchesStructure =
          instTypeArg.elements.length === concreteType.elements.length;
      } else if (
        instTypeArg.kind === "record" &&
        concreteType.kind === "record"
      ) {
        // Exact record match is strict, usually relying on structural matching below
        // But for safety, we can check keys
        const k1 = Object.keys(instTypeArg.fields).sort();
        const k2 = Object.keys(concreteType.fields).sort();
        matchesStructure =
          k1.length === k2.length && k1.every((k, i) => k === k2[i]);
      } else {
        // Functions etc
        matchesStructure = true;
      }
    }

    if (matchesStructure) {
      // Check if type arguments match structurally
      if (instanceTypeMatches(instTypeArg, concreteType)) {
        // If the instance has no constraints, it's a direct match
        if (inst.constraints.length === 0) {
          return { found: true };
        }

        // Instance has constraints - we need to check them
        // Build a substitution from instance type variables to concrete types
        const typeVarSubst = new Map<number, Type>();
        buildTypeVarSubstitution(instTypeArg, concreteType, typeVarSubst);

        // Check each constraint with the substituted types
        let allConstraintsSatisfied = true;
        for (const constraint of inst.constraints) {
          // Apply substitution to the constraint's type args
          const substitutedTypeArgs = constraint.typeArgs.map((t) =>
            applySubstitutionToType(t, typeVarSubst),
          );

          // For each type arg in the constraint, check if an instance exists
          for (const typeArg of substitutedTypeArgs) {
            const constraintResult = findInstanceForTypeWithReason(
              constraint.protocolName,
              typeArg,
              instances,
            );
            if (!constraintResult.found) {
              allConstraintsSatisfied = false;
              hasPolymorphicInstance = true;
              if (!firstUnsatisfiedConstraint) {
                firstUnsatisfiedConstraint = {
                  constraint: constraint.protocolName,
                  forType: formatType(typeArg),
                };
              }
              break;
            }
          }
          if (!allConstraintsSatisfied) break;
        }

        if (allConstraintsSatisfied) {
          return { found: true };
        }
      }
    }

    // Polymorphic match: instance head is just a type variable (e.g., `Show a`)
    if (instTypeArg.kind === "var") {
      hasPolymorphicInstance = true;
      if (inst.constraints.length === 0) {
        return { found: true };
      }

      let allConstraintsSatisfied = true;
      for (const constraint of inst.constraints) {
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

  // Hook for synthetic instances (Tuple/Record auto-derivation)
  if (trySynthesizeInstance(protocolName, concreteType, instances)) {
    return { found: true };
  }

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
 * Build a substitution mapping type variables in `instType` to concrete types in `concreteType`.
 * Assumes the types structurally match (i.e., instanceTypeMatches returned true).
 */
function buildTypeVarSubstitution(
  instType: Type,
  concreteType: Type,
  subst: Map<number, Type>,
): void {
  if (instType.kind === "var") {
    subst.set(instType.id, concreteType);
    return;
  }

  if (instType.kind === "con" && concreteType.kind === "con") {
    for (let i = 0; i < instType.args.length; i++) {
      buildTypeVarSubstitution(instType.args[i]!, concreteType.args[i]!, subst);
    }
  } else if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    for (let i = 0; i < instType.elements.length; i++) {
      buildTypeVarSubstitution(
        instType.elements[i]!,
        concreteType.elements[i]!,
        subst,
      );
    }
  } else if (instType.kind === "fun" && concreteType.kind === "fun") {
    buildTypeVarSubstitution(instType.from, concreteType.from, subst);
    buildTypeVarSubstitution(instType.to, concreteType.to, subst);
  } else if (instType.kind === "record" && concreteType.kind === "record") {
    for (const key of Object.keys(instType.fields)) {
      if (key in concreteType.fields) {
        buildTypeVarSubstitution(
          instType.fields[key]!,
          concreteType.fields[key]!,
          subst,
        );
      }
    }
  }
}

/**
 * Apply a type variable substitution to a type.
 */
function applySubstitutionToType(type: Type, subst: Map<number, Type>): Type {
  if (type.kind === "var") {
    return subst.get(type.id) ?? type;
  }
  if (type.kind === "con") {
    return {
      ...type,
      args: type.args.map((a) => applySubstitutionToType(a, subst)),
    };
  }
  if (type.kind === "tuple") {
    return {
      ...type,
      elements: type.elements.map((e) => applySubstitutionToType(e, subst)),
    };
  }
  if (type.kind === "fun") {
    return {
      ...type,
      from: applySubstitutionToType(type.from, subst),
      to: applySubstitutionToType(type.to, subst),
    };
  }
  if (type.kind === "record") {
    const newFields: Record<string, Type> = {};
    for (const [k, v] of Object.entries(type.fields)) {
      newFields[k] = applySubstitutionToType(v, subst);
    }
    return { ...type, fields: newFields };
  }
  return type;
}

/**
 * Simple boolean version for backward compatibility
 */
function findInstanceForTypeInternal(
  protocolName: string,
  concreteType: Type,
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

 */

const syntheticSpan: Span = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 },
};

/**
 * Module-level record registry reference, set by the analyzer before
 * auto-derive runs. Used by standalone synthesis functions to resolve
 * TypeCon records to their field names.
 */
let _recordRegistry: Record<string, RecordInfo> = {};
let _protocolMethodUsages: Map<Expr, ProtocolMethodUsage> | undefined;
let _currentModuleName: string | undefined;

function getRecordFieldNames(type: Type): string[] | undefined {
  if (type.kind === "record") {
    return Object.keys(type.fields).sort();
  }
  if (type.kind === "con") {
    const info = _recordRegistry[type.name];
    if (info) return info.fields.map((f) => f.name).sort();
  }
  return undefined;
}

function getRecordFieldTypes(type: Type): Record<string, Type> | undefined {
  if (type.kind === "record") {
    return type.fields;
  }
  if (type.kind === "con") {
    const info = _recordRegistry[type.name];
    if (info) {
      const fields: Record<string, Type> = {};
      const resolveCtx = new Map<string, TypeVar>();
      info.params.forEach((p, i) => {
        const v: TypeVar =
          type.args[i]?.kind === "var"
            ? (type.args[i] as TypeVar)
            : freshType();
        resolveCtx.set(p, v);
      });
      for (const field of info.fields) {
        fields[field.name] = resolveTypeExprForRecord(
          field.typeExpr,
          resolveCtx,
        );
      }
      return fields;
    }
  }
  return undefined;
}

function resolveTypeExprForRecord(
  typeExpr: TypeExpr,
  context: Map<string, TypeVar>,
): Type {
  switch (typeExpr.kind) {
    case "TypeRef": {
      const tv = context.get(typeExpr.name);
      if (tv) return tv;
      return {
        kind: "con",
        name: typeExpr.name,
        args: typeExpr.args.map((a) => resolveTypeExprForRecord(a, context)),
      };
    }
    case "FunctionType":
      return {
        kind: "fun",
        from: resolveTypeExprForRecord(typeExpr.from, context),
        to: resolveTypeExprForRecord(typeExpr.to, context),
      };
    case "TupleType":
      return {
        kind: "tuple",
        elements: typeExpr.elements.map((e) =>
          resolveTypeExprForRecord(e, context),
        ),
      };
    default:
      return freshType();
  }
}

function trySynthesizeInstance(
  protocolName: string,
  type: Type,
  instances: InstanceInfo[],
): boolean {
  if (protocolName !== "Eq" && protocolName !== "Show") return false;

  // Tuples
  if (type.kind === "tuple") {
    for (const elem of type.elements) {
      if (!findInstanceForTypeInternal(protocolName, elem, instances)) {
        return false;
      }
    }

    const instance = generateSyntheticInstance(
      protocolName,
      type,
      _protocolMethodUsages,
    );
    instances.push(instance);
    return true;
  }

  // Records (structural or named TypeCon)
  const fieldTypes = getRecordFieldTypes(type);
  if (fieldTypes) {
    for (const fieldType of Object.values(fieldTypes)) {
      if (!findInstanceForTypeInternal(protocolName, fieldType, instances)) {
        return false;
      }
    }

    const instance = generateSyntheticInstance(
      protocolName,
      type,
      _protocolMethodUsages,
    );
    instances.push(instance);
    return true;
  }

  return false;
}

function generateSyntheticInstance(
  protocolName: string,
  type: Type,
  usages?: Map<Expr, ProtocolMethodUsage>,
): InstanceInfo {
  const methods = new Map<string, Expr>();
  const span = syntheticSpan;

  if (protocolName === "Eq") {
    methods.set("==", generateSyntheticEq(type, usages));
  } else if (protocolName === "Show") {
    methods.set("toString", generateSyntheticShow(type, usages));
  }

  return {
    protocolName,
    moduleName: _currentModuleName ?? "Synthetic",
    typeArgs: [type],
    constraints: [],
    methods,
    explicitMethods: new Set(methods.keys()),
    span,
  };
}

function generateSyntheticEq(
  type: Type,
  usages?: Map<Expr, ProtocolMethodUsage>,
): Expr {
  const span = syntheticSpan;
  // (==) x y = ...
  if (type.kind === "tuple") {
    const arity = type.elements.length;
    const xVars = Array.from({ length: arity }, (_, i) => `x${i}`);
    const yVars = Array.from({ length: arity }, (_, i) => `y${i}`);

    let body: Expr = { kind: "Var", name: "True", namespace: "upper", span };

    if (arity > 0) {
      const checks: Expr[] = xVars.map((xv, i) => {
        const node: Expr = {
          kind: "Infix",
          left: { kind: "Var", name: xv, namespace: "lower", span },
          operator: "==",
          right: { kind: "Var", name: yVars[i]!, namespace: "lower", span },
          span,
        };
        if (usages) {
          usages.set(node, {
            protocolName: "Eq",
            typeArgs: [type.elements[i]!],
          });
        }
        return node;
      });

      body = checks.reduce((acc, check) => ({
        kind: "Infix",
        left: acc,
        operator: "&&",
        right: check,
        span,
      }));
    }

    const pattern: Pattern = {
      kind: "TuplePattern",
      elements: [
        {
          kind: "TuplePattern",
          elements: xVars.map((v) => ({ kind: "VarPattern", name: v, span })),
          span,
        },
        {
          kind: "TuplePattern",
          elements: yVars.map((v) => ({ kind: "VarPattern", name: v, span })),
          span,
        },
      ],
      span,
    };

    return {
      kind: "Lambda",
      args: [
        { kind: "VarPattern", name: "x_impl", span },
        { kind: "VarPattern", name: "y_impl", span },
      ],
      body: {
        kind: "Case",
        discriminant: {
          kind: "Tuple",
          elements: [
            { kind: "Var", name: "x_impl", namespace: "lower", span },
            { kind: "Var", name: "y_impl", namespace: "lower", span },
          ],
          span,
        },
        branches: [{ pattern, body, span }],
        span,
      },
      span,
    };
  } else {
    const fieldTypes = getRecordFieldTypes(type);
    if (fieldTypes) {
      const fields = Object.keys(fieldTypes).sort();
      let body: Expr = { kind: "Var", name: "True", namespace: "upper", span };

      if (fields.length > 0) {
        const checks: Expr[] = fields.map((f) => {
          const node: Expr = {
            kind: "Infix",
            left: {
              kind: "FieldAccess",
              target: { kind: "Var", name: "x_impl", namespace: "lower", span },
              field: f,
              span,
            },
            operator: "==",
            right: {
              kind: "FieldAccess",
              target: { kind: "Var", name: "y_impl", namespace: "lower", span },
              field: f,
              span,
            },
            span,
          };
          if (usages) {
            usages.set(node, {
              protocolName: "Eq",
              typeArgs: [fieldTypes[f]!],
            });
          }
          return node;
        });
        body = checks.reduce((acc, check) => ({
          kind: "Infix",
          left: acc,
          operator: "&&",
          right: check,
          span,
        }));
      }

      return {
        kind: "Lambda",
        args: [
          { kind: "VarPattern", name: "x_impl", span },
          { kind: "VarPattern", name: "y_impl", span },
        ],
        body,
        span,
      };
    }
  }
  throw new Error("Unsupported type for synthetic Eq");
}

function generateSyntheticShow(
  type: Type,
  usages?: Map<Expr, ProtocolMethodUsage>,
): Expr {
  const span = syntheticSpan;

  if (type.kind === "tuple") {
    const arity = type.elements.length;
    const vars = Array.from({ length: arity }, (_, i) => `x${i}`);

    const str = (s: string): Expr => ({
      kind: "String",
      value: `"${s}"`,
      span,
    });
    const append = (a: Expr, b: Expr): Expr => ({
      kind: "Infix",
      left: a,
      operator: "++",
      right: b,
      span,
    });
    const toStringCall = (valName: string, elemType: Type): Expr => {
      const callee: Expr = {
        kind: "Var",
        name: "toString",
        namespace: "lower",
        span,
      };
      const node: Expr = {
        kind: "Apply",
        callee,
        args: [{ kind: "Var", name: valName, namespace: "lower", span }],
        span,
      };
      if (usages) {
        usages.set(callee, { protocolName: "Show", typeArgs: [elemType] });
      }
      return node;
    };

    let body: Expr = str("(");
    vars.forEach((v, i) => {
      if (i > 0) body = append(body, str(", "));
      body = append(body, toStringCall(v, type.elements[i]!));
    });
    body = append(body, str(")"));

    const pattern: Pattern = {
      kind: "TuplePattern",
      elements: vars.map((v) => ({ kind: "VarPattern", name: v, span })),
      span,
    };

    return {
      kind: "Lambda",
      args: [{ kind: "VarPattern", name: "x_impl", span }],
      body: {
        kind: "Case",
        discriminant: { kind: "Var", name: "x_impl", namespace: "lower", span },
        branches: [{ pattern, body, span }],
        span,
      },
      span,
    };
  } else {
    const fieldTypes = getRecordFieldTypes(type);
    if (fieldTypes) {
      const fields = Object.keys(fieldTypes).sort();
      const typeName = type.kind === "con" ? type.name : null;
      const str = (s: string): Expr => ({
        kind: "String",
        value: `"${s}"`,
        span,
      });
      const append = (a: Expr, b: Expr): Expr => ({
        kind: "Infix",
        left: a,
        operator: "++",
        right: b,
        span,
      });
      const toStringField = (f: string): Expr => {
        const callee: Expr = {
          kind: "Var",
          name: "toString",
          namespace: "lower",
          span,
        };
        const node: Expr = {
          kind: "Apply",
          callee,
          args: [
            {
              kind: "FieldAccess",
              target: { kind: "Var", name: "x_impl", namespace: "lower", span },
              field: f,
              span,
            },
          ],
          span,
        };
        if (usages) {
          usages.set(callee, {
            protocolName: "Show",
            typeArgs: [fieldTypes[f]!],
          });
        }
        return node;
      };

      let body: Expr = str(typeName ? `${typeName} { ` : "{ ");
      fields.forEach((f, i) => {
        if (i > 0) body = append(body, str(", "));
        body = append(body, str(`${f} = `));
        body = append(body, toStringField(f));
      });
      body = append(body, str(" }"));

      return {
        kind: "Lambda",
        args: [{ kind: "VarPattern", name: "x_impl", span }],
        body,
        span,
      };
    }
  }
  throw new Error("Unsupported type for synthetic Show");
}

/**
 * Check if a symbol exists in the scope hierarchy (without throwing).
 */
function symbolExists(scope: Scope, name: string): boolean {
  return scope.lookup(name) !== undefined;
}

/**
 * Bind pattern variable names into a scope (for lambda args, case branches, etc.)
 * This is a simple version that just adds placeholder type schemes.
 */
function bindPatternNames(pattern: Pattern, scope: Scope): void {
  switch (pattern.kind) {
    case "VarPattern":
      scope.define(pattern.name, {
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

/**
 * Check if two arrays of types are structurally equal.
 * Used for deduplicating instances during import.
 */
function typeArgsEqual(args1: Type[], args2: Type[]): boolean {
  if (args1.length !== args2.length) return false;
  return args1.every((t, i) => typesEqual(t, args2[i]!));
}

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
 * Resolve a field from a dependency module, properly instantiating its type scheme
 * so that each use site gets fresh type variables.
 */
function resolveModuleField(
  depModule: SemanticModule,
  field: string,
  substitution: Substitution,
): { type: Type; constraints: Constraint[] } | null {
  // Prefer type schemes (with quantified vars) for proper instantiation
  // Use Object.hasOwn to avoid prototype pollution (e.g., 'toString' from Object.prototype)
  if (Object.hasOwn(depModule.typeSchemes, field)) {
    const scheme = depModule.typeSchemes[field]!;
    const { type, constraints } = instantiateWithConstraints(
      scheme,
      substitution,
    );
    return { type, constraints };
  }
  // Fall back to raw type on ValueInfo (already monomorphic or no scheme available)
  if (Object.hasOwn(depModule.values, field)) {
    const valueInfo = depModule.values[field]!;
    const valueType = valueInfo.type || depModule.types[field];
    if (valueType) {
      return { type: valueType, constraints: [] };
    }
  }
  // Check if it's a constructor
  if (Object.hasOwn(depModule.constructorTypes, field)) {
    const ctorScheme = depModule.constructorTypes[field]!;
    const { type, constraints } = instantiateWithConstraints(
      ctorScheme,
      substitution,
    );
    return { type, constraints };
  }
  return null;
}

/**
 * Try to resolve a module-qualified field access (e.g., Vibe.JS.null)
 * Returns the type and constraints of the accessed symbol if it's a module access, or null if not.
 */
function tryResolveModuleFieldAccess(
  expr: Extract<Expr, { kind: "FieldAccess" }>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
  substitution: Substitution,
): { type: Type; constraints: Constraint[] } | null {
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
        const resolved = resolveModuleField(depModule, field, substitution);
        if (resolved) return resolved;
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
          const resolved = resolveModuleField(depModule, field, substitution);
          if (resolved) return resolved;
        }
        // For nested field accesses like Vibe.JS.value.field, we return null here
        // and let the recursive FieldAccess analysis in analyzeExpr handle it.
      }
    }
  }

  return null;
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

/**
 * Helper to resolve a potentially qualified type name (e.g., "R.Result" or "Result").
 * Looks up in local definitions, aliases, opaque types, records, and imports.
 */
function resolveQualifiedType(
  name: string,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>,
  opaqueTypes: Record<string, OpaqueTypeInfo>,
  records: Record<string, RecordInfo>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
): {
  kind: "adt" | "alias" | "opaque" | "record";
  name: string;
  info: ADTInfo | TypeAliasInfo | OpaqueTypeInfo | RecordInfo;
} | null {
  // 1. Check local definitions first (exact match)
  if (adts[name]) return { kind: "adt", name, info: adts[name]! };
  if (typeAliases[name])
    return { kind: "alias", name, info: typeAliases[name]! };
  if (opaqueTypes[name])
    return { kind: "opaque", name, info: opaqueTypes[name]! };
  if (records[name]) return { kind: "record", name, info: records[name]! };

  // 2. Check if it's a qualified name (e.g. Module.Type)
  const parts = name.split(".");
  if (parts.length > 1) {
    const typeName = parts.pop()!;
    const moduleName = parts.join("."); // Remaining part is module path/alias

    // Find import matching moduleName
    for (const imp of imports) {
      if (imp.alias === moduleName || imp.moduleName === moduleName) {
        // Found the module import, look up the type in dependencies
        const depModule = dependencies.get(imp.moduleName);
        if (depModule) {
          // Check exports of dependency module
          if (depModule.adts[typeName])
            return {
              kind: "adt",
              name: typeName,
              info: depModule.adts[typeName]!,
            };
          if (depModule.typeAliases[typeName])
            return {
              kind: "alias",
              name: typeName,
              info: depModule.typeAliases[typeName]!,
            };
          if (depModule.opaqueTypes[typeName])
            return {
              kind: "opaque",
              name: typeName,
              info: depModule.opaqueTypes[typeName]!,
            };
          if (depModule.records[typeName])
            return {
              kind: "record",
              name: typeName,
              info: depModule.records[typeName]!,
            };
        }
      }
    }
  }

  // 3. Not found
  return null;
}

/**
 * Helper to resolve a potentially qualified constructor name (e.g., "R.Just" or "Just").
 * Looks up in local constructors and imported modules.
 */
function resolveQualifiedConstructor(
  name: string,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  imports: ImportDeclaration[],
  dependencies: Map<string, SemanticModule>,
  moduleContext?: string,
): {
  name: string;
  info: ConstructorInfo;
  adt: ADTInfo;
} | null {
  // 0. Check module context if provided (for resolving siblings in same module)
  if (moduleContext) {
    const depModule = dependencies.get(moduleContext);
    if (depModule && depModule.constructors[name]) {
      const info = depModule.constructors[name]!;
      const adt = depModule.adts[info.parentType];
      if (adt) {
        return { name, info, adt };
      }
    }
    // If not found in module context, fall back or fail?
    // Usually strict lookup.
    // But continue to standard lookup just in case?
  }

  if (constructors[name]) {
    const info = constructors[name]!;
    const adt = adts[info.parentType];
    if (adt) {
      return { name, info, adt };
    }
  }

  // 2. Check if it's a qualified name (e.g. Module.Ctor)
  const parts = name.split(".");
  if (parts.length > 1) {
    const ctorName = parts.pop()!;
    const moduleName = parts.join(".");

    for (const imp of imports) {
      if (imp.alias === moduleName || imp.moduleName === moduleName) {
        const depModule = dependencies.get(imp.moduleName);
        if (depModule) {
          if (depModule.constructors[ctorName]) {
            const info = depModule.constructors[ctorName]!;
            const adt = depModule.adts[info.parentType];
            if (adt) {
              return { name: ctorName, info, adt };
            }
          }
        }
      }
    }
  }
  return null;
}
