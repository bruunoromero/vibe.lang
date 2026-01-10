import type {
  Program,
  ValueDeclaration,
  TypeAnnotationDeclaration,
  ExternalDeclaration,
  TypeDeclaration,
  TypeAliasDeclaration,
  ConstructorVariant,
  TypeExpr,
  Span,
  ImportDeclaration,
  ModuleDeclaration,
  Expr,
  Pattern,
} from "@vibe/syntax";

export class SemanticError extends Error {
  constructor(message: string, public readonly span: Span) {
    super(message);
  }
}

// ===== Internal Type Representation =====
// Simple HM-style types for type inference using Hindley-Milner algorithm.

type TypeVar = { kind: "var"; id: number };
type TypeCon = { kind: "con"; name: string; args: Type[] };
type TypeFun = { kind: "fun"; from: Type; to: Type };
type TypeTuple = { kind: "tuple"; elements: Type[] };
type TypeRecord = { kind: "record"; fields: Record<string, Type> };
type TypeList = { kind: "list"; element: Type };
type Type = TypeVar | TypeCon | TypeFun | TypeTuple | TypeRecord | TypeList;

/**
 * TypeScheme represents a polymorphic type with universally quantified type variables.
 * Example: `forall a. a -> a` is represented as { vars: Set([0]), type: { kind: "fun", from: TypeVar(0), to: TypeVar(0) } }
 *
 * In Elm/ML terminology:
 * - A monomorphic type has no quantified variables (empty Set)
 * - A polymorphic type has one or more quantified variables
 */
type TypeScheme = {
  vars: Set<number>; // Set of type variable IDs that are quantified (polymorphic)
  type: Type; // The underlying type structure
};

type Substitution = Map<number, Type>;

/**
 * Scope maintains a symbol table mapping names to their type schemes.
 * Type schemes enable let-polymorphism: bindings can be polymorphic,
 * and each use site gets a fresh instantiation of the type.
 */
type Scope = {
  parent?: Scope;
  symbols: Map<string, TypeScheme>; // Maps names to their polymorphic type schemes
};

let nextTypeVarId = 0;

const tNumber: TypeCon = { kind: "con", name: "number", args: [] };
const tString: TypeCon = { kind: "con", name: "string", args: [] };
const tBool: TypeCon = { kind: "con", name: "bool", args: [] };
const tChar: TypeCon = { kind: "con", name: "char", args: [] };
const tUnit: TypeTuple = { kind: "tuple", elements: [] };

// ===== Algebraic Data Type (ADT) Registry =====
// The ADT registry tracks user-defined types and their constructors.
// This enables proper type checking and exhaustiveness analysis.

/**
 * Information about a single constructor in an ADT.
 *
 * For example, in `type Maybe a = Just a | Nothing`:
 * - Just: { arity: 1, argTypes: [TypeVar(a)], parentType: "Maybe", parentParams: ["a"] }
 * - Nothing: { arity: 0, argTypes: [], parentType: "Maybe", parentParams: ["a"] }
 */
export type ConstructorInfo = {
  /** Number of arguments the constructor takes */
  arity: number;
  /** Type expressions for each argument (from source) */
  argTypes: TypeExpr[];
  /** Name of the parent ADT (e.g., "Maybe") */
  parentType: string;
  /** Type parameters of the parent ADT (e.g., ["a"]) */
  parentParams: string[];
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about an Algebraic Data Type.
 *
 * For example, `type Maybe a = Just a | Nothing` becomes:
 * {
 *   name: "Maybe",
 *   params: ["a"],
 *   constructors: ["Just", "Nothing"],
 *   span: <source span>
 * }
 */
export type ADTInfo = {
  /** The type name (e.g., "Maybe", "Result") */
  name: string;
  /** Type parameters (e.g., ["a"] for Maybe a) */
  params: string[];
  /** Names of all constructors in this type */
  constructors: string[];
  /** Source span for error messages */
  span: Span;
};

/**
 * Information about a type alias.
 *
 * For example, `type alias UserId = number` becomes:
 * {
 *   name: "UserId",
 *   params: [],
 *   value: TypeRef "number"
 * }
 */
export type TypeAliasInfo = {
  /** The alias name (e.g., "UserId") */
  name: string;
  /** Type parameters (e.g., ["a", "b"] for Pair a b) */
  params: string[];
  /** The type expression this alias expands to */
  value: TypeExpr;
  /** Source span for error messages */
  span: Span;
};

// Legacy built-in constructors for backwards compatibility.
// These will be replaced by proper ADT definitions in the prelude.
const BUILTIN_CONSTRUCTORS: Record<string, number> = {
  True: 0,
  False: 0,
  Nothing: 0,
  Just: 1,
  Ok: 1,
  Err: 1,
};

const INFIX_TYPES: Record<string, Type> = {
  "+": fn(tNumber, tNumber, tNumber),
  "-": fn(tNumber, tNumber, tNumber),
  "*": fn(tNumber, tNumber, tNumber),
  "/": fn(tNumber, tNumber, tNumber),
  "++": fn(tString, tString, tString),
  "==": fn(tNumber, tNumber, tBool),
  "/=": fn(tNumber, tNumber, tBool),
  "<": fn(tNumber, tNumber, tBool),
  "<=": fn(tNumber, tNumber, tBool),
  ">": fn(tNumber, tNumber, tBool),
  ">=": fn(tNumber, tNumber, tBool),
};

export type ValueInfo = {
  declaration: ValueDeclaration | ExternalDeclaration;
  annotation?: TypeExpr;
  externalTarget?: ExternalDeclaration["target"];
  type?: Type;
};

/**
 * The result of semantic analysis for a module.
 *
 * Contains all the analyzed information needed for code generation:
 * - values: All value declarations with their inferred types
 * - annotations: Standalone type annotations
 * - types: Inferred types for all values
 * - adts: User-defined algebraic data types
 * - constructors: Map from constructor names to their info
 * - typeAliases: Type alias definitions
 * - module: Module declaration (if any)
 * - imports: Import declarations
 */
export type SemanticModule = {
  values: Record<string, ValueInfo>;
  annotations: Record<string, TypeAnnotationDeclaration>;
  module?: ModuleDeclaration;
  imports: ImportDeclaration[];
  types: Record<string, Type>;
  /** Registry of user-defined algebraic data types */
  adts: Record<string, ADTInfo>;
  /** Map from constructor names to their type information */
  constructors: Record<string, ConstructorInfo>;
  /** Registry of type aliases */
  typeAliases: Record<string, TypeAliasInfo>;
};

export interface AnalyzeOptions {
  /** Pre-analyzed dependency modules to merge into scope */
  dependencies?: Map<string, SemanticModule>;
}

export function analyze(
  program: Program,
  options: AnalyzeOptions = {}
): SemanticModule {
  const values: Record<string, ValueInfo> = {};
  const annotations: Record<string, TypeAnnotationDeclaration> = {};
  const imports: ImportDeclaration[] = program.imports ?? [];
  const types: Record<string, Type> = {};
  const substitution: Substitution = new Map();

  // ===== ADT and Type Alias Registries =====
  // These track user-defined types for constructor validation and exhaustiveness checking.
  const adts: Record<string, ADTInfo> = {};
  const constructors: Record<string, ConstructorInfo> = {};
  const typeAliases: Record<string, TypeAliasInfo> = {};

  validateImports(imports);

  const globalScope: Scope = { symbols: new Map() };

  // Seed built-in operators as functions for prefix/infix symmetry.
  // Built-in operators are monomorphic (not polymorphic).
  for (const [op, ty] of Object.entries(INFIX_TYPES)) {
    globalScope.symbols.set(op, { vars: new Set(), type: ty });
  }

  // Merge types from imported dependency modules
  // This replaces the previous approach of seeding placeholder types for imports.
  // Now we use actual types from pre-analyzed dependency modules.
  const { dependencies = new Map() } = options;
  for (const imp of imports) {
    const depModule = dependencies.get(imp.moduleName);
    if (!depModule) {
      // If dependency not provided, seed with placeholder (for backward compatibility)
      if (imp.alias) {
        globalScope.symbols.set(imp.alias, {
          vars: new Set(),
          type: freshType(),
        });
      }
      continue;
    }

    // Handle import alias (e.g., `import Html as H`)
    if (imp.alias) {
      // Create a namespace-like structure for aliased imports
      // For now, we'll use a placeholder type, but this should eventually
      // support qualified name access (e.g., H.div)
      globalScope.symbols.set(imp.alias, {
        vars: new Set(),
        type: freshType(), // TODO: Implement proper module namespace types
      });
    }

    // Handle explicit exposing (e.g., `import Html exposing (div, span)`)
    if (imp.exposing?.kind === "Explicit") {
      for (const exposedName of imp.exposing.names) {
        const depValue = depModule.values[exposedName];
        if (depValue && depValue.type) {
          // Import the type from the dependency
          // Create a fresh type scheme to avoid sharing type variable IDs
          const importedType = depValue.type;
          const scheme = generalize(importedType, globalScope, substitution);
          globalScope.symbols.set(exposedName, scheme);
        }

        // Also check for constructors
        const depConstructor = depModule.constructors[exposedName];
        if (depConstructor) {
          constructors[exposedName] = depConstructor;
        }

        // Check for ADTs
        const depADT = depModule.adts[exposedName];
        if (depADT) {
          adts[exposedName] = depADT;
          // When importing an ADT, also import all its constructors
          for (const ctorName of depADT.constructors) {
            const ctor = depModule.constructors[ctorName];
            if (ctor) {
              constructors[ctorName] = ctor;
            }
          }
        }

        // Check for type aliases
        const depTypeAlias = depModule.typeAliases[exposedName];
        if (depTypeAlias) {
          typeAliases[exposedName] = depTypeAlias;
        }
      }
    }

    // Handle exposing all (e.g., `import Html exposing (..)`)
    if (imp.exposing?.kind === "All") {
      // Import all values
      for (const [name, depValue] of Object.entries(depModule.values) as [
        string,
        ValueInfo
      ][]) {
        if (depValue.type) {
          const importedType = depValue.type;
          const scheme = generalize(importedType, globalScope, substitution);
          globalScope.symbols.set(name, scheme);
        }
      }

      // Import all constructors
      for (const [name, ctor] of Object.entries(depModule.constructors) as [
        string,
        ConstructorInfo
      ][]) {
        constructors[name] = ctor;
      }

      // Import all ADTs
      for (const [name, adt] of Object.entries(depModule.adts) as [
        string,
        ADTInfo
      ][]) {
        adts[name] = adt;
      }

      // Import all type aliases
      for (const [name, alias] of Object.entries(depModule.typeAliases) as [
        string,
        TypeAliasInfo
      ][]) {
        typeAliases[name] = alias;
      }
    }
  }

  // ===== PASS 1: Register type declarations (ADTs and aliases) =====
  // We register types before values so constructors can be used in value expressions.
  for (const decl of program.declarations) {
    if (decl.kind === "TypeDeclaration") {
      registerTypeDeclaration(decl, adts, constructors, globalScope);
      continue;
    }

    if (decl.kind === "TypeAliasDeclaration") {
      registerTypeAlias(decl, typeAliases);
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
      if (annotations[decl.name]) {
        throw new SemanticError(
          `Duplicate type annotation for '${decl.name}'`,
          decl.span
        );
      }
      annotations[decl.name] = decl;
    }
  }

  for (const [name, ann] of Object.entries(annotations)) {
    const value = values[name];
    if (!value) {
      throw new SemanticError(
        `Type annotation for '${name}' has no matching definition`,
        ann.span
      );
    }
    if (value.declaration.kind === "ExternalDeclaration") {
      throw new SemanticError(
        `External declaration '${name}' already includes a type annotation`,
        ann.span
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

    if (annotationExpr && info.declaration.kind === "ValueDeclaration") {
      validateAnnotationArity(
        annotationExpr,
        info.declaration.args.length,
        info.declaration.span,
        name
      );
    }

    const annotationType = annotationExpr
      ? typeFromAnnotation(annotationExpr, new Map(), adts, typeAliases)
      : undefined;

    const seeded =
      annotationType ?? seedValueType(info.declaration, adts, typeAliases);
    // Seed with a monomorphic scheme (no quantified variables yet)
    declareSymbol(
      globalScope,
      name,
      { vars: new Set(), type: seeded },
      info.declaration.span
    );
    types[name] = seeded;
  }

  // Infer value bodies and generalize their types ONE AT A TIME.
  // This ensures that each function is fully generalized before the next one uses it.
  // This approach works for non-mutually-recursive definitions.
  // TODO: Handle mutually recursive definition groups properly.
  for (const info of Object.values(values)) {
    if (info.declaration.kind === "ExternalDeclaration") {
      // External declarations with type variables become polymorphic
      // Each external gets a fresh type variable context
      info.type = typeFromAnnotation(
        info.declaration.annotation,
        new Map(),
        adts,
        typeAliases
      );
      // External declarations are monomorphic
      globalScope.symbols.set(info.declaration.name, {
        vars: new Set(),
        type: info.type,
      });
      continue;
    }

    const declaredType = types[info.declaration.name]!;
    const annotationType = info.annotation
      ? typeFromAnnotation(info.annotation, new Map(), adts, typeAliases)
      : undefined;

    const inferred = analyzeValueDeclaration(
      info.declaration,
      globalScope,
      substitution,
      declaredType,
      annotationType,
      constructors,
      adts,
      typeAliases
    );

    // IMPORTANT: Generalize IMMEDIATELY after inference, before analyzing the next declaration.
    // This ensures that when the next declaration uses this one, it gets the polymorphic version.
    // Note: We need to generalize with respect to the scope BEFORE this binding was added.
    // For top-level bindings, we generalize with respect to the initial global scope.
    const generalizedScheme = generalize(
      inferred,
      { symbols: new Map(), parent: globalScope.parent },
      substitution
    );
    globalScope.symbols.set(info.declaration.name, generalizedScheme);

    types[info.declaration.name] = inferred;
    info.type = inferred;
  }

  if (program.module) {
    validateModuleExposing(program.module, values);
  }

  return {
    values,
    annotations,
    module: program.module,
    imports,
    types,
    adts,
    constructors,
    typeAliases,
  };
}

function registerValue(
  values: Record<string, ValueInfo>,
  decl: ValueDeclaration | ExternalDeclaration
) {
  const existing = values[decl.name];
  if (existing) {
    throw new SemanticError(
      `Duplicate definition for '${decl.name}'`,
      decl.span
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
  constructors: Record<string, ConstructorInfo>,
  globalScope: Scope
) {
  // Check for duplicate type name
  if (adts[decl.name]) {
    throw new SemanticError(
      `Duplicate type declaration for '${decl.name}'`,
      decl.span
    );
  }

  // Validate type parameters are unique
  const paramSet = new Set<string>();
  for (const param of decl.params) {
    if (paramSet.has(param)) {
      throw new SemanticError(
        `Duplicate type parameter '${param}' in type '${decl.name}'`,
        decl.span
      );
    }
    paramSet.add(param);
  }

  // Validate at least one constructor
  if (decl.constructors.length === 0) {
    throw new SemanticError(
      `Type '${decl.name}' must have at least one constructor`,
      decl.span
    );
  }

  // Register the ADT
  const constructorNames = decl.constructors.map((c) => c.name);
  adts[decl.name] = {
    name: decl.name,
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
  // For Maybe a, this is: { kind: "con", name: "maybe", args: [TypeVar(a)] }
  const resultType: TypeCon = {
    kind: "con",
    name: decl.name.toLowerCase(),
    args: decl.params.map((p) => paramTypeVars.get(p)!),
  };

  // Register each constructor
  for (const ctor of decl.constructors) {
    // Check for duplicate constructor name (must be unique within module)
    if (constructors[ctor.name]) {
      throw new SemanticError(
        `Duplicate constructor '${ctor.name}' (constructor names must be unique within a module)`,
        ctor.span
      );
    }

    // Also check it doesn't shadow a built-in constructor
    if (BUILTIN_CONSTRUCTORS[ctor.name] !== undefined) {
      throw new SemanticError(
        `Constructor '${ctor.name}' shadows a built-in constructor`,
        ctor.span
      );
    }

    // Register constructor info
    constructors[ctor.name] = {
      arity: ctor.args.length,
      argTypes: ctor.args,
      parentType: decl.name,
      parentParams: decl.params,
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

    // Register constructor as a polymorphic value in global scope
    globalScope.symbols.set(ctor.name, {
      vars: quantifiedVars,
      type: ctorType,
    });
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
function buildConstructorType(
  ctor: ConstructorVariant,
  resultType: TypeCon,
  paramTypeVars: Map<string, TypeVar>
): Type {
  if (ctor.args.length === 0) {
    // Nullary constructor: just return the result type
    return resultType;
  }

  // Convert each argument TypeExpr to internal Type
  const argTypes: Type[] = ctor.args.map((argExpr) =>
    constructorArgToType(argExpr, paramTypeVars)
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
  paramTypeVars: Map<string, TypeVar>
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
        constructorArgToType(arg, paramTypeVars)
      );
      return {
        kind: "con",
        name: expr.name.toLowerCase(),
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
          constructorArgToType(el, paramTypeVars)
        ),
      };
    }
  }
}

/**
 * Register a type alias declaration.
 *
 * Type aliases don't introduce new constructors, they just create
 * a new name for an existing type expression.
 *
 * For example, `type alias UserId = number` allows using "UserId"
 * anywhere "number" is expected.
 */
function registerTypeAlias(
  decl: TypeAliasDeclaration,
  typeAliases: Record<string, TypeAliasInfo>
) {
  // Check for duplicate alias name
  if (typeAliases[decl.name]) {
    throw new SemanticError(`Duplicate type alias '${decl.name}'`, decl.span);
  }

  // Validate type parameters are unique
  const paramSet = new Set<string>();
  for (const param of decl.params) {
    if (paramSet.has(param)) {
      throw new SemanticError(
        `Duplicate type parameter '${param}' in type alias '${decl.name}'`,
        decl.span
      );
    }
    paramSet.add(param);
  }

  // Register the alias
  typeAliases[decl.name] = {
    name: decl.name,
    params: decl.params,
    value: decl.value,
    span: decl.span,
  };
}

function validateModuleExposing(
  moduleDecl: ModuleDeclaration,
  values: Record<string, ValueInfo>
) {
  if (moduleDecl.exposing?.kind !== "Explicit") {
    return;
  }
  for (const name of moduleDecl.exposing.names) {
    if (!values[name]) {
      throw new SemanticError(
        `Module exposes '${name}' which is not defined`,
        moduleDecl.exposing.span
      );
    }
  }
}

function validateImports(imports: ImportDeclaration[]) {
  const byModule = new Map<string, ImportDeclaration>();
  const byAlias = new Map<string, ImportDeclaration>();

  for (const imp of imports) {
    const duplicateModule = byModule.get(imp.moduleName);
    if (duplicateModule) {
      throw new SemanticError(
        `Duplicate import of module '${imp.moduleName}'`,
        imp.span
      );
    }
    byModule.set(imp.moduleName, imp);

    if (imp.alias) {
      const duplicateAlias = byAlias.get(imp.alias);
      if (duplicateAlias) {
        throw new SemanticError(
          `Duplicate import alias '${imp.alias}'`,
          imp.span
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
  typeAliases: Record<string, TypeAliasInfo>
): Type {
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
    adts
  );

  const bodyType = analyzeExpr(
    decl.body,
    fnScope,
    substitution,
    globalScope,
    constructors,
    adts,
    typeAliases
  );
  unify(bodyType, returnType, decl.body.span, substitution);

  return applySubstitution(expected, substitution);
}

function analyzeExpr(
  expr: Expr,
  scope: Scope,
  substitution: Substitution,
  globalScope: Scope,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>
): Type {
  switch (expr.kind) {
    case "Var": {
      const resolved = lookupSymbol(scope, expr.name, expr.span, substitution);
      return applySubstitution(resolved, substitution);
    }
    case "Number":
      return tNumber;
    case "String":
      return tString;
    case "Char":
      return tChar;
    case "Unit":
      return tUnit;
    case "Tuple": {
      const elements = expr.elements.map((el) =>
        analyzeExpr(
          el,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases
        )
      );
      return { kind: "tuple", elements };
    }
    case "List": {
      if (expr.elements.length === 0) {
        return { kind: "list", element: freshType() };
      }
      const first = analyzeExpr(
        expr.elements[0]!,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
      );
      for (const el of expr.elements.slice(1)) {
        const elType = analyzeExpr(
          el,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases
        );
        unify(first, elType, el.span, substitution);
      }
      return { kind: "list", element: applySubstitution(first, substitution) };
    }
    case "ListRange": {
      const startType = analyzeExpr(
        expr.start,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
      );
      const endType = analyzeExpr(
        expr.end,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
      );
      unify(startType, endType, expr.span, substitution);
      return {
        kind: "list",
        element: applySubstitution(startType, substitution),
      };
    }
    case "Record": {
      const fields: Record<string, Type> = {};
      for (const field of expr.fields) {
        if (fields[field.name]) {
          throw new SemanticError(
            `Duplicate record field '${field.name}'`,
            field.span
          );
        }
        fields[field.name] = analyzeExpr(
          field.value,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases
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
          expr.span
        );
      }
      const updatedFields: Record<string, Type> = { ...concreteBase.fields };
      for (const field of expr.fields) {
        if (!updatedFields[field.name]) {
          throw new SemanticError(
            `Record '${expr.base}' has no field '${field.name}'`,
            field.span
          );
        }
        const fieldType = analyzeExpr(
          field.value,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases
        );
        unify(updatedFields[field.name]!, fieldType, field.span, substitution);
        updatedFields[field.name] = applySubstitution(
          updatedFields[field.name]!,
          substitution
        );
      }
      return { kind: "record", fields: updatedFields };
    }
    case "FieldAccess": {
      const targetType = analyzeExpr(
        expr.target,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
      );
      const concrete = applySubstitution(targetType, substitution);
      if (concrete.kind !== "record") {
        throw new SemanticError(
          `Cannot access field '${expr.field}' on non-record value`,
          expr.span
        );
      }
      const fieldType = concrete.fields[expr.field];
      if (!fieldType) {
        throw new SemanticError(
          `Record has no field '${expr.field}'`,
          expr.span
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
        adts
      );
      const bodyType = analyzeExpr(
        expr.body,
        fnScope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
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
        typeAliases
      );
      for (const arg of expr.args) {
        const argType = analyzeExpr(
          arg,
          scope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases
        );
        const resultType = freshType();
        unify(calleeType, fn(argType, resultType), expr.span, substitution);
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
        typeAliases
      );
      unify(condType, tBool, expr.condition.span, substitution);
      const thenType = analyzeExpr(
        expr.thenBranch,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
      );
      const elseType = analyzeExpr(
        expr.elseBranch,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
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
            binding.span
          );
        }
        const seeded = seedValueType(binding, adts, typeAliases);
        // Seed with monomorphic scheme (empty quantifier set)
        declareSymbol(
          letScope,
          binding.name,
          { vars: new Set(), type: seeded },
          binding.span
        );
      }

      // Second pass: analyze each binding and generalize its type
      // This is where let-polymorphism happens for local bindings
      for (const binding of expr.bindings) {
        const declared = lookupSymbol(
          letScope,
          binding.name,
          binding.span,
          substitution
        );
        const inferred = analyzeValueDeclaration(
          binding,
          letScope,
          substitution,
          declared,
          undefined,
          constructors,
          adts,
          typeAliases
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
        typeAliases
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
        typeAliases
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
          adts
        );
        unify(discriminantType, patternType, branch.pattern.span, substitution);
        const bodyType = analyzeExpr(
          branch.body,
          branchScope,
          substitution,
          globalScope,
          constructors,
          adts,
          typeAliases
        );
        branchTypes.push(bodyType);

        if (branch.pattern.kind === "WildcardPattern") {
          hasWildcard = true;
          if (index !== expr.branches.length - 1) {
            throw new SemanticError(
              "Wildcard pattern makes following branches unreachable",
              branch.pattern.span
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
          substitution
        );
        if (!coverage.exhaustive) {
          const missingMsg =
            coverage.missing && coverage.missing.length > 0
              ? ` (missing: ${coverage.missing.join(", ")})`
              : "";
          throw new SemanticError(
            `Non-exhaustive case expression${missingMsg}`,
            expr.span
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
        typeAliases
      );
      const rightType = analyzeExpr(
        expr.right,
        scope,
        substitution,
        globalScope,
        constructors,
        adts,
        typeAliases
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
          substitution
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
        typeAliases
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
        typeAliases
      );
    default: {
      const _exhaustive: never = expr;
      throw new SemanticError(
        "Unsupported expression",
        (expr as { span: Span }).span
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
  substitution: Substitution
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
    // Look up ADT by normalized type name
    for (const [name, adt] of Object.entries(adts)) {
      if (
        name.toLowerCase() === concreteType.name ||
        concreteType.name === name.toLowerCase()
      ) {
        if (!adtName) {
          adtName = name;
        }
        break;
      }
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

function bindPatterns(
  scope: Scope,
  patterns: Pattern[],
  paramTypes: Type[],
  substitution: Substitution,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>
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
      adts
    );
    if (pattern.kind === "VarPattern") {
      // Pattern variables are monomorphic
      scope.symbols.set(pattern.name, { vars: new Set(), type: paramType });
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
  adts: Record<string, ADTInfo>
): Type {
  switch (pattern.kind) {
    case "VarPattern": {
      if (seen.has(pattern.name)) {
        throw new SemanticError(
          `Duplicate pattern variable '${pattern.name}'`,
          pattern.span
        );
      }
      seen.add(pattern.name);
      // Pattern variables are monomorphic (not generalized)
      // This follows the let-polymorphism discipline where only let-bound names are polymorphic
      declareSymbol(
        scope,
        pattern.name,
        { vars: new Set(), type: expected },
        pattern.span
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
        substitution
      );
      pattern.elements.forEach((el, idx) =>
        bindPattern(
          el,
          scope,
          substitution,
          seen,
          subTypes[idx]!,
          constructors,
          adts
        )
      );
      return applySubstitution(
        { kind: "tuple", elements: subTypes },
        substitution
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
          name: ctorInfo.parentType.toLowerCase(),
          args: ctorInfo.parentParams.map((p) => paramTypeVars.get(p)!),
        };

        // Build types for constructor arguments
        const argTypes: Type[] = ctorInfo.argTypes.map((argExpr) =>
          constructorArgToType(argExpr, paramTypeVars)
        );

        // Unify the result type with the expected type to bind type parameters
        unify(resultType, expected, pattern.span, substitution);

        // Validate we have the right number of argument patterns
        if (pattern.args.length !== argTypes.length) {
          throw new SemanticError(
            `Constructor '${pattern.name}' expects ${argTypes.length} argument(s), got ${pattern.args.length}`,
            pattern.span
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
            adts
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
            adts
          )
        );
        return applySubstitution(expected, substitution);
      }
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
  constructors: Record<string, ConstructorInfo>
) {
  // First check user-defined constructors
  const ctorInfo = constructors[pattern.name];
  if (ctorInfo) {
    if (ctorInfo.arity !== pattern.args.length) {
      throw new SemanticError(
        `Constructor '${pattern.name}' expects ${ctorInfo.arity} argument(s), got ${pattern.args.length}`,
        pattern.span
      );
    }
    return;
  }

  // Fall back to built-in constructors
  const expected = BUILTIN_CONSTRUCTORS[pattern.name];
  if (expected !== undefined && expected !== pattern.args.length) {
    throw new SemanticError(
      `Constructor '${pattern.name}' expects ${expected} argument(s)`,
      pattern.span
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
  span: Span
) {
  if (scope.symbols.has(name)) {
    throw new SemanticError(`Duplicate definition for '${name}'`, span);
  }
  scope.symbols.set(name, scheme);
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
  substitution: Substitution
): Type {
  if (scope.symbols.has(name)) {
    const scheme = scope.symbols.get(name)!;
    // Instantiate the scheme to get a fresh type for this use site
    return instantiate(scheme, substitution);
  }
  if (scope.parent) {
    return lookupSymbol(scope.parent, name, span, substitution);
  }
  throw new SemanticError(`Undefined name '${name}'`, span);
}

function seedValueType(
  decl: ValueDeclaration | ExternalDeclaration,
  adts: Record<string, ADTInfo>,
  typeAliases: Record<string, TypeAliasInfo>
): Type {
  if (decl.kind === "ExternalDeclaration") {
    return typeFromAnnotation(decl.annotation, new Map(), adts, typeAliases);
  }
  const argTypes = decl.args.map(() => freshType());
  const resultType = freshType();
  return fnChain(argTypes, resultType);
}

function freshType(): TypeVar {
  return { kind: "var", id: nextTypeVarId++ };
}

/**
 * Compute the set of free type variables in a type.
 * A type variable is "free" if it appears in the type and isn't bound by a quantifier.
 * This is used during generalization to determine which type variables should be quantified.
 *
 * Example:
 * - getFreeTypeVars(number) = {}
 * - getFreeTypeVars(a -> b) = {a.id, b.id}
 * - getFreeTypeVars([a]) = {a.id}
 */
function getFreeTypeVars(type: Type, substitution: Substitution): Set<number> {
  const concrete = applySubstitution(type, substitution);

  if (concrete.kind === "var") {
    return new Set([concrete.id]);
  }

  if (concrete.kind === "con") {
    const result = new Set<number>();
    for (const arg of concrete.args) {
      for (const v of getFreeTypeVars(arg, substitution)) {
        result.add(v);
      }
    }
    return result;
  }

  if (concrete.kind === "fun") {
    const result = new Set<number>();
    for (const v of getFreeTypeVars(concrete.from, substitution)) {
      result.add(v);
    }
    for (const v of getFreeTypeVars(concrete.to, substitution)) {
      result.add(v);
    }
    return result;
  }

  if (concrete.kind === "tuple") {
    const result = new Set<number>();
    for (const el of concrete.elements) {
      for (const v of getFreeTypeVars(el, substitution)) {
        result.add(v);
      }
    }
    return result;
  }

  if (concrete.kind === "record") {
    const result = new Set<number>();
    for (const fieldType of Object.values(concrete.fields)) {
      for (const v of getFreeTypeVars(fieldType, substitution)) {
        result.add(v);
      }
    }
    return result;
  }

  if (concrete.kind === "list") {
    return getFreeTypeVars(concrete.element, substitution);
  }

  return new Set();
}

/**
 * Compute the set of free type variables in a scope.
 * This includes all type variables that appear in any type scheme in the scope
 * but are NOT quantified by that scheme.
 *
 * Used during generalization to avoid quantifying over variables that are
 * already bound in the enclosing scope.
 */
function getFreeTypeVarsInScope(
  scope: Scope,
  substitution: Substitution
): Set<number> {
  const result = new Set<number>();

  // Collect free variables from this scope
  for (const scheme of scope.symbols.values()) {
    const typeFree = getFreeTypeVars(scheme.type, substitution);
    for (const v of typeFree) {
      // Only include variables that are NOT quantified in the scheme
      if (!scheme.vars.has(v)) {
        result.add(v);
      }
    }
  }

  // Recursively collect from parent scope
  if (scope.parent) {
    for (const v of getFreeTypeVarsInScope(scope.parent, substitution)) {
      result.add(v);
    }
  }

  return result;
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
  substitution: Substitution
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

  return { vars: quantified, type };
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
  // If no variables are quantified, the type is monomorphic - return as-is
  if (scheme.vars.size === 0) {
    return scheme.type;
  }

  // Create fresh type variables for each quantified variable
  const instantiationMap = new Map<number, Type>();
  for (const varId of scheme.vars) {
    instantiationMap.set(varId, freshType());
  }

  // Apply the instantiation to get a fresh copy of the type
  return instantiateType(scheme.type, instantiationMap, substitution);
}

/**
 * Helper function to apply an instantiation mapping to a type.
 * Recursively replaces quantified variables with their fresh instantiations.
 */
function instantiateType(
  type: Type,
  instantiationMap: Map<number, Type>,
  substitution: Substitution
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
        instantiateType(arg, instantiationMap, substitution)
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
        instantiateType(el, instantiationMap, substitution)
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

  if (concrete.kind === "list") {
    return {
      kind: "list",
      element: instantiateType(
        concrete.element,
        instantiationMap,
        substitution
      ),
    };
  }

  return concrete;
}

function fn(a: Type, b: Type, c?: Type): TypeFun {
  if (c) {
    return fn(a, fn(b, c));
  }
  return { kind: "fun", from: a, to: b };
}

function fnChain(args: Type[], result: Type): Type {
  return args.reduceRight((acc, arg) => fn(arg, acc), result);
}

function validateAnnotationArity(
  annotation: TypeExpr,
  argCount: number,
  span: Span,
  name: string
) {
  const paramCount = countAnnotationParams(annotation);
  if (paramCount !== argCount) {
    throw new SemanticError(
      `Type annotation for '${name}' does not match its argument count`,
      span
    );
  }
}

function extractAnnotationParams(
  annotation: Type,
  argCount: number,
  span: Span
): Type[] {
  const params = flattenFunctionParams(annotation);
  if (params.length !== argCount) {
    throw new SemanticError(
      `Type annotation expects ${params.length} argument(s), but definition has ${argCount}`,
      span
    );
  }
  return params.slice(0, argCount);
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

function flattenFunctionParams(type: Type): Type[] {
  const params: Type[] = [];
  let current: Type = type;
  while (current.kind === "fun") {
    params.push(current.from);
    current = current.to;
  }
  return params;
}

function countAnnotationParams(annotation: TypeExpr): number {
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
 * Check if a type name should be treated as a type variable.
 * Type variables are:
 * - Lowercase identifiers (first character is lowercase)
 * - Typically single letters (a, b, c) but can be longer (result, error)
 *
 * Examples:
 * - "a", "b", "c" -> type variables
 * - "number", "string", "bool" -> concrete types (but also lowercase!)
 *
 * To distinguish, we use a heuristic: single lowercase letters are type variables,
 * longer lowercase names are concrete types. This matches Elm/Haskell convention.
 */
function isTypeVariable(name: string): boolean {
  // Type variables are lowercase AND either:
  // - Single character (a, b, c)
  // - Multiple characters but not a known concrete type
  if (name.length === 0) return false;

  const firstChar = name[0]!; // Safe because length > 0
  if (firstChar !== firstChar.toLowerCase()) return false; // Not lowercase

  // Single letter lowercase = type variable
  if (name.length === 1) return true;

  // Multi-letter: check against known concrete types
  // This allows "number", "string", "bool" to remain concrete
  // while "result", "error" can be type variables
  const knownConcreteTypes = new Set([
    "number",
    "string",
    "bool",
    "char",
    "list",
  ]);
  if (knownConcreteTypes.has(name.toLowerCase())) return false;

  // Default: multi-letter lowercase names are type variables
  // This allows descriptive type variables like "error", "value", etc.
  return true;
}

/**
 * Convert a type expression (from source annotation) to an internal type.
 * Handles type variables by maintaining a context of variable names to TypeVar IDs.
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
  typeAliases: Record<string, TypeAliasInfo> = {}
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
        // Create a context mapping alias params to the provided type arguments
        const aliasContext: TypeVarContext = new Map(context);

        if (annotation.args.length !== aliasInfo.params.length) {
          // Mismatch in type arguments - just fall through to treat as type constructor
        } else {
          // Map each alias parameter to the corresponding argument type
          for (let i = 0; i < aliasInfo.params.length; i++) {
            const paramName = aliasInfo.params[i]!;
            const argType = typeFromAnnotation(
              annotation.args[i]!,
              context,
              adts,
              typeAliases
            );
            // Convert the Type back to a TypeVar for the context (if it's a var)
            if (argType.kind === "var") {
              aliasContext.set(paramName, argType);
            } else {
              // For non-variable types, we need a different approach
              // Create a fresh var and immediately substitute it
              const fresh = freshType();
              aliasContext.set(paramName, fresh);
            }
          }

          // Recursively convert the alias's value with the new context
          return typeFromAnnotation(
            aliasInfo.value,
            aliasContext,
            adts,
            typeAliases
          );
        }
      }

      // Concrete type constructor (Number, String, List, Maybe, etc.)
      const normalized = annotation.name.toLowerCase();
      if (normalized === "list" && annotation.args.length === 1) {
        const elementAnn = annotation.args[0];
        if (elementAnn) {
          return {
            kind: "list",
            element: typeFromAnnotation(elementAnn, context, adts, typeAliases),
          };
        }
      }
      return {
        kind: "con",
        name: normalized,
        args: annotation.args.map((arg) =>
          typeFromAnnotation(arg, context, adts, typeAliases)
        ),
      };
    }
    case "FunctionType": {
      const from = typeFromAnnotation(
        annotation.from,
        context,
        adts,
        typeAliases
      );
      const to = typeFromAnnotation(annotation.to, context, adts, typeAliases);
      return fn(from, to);
    }
    case "TupleType": {
      return {
        kind: "tuple",
        elements: annotation.elements.map((el) =>
          typeFromAnnotation(el, context, adts, typeAliases)
        ),
      };
    }
  }
}

function applySubstitution(type: Type, substitution: Substitution): Type {
  if (type.kind === "var") {
    const replacement = substitution.get(type.id);
    return replacement ? applySubstitution(replacement, substitution) : type;
  }
  if (type.kind === "fun") {
    return fn(
      applySubstitution(type.from, substitution),
      applySubstitution(type.to, substitution)
    );
  }
  if (type.kind === "tuple") {
    return {
      kind: "tuple",
      elements: type.elements.map((t) => applySubstitution(t, substitution)),
    };
  }
  if (type.kind === "record") {
    const fields: Record<string, Type> = {};
    for (const [k, v] of Object.entries(type.fields)) {
      fields[k] = applySubstitution(v, substitution);
    }
    return { kind: "record", fields };
  }
  if (type.kind === "list") {
    return {
      kind: "list",
      element: applySubstitution(type.element, substitution),
    };
  }
  if (type.kind === "con") {
    return {
      kind: "con",
      name: type.name,
      args: type.args.map((t) => applySubstitution(t, substitution)),
    };
  }
  return type;
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
        occursIn(id, t, substitution)
      );
    case "list":
      return occursIn(id, concrete.element, substitution);
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
      throw new SemanticError("Type mismatch", span);
    }
    left.args.forEach((arg, idx) =>
      unify(arg, right.args[idx]!, span, substitution)
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
      unify(el, right.elements[idx]!, span, substitution)
    );
    return;
  }

  if (left.kind === "record" && right.kind === "record") {
    const shared = Object.keys(left.fields).filter(
      (k) => right.fields[k] !== undefined
    );
    for (const key of shared) {
      unify(left.fields[key]!, right.fields[key]!, span, substitution);
    }
    // Row-typed approximation: allow extra fields on either side.
    return;
  }

  if (left.kind === "list" && right.kind === "list") {
    unify(left.element, right.element, span, substitution);
    return;
  }

  throw new SemanticError("Type mismatch", span);
}

function typesEqual(a: Type, b: Type): boolean {
  if (a.kind !== b.kind) return false;
  switch (a.kind) {
    case "var":
      return (b as TypeVar).id === a.id;
    case "con":
      return (
        (b as TypeCon).name === a.name &&
        a.args.length === (b as TypeCon).args.length &&
        a.args.every((arg, idx) => typesEqual(arg, (b as TypeCon).args[idx]!))
      );
    case "fun":
      return (
        typesEqual(a.from, (b as TypeFun).from) &&
        typesEqual(a.to, (b as TypeFun).to)
      );
    case "tuple":
      return (
        a.elements.length === (b as TypeTuple).elements.length &&
        a.elements.every((el, idx) =>
          typesEqual(el, (b as TypeTuple).elements[idx]!)
        )
      );
    case "record":
      return (
        Object.keys(a.fields).length ===
          Object.keys((b as TypeRecord).fields).length &&
        Object.keys(a.fields).every((k) =>
          a.fields[k] !== undefined && (b as TypeRecord).fields[k] !== undefined
            ? typesEqual(a.fields[k]!, (b as TypeRecord).fields[k]!)
            : false
        )
      );
    case "list":
      return typesEqual(a.element, (b as TypeList).element);
  }
}
