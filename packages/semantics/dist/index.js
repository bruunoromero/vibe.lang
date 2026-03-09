// @bun
// ../syntax/src/operators.ts
var OPERATOR_CHARS = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));
var BUILTIN_OPERATORS = [
  {
    symbol: "&&",
    fixity: { associativity: "right", precedence: 3 },
    isShortCircuit: true,
    helper: { name: "_AMP_AMP", impl: "(a) => (b) => a && b()" }
  },
  {
    symbol: "||",
    fixity: { associativity: "right", precedence: 2 },
    isShortCircuit: true,
    helper: { name: "_PIPE_PIPE", impl: "(a) => (b) => a || b()" }
  }
];
var SHORT_CIRCUIT_OPERATORS = new Set(BUILTIN_OPERATORS.filter((op) => op.isShortCircuit).map((op) => op.symbol));
var BUILTIN_OPERATOR_FIXITY = Object.fromEntries(BUILTIN_OPERATORS.map((op) => [op.symbol, op.fixity]));
var SHORT_CIRCUIT_HELPERS = Object.fromEntries(BUILTIN_OPERATORS.filter((op) => op.helper).map((op) => [
  op.symbol,
  op.helper
]));
// ../syntax/src/index.ts
var KEYWORDS = [
  "if",
  "then",
  "else",
  "let",
  "in",
  "case",
  "of",
  "type",
  "alias",
  "module",
  "import",
  "exposing",
  "as",
  "port",
  "infix",
  "infixl",
  "infixr",
  "protocol",
  "implement",
  "where"
];
var KEYWORD_SET = new Set(KEYWORDS);
var BUILTIN_MODULE_NAME = "__builtin__";

// src/errors.ts
class SemanticError extends Error {
  span;
  filePath;
  constructor(message, span, filePath) {
    super(message);
    this.span = span;
    this.filePath = filePath;
  }
}

class ImplementingProtocolError extends SemanticError {
  typeName;
  protocolName;
  methodsWithoutDefaults;
  methodsWithDefaults;
  filePath;
  constructor(typeName, protocolName, methodsWithoutDefaults, methodsWithDefaults, span, filePath) {
    const missing = methodsWithoutDefaults.join(", ");
    const hasDefaults = methodsWithDefaults.length > 0 ? `
  Methods with defaults: ${methodsWithDefaults.join(", ")}` : "";
    const message = `Cannot use 'implementing' with protocol '${protocolName}' for type '${typeName}'
` + `  Protocol '${protocolName}' has methods without default implementations
` + `  Missing defaults for: ${missing}${hasDefaults}

` + `Hint: Either add default implementations for these methods in the protocol,
` + `      or write a manual 'implement' block for this type.`;
    super(message, span, filePath);
    this.typeName = typeName;
    this.protocolName = protocolName;
    this.methodsWithoutDefaults = methodsWithoutDefaults;
    this.methodsWithDefaults = methodsWithDefaults;
    this.filePath = filePath;
    this.name = "ImplementingProtocolError";
  }
}

class MultipleSemanticErrors extends Error {
  errors;
  constructor(errors) {
    const count = errors.length;
    const summary = count === 1 ? errors[0].message : `${count} semantic error(s) found`;
    super(summary);
    this.errors = errors;
    this.name = "MultipleSemanticErrors";
  }
}
// src/types.ts
class Scope {
  _parent;
  _symbols;
  constructor(parent) {
    this._parent = parent;
    this._symbols = new Map;
  }
  get parent() {
    return this._parent;
  }
  get symbols() {
    return this._symbols;
  }
  define(name, scheme) {
    this._symbols.set(name, scheme);
  }
  has(name) {
    return this._symbols.has(name);
  }
  get(name) {
    return this._symbols.get(name);
  }
  lookup(name) {
    if (this._symbols.has(name)) {
      return this._symbols.get(name);
    }
    if (this._parent) {
      return this._parent.lookup(name);
    }
    return;
  }
  child() {
    return new Scope(this);
  }
  getLocalNames() {
    return Array.from(this._symbols.keys());
  }
}

class RegistryManager {
  adts = {};
  constructors = {};
  constructorTypes = {};
  typeAliases = {};
  records = {};
  opaqueTypes = {};
  protocols = {};
  instances = [];
  operators = new Map;
  values = {};
  typeSchemes = {};
  types = {};
  annotations = {};
  localInstances = [];
  infixDeclarations = [];
  localProtocolMethods = new Set;
  importedValues = new Map;
  registerADT(info) {
    this.adts[info.name] = info;
  }
  registerConstructor(name, info) {
    this.constructors[name] = info;
  }
  registerConstructorType(name, scheme) {
    this.constructorTypes[name] = scheme;
  }
  registerTypeAlias(info) {
    this.typeAliases[info.name] = info;
  }
  registerRecord(info) {
    this.records[info.name] = info;
  }
  registerOpaqueType(info) {
    this.opaqueTypes[info.name] = info;
  }
  registerProtocol(info) {
    this.protocols[info.name] = info;
  }
  registerInstance(info) {
    this.instances.push(info);
  }
  registerOperator(name, info) {
    this.operators.set(name, info);
  }
  registerValue(name, info) {
    this.values[name] = info;
  }
  registerTypeScheme(name, scheme) {
    this.typeSchemes[name] = scheme;
  }
  registerType(name, type) {
    this.types[name] = type;
  }
  hasADT(name) {
    return name in this.adts;
  }
  hasConstructor(name) {
    return name in this.constructors;
  }
  hasTypeAlias(name) {
    return name in this.typeAliases;
  }
  hasOpaqueType(name) {
    return name in this.opaqueTypes;
  }
  hasProtocol(name) {
    return name in this.protocols;
  }
  hasRecord(name) {
    return name in this.records;
  }
  getADT(name) {
    return this.adts[name];
  }
  getConstructor(name) {
    return this.constructors[name];
  }
  getConstructorType(name) {
    return this.constructorTypes[name];
  }
  getTypeAlias(name) {
    return this.typeAliases[name];
  }
  getRecord(name) {
    return this.records[name];
  }
  getOpaqueType(name) {
    return this.opaqueTypes[name];
  }
  getProtocol(name) {
    return this.protocols[name];
  }
  registerAnnotation(name, ann) {
    this.annotations[name] = ann;
  }
  getAnnotation(name) {
    return this.annotations[name];
  }
  registerLocalInstance(info) {
    this.localInstances.push(info);
  }
  registerInfixDeclaration(decl) {
    this.infixDeclarations.push(decl);
  }
  registerLocalProtocolMethod(name) {
    this.localProtocolMethods.add(name);
  }
  hasLocalProtocolMethod(name) {
    return this.localProtocolMethods.has(name);
  }
  registerImportedValue(name, moduleName) {
    this.importedValues.set(name, moduleName);
  }
  getImportedValueSource(name) {
    return this.importedValues.get(name);
  }
  isValueImported(name) {
    return this.importedValues.has(name);
  }
}
// src/utils.ts
var nextTypeVarId = 0;
function freshType() {
  return { kind: "var", id: nextTypeVarId++ };
}
function listType(element) {
  return { kind: "con", name: "List", args: [element] };
}
function fn(a, b, c) {
  if (c) {
    return fn(a, fn(b, c));
  }
  return { kind: "fun", from: a, to: b };
}
function fnChain(args, result) {
  return args.reduceRight((acc, arg) => fn(arg, acc), result);
}
function typesEqual(t1, t2) {
  if (t1.kind !== t2.kind)
    return false;
  switch (t1.kind) {
    case "var":
      return t2.id === t1.id;
    case "con":
      return t2.name === t1.name && t1.args.length === t2.args.length && t1.args.every((a, i) => typesEqual(a, t2.args[i]));
    case "fun":
      return typesEqual(t1.from, t2.from) && typesEqual(t1.to, t2.to);
    case "tuple":
      return t1.elements.length === t2.elements.length && t1.elements.every((e, i) => typesEqual(e, t2.elements[i]));
    case "record": {
      const r2 = t2;
      const keys1 = Object.keys(t1.fields).sort();
      const keys2 = Object.keys(r2.fields).sort();
      return keys1.length === keys2.length && keys1.every((k, i) => k === keys2[i] && typesEqual(t1.fields[k], r2.fields[k]));
    }
    case "error":
      return true;
  }
}
function applySubstitution(type, substitution) {
  if (type.kind === "var") {
    const replacement = substitution.get(type.id);
    return replacement ? applySubstitution(replacement, substitution) : type;
  }
  if (type.kind === "fun") {
    return fn(applySubstitution(type.from, substitution), applySubstitution(type.to, substitution));
  }
  if (type.kind === "tuple") {
    return {
      kind: "tuple",
      elements: type.elements.map((t) => applySubstitution(t, substitution))
    };
  }
  if (type.kind === "record") {
    const fields = {};
    for (const [k, v] of Object.entries(type.fields)) {
      fields[k] = applySubstitution(v, substitution);
    }
    return { kind: "record", fields };
  }
  if (type.kind === "con") {
    return {
      kind: "con",
      name: type.name,
      args: type.args.map((t) => applySubstitution(t, substitution))
    };
  }
  return type;
}
function applySubstitutionToConstraints(constraints, substitution) {
  return constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => applySubstitution(t, substitution))
  }));
}
function getFreeTypeVars(type, substitution) {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
    return new Set([concrete.id]);
  }
  if (concrete.kind === "con") {
    const result = new Set;
    for (const arg of concrete.args) {
      for (const v of getFreeTypeVars(arg, substitution)) {
        result.add(v);
      }
    }
    return result;
  }
  if (concrete.kind === "fun") {
    const result = new Set;
    for (const v of getFreeTypeVars(concrete.from, substitution)) {
      result.add(v);
    }
    for (const v of getFreeTypeVars(concrete.to, substitution)) {
      result.add(v);
    }
    return result;
  }
  if (concrete.kind === "tuple") {
    const result = new Set;
    for (const el of concrete.elements) {
      for (const v of getFreeTypeVars(el, substitution)) {
        result.add(v);
      }
    }
    return result;
  }
  if (concrete.kind === "record") {
    const result = new Set;
    for (const fieldType of Object.values(concrete.fields)) {
      for (const v of getFreeTypeVars(fieldType, substitution)) {
        result.add(v);
      }
    }
    return result;
  }
  return new Set;
}
function getFreeTypeVarsInScope(scope, substitution) {
  const result = new Set;
  for (const scheme of scope.symbols.values()) {
    const typeFree = getFreeTypeVars(scheme.type, substitution);
    for (const v of typeFree) {
      if (!scheme.vars.has(v)) {
        result.add(v);
      }
    }
  }
  if (scope.parent) {
    for (const v of getFreeTypeVarsInScope(scope.parent, substitution)) {
      result.add(v);
    }
  }
  return result;
}
function collectTypeVarIds(type) {
  const ids = new Set;
  collectTypeVarIdsHelper(type, ids);
  return ids;
}
function collectTypeVarIdsHelper(type, ids) {
  switch (type.kind) {
    case "var":
      ids.add(type.id);
      break;
    case "fun":
      collectTypeVarIdsHelper(type.from, ids);
      collectTypeVarIdsHelper(type.to, ids);
      break;
    case "tuple":
      for (const el of type.elements) {
        collectTypeVarIdsHelper(el, ids);
      }
      break;
    case "con":
      for (const arg of type.args) {
        collectTypeVarIdsHelper(arg, ids);
      }
      break;
    case "record":
      for (const v of Object.values(type.fields)) {
        collectTypeVarIdsHelper(v, ids);
      }
      break;
  }
}
function collectTypeVarIdsOrdered(type, result, seen) {
  switch (type.kind) {
    case "var":
      if (!seen.has(type.id)) {
        seen.add(type.id);
        result.push(type.id);
      }
      break;
    case "con":
      for (const arg of type.args) {
        collectTypeVarIdsOrdered(arg, result, seen);
      }
      break;
    case "fun":
      collectTypeVarIdsOrdered(type.from, result, seen);
      collectTypeVarIdsOrdered(type.to, result, seen);
      break;
    case "tuple":
      for (const el of type.elements) {
        collectTypeVarIdsOrdered(el, result, seen);
      }
      break;
    case "record":
      for (const v of Object.values(type.fields)) {
        collectTypeVarIdsOrdered(v, result, seen);
      }
      break;
  }
}
var LETTERS = "abcdefghijklmnopqrstuvwxyz";
function formatType(type) {
  switch (type.kind) {
    case "var":
      return `t${type.id}`;
    case "con":
      if (type.args.length === 0) {
        return type.name;
      }
      return `${type.name} ${type.args.map(formatType).join(" ")}`;
    case "fun":
      const from = type.from.kind === "fun" ? `(${formatType(type.from)})` : formatType(type.from);
      return `${from} -> ${formatType(type.to)}`;
    case "tuple":
      return `(${type.elements.map(formatType).join(", ")})`;
    case "record":
      const fields = Object.entries(type.fields).map(([k, v]) => `${k}: ${formatType(v)}`).join(", ");
      return `{ ${fields} }`;
    case "error":
      return "<error>";
  }
}
function buildNormalizedNames(scheme) {
  const ids = [];
  const seen = new Set;
  for (const c of scheme.constraints) {
    for (const t of c.typeArgs) {
      collectTypeVarIdsOrdered(t, ids, seen);
    }
  }
  collectTypeVarIdsOrdered(scheme.type, ids, seen);
  const map = new Map;
  for (let i = 0;i < ids.length; i++) {
    map.set(ids[i], i < LETTERS.length ? LETTERS[i] : `t${ids[i]}`);
  }
  return map;
}
function formatTypeForDisplay(type, paramNames) {
  switch (type.kind) {
    case "var":
      return paramNames.get(type.id) ?? `t${type.id}`;
    case "con":
      if (type.args.length === 0)
        return type.name;
      return `${type.name} ${type.args.map((a) => formatTypeArgForDisplay(a, paramNames)).join(" ")}`;
    case "fun": {
      const from = formatTypeArgForDisplay(type.from, paramNames);
      return `${from} -> ${formatTypeForDisplay(type.to, paramNames)}`;
    }
    case "tuple":
      return `(${type.elements.map((e) => formatTypeForDisplay(e, paramNames)).join(", ")})`;
    case "record": {
      const fields = Object.entries(type.fields).map(([k, v]) => `${k} : ${formatTypeForDisplay(v, paramNames)}`).join(", ");
      return `{ ${fields} }`;
    }
    case "error":
      return "<error>";
  }
}
function formatTypeArgForDisplay(type, paramNames) {
  if (type.kind === "fun" || type.kind === "con" && type.args.length > 0) {
    return `(${formatTypeForDisplay(type, paramNames)})`;
  }
  return formatTypeForDisplay(type, paramNames);
}
function formatConstraintsForDisplay(constraints, paramNames) {
  if (!constraints || constraints.length === 0)
    return "";
  const parts = constraints.map((c) => {
    const args = c.typeArgs.map((t) => formatTypeForDisplay(t, paramNames)).join(" ");
    return `${c.protocolName} ${args}`;
  });
  if (parts.length === 1)
    return parts[0] ?? "";
  return `(${parts.join(", ")})`;
}
function formatTypeSchemeForDisplay(scheme) {
  if (!scheme || !scheme.type)
    return "<unknown type>";
  const names = scheme.paramNames ?? buildNormalizedNames(scheme);
  const constraintStr = formatConstraintsForDisplay(scheme.constraints, names);
  const typeStr = formatTypeForDisplay(scheme.type, names);
  return constraintStr ? `${constraintStr} => ${typeStr}` : typeStr;
}
function createConstraintContext() {
  return { constraints: [] };
}
function addConstraint(ctx, constraint) {
  const isDuplicate = ctx.constraints.some((c) => c.protocolName === constraint.protocolName && c.typeArgs.length === constraint.typeArgs.length && c.typeArgs.every((t, i) => typesEqual(t, constraint.typeArgs[i])));
  if (!isDuplicate) {
    ctx.constraints.push(constraint);
  }
}
function flattenFunctionParams(type) {
  const params = [];
  let current = type;
  while (current.kind === "fun") {
    params.push(current.from);
    current = current.to;
  }
  return params;
}
function applyVarSubstitution(type, subst) {
  switch (type.kind) {
    case "var": {
      const replacement = subst.get(type.id);
      return replacement ?? type;
    }
    case "fun":
      return {
        kind: "fun",
        from: applyVarSubstitution(type.from, subst),
        to: applyVarSubstitution(type.to, subst)
      };
    case "tuple":
      return {
        kind: "tuple",
        elements: type.elements.map((el) => applyVarSubstitution(el, subst))
      };
    case "con":
      return {
        kind: "con",
        name: type.name,
        args: type.args.map((arg) => applyVarSubstitution(arg, subst))
      };
    case "record":
      const fields = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = applyVarSubstitution(v, subst);
      }
      return { kind: "record", fields };
    default:
      return type;
  }
}
function applyTypeSubstitution(type, substitution) {
  switch (type.kind) {
    case "var": {
      const mapped = substitution.get(type.id);
      return mapped ?? type;
    }
    case "con":
      return {
        kind: "con",
        name: type.name,
        args: type.args.map((arg) => applyTypeSubstitution(arg, substitution))
      };
    case "fun":
      return {
        kind: "fun",
        from: applyTypeSubstitution(type.from, substitution),
        to: applyTypeSubstitution(type.to, substitution)
      };
    case "tuple":
      return {
        kind: "tuple",
        elements: type.elements.map((el) => applyTypeSubstitution(el, substitution))
      };
    case "record": {
      const fields = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = applyTypeSubstitution(v, substitution);
      }
      return { kind: "record", fields };
    }
    case "error":
      return type;
  }
}
// src/builtins.ts
var BUILTIN_CONSTRUCTORS = {
  True: 0,
  False: 0,
  Unit: 0,
  Int: 0,
  Float: 0,
  String: 0,
  Char: 0
};
var BOOL_TYPE = { kind: "con", name: "Bool", args: [] };
var INFIX_TYPES = {
  "&&": {
    kind: "fun",
    from: BOOL_TYPE,
    to: { kind: "fun", from: BOOL_TYPE, to: BOOL_TYPE }
  },
  "||": {
    kind: "fun",
    from: BOOL_TYPE,
    to: { kind: "fun", from: BOOL_TYPE, to: BOOL_TYPE }
  }
};
var BUILTIN_SPAN = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 }
};

// src/exhaustiveness.ts
function checkExhaustiveness(branches, scrutineeType, adts, constructors, resolveConstructor) {
  const resolver = resolveConstructor || ((name) => {
    const info = constructors[name];
    if (info) {
      const adt = adts[info.parentType];
      if (adt)
        return { name, info, adt };
    }
    return;
  });
  const matrix = branches.map((p) => [p]);
  const witness = isUseful(matrix, [{ kind: "WildcardPattern", span: createDummySpan() }], adts, constructors, resolver);
  if (witness) {
    return { exhaustive: false, missing: patternToString(witness[0]) };
  }
  return { exhaustive: true };
}
function isUseful(matrix, vector, adts, constructors, resolver) {
  if (matrix.length === 0) {
    return vector;
  }
  if (vector.length === 0) {
    return null;
  }
  const p = vector[0];
  const restVector = vector.slice(1);
  if (isConstructorLike(p)) {
    const { name, args } = getConstructorDecomposition(p);
    const resolved = resolver(name);
    const canonicalName = resolved ? resolved.name : name;
    const specializedMatrix = specialize(matrix, canonicalName, args.length, adts, constructors, resolver);
    const witness = isUseful(specializedMatrix, [...args, ...restVector], adts, constructors, resolver);
    if (witness) {
      const witnessArgs = witness.slice(0, args.length);
      const witnessRest = witness.slice(args.length);
      return [reconstructConstructor(p, witnessArgs), ...witnessRest];
    }
    return null;
  }
  if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
    const { names: usedConstructors, adt } = getUsedConstructors(matrix, resolver);
    const typeInfo = inferTypeInfo(usedConstructors, adt, adts, constructors, resolver);
    if (typeInfo.isComplete) {
      for (const ctor of typeInfo.allConstructors) {
        const arity = getConstructorArity(ctor, constructors, adts, resolver, typeInfo.adtModuleName);
        const specializedMatrix = specialize(matrix, ctor, arity, adts, constructors, resolver);
        const dummyArgs = Array(arity).fill({ kind: "WildcardPattern", span: createDummySpan() });
        const witness = isUseful(specializedMatrix, [...dummyArgs, ...restVector], adts, constructors, resolver);
        if (witness) {
          const witnessArgs = witness.slice(0, arity);
          const witnessRest = witness.slice(arity);
          const ctorPattern = createConstructorPattern(ctor, witnessArgs);
          return [ctorPattern, ...witnessRest];
        }
      }
      return null;
    } else {
      const defaultMatrix = specializeDefault(matrix);
      const witness = isUseful(defaultMatrix, restVector, adts, constructors, resolver);
      if (witness) {
        const missingCtor = pickMissingConstructor(typeInfo.allConstructors, usedConstructors);
        if (missingCtor) {
          const arity = getConstructorArity(missingCtor, constructors, adts, resolver, typeInfo.adtModuleName);
          const args = Array(arity).fill({ kind: "WildcardPattern", span: createDummySpan() });
          return [createConstructorPattern(missingCtor, args), ...witness];
        } else {
          return [p, ...witness];
        }
      }
      return null;
    }
  }
  return null;
}
function isConstructorLike(p) {
  return p.kind === "ConstructorPattern" || p.kind === "TuplePattern" || p.kind === "ListPattern" || p.kind === "ConsPattern" || p.kind === "IntPattern" || p.kind === "FloatPattern" || p.kind === "StringPattern" || p.kind === "CharPattern";
}
function getConstructorDecomposition(p) {
  switch (p.kind) {
    case "ConstructorPattern":
      return { name: p.name, args: p.args };
    case "TuplePattern":
      return { name: `Tuple${p.elements.length}`, args: p.elements };
    case "ConsPattern":
      return { name: "::", args: [p.head, p.tail] };
    case "ListPattern":
      if (p.elements.length === 0)
        return { name: "[]", args: [] };
      return convertListToCons(p.elements);
    case "IntPattern":
      return { name: `Int:${p.value}`, args: [] };
    case "FloatPattern":
      return { name: `Float:${p.value}`, args: [] };
    case "StringPattern":
      return { name: `String:${p.value}`, args: [] };
    case "CharPattern":
      return { name: `Char:${p.value}`, args: [] };
    default:
      throw new Error(`Unexpected pattern kind in decomposition: ${p.kind}`);
  }
}
function convertListToCons(elements) {
  if (elements.length === 0) {
    return { name: "[]", args: [] };
  }
  const [head, ...tail] = elements;
  const tailPattern = { kind: "ListPattern", elements: tail, span: createDummySpan() };
  return { name: "::", args: [head, tailPattern] };
}
function reconstructConstructor(original, args) {
  if (original.kind === "ConstructorPattern") {
    return { ...original, args };
  }
  if (original.kind === "TuplePattern") {
    return { ...original, elements: args };
  }
  if (original.kind === "ConsPattern") {
    return { ...original, head: args[0], tail: args[1] };
  }
  if (original.kind === "ListPattern") {
    return { kind: "ConsPattern", head: args[0], tail: args[1], span: createDummySpan() };
  }
  return original;
}
function createConstructorPattern(name, args) {
  if (name.startsWith("Tuple")) {
    return { kind: "TuplePattern", elements: args, span: createDummySpan() };
  }
  if (name === "::") {
    return { kind: "ConsPattern", head: args[0], tail: args[1], span: createDummySpan() };
  }
  if (name === "[]") {
    return { kind: "ListPattern", elements: [], span: createDummySpan() };
  }
  return { kind: "ConstructorPattern", name, args, span: createDummySpan() };
}
function specialize(matrix, ctorName, arity, adts, constructors, resolver) {
  const newMatrix = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const p = row[0];
    const rest = row.slice(1);
    if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
      const newArgs = Array(arity).fill({ kind: "WildcardPattern", span: p.span });
      newMatrix.push([...newArgs, ...rest]);
    } else if (isConstructorLike(p)) {
      const { name, args } = getConstructorDecomposition(p);
      const resolved = resolver(name);
      const canonicalName = resolved ? resolved.name : name;
      if (canonicalName === ctorName) {
        newMatrix.push([...args, ...rest]);
      }
    }
  }
  return newMatrix;
}
function specializeDefault(matrix) {
  const newMatrix = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const p = row[0];
    const rest = row.slice(1);
    if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
      newMatrix.push(rest);
    }
  }
  return newMatrix;
}
function getUsedConstructors(matrix, resolver) {
  const used = new Set;
  let adtInfo;
  for (const row of matrix) {
    if (row.length > 0) {
      const p = row[0];
      if (isConstructorLike(p)) {
        const { name } = getConstructorDecomposition(p);
        const resolved = resolver(name);
        if (resolved) {
          used.add(resolved.name);
          if (!adtInfo && resolved.adt)
            adtInfo = resolved.adt;
        } else {
          used.add(name);
        }
      }
    }
  }
  return { names: used, adt: adtInfo };
}
function inferTypeInfo(usedConstructors, adtContext, adts, constructors, resolver) {
  if (usedConstructors.size === 0) {
    return { isComplete: false, allConstructors: [] };
  }
  const firstCtor = usedConstructors.values().next().value;
  if (firstCtor.startsWith("Tuple")) {
    return { isComplete: true, allConstructors: [firstCtor] };
  }
  if (firstCtor === "::" || firstCtor === "[]") {
    return { isComplete: true, allConstructors: ["[]", "::"] };
  }
  if (adtContext) {
    return {
      isComplete: true,
      allConstructors: adtContext.constructors,
      adtModuleName: adtContext.moduleName
    };
  }
  const resolved = resolver(firstCtor);
  if (resolved) {
    const { info: info2, adt } = resolved;
    return {
      isComplete: true,
      allConstructors: adt.constructors,
      adtModuleName: adt.moduleName
    };
  }
  const info = constructors[firstCtor];
  if (info) {
    const adt = adts[info.parentType];
    if (adt) {
      const all = adt.constructors;
      return { isComplete: true, allConstructors: all, adtModuleName: adt.moduleName };
    }
  }
  return { isComplete: false, allConstructors: [] };
}
function getConstructorArity(name, constructors, adts, resolver, moduleName) {
  if (name === "::")
    return 2;
  if (name === "[]")
    return 0;
  if (name.startsWith("Tuple")) {
    const match = name.match(/^Tuple(\d+)$/);
    if (match)
      return parseInt(match[1], 10);
    return 0;
  }
  if (moduleName) {
    const resolved2 = resolver(name, moduleName);
    if (resolved2)
      return resolved2.info.arity;
  }
  const info = constructors[name];
  if (info)
    return info.arity;
  const resolved = resolver(name);
  if (resolved)
    return resolved.info.arity;
  return 0;
}
function pickMissingConstructor(all, used) {
  for (const c of all) {
    if (!used.has(c))
      return c;
  }
  return;
}
function createDummySpan() {
  return { start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } };
}
function patternToString(p) {
  switch (p.kind) {
    case "WildcardPattern":
      return "_";
    case "VarPattern":
      return p.name;
    case "ConstructorPattern":
      if (p.args.length === 0)
        return p.name;
      return `(${p.name} ${p.args.map(patternToString).join(" ")})`;
    case "TuplePattern":
      return `(${p.elements.map(patternToString).join(", ")})`;
    case "ListPattern":
      return `[${p.elements.map(patternToString).join(", ")}]`;
    case "ConsPattern":
      return `(${patternToString(p.head)} :: ${patternToString(p.tail)})`;
    case "RecordPattern":
      const fields = p.fields.map((f) => `${f.name}${f.pattern ? " = " + patternToString(f.pattern) : ""}`);
      return `{ ${fields.join(", ")} }`;
    case "IntPattern":
      return p.value;
    case "FloatPattern":
      return p.value;
    case "StringPattern":
      return p.value;
    case "CharPattern":
      return p.value;
  }
}

// src/index.ts
function makeLambda(args, body, span) {
  if (args.length === 0) {
    return body;
  }
  return {
    kind: "Lambda",
    args,
    body,
    span
  };
}
var ERROR_TYPE = { kind: "error" };
function invertContext(context) {
  const result = new Map;
  for (const [name, typeVar] of context) {
    result.set(typeVar.id, name);
  }
  return result;
}
function resolveParamNames(paramNames, substitution) {
  const result = new Map;
  for (const [id, name] of paramNames) {
    const resolved = applySubstitution({ kind: "var", id }, substitution);
    if (resolved.kind === "var") {
      result.set(resolved.id, name);
    }
  }
  return result;
}

class SemanticAnalyzer {
  program;
  options;
  currentErrors = [];
  errorSignatures = new Set;
  currentConstraintContext = createConstraintContext();
  _pendingProtocolUsages = [];
  _resolvedProtocolUsages = new Map;
  _registry = new RegistryManager;
  _globalScope = new Scope;
  _substitution = new Map;
  _dependencies = new Map;
  constructor(program, options) {
    this.program = program;
    this.options = options;
    this._dependencies = options.dependencies ?? new Map;
  }
  analyze() {
    this.ensureModuleNameConsistency();
    this.initializeBuiltinADTs();
    this.initializeBuiltinOpaqueTypes();
    this.validateImports();
    this.seedBuiltinOperators();
    this.mergeImportedModules();
    this.registerInfixDeclarations();
    this.registerADTTypeDeclarations();
    this.registerOpaqueTypeDeclarations();
    const typeAliasDecls = this.registerTypeAliasDeclarations();
    this.validateTypeAliasDeclarations(typeAliasDecls);
    this.registerProtocolDeclarations();
    this.registerImplementationDeclarations();
    _recordRegistry = this.records;
    _protocolMethodUsages = this._resolvedProtocolUsages;
    _currentModuleName = this.getModuleName();
    this.autoImplementProtocols();
    this.registerValueDeclarations();
    this.validateAnnotationsAndSeedGlobalNames();
    this.inferValueDeclarations();
    this.validateImplementationMethodExpressions();
    this.validateImplementationMethodTypes();
    this.validateInstanceConstraintSatisfiability();
    this.validateConcreteConstraintInstances();
    this.validateProtocolDefaultImplementations();
    const result = this.buildSemanticModule();
    if (this.getErrors().length > 0) {
      throw new MultipleSemanticErrors(this.getErrors());
    }
    return result;
  }
  getFilePath() {
    return this.options.fileContext.filePath;
  }
  getSrcDir() {
    return this.options.fileContext.srcDir;
  }
  getDeclarations() {
    return this.program.declarations;
  }
  getModule() {
    return this.program.module;
  }
  getModuleName() {
    return this.getModule().name;
  }
  get registry() {
    return this._registry;
  }
  get adts() {
    return this._registry.adts;
  }
  get constructors() {
    return this._registry.constructors;
  }
  get constructorTypes() {
    return this._registry.constructorTypes;
  }
  get typeAliases() {
    return this._registry.typeAliases;
  }
  get records() {
    return this._registry.records;
  }
  get opaqueTypes() {
    return this._registry.opaqueTypes;
  }
  get protocols() {
    return this._registry.protocols;
  }
  get instances() {
    return this._registry.instances;
  }
  get operators() {
    return this._registry.operators;
  }
  get values() {
    return this._registry.values;
  }
  get typeSchemes() {
    return this._registry.typeSchemes;
  }
  get types() {
    return this._registry.types;
  }
  get annotations() {
    return this._registry.annotations;
  }
  get localInstances() {
    return this._registry.localInstances;
  }
  get infixDeclarations() {
    return this._registry.infixDeclarations;
  }
  get localProtocolMethods() {
    return this._registry.localProtocolMethods;
  }
  get importedValues() {
    return this._registry.importedValues;
  }
  get globalScope() {
    return this._globalScope;
  }
  createScope() {
    return this._globalScope.child();
  }
  get substitution() {
    return this._substitution;
  }
  get imports() {
    return this.program.imports;
  }
  get dependencies() {
    return this._dependencies;
  }
  getErrors() {
    return this.currentErrors;
  }
  addError(message, span) {
    const signature = `${this.getFilePath()}:${span.start.line}:${span.start.column}`;
    if (!this.errorSignatures.has(signature)) {
      this.errorSignatures.add(signature);
      this.currentErrors.push(new SemanticError(message, span, this.getFilePath()));
    }
  }
  resetErrors() {
    this.currentErrors = [];
    this.errorSignatures = new Set;
  }
  getCollectedConstraints() {
    return this.currentConstraintContext.constraints;
  }
  resetConstraintContext() {
    this.currentConstraintContext = createConstraintContext();
    this._pendingProtocolUsages = [];
  }
  getConstraintContext() {
    return this.currentConstraintContext;
  }
  analyzeExpr(exprArg, contextArg) {
    const { constructors, adts, typeAliases, opaqueTypes, records } = this;
    const { imports, dependencies } = this;
    let expr = exprArg;
    let scope = contextArg.scope;
    let substitution = contextArg.substitution;
    let expectedType = contextArg.expectedType ?? null;
    while (true) {
      switch (expr.kind) {
        case "Var": {
          const { type: resolved, constraints } = this.lookupSymbolWithConstraints(scope, expr.name, expr.span, substitution);
          for (const constraint of constraints) {
            addConstraint(this.getConstraintContext(), constraint);
            this._pendingProtocolUsages.push({ node: expr, constraint });
          }
          return applySubstitution(resolved, substitution);
        }
        case "Number": {
          const hasDecimal = expr.value.includes(".");
          const typeName = hasDecimal ? "Float" : "Int";
          const opaque = opaqueTypes[typeName];
          if (!opaque && !adts[typeName]) {
            throw new SemanticError(`Type '${typeName}' not found. Make sure the prelude is imported.`, expr.span, this.getFilePath());
          }
          return { kind: "con", name: typeName, args: [] };
        }
        case "String": {
          const opaque = opaqueTypes["String"];
          if (!opaque && !adts["String"]) {
            throw new SemanticError("Type 'String' not found. Make sure the prelude is imported.", expr.span, this.getFilePath());
          }
          return { kind: "con", name: "String", args: [] };
        }
        case "Char": {
          const opaque = opaqueTypes["Char"];
          if (!opaque && !adts["Char"]) {
            throw new SemanticError("Type 'Char' not found. Make sure the prelude is imported.", expr.span, this.getFilePath());
          }
          return { kind: "con", name: "Char", args: [] };
        }
        case "Unit": {
          const opaque = opaqueTypes["Unit"];
          if (!opaque && !adts["Unit"]) {
            throw new SemanticError("Type 'Unit' not found. Make sure the prelude is imported.", expr.span, this.getFilePath());
          }
          return { kind: "con", name: "Unit", args: [] };
        }
        case "Tuple": {
          const elements = expr.elements.map((el) => this.analyzeExpr(el, { scope, substitution }));
          return { kind: "tuple", elements };
        }
        case "List": {
          if (expr.elements.length === 0) {
            return listType(freshType());
          }
          const first = this.analyzeExpr(expr.elements[0], {
            scope,
            substitution,
            expectedType: null
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
            substitution
          });
          const endType = this.analyzeExpr(expr.end, { scope, substitution });
          this.unify(startType, endType, expr.span, substitution);
          return listType(applySubstitution(startType, substitution));
        }
        case "Record": {
          const fields = {};
          for (const field of expr.fields) {
            if (Object.hasOwn(fields, field.name)) {
              throw new SemanticError(`Duplicate record field '${field.name}'`, field.span, this.getFilePath());
            }
            fields[field.name] = this.analyzeExpr(field.value, {
              scope,
              substitution
            });
          }
          const literalFieldNames = new Set(Object.keys(fields));
          let targetRecord;
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
              const shape = Object.entries(fields).map(([k, v]) => `${k} : ${formatType(v)}`).join(", ");
              throw new SemanticError(`No record type in scope has shape { ${shape} }. ` + `Define a named record type with these fields.`, expr.span, this.getFilePath());
            }
            if (candidates.length > 1) {
              const names = candidates.map((r) => r.name).join(", ");
              throw new SemanticError(`Ambiguous record literal \u2014 multiple types in scope match this shape: ${names}. ` + `Add a type annotation to disambiguate.`, expr.span, this.getFilePath());
            }
            targetRecord = candidates[0];
          }
          const recordFieldNames = new Set(targetRecord.fields.map((f) => f.name));
          for (const fname of literalFieldNames) {
            if (!recordFieldNames.has(fname)) {
              throw new SemanticError(`Field '${fname}' is not a field of record type '${targetRecord.name}'`, expr.fields.find((f) => f.name === fname).span, this.getFilePath());
            }
          }
          for (const fname of recordFieldNames) {
            if (!literalFieldNames.has(fname)) {
              throw new SemanticError(`Missing field '${fname}' for record type '${targetRecord.name}'`, expr.span, this.getFilePath());
            }
          }
          const paramVars = targetRecord.params.map(() => freshType());
          const resolveCtx = new Map;
          targetRecord.params.forEach((p, i) => {
            resolveCtx.set(p, paramVars[i]);
          });
          for (const fieldInfo of targetRecord.fields) {
            const declaredFieldType = this.typeFromAnnotation(fieldInfo.typeExpr, resolveCtx);
            this.unify(fields[fieldInfo.name], declaredFieldType, expr.fields.find((f) => f.name === fieldInfo.name).span, substitution);
          }
          const resolvedArgs = paramVars.map((v) => applySubstitution(v, substitution));
          return { kind: "con", name: targetRecord.name, args: resolvedArgs };
        }
        case "RecordUpdate": {
          const baseType = this.lookupSymbol(scope, expr.base, expr.span, substitution);
          const concreteBase = applySubstitution(baseType, substitution);
          if (concreteBase.kind === "con") {
            const recordInfo = this.resolveRecordInfo(concreteBase.name);
            if (recordInfo) {
              const paramVars = recordInfo.params.map(() => freshType());
              const resolveCtx = new Map;
              recordInfo.params.forEach((p, i) => {
                resolveCtx.set(p, paramVars[i]);
              });
              paramVars.forEach((v, i) => {
                if (concreteBase.args[i]) {
                  this.unify(v, concreteBase.args[i], expr.span, substitution);
                }
              });
              const recordFieldNames = new Set(recordInfo.fields.map((f) => f.name));
              for (const field of expr.fields) {
                if (!recordFieldNames.has(field.name)) {
                  throw new SemanticError(`Record '${concreteBase.name}' has no field '${field.name}'`, field.span, this.getFilePath());
                }
                const fieldInfo = recordInfo.fields.find((f) => f.name === field.name);
                const declaredFieldType = this.typeFromAnnotation(fieldInfo.typeExpr, resolveCtx);
                const fieldType = this.analyzeExpr(field.value, {
                  scope,
                  substitution
                });
                this.unify(fieldType, declaredFieldType, field.span, substitution);
              }
              const resolvedArgs = paramVars.map((v) => applySubstitution(v, substitution));
              return {
                kind: "con",
                name: concreteBase.name,
                args: resolvedArgs
              };
            }
          }
          if (concreteBase.kind !== "record") {
            throw new SemanticError(`Cannot update non-record '${expr.base}'`, expr.span, this.getFilePath());
          }
          const updatedFields = {
            ...concreteBase.fields
          };
          for (const field of expr.fields) {
            if (!updatedFields[field.name]) {
              throw new SemanticError(`Record '${expr.base}' has no field '${field.name}'`, field.span, this.getFilePath());
            }
            const fieldType = this.analyzeExpr(field.value, {
              scope,
              substitution
            });
            this.unify(updatedFields[field.name], fieldType, field.span, substitution);
            updatedFields[field.name] = applySubstitution(updatedFields[field.name], substitution);
          }
          return { kind: "record", fields: updatedFields };
        }
        case "FieldAccess": {
          const fieldExpr = expr;
          const moduleAccess = tryResolveModuleFieldAccess(fieldExpr, imports, dependencies, substitution);
          if (moduleAccess) {
            return moduleAccess;
          }
          const targetType = this.analyzeExpr(fieldExpr.target, {
            scope,
            substitution
          });
          const concrete = applySubstitution(targetType, substitution);
          if (concrete.kind === "error") {
            return ERROR_TYPE;
          }
          let fieldType;
          if (concrete.kind === "record") {
            fieldType = concrete.fields[fieldExpr.field];
          } else if (concrete.kind === "con") {
            const recordInfo = this.resolveRecordInfo(concrete.name);
            if (recordInfo) {
              const fieldInfo = recordInfo.fields.find((f) => f.name === fieldExpr.field);
              if (fieldInfo) {
                const resolveCtx = new Map;
                const freshVars = [];
                recordInfo.params.forEach((p) => {
                  const v = { kind: "var", id: freshType().id };
                  resolveCtx.set(p, v);
                  freshVars.push(v);
                });
                const genericFieldType = this.typeFromAnnotation(fieldInfo.typeExpr, resolveCtx);
                const instSub = new Map;
                freshVars.forEach((v, i) => {
                  instSub.set(v.id, concrete.args[i]);
                });
                fieldType = applySubstitution(genericFieldType, instSub);
              }
            }
          }
          if (!fieldType) {
            if (concrete.kind !== "record" && (concrete.kind !== "con" || !this.resolveRecordInfo(concrete.name))) {
              throw new SemanticError(`Cannot access field '${fieldExpr.field}' on non-record value '${formatType(concrete)}'`, fieldExpr.span, this.getFilePath());
            }
            throw new SemanticError(`Record has no field '${fieldExpr.field}'`, fieldExpr.span, this.getFilePath());
          }
          return applySubstitution(fieldType, substitution);
        }
        case "Lambda": {
          const paramTypes = expr.args.map(() => freshType());
          const fnScope = scope.child();
          this.bindPatterns(fnScope, expr.args, paramTypes, substitution);
          const bodyType = this.analyzeExpr(expr.body, {
            scope: fnScope,
            substitution
          });
          return fnChain(paramTypes, bodyType);
        }
        case "Apply": {
          let calleeType = this.analyzeExpr(expr.callee, {
            scope,
            substitution
          });
          for (const arg of expr.args) {
            const argType = this.analyzeExpr(arg, { scope, substitution });
            const resultType = freshType();
            this.unify(calleeType, fn(argType, resultType), expr.span, substitution);
            this.validateConstraintsEagerly(substitution, arg.span);
            calleeType = applySubstitution(resultType, substitution);
          }
          return applySubstitution(calleeType, substitution);
        }
        case "If": {
          const condType = this.analyzeExpr(expr.condition, {
            scope,
            substitution
          });
          const boolAdt = adts["Bool"];
          if (!boolAdt) {
            throw new SemanticError("Type 'Bool' not found. Make sure the prelude is imported.", expr.condition.span, this.getFilePath());
          }
          const tBool = { kind: "con", name: "Bool", args: [] };
          this.unify(condType, tBool, expr.condition.span, substitution);
          const thenType = this.analyzeExpr(expr.thenBranch, {
            scope,
            substitution
          });
          const elseType = this.analyzeExpr(expr.elseBranch, {
            scope,
            substitution
          });
          this.unify(thenType, elseType, expr.span, substitution);
          return applySubstitution(thenType, substitution);
        }
        case "LetIn": {
          const letScope = scope.child();
          for (const binding of expr.bindings) {
            if (letScope.symbols.has(binding.name)) {
              throw new SemanticError(`Duplicate let-binding '${binding.name}'`, binding.span, this.getFilePath());
            }
            const seeded = this.seedValueType(binding);
            this.declareSymbol(letScope, binding.name, { vars: new Set, constraints: [], type: seeded }, binding.span);
          }
          for (const binding of expr.bindings) {
            const declared = this.lookupSymbol(letScope, binding.name, binding.span, substitution);
            const inferred = this.analyzeValueDeclaration(binding, letScope, substitution, declared);
            const generalizedScheme = this.generalize(inferred, scope, substitution);
            letScope.symbols.set(binding.name, generalizedScheme);
          }
          expr = expr.body;
          scope = letScope;
          continue;
        }
        case "Case": {
          const caseExpr = expr;
          const discriminantType = this.analyzeExpr(caseExpr.discriminant, {
            scope,
            substitution
          });
          const branchTypes = [];
          caseExpr.branches.forEach((branch, index) => {
            const branchScope = new Scope(scope);
            const patternType = this.bindPattern(branch.pattern, branchScope, new Set, freshType(), substitution);
            this.unify(discriminantType, patternType, branch.pattern.span, substitution);
            const bodyType = this.analyzeExpr(branch.body, {
              scope: branchScope,
              substitution,
              expectedType
            });
            branchTypes.push(bodyType);
            if (branch.pattern.kind === "WildcardPattern") {
              if (index !== caseExpr.branches.length - 1) {
                throw new SemanticError("Wildcard pattern makes following branches unreachable", branch.pattern.span, this.getFilePath());
              }
            }
            if (branch.pattern.kind === "ConstructorPattern") {
              this.validateConstructorArity(branch.pattern);
            }
          });
          if (branchTypes.length === 0) {
            throw new SemanticError("Case expression has no branches", caseExpr.span, this.getFilePath());
          }
          const firstType = branchTypes[0];
          for (const bt of branchTypes.slice(1)) {
            this.unify(firstType, bt, caseExpr.span, substitution);
          }
          const patterns = caseExpr.branches.map((b) => b.pattern);
          const result = checkExhaustiveness(patterns, discriminantType, adts, constructors, (name) => resolveQualifiedConstructor(name, constructors, adts, imports, dependencies) || undefined);
          if (!result.exhaustive) {
            throw new SemanticError(`Non-exhaustive case expression (missing: ${result.missing})`, caseExpr.span, this.getFilePath());
          }
          return applySubstitution(firstType, substitution);
        }
        case "Infix": {
          const opType = INFIX_TYPES[expr.operator];
          if (opType) {
            const leftType = this.analyzeExpr(expr.left, {
              scope,
              substitution
            });
            const rightType = this.analyzeExpr(expr.right, {
              scope,
              substitution
            });
            const expected = applySubstitution(opType, substitution);
            const params = flattenFunctionParams(expected);
            if (params.length < 2) {
              throw new SemanticError("Invalid operator type", expr.span, this.getFilePath());
            }
            this.unify(params[0], leftType, expr.left.span, substitution);
            this.unify(params[1], rightType, expr.right.span, substitution);
            return applySubstitution(extractAnnotationReturn(expected, 2), substitution);
          }
          const callee = {
            kind: "Var",
            name: expr.operator,
            namespace: "lower",
            span: expr.span
          };
          const applyExpr = {
            kind: "Apply",
            callee,
            args: [expr.left, expr.right],
            span: expr.span
          };
          const beforeLen = this._pendingProtocolUsages.length;
          const result = this.analyzeExpr(applyExpr, {
            scope,
            substitution,
            expectedType
          });
          for (let i = beforeLen;i < this._pendingProtocolUsages.length; i++) {
            if (this._pendingProtocolUsages[i].node === callee) {
              this._pendingProtocolUsages[i].node = expr;
            }
          }
          return result;
        }
        case "Paren":
          expr = expr.expression;
          continue;
        case "Unary": {
          const operandType = this.analyzeExpr(expr.operand, {
            scope,
            substitution
          });
          const concreteType = applySubstitution(operandType, substitution);
          if (concreteType.kind === "con") {
            if (concreteType.name === "Int" || concreteType.name === "Float") {
              return concreteType;
            }
          }
          if (concreteType.kind === "var") {
            throw new SemanticError(`Unary negation requires a concrete numeric type (Int or Float), but got an unknown type. Add a type annotation to disambiguate.`, expr.span, this.getFilePath());
          }
          throw new SemanticError(`Unary negation is only allowed for Int and Float, but got '${formatType(concreteType)}'`, expr.span, this.getFilePath());
        }
        default: {
          const _exhaustive = expr;
          throw new SemanticError("Unsupported expression", expr.span, this.getFilePath());
        }
      }
    }
  }
  seedValueType(decl) {
    if (decl.kind === "DecoratedDeclaration") {
      return this.typeFromAnnotation(decl.annotation, new Map);
    }
    const argTypes = decl.args.map(() => freshType());
    const resultType = freshType();
    return fnChain(argTypes, resultType);
  }
  typeFromAnnotation(annotation, context = new Map) {
    const { adts, typeAliases, records, imports, dependencies } = this;
    function resolve(name) {
      return resolveQualifiedType(name, adts, typeAliases, {}, records, imports, dependencies);
    }
    switch (annotation.kind) {
      case "TypeRef": {
        if (isTypeVariable(annotation.name)) {
          let typeVar = context.get(annotation.name);
          if (!typeVar) {
            typeVar = freshType();
            context.set(annotation.name, typeVar);
          }
          if (annotation.args.length > 0) {} else {
            return typeVar;
          }
        }
        const resolved = resolve(annotation.name);
        if (resolved && resolved.kind === "alias") {
          const aliasInfo = resolved.info;
          if (annotation.args.length !== aliasInfo.params.length) {} else {
            const argTypes = [];
            for (let i = 0;i < aliasInfo.params.length; i++) {
              const argType = this.typeFromAnnotation(annotation.args[i], context);
              argTypes.push(argType);
            }
            const aliasContext = new Map(context);
            const substitutionMap = new Map;
            for (let i = 0;i < aliasInfo.params.length; i++) {
              const paramName = aliasInfo.params[i];
              const fresh = freshType();
              aliasContext.set(paramName, fresh);
              substitutionMap.set(fresh.id, argTypes[i]);
            }
            const expandedType = this.typeFromAnnotation(aliasInfo.value, aliasContext);
            return applySubstitution(expandedType, substitutionMap);
          }
        }
        if (resolved && resolved.kind === "record") {
          const recordInfo = resolved.info;
          if (annotation.args.length === recordInfo.params.length) {
            const argTypes = [];
            for (let i = 0;i < recordInfo.params.length; i++) {
              argTypes.push(this.typeFromAnnotation(annotation.args[i], context));
            }
            return {
              kind: "con",
              name: recordInfo.name,
              args: argTypes
            };
          }
        }
        const canonicalName = resolved ? resolved.name : annotation.name;
        return {
          kind: "con",
          name: canonicalName,
          args: annotation.args.map((arg) => this.typeFromAnnotation(arg, context))
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
          elements: annotation.elements.map((el) => this.typeFromAnnotation(el, context))
        };
      }
      case "RecordType": {
        throw new SemanticError(`Record types cannot be used directly in type annotations. ` + `Define a named record type using 'type RecordName = { ... }' and reference it by name.`, annotation.span, this.getFilePath());
      }
      case "QualifiedType": {
        return this.typeFromAnnotation(annotation.type, context);
      }
    }
  }
  validateConstructorArity(pattern) {
    const ctorInfo = this.constructors[pattern.name];
    if (ctorInfo) {
      if (ctorInfo.arity !== pattern.args.length) {
        throw new SemanticError(`Constructor '${pattern.name}' expects ${ctorInfo.arity} argument(s), got ${pattern.args.length}`, pattern.span, this.getFilePath());
      }
      return;
    }
    const expected = BUILTIN_CONSTRUCTORS[pattern.name];
    if (expected !== undefined && expected !== pattern.args.length) {
      throw new SemanticError(`Constructor '${pattern.name}' expects ${expected} argument(s)`, pattern.span, this.getFilePath());
    }
  }
  bindPatterns(scope, patterns, paramTypes, substitution = this.substitution) {
    if (patterns.length !== paramTypes.length) {
      throw new Error("Internal arity mismatch during pattern binding");
    }
    const seen = new Set;
    patterns.forEach((pattern, idx) => {
      const paramType = paramTypes[idx];
      this.bindPattern(pattern, scope, seen, paramType, substitution);
      if (pattern.kind === "VarPattern") {
        scope.symbols.set(pattern.name, {
          vars: new Set,
          constraints: [],
          type: paramType
        });
      }
    });
  }
  bindPattern(pattern, scope, seen = new Set, expected, substitution = this.substitution) {
    const { constructors, adts, imports, dependencies } = this;
    const bind = (p, exp) => this.bindPattern(p, scope, seen, exp);
    switch (pattern.kind) {
      case "VarPattern": {
        if (seen.has(pattern.name)) {
          throw new SemanticError(`Duplicate pattern variable '${pattern.name}'`, pattern.span, this.getFilePath());
        }
        seen.add(pattern.name);
        this.declareSymbol(scope, pattern.name, { vars: new Set, constraints: [], type: expected }, pattern.span);
        return expected;
      }
      case "WildcardPattern":
        return expected;
      case "TuplePattern": {
        const subTypes = pattern.elements.map(() => freshType());
        this.unify({ kind: "tuple", elements: subTypes }, expected, pattern.span, substitution);
        pattern.elements.forEach((el, idx) => bind(el, subTypes[idx]));
        return applySubstitution({ kind: "tuple", elements: subTypes }, substitution);
      }
      case "ConstructorPattern": {
        this.validateConstructorArity(pattern);
        const resolved = resolveQualifiedConstructor(pattern.name, constructors, adts, imports, dependencies);
        const ctorInfo = resolved ? resolved.info : undefined;
        if (ctorInfo) {
          const paramTypeVars = new Map;
          for (const param of ctorInfo.parentParams) {
            paramTypeVars.set(param, freshType());
          }
          const resultType = {
            kind: "con",
            name: ctorInfo.parentType,
            args: ctorInfo.parentParams.map((p) => paramTypeVars.get(p))
          };
          const argTypes = ctorInfo.argTypes.map((argExpr) => this.constructorArgToType(argExpr, paramTypeVars));
          this.unify(resultType, expected, pattern.span, substitution);
          if (pattern.args.length !== argTypes.length) {
            throw new SemanticError(`Constructor '${pattern.name}' expects ${argTypes.length} argument(s), got ${pattern.args.length}`, pattern.span, this.getFilePath());
          }
          pattern.args.forEach((arg, idx) => {
            const argType = applySubstitution(argTypes[idx], substitution);
            bind(arg, argType);
          });
          return applySubstitution(resultType, substitution);
        } else {
          const argTypes = pattern.args.map(() => freshType());
          const constructed = fnChain(argTypes, freshType());
          this.unify(constructed, expected, pattern.span, substitution);
          pattern.args.forEach((arg, idx) => bind(arg, argTypes[idx]));
          return applySubstitution(expected, substitution);
        }
      }
      case "ListPattern": {
        const elemType = freshType();
        const lt = listType(elemType);
        this.unify(lt, expected, pattern.span, substitution);
        pattern.elements.forEach((el) => bind(el, applySubstitution(elemType, substitution)));
        return applySubstitution(lt, substitution);
      }
      case "ConsPattern": {
        const elemType = freshType();
        const lt = listType(elemType);
        this.unify(lt, expected, pattern.span, substitution);
        bind(pattern.head, applySubstitution(elemType, substitution));
        bind(pattern.tail, applySubstitution(lt, substitution));
        return applySubstitution(lt, substitution);
      }
      case "RecordPattern": {
        const fieldTypes = {};
        for (const field of pattern.fields) {
          fieldTypes[field.name] = freshType();
        }
        const recordType = {
          kind: "record",
          fields: fieldTypes
        };
        this.unify(recordType, expected, pattern.span, substitution);
        for (const field of pattern.fields) {
          const fieldType = fieldTypes[field.name];
          const appliedFieldType = applySubstitution(fieldType, substitution);
          if (field.pattern) {
            bind(field.pattern, appliedFieldType);
          } else {
            if (seen.has(field.name)) {
              throw new SemanticError(`Duplicate pattern variable '${field.name}'`, pattern.span, this.getFilePath());
            }
            seen.add(field.name);
            this.declareSymbol(scope, field.name, { vars: new Set, constraints: [], type: appliedFieldType }, pattern.span);
          }
        }
        return applySubstitution(recordType, substitution);
      }
      default:
        return expected;
    }
  }
  constructorArgToType(expr, paramTypeVars) {
    switch (expr.kind) {
      case "TypeRef": {
        const typeVar = paramTypeVars.get(expr.name);
        if (typeVar && expr.args.length === 0) {
          return typeVar;
        }
        const args = expr.args.map((arg) => this.constructorArgToType(arg, paramTypeVars));
        return {
          kind: "con",
          name: expr.name,
          args
        };
      }
      case "FunctionType": {
        return {
          kind: "fun",
          from: this.constructorArgToType(expr.from, paramTypeVars),
          to: this.constructorArgToType(expr.to, paramTypeVars)
        };
      }
      case "TupleType": {
        return {
          kind: "tuple",
          elements: expr.elements.map((el) => this.constructorArgToType(el, paramTypeVars))
        };
      }
      case "RecordType": {
        const sortedFields = [...expr.fields].sort((a, b) => a.name.localeCompare(b.name));
        const fields = {};
        for (const field of sortedFields) {
          fields[field.name] = this.constructorArgToType(field.type, paramTypeVars);
        }
        return {
          kind: "record",
          fields
        };
      }
      case "QualifiedType": {
        throw new SemanticError("Constructor arguments cannot have constraints", expr.span, this.getFilePath());
      }
    }
  }
  validateFunctionParamPatterns(patterns) {
    for (const pattern of patterns) {
      this.validateFunctionParamPattern(pattern);
    }
  }
  validateFunctionParamPattern(pattern) {
    const { constructors, adts } = this;
    switch (pattern.kind) {
      case "VarPattern":
      case "WildcardPattern":
        return;
      case "TuplePattern":
        for (const element of pattern.elements) {
          this.validateFunctionParamPattern(element);
        }
        return;
      case "RecordPattern":
        for (const field of pattern.fields) {
          if (field.pattern) {
            this.validateFunctionParamPattern(field.pattern);
          }
        }
        return;
      case "ConstructorPattern": {
        const ctorInfo = constructors[pattern.name];
        if (!ctorInfo) {
          return;
        }
        const adtInfo = adts[ctorInfo.parentType];
        if (!adtInfo) {
          return;
        }
        if (adtInfo.constructors.length > 1) {
          const constructorNames = adtInfo.constructors.join(", ");
          throw new SemanticError(`Constructor pattern '${pattern.name}' is not allowed in function parameters. ` + `The type '${ctorInfo.parentType}' has multiple constructors (${constructorNames}). ` + `Use a case expression in the function body instead.`, pattern.span, this.getFilePath());
        }
        for (const arg of pattern.args) {
          this.validateFunctionParamPattern(arg);
        }
        return;
      }
      case "ListPattern":
      case "ConsPattern":
        throw new SemanticError(`List patterns are not allowed in function parameters. ` + `Use a case expression in the function body instead.`, pattern.span, this.getFilePath());
    }
  }
  resolveRecordInfo(name) {
    if (this.records[name])
      return this.records[name];
    for (const [, dep] of this.dependencies) {
      if (dep.records[name])
        return dep.records[name];
    }
    return;
  }
  findRecordsByFieldNames(fieldNames) {
    const results = [];
    const seen = new Set;
    const check = (rec) => {
      if (seen.has(rec.name))
        return;
      seen.add(rec.name);
      const recFieldNames = new Set(rec.fields.map((f) => f.name));
      if (recFieldNames.size === fieldNames.size && [...fieldNames].every((f) => recFieldNames.has(f))) {
        results.push(rec);
      }
    };
    for (const rec of Object.values(this.records)) {
      check(rec);
    }
    return results;
  }
  analyzeValueDeclaration(decl, scope, substitution, declaredType, annotationType) {
    this.validateFunctionParamPatterns(decl.args);
    const paramTypes = annotationType ? extractAnnotationParams(annotationType, decl.args.length, decl.span) : decl.args.map(() => freshType());
    const returnType = annotationType ? extractAnnotationReturn(annotationType, decl.args.length) : freshType();
    const expected = fnChain(paramTypes, returnType);
    this.unify(expected, declaredType, decl.span, substitution);
    const fnScope = scope.child();
    this.bindPatterns(fnScope, decl.args, paramTypes, substitution);
    const bodyType = this.analyzeExpr(decl.body, {
      scope: fnScope,
      substitution,
      expectedType: returnType
    });
    this.unify(bodyType, returnType, decl.body.span, substitution);
    return applySubstitution(expected, substitution);
  }
  unify(a, b, span, substitution = this.substitution) {
    const left = applySubstitution(a, substitution);
    const right = applySubstitution(b, substitution);
    if (left.kind === "error" || right.kind === "error") {
      return;
    }
    if (left.kind === "var") {
      if (!typesEqual(left, right)) {
        if (occursIn(left.id, right, substitution)) {
          throw new SemanticError("Recursive type detected", span, this.getFilePath());
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
        throw new SemanticError(`Type mismatch: cannot unify '${formatType(left)}' with '${formatType(right)}'`, span, this.getFilePath());
      }
      left.args.forEach((arg, idx) => this.unify(arg, right.args[idx], span, substitution));
      return;
    }
    if (left.kind === "fun" && right.kind === "fun") {
      this.unify(left.from, right.from, span, substitution);
      this.unify(left.to, right.to, span, substitution);
      return;
    }
    if (left.kind === "tuple" && right.kind === "tuple") {
      if (left.elements.length !== right.elements.length) {
        throw new SemanticError("Tuple length mismatch", span, this.getFilePath());
      }
      left.elements.forEach((el, idx) => this.unify(el, right.elements[idx], span, substitution));
      return;
    }
    if (left.kind === "record" && right.kind === "record") {
      const shared = Object.keys(left.fields).filter((k) => right.fields[k] !== undefined);
      for (const key of shared) {
        this.unify(left.fields[key], right.fields[key], span, substitution);
      }
      return;
    }
    throw new SemanticError(`Type mismatch: cannot unify '${formatType(left)}' with '${formatType(right)}'`, span, this.getFilePath());
  }
  importExportSpec(spec, depModule, imp) {
    switch (spec.kind) {
      case "ExportValue": {
        const name = spec.name;
        if (!isExportedFromModule(depModule, name, "value")) {
          if (isExportedFromModule(depModule, name, "type") && depModule.adts[name]) {
            const depADT = depModule.adts[name];
            this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
            this.adts[name] = depADT;
            return;
          }
          if (depModule.typeAliases[name]) {
            this.checkTypeCollision(name, imp.moduleName, this.typeAliases[name], imp.span, "type alias");
            this.typeAliases[name] = depModule.typeAliases[name];
            return;
          }
          if (depModule.opaqueTypes[name]) {
            this.opaqueTypes[name] = depModule.opaqueTypes[name];
            return;
          }
          if (depModule.records[name]) {
            this.checkTypeCollision(name, imp.moduleName, this.records[name], imp.span, "record type");
            this.records[name] = depModule.records[name];
            return;
          }
          if (isExportedFromModule(depModule, name, "protocol") && depModule.protocols[name]) {
            this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
            this.protocols[name] = depModule.protocols[name];
            return;
          }
          throw new SemanticError(`Cannot import '${name}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
        }
        const depValue = depModule.values[name];
        if (depValue && depValue.type) {
          const importedType = depValue.type;
          const scheme = this.generalize(importedType, this.globalScope, this.substitution);
          this.globalScope.define(name, scheme);
          this.importedValues.set(name, imp.moduleName);
        }
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
        if (!isExportedFromModule(depModule, op, "operator")) {
          throw new SemanticError(`Cannot import operator '${op}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
        }
        const depValue = depModule.values[op];
        if (depValue && depValue.type) {
          const importedType = depValue.type;
          const scheme = this.generalize(importedType, this.globalScope, this.substitution);
          this.globalScope.define(op, scheme);
        }
        const opInfo = depModule.operators.get(op);
        if (opInfo) {
          this.operators.set(op, opInfo);
          this.importedValues.set(op, imp.moduleName);
        }
        break;
      }
      case "ExportTypeAll": {
        const name = spec.name;
        const depADT = depModule.adts[name];
        if (depADT) {
          if (!isExportedFromModule(depModule, name, "type")) {
            throw new SemanticError(`Cannot import type '${name}(..)' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
          this.adts[name] = depADT;
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
        const depProtocol = depModule.protocols[name];
        if (depProtocol) {
          if (!isExportedFromModule(depModule, name, "protocol")) {
            throw new SemanticError(`Cannot import protocol '${name}(..)' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
          this.protocols[name] = depProtocol;
          const methodSchemes = addProtocolMethodsToScope(depProtocol, this.globalScope);
          for (const [methodName, scheme] of methodSchemes) {
            this.typeSchemes[methodName] = scheme;
          }
          return;
        }
        const depRecord = depModule.records[name];
        if (depRecord) {
          throw new SemanticError(`Record type '${name}' cannot use (..) syntax - records have no constructors. Use '${name}' instead`, spec.span, this.getFilePath());
        }
        throw new SemanticError(`Cannot import '${name}(..)' from module '${imp.moduleName}' - it is not a type or protocol`, spec.span, this.getFilePath());
      }
      case "ExportTypeSome": {
        const name = spec.name;
        const members = spec.members;
        const depADT = depModule.adts[name];
        if (depADT) {
          if (!isExportedFromModule(depModule, name, "type")) {
            throw new SemanticError(`Cannot import type '${name}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
          this.adts[name] = depADT;
          for (const ctorName of members) {
            if (!depADT.constructors.includes(ctorName)) {
              throw new SemanticError(`Constructor '${ctorName}' is not defined in type '${name}'`, spec.span, this.getFilePath());
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
        const depProtocol = depModule.protocols[name];
        if (depProtocol) {
          if (!isExportedFromModule(depModule, name, "protocol")) {
            throw new SemanticError(`Cannot import protocol '${name}' from module '${imp.moduleName}' - it is not exported`, spec.span, this.getFilePath());
          }
          this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
          this.protocols[name] = depProtocol;
          for (const methodName of members) {
            const methodInfo = depProtocol.methods.get(methodName);
            if (!methodInfo) {
              throw new SemanticError(`Method '${methodName}' is not defined in protocol '${name}'`, spec.span, this.getFilePath());
            }
            const constraintTypeVar = freshType();
            const methodType = methodInfo.type;
            const scheme = {
              vars: new Set([
                constraintTypeVar.kind === "var" ? constraintTypeVar.id : -1
              ]),
              constraints: [
                { protocolName: name, typeArgs: [constraintTypeVar] }
              ],
              type: methodType
            };
            this.globalScope.define(methodName, scheme);
          }
          return;
        }
        throw new SemanticError(`Cannot import '${name}(...)' from module '${imp.moduleName}' - it is not a type or protocol`, spec.span, this.getFilePath());
      }
    }
  }
  registerValue(decl) {
    if (Object.hasOwn(this.values, decl.name)) {
      throw new SemanticError(`Duplicate definition for '${decl.name}'`, decl.span, this.getFilePath());
    }
    const hasBuiltinAnnotation = decl.kind === "DecoratedDeclaration";
    let externalTarget;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
      externalTarget = {
        modulePath: decl.args[0],
        exportName: decl.args[1],
        span: decl.span
      };
    }
    this.values[decl.name] = {
      declaration: decl,
      annotation: hasBuiltinAnnotation ? decl.annotation : undefined,
      externalTarget
    };
  }
  createChildScope(parent) {
    return (parent ?? this._globalScope).child();
  }
  computeExpectedModuleName() {
    const normalizedFilePath = this.getFilePath().replace(/\\/g, "/");
    const rawSrcDir = this.getSrcDir();
    const normalizedSrcDir = rawSrcDir.replace(/\\/g, "/").replace(/\/$/, "");
    if (!normalizedSrcDir || !normalizedFilePath.startsWith(normalizedSrcDir + "/")) {
      const fileName = normalizedFilePath.split("/").pop() ?? "";
      return fileName.replace(/\.vibe$/, "");
    }
    const relativePath = normalizedFilePath.slice(normalizedSrcDir.length + 1).replace(/\.vibe$/, "");
    return relativePath.replace(/\//g, ".");
  }
  ensureModuleNameConsistency() {
    const expectedModuleName = this.computeExpectedModuleName();
    const declaredModuleName = this.program.module.name;
    if (declaredModuleName !== expectedModuleName) {
      throw new SemanticError(`Module name '${declaredModuleName}' does not match file path.
` + `Expected: module ${expectedModuleName} [exposing (..)]
` + `File path: ${this.getFilePath()}`, this.program.module.span, this.getFilePath());
    }
  }
  initializeBuiltinADTs() {
    this.adts["Bool"] = {
      name: "Bool",
      params: [],
      constructors: ["True", "False"],
      constraints: [],
      span: BUILTIN_SPAN
    };
    this.constructors["True"] = {
      arity: 0,
      argTypes: [],
      parentType: "Bool",
      parentParams: [],
      moduleName: BUILTIN_MODULE_NAME,
      span: BUILTIN_SPAN
    };
    this.constructorTypes["True"] = {
      vars: new Set,
      constraints: [],
      type: { kind: "con", name: "Bool", args: [] }
    };
    this.constructors["False"] = {
      arity: 0,
      argTypes: [],
      parentType: "Bool",
      parentParams: [],
      moduleName: BUILTIN_MODULE_NAME,
      span: BUILTIN_SPAN
    };
    this.constructorTypes["False"] = {
      vars: new Set,
      constraints: [],
      type: { kind: "con", name: "Bool", args: [] }
    };
    this.adts["List"] = {
      name: "List",
      params: ["a"],
      constructors: [],
      constraints: [],
      span: BUILTIN_SPAN
    };
    this.globalScope.define("True", this.constructorTypes["True"]);
    this.globalScope.define("False", this.constructorTypes["False"]);
  }
  initializeBuiltinOpaqueTypes() {
    this.opaqueTypes["Unit"] = {
      name: "Unit",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["Int"] = {
      name: "Int",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["Float"] = {
      name: "Float",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["String"] = {
      name: "String",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
    this.opaqueTypes["Char"] = {
      name: "Char",
      moduleName: BUILTIN_MODULE_NAME,
      params: [],
      span: BUILTIN_SPAN
    };
  }
  validateImports() {
    const byModule = new Map;
    const byAlias = new Map;
    for (const imp of this.imports) {
      const duplicateModule = byModule.get(imp.moduleName);
      if (duplicateModule) {
        throw new SemanticError(`Duplicate import of module '${imp.moduleName}'`, imp.span, this.getFilePath());
      }
      byModule.set(imp.moduleName, imp);
      if (imp.alias) {
        const duplicateAlias = byAlias.get(imp.alias);
        if (duplicateAlias) {
          throw new SemanticError(`Duplicate import alias '${imp.alias}'`, imp.span, this.getFilePath());
        }
        byAlias.set(imp.alias, imp);
      }
    }
  }
  seedBuiltinOperators() {
    for (const [op, ty] of Object.entries(INFIX_TYPES)) {
      this.globalScope.define(op, {
        vars: new Set,
        constraints: [],
        type: ty
      });
    }
    for (const [op, fixity] of Object.entries(BUILTIN_OPERATOR_FIXITY)) {
      this.operators.set(op, fixity);
    }
  }
  mergeImportedModules() {
    for (const imp of this.imports) {
      const depModule = this.dependencies.get(imp.moduleName);
      if (!depModule) {
        throw new SemanticError("Unresolved module import", imp.span, this.getFilePath());
      }
      const currentModuleName = this.getModuleName();
      for (const instance of depModule.instances) {
        if (instance.moduleName === currentModuleName)
          continue;
        const isDuplicate = this.instances.some((existing) => existing.protocolName === instance.protocolName && existing.moduleName === instance.moduleName && typeArgsEqual(existing.typeArgs, instance.typeArgs));
        if (!isDuplicate) {
          this.instances.push(instance);
        }
      }
      if (imp.alias) {
        this.globalScope.define(imp.alias, {
          vars: new Set,
          constraints: [],
          type: freshType()
        });
      }
      if (!imp.alias) {
        const moduleParts = imp.moduleName.split(".");
        const rootModule = moduleParts[0];
        if (!this.globalScope.has(rootModule)) {
          this.globalScope.define(rootModule, {
            vars: new Set,
            constraints: [],
            type: freshType()
          });
        }
      }
      if (imp.exposing?.kind === "Explicit") {
        for (const spec of imp.exposing.exports) {
          this.importExportSpec(spec, depModule, imp);
        }
      }
      if (imp.exposing?.kind === "All") {
        for (const [name, depValue] of Object.entries(depModule.values)) {
          if (depValue.type && isExportedFromModule(depModule, name, "value")) {
            const importedType = depValue.type;
            const scheme = this.generalize(importedType, this.globalScope, this.substitution);
            this.globalScope.define(name, scheme);
            this.importedValues.set(name, imp.moduleName);
          }
        }
        for (const [name] of depModule.exports.reExportedValues) {
          if (this.globalScope.has(name))
            continue;
          const scheme = depModule.typeSchemes[name];
          if (scheme) {
            this.globalScope.define(name, scheme);
            this.importedValues.set(name, imp.moduleName);
          }
        }
        for (const [name, ctor] of Object.entries(depModule.constructors)) {
          if (isExportedFromModule(depModule, name, "constructor")) {
            this.constructors[name] = ctor;
            const ctorScheme = depModule.constructorTypes[name];
            if (ctorScheme) {
              this.globalScope.define(name, ctorScheme);
              this.constructorTypes[name] = ctorScheme;
            }
          }
        }
        for (const [name, adt] of Object.entries(depModule.adts)) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(name, imp.moduleName, this.adts[name], imp.span, "type");
            this.adts[name] = adt;
          }
        }
        for (const [name, alias] of Object.entries(depModule.typeAliases)) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(name, imp.moduleName, this.typeAliases[name], imp.span, "type alias");
            this.typeAliases[name] = alias;
          }
        }
        for (const [name, opaque] of Object.entries(depModule.opaqueTypes)) {
          if (isExportedFromModule(depModule, name, "type")) {
            if (Object.hasOwn(this.opaqueTypes, name) && this.opaqueTypes[name].moduleName !== BUILTIN_MODULE_NAME && this.opaqueTypes[name].moduleName !== imp.moduleName) {
              throw new SemanticError(`Opaque type '${name}' conflicts with opaque type from module '${this.opaqueTypes[name].moduleName}'. ` + `Consider using a different name or qualified imports.`, imp.span, this.getFilePath());
            }
            this.opaqueTypes[name] = opaque;
          }
        }
        for (const [name, rec] of Object.entries(depModule.records)) {
          if (isExportedFromModule(depModule, name, "type")) {
            this.checkTypeCollision(name, imp.moduleName, this.records[name], imp.span, "record type");
            this.records[name] = rec;
          }
        }
        for (const [name, protocol] of Object.entries(depModule.protocols)) {
          if (isExportedFromModule(depModule, name, "protocol")) {
            this.checkTypeCollision(name, imp.moduleName, this.protocols[name], imp.span, "protocol");
            this.protocols[name] = protocol;
            const methodSchemes = addProtocolMethodsToScope(protocol, this.globalScope);
            for (const [methodName, scheme] of methodSchemes) {
              this.typeSchemes[methodName] = scheme;
            }
          }
        }
        for (const [op, info] of depModule.operators) {
          if (isExportedFromModule(depModule, op, "operator")) {
            this.operators.set(op, info);
            this.importedValues.set(op, imp.moduleName);
          }
        }
      }
    }
    const importedModuleNames = new Set(this.imports.map((imp) => imp.moduleName));
    const thisModuleName = this.getModuleName();
    for (const [depName, depModule] of this.dependencies) {
      if (importedModuleNames.has(depName))
        continue;
      for (const instance of depModule.instances) {
        if (instance.moduleName === thisModuleName)
          continue;
        const isDuplicate = this.instances.some((existing) => existing.protocolName === instance.protocolName && existing.moduleName === instance.moduleName && typeArgsEqual(existing.typeArgs, instance.typeArgs));
        if (!isDuplicate) {
          this.instances.push(instance);
        }
      }
    }
  }
  registerInfixDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "InfixDeclaration") {
        this.registerInfixDeclaration(decl);
        continue;
      }
    }
  }
  registerInfixDeclaration(decl) {
    if (this.importedValues.has(decl.operator)) {
      const sourceModule = this.importedValues.get(decl.operator);
      throw new SemanticError(`Cannot declare fixity for imported operator '${decl.operator}' from module '${sourceModule}'. ` + `Fixity is an intrinsic property of the operator and cannot be redefined.`, decl.span, this.getFilePath());
    }
    if (this.operators.has(decl.operator)) {
      throw new SemanticError(`Duplicate infix declaration for operator '${decl.operator}'`, decl.span, this.getFilePath());
    }
    const associativity = decl.fixity === "infixl" ? "left" : decl.fixity === "infixr" ? "right" : "none";
    if (decl.precedence < 0 || decl.precedence > 9) {
      throw new SemanticError(`Precedence must be between 0 and 9, got ${decl.precedence}`, decl.span, this.getFilePath());
    }
    this.operators.set(decl.operator, {
      precedence: decl.precedence,
      associativity
    });
    this.infixDeclarations.push(decl);
  }
  registerRecordTypeDeclaration(decl) {
    if (this.records[decl.name]) {
      throw new SemanticError(`Duplicate record type declaration for '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (this.adts[decl.name]) {
      throw new SemanticError(`Record type '${decl.name}' conflicts with ADT '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (this.typeAliases[decl.name]) {
      throw new SemanticError(`Record type '${decl.name}' conflicts with type alias '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (this.opaqueTypes[decl.name]) {
      throw new SemanticError(`Record type '${decl.name}' conflicts with opaque type '${decl.name}'`, decl.span, this.getFilePath());
    }
    if (decl.constructors && decl.constructors.length > 0) {
      throw new SemanticError(`Type '${decl.name}' cannot have both constructors and record fields. ` + `Use 'type' for ADTs or record types separately.`, decl.span, this.getFilePath());
    }
    if (!decl.recordFields) {
      throw new SemanticError(`Record type '${decl.name}' is missing field definitions`, decl.span, this.getFilePath());
    }
    const fieldNames = new Set;
    for (const field of decl.recordFields) {
      if (fieldNames.has(field.name)) {
        throw new SemanticError(`Duplicate field '${field.name}' in record type '${decl.name}'`, field.span, this.getFilePath());
      }
      fieldNames.add(field.name);
    }
    const paramTypeVars = new Map;
    for (const param of decl.params) {
      paramTypeVars.set(param, freshType());
    }
    const semanticConstraints = [];
    if (decl.constraints) {
      for (const c of decl.constraints) {
        semanticConstraints.push({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => this.constructorArgToType(t, paramTypeVars))
        });
      }
    }
    for (const field of decl.recordFields) {
      if (containsRecordType(field.type)) {
        throw new SemanticError(`Record types cannot be used directly in type annotations. ` + `Define a named record type using 'type RecordName = { ... }' and reference it by name.`, field.span, this.getFilePath());
      }
    }
    const fields = decl.recordFields.map((field) => ({
      name: field.name,
      typeExpr: field.type,
      span: field.span
    }));
    this.records[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      constraints: semanticConstraints,
      fields,
      span: decl.span
    };
  }
  registerTypeDeclaration(decl) {
    const existingADT = this.adts[decl.name];
    if (existingADT) {
      if (existingADT.moduleName && existingADT.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Type '${decl.name}' conflicts with type from module '${existingADT.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate type declaration for '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in type '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    if (!decl.constructors || decl.constructors.length === 0) {
      if (decl.recordFields) {
        this.registerRecordTypeDeclaration(decl);
        return;
      }
      throw new SemanticError(`Type '${decl.name}' must have at least one constructor`, decl.span, this.getFilePath());
    }
    const paramTypeVars = new Map;
    for (const param of decl.params) {
      paramTypeVars.set(param, freshType());
    }
    const semanticConstraints = [];
    if (decl.constraints) {
      for (const c of decl.constraints) {
        semanticConstraints.push({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => this.constructorArgToType(t, paramTypeVars))
        });
      }
    }
    const constructorNames = decl.constructors.map((c) => c.name);
    this.adts[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      constructors: constructorNames,
      constraints: semanticConstraints,
      span: decl.span
    };
    const resultType = {
      kind: "con",
      name: decl.name,
      args: decl.params.map((p) => paramTypeVars.get(p))
    };
    for (const ctor of decl.constructors) {
      const existingCtor = this.constructors[ctor.name];
      if (existingCtor && existingCtor.moduleName === this.getModuleName()) {
        throw new SemanticError(`Duplicate constructor '${ctor.name}' (constructor names must be unique within a module)`, ctor.span, this.getFilePath());
      }
      this.constructors[ctor.name] = {
        arity: ctor.args.length,
        argTypes: ctor.args,
        parentType: decl.name,
        parentParams: decl.params,
        moduleName: this.getModuleName(),
        span: ctor.span
      };
      const ctorType = this.buildConstructorType(ctor, resultType, paramTypeVars);
      const quantifiedVars = new Set;
      for (const tv of paramTypeVars.values()) {
        quantifiedVars.add(tv.id);
      }
      const ctorScheme = {
        vars: quantifiedVars,
        constraints: semanticConstraints,
        type: ctorType,
        paramNames: invertContext(paramTypeVars)
      };
      this.globalScope.define(ctor.name, ctorScheme);
      this.constructorTypes[ctor.name] = ctorScheme;
    }
  }
  buildConstructorType(ctor, resultType, paramTypeVars) {
    if (ctor.args.length === 0) {
      return resultType;
    }
    const argTypes = ctor.args.map((argExpr) => this.constructorArgToType(argExpr, paramTypeVars));
    return fnChain(argTypes, resultType);
  }
  registerADTTypeDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "TypeDeclaration") {
        this.registerTypeDeclaration(decl);
        continue;
      }
    }
  }
  registerOpaqueTypeDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "OpaqueTypeDeclaration") {
        this.registerOpaqueType(decl);
        continue;
      }
    }
  }
  registerOpaqueType(decl) {
    if (Object.hasOwn(this.opaqueTypes, decl.name)) {
      const existing = this.opaqueTypes[decl.name];
      if (existing.moduleName && existing.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Opaque type '${decl.name}' conflicts with opaque type from module '${existing.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate opaque type declaration for '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in opaque type '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    this.opaqueTypes[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      span: decl.span
    };
  }
  registerTypeAliasDeclarations() {
    const typeAliasDecls = [];
    for (const decl of this.program.declarations) {
      if (decl.kind === "TypeAliasDeclaration") {
        this.registerTypeAliasWithoutValidation(decl);
        typeAliasDecls.push(decl);
        continue;
      }
    }
    return typeAliasDecls;
  }
  registerTypeAliasWithoutValidation(decl) {
    const existingAlias = this.typeAliases[decl.name];
    if (existingAlias) {
      if (existingAlias.moduleName && existingAlias.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Type alias '${decl.name}' conflicts with type alias from module '${existingAlias.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate type alias '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in type alias '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    this.typeAliases[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      value: decl.value,
      span: decl.span
    };
  }
  validateTypeAliasDeclarations(typeAliasDecls) {
    for (const decl of typeAliasDecls) {
      this.validateTypeAliasReferences(decl);
    }
  }
  validateTypeAliasReferences(decl) {
    if (decl.value.kind === "RecordType") {
      throw new SemanticError(`Type alias '${decl.name}' cannot directly define a record type. ` + `Use 'type ${decl.name} = { ... }' instead of 'type alias'.`, decl.value.span, this.getFilePath());
    }
    const paramSet = new Set(decl.params);
    const validationErrors = validateTypeExpr(this, decl.value, paramSet, decl.span);
    if (validationErrors.length > 0) {
      const err = validationErrors[0];
      const message = err.suggestion ? `${err.message}. ${err.suggestion}` : err.message;
      throw new SemanticError(message, err.span, this.getFilePath());
    }
  }
  registerProtocolDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "ProtocolDeclaration") {
        this.registerProtocol(decl);
        for (const method of decl.methods) {
          this.localProtocolMethods.add(method.name);
        }
        continue;
      }
    }
  }
  registerProtocol(decl) {
    const { protocols, globalScope, substitution } = this;
    const existingProtocol = protocols[decl.name];
    if (existingProtocol) {
      if (existingProtocol.moduleName && existingProtocol.moduleName !== this.getModuleName()) {
        throw new SemanticError(`Protocol '${decl.name}' conflicts with protocol from module '${existingProtocol.moduleName}'. ` + `Consider using a different name or qualified imports.`, decl.span, this.getFilePath());
      }
      throw new SemanticError(`Duplicate protocol '${decl.name}'`, decl.span, this.getFilePath());
    }
    const paramSet = new Set;
    for (const param of decl.params) {
      if (paramSet.has(param)) {
        throw new SemanticError(`Duplicate type parameter '${param}' in protocol '${decl.name}'`, decl.span, this.getFilePath());
      }
      paramSet.add(param);
    }
    if (decl.methods.length === 0) {
      throw new SemanticError(`Protocol '${decl.name}' must have at least one method`, decl.span, this.getFilePath());
    }
    const sharedTypeVarCtx = new Map;
    for (const param of decl.params) {
      sharedTypeVarCtx.set(param, freshType());
    }
    const protocolConstraint = {
      protocolName: decl.name,
      typeArgs: decl.params.map((p) => sharedTypeVarCtx.get(p))
    };
    const quantifiedVars = new Set;
    for (const tv of sharedTypeVarCtx.values()) {
      quantifiedVars.add(tv.id);
    }
    const methods = new Map;
    const methodNames = new Set;
    for (const method of decl.methods) {
      if (methodNames.has(method.name)) {
        throw new SemanticError(`Duplicate method '${method.name}' in protocol '${decl.name}'`, method.span, this.getFilePath());
      }
      methodNames.add(method.name);
      let methodType;
      if (method.type) {
        methodType = this.typeFromAnnotation(method.type, sharedTypeVarCtx);
      } else if (method.defaultImpl) {
        const lambdaExpr = makeLambda(method.defaultImpl.args, method.defaultImpl.body, method.span);
        const tempScope = this.createChildScope();
        const inferredType = this.analyzeExpr(lambdaExpr, {
          scope: tempScope,
          substitution
        });
        methodType = applySubstitution(inferredType, substitution);
        methodType = substituteProtocolVars(methodType, sharedTypeVarCtx);
      } else {
        throw new SemanticError(`Protocol method '${method.name}' must have either a type annotation or a default implementation`, method.span, this.getFilePath());
      }
      const methodInfo = {
        type: methodType,
        span: method.span
      };
      if (method.defaultImpl) {
        methodInfo.defaultImpl = {
          args: method.defaultImpl.args,
          body: method.defaultImpl.body
        };
      }
      methods.set(method.name, methodInfo);
      if (!globalScope.symbols.has(method.name)) {
        const allConstraints = [
          protocolConstraint,
          ...decl.constraints.map((c) => ({
            protocolName: c.protocolName,
            typeArgs: c.typeArgs.map((ta) => this.typeFromAnnotation(ta, sharedTypeVarCtx))
          }))
        ];
        const scheme = {
          vars: new Set(quantifiedVars),
          constraints: allConstraints,
          type: methodType
        };
        globalScope.symbols.set(method.name, scheme);
      }
    }
    const superclassConstraints = decl.constraints.map((c) => ({
      protocolName: c.protocolName,
      typeArgs: c.typeArgs.map((ta) => this.typeFromAnnotation(ta, sharedTypeVarCtx))
    }));
    protocols[decl.name] = {
      name: decl.name,
      moduleName: this.getModuleName(),
      params: decl.params,
      superclassConstraints,
      methods,
      span: decl.span
    };
  }
  registerImplementationDeclarations() {
    for (const decl of this.program.declarations) {
      if (decl.kind === "ImplementationDeclaration") {
        this.registerImplementation(decl);
        continue;
      }
    }
  }
  registerImplementation(decl) {
    const protocol = this.protocols[decl.protocolName];
    if (!protocol) {
      throw new SemanticError(`Unknown protocol '${decl.protocolName}'`, decl.span, this.getFilePath());
    }
    if (decl.typeArgs.length !== protocol.params.length) {
      throw new SemanticError(`Protocol '${decl.protocolName}' expects ${protocol.params.length} type argument(s), but got ${decl.typeArgs.length}`, decl.span, this.getFilePath());
    }
    const typeVarCtx = new Map;
    const typeArgs = [];
    for (const typeArg of decl.typeArgs) {
      typeArgs.push(this.typeFromAnnotation(typeArg, typeVarCtx));
    }
    const constraints = [];
    for (const astConstraint of decl.constraints) {
      if (!this.protocols[astConstraint.protocolName]) {
        throw new SemanticError(`Unknown protocol '${astConstraint.protocolName}' in constraint`, decl.span, this.getFilePath());
      }
      const constraintTypeArgs = [];
      for (const typeArg of astConstraint.typeArgs) {
        constraintTypeArgs.push(this.typeFromAnnotation(typeArg, typeVarCtx));
      }
      constraints.push({
        protocolName: astConstraint.protocolName,
        typeArgs: constraintTypeArgs
      });
    }
    const implementedMethods = new Set(decl.methods.map((m) => m.name));
    const allMethods = new Set(protocol.methods.keys());
    if (decl.methods.length === 0 && (decl.protocolName === "Eq" || decl.protocolName === "Show")) {
      if (typeArgs.length === 1 && typeArgs[0].kind === "con") {
        const typeName = typeArgs[0].name;
        const typeDecl = this.getDeclarations().find((d) => (d.kind === "TypeDeclaration" || d.kind === "TypeAliasDeclaration") && d.name === typeName);
        if (typeDecl && typeDecl.kind === "TypeDeclaration") {
          let synthetic;
          if (decl.protocolName === "Eq") {
            synthetic = this.generateEqImplementation(typeDecl, protocol);
          } else {
            synthetic = this.generateShowImplementation(typeDecl);
          }
          if (synthetic) {
            synthetic.methods.forEach((impl, name) => {
              const methodInfo = protocol.methods.get(name);
              decl.methods.push({
                name,
                implementation: impl,
                span: decl.span
              });
              implementedMethods.add(name);
            });
          }
        }
      }
    }
    for (const methodName of allMethods) {
      const methodInfo = protocol.methods.get(methodName);
      if (!implementedMethods.has(methodName) && !methodInfo.defaultImpl) {
        throw new SemanticError(`Instance is missing implementation for method '${methodName}'`, decl.span, this.getFilePath());
      }
    }
    for (const implemented of implementedMethods) {
      if (!allMethods.has(implemented)) {
        throw new SemanticError(`Method '${implemented}' is not part of protocol '${decl.protocolName}'`, decl.span, this.getFilePath());
      }
    }
    const methods = new Map;
    const explicitMethods = new Set;
    for (const method of decl.methods) {
      if (method.args && method.args.length > 0) {
        const lambda = makeLambda(method.args, method.implementation, method.span);
        methods.set(method.name, lambda);
      } else {
        methods.set(method.name, method.implementation);
      }
      explicitMethods.add(method.name);
    }
    for (const [methodName, methodInfo] of protocol.methods) {
      if (!methods.has(methodName) && methodInfo.defaultImpl) {
        const defaultLambda = makeLambda(methodInfo.defaultImpl.args, methodInfo.defaultImpl.body, methodInfo.span);
        methods.set(methodName, defaultLambda);
      }
    }
    const instanceInfo = {
      protocolName: decl.protocolName,
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods,
      span: decl.span
    };
    for (const existing of this.instances) {
      if (existing.protocolName !== decl.protocolName)
        continue;
      if (instancesOverlap(existing, instanceInfo, this.instances)) {
        throw new SemanticError(`Overlapping implementation for protocol '${decl.protocolName}'`, decl.span, this.getFilePath());
      }
    }
    this.instances.push(instanceInfo);
    this.localInstances.push(instanceInfo);
  }
  generateShowImplementation(decl) {
    const typeArgs = [
      {
        kind: "con",
        name: decl.name,
        args: decl.params.map((param) => ({
          kind: "var",
          id: freshType().id
        }))
      }
    ];
    const paramMap = new Map;
    const headType = typeArgs[0];
    decl.params.forEach((p, i) => {
      paramMap.set(p, headType.args[i]);
    });
    const constraints = decl.params.map((p) => ({
      protocolName: "Show",
      typeArgs: [paramMap.get(p)]
    }));
    const methods = new Map;
    const span = decl.span;
    let body;
    const str = (s) => ({
      kind: "String",
      value: `"${s}"`,
      span
    });
    const append = (a, b) => ({
      kind: "Infix",
      left: a,
      operator: "++",
      right: b,
      span
    });
    const toStringWithUsage = (val, valType) => {
      const callee = {
        kind: "Var",
        name: "toString",
        namespace: "lower",
        span
      };
      const node = {
        kind: "Apply",
        callee,
        args: [val],
        span
      };
      this._resolvedProtocolUsages.set(callee, {
        protocolName: "Show",
        typeArgs: [valType]
      });
      return node;
    };
    if (decl.recordFields) {
      let expr = str(`${decl.name} { `);
      decl.recordFields.forEach((field, i) => {
        if (i > 0)
          expr = append(expr, str(", "));
        expr = append(expr, str(`${field.name} = `));
        const fieldAccess = {
          kind: "FieldAccess",
          target: { kind: "Var", name: "x_impl", namespace: "lower", span },
          field: field.name,
          span
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
        const pattern = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: args.map((a) => ({ kind: "VarPattern", name: a, span })),
          span
        };
        let expr;
        if (args.length === 0) {
          expr = str(`${ctor.name}`);
        } else {
          expr = str(`${ctor.name}(`);
          args.forEach((a, i) => {
            if (i > 0)
              expr = append(expr, str(", "));
            const argType = this.typeFromAnnotation(ctor.args[i], paramMap);
            this.ensureNestedTupleInstances("Show", argType);
            expr = append(expr, toStringWithUsage({ kind: "Var", name: a, namespace: "lower", span }, argType));
          });
          expr = append(expr, str(")"));
        }
        return { pattern, body: expr, span };
      });
      body = {
        kind: "Case",
        discriminant: { kind: "Var", name: "x_impl", namespace: "lower", span },
        branches,
        span
      };
    } else {
      body = str(`${decl.name}`);
    }
    methods.set("toString", {
      kind: "Lambda",
      args: [{ kind: "VarPattern", name: "x_impl", span }],
      body,
      span
    });
    return {
      protocolName: "Show",
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods: new Set(["toString"]),
      span
    };
  }
  ensureNestedTupleInstances(protocolName, type) {
    if (type.kind === "tuple") {
      for (const elem of type.elements) {
        this.ensureNestedTupleInstances(protocolName, elem);
      }
      if (!findInstanceForTypeInternal(protocolName, type, this.instances)) {
        const instance = generateSyntheticInstance(protocolName, type, this._resolvedProtocolUsages);
        this.instances.push(instance);
      }
    } else if (type.kind === "con") {
      for (const arg of type.args) {
        this.ensureNestedTupleInstances(protocolName, arg);
      }
    }
  }
  generateEqImplementation(decl, protocol) {
    const typeParams = new Set(decl.params);
    if (decl.recordFields) {
      for (const field of decl.recordFields) {
        this.validateTypeImplementsEq(field.type, decl.span, typeParams, new Set, decl.name);
      }
    }
    if (decl.constructors) {
      for (const ctor of decl.constructors) {
        for (const arg of ctor.args) {
          this.validateTypeImplementsEq(arg, decl.span, typeParams, new Set, decl.name);
        }
      }
    }
    const typeArgs = [
      {
        kind: "con",
        name: decl.name,
        args: decl.params.map((param) => ({
          kind: "var",
          id: freshType().id
        }))
      }
    ];
    const paramMap = new Map;
    const headType = typeArgs[0];
    if (headType && headType.kind === "con") {
      const typeCon = headType;
      decl.params.forEach((p, i) => {
        paramMap.set(p, typeCon.args[i]);
      });
    }
    const constraints = decl.params.map((p) => {
      const tvar = paramMap.get(p);
      if (!tvar)
        throw new Error("Type variable missing");
      return {
        protocolName: "Eq",
        typeArgs: [tvar]
      };
    });
    const methods = new Map;
    const xVar = "x_impl";
    const yVar = "y_impl";
    const span = decl.span;
    let body;
    if (decl.recordFields) {
      const checks = decl.recordFields.map((field) => {
        const node = {
          kind: "Infix",
          left: {
            kind: "FieldAccess",
            target: { kind: "Var", name: xVar, namespace: "lower", span },
            field: field.name,
            span
          },
          operator: "==",
          right: {
            kind: "FieldAccess",
            target: { kind: "Var", name: yVar, namespace: "lower", span },
            field: field.name,
            span
          },
          span
        };
        const fieldType = this.typeFromAnnotation(field.type, paramMap);
        this.ensureNestedTupleInstances("Eq", fieldType);
        this._resolvedProtocolUsages.set(node, {
          protocolName: "Eq",
          typeArgs: [fieldType]
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
          span
        }));
      }
    } else if (decl.constructors) {
      const branches = [];
      const hasMultipleConstructors = decl.constructors.length > 1;
      for (const ctor of decl.constructors) {
        const argsX = ctor.args.map((_, i) => ({
          kind: "VarPattern",
          name: `a_${i}`,
          span
        }));
        const argsY = ctor.args.map((_, i) => ({
          kind: "VarPattern",
          name: `b_${i}`,
          span
        }));
        const patX = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: argsX,
          span
        };
        const patY = {
          kind: "ConstructorPattern",
          name: ctor.name,
          args: argsY,
          span
        };
        const pattern = {
          kind: "TuplePattern",
          elements: [patX, patY],
          span
        };
        const checks = ctor.args.map((arg, i) => {
          const node = {
            kind: "Infix",
            left: { kind: "Var", name: `a_${i}`, namespace: "lower", span },
            operator: "==",
            right: { kind: "Var", name: `b_${i}`, namespace: "lower", span },
            span
          };
          const argType = this.typeFromAnnotation(arg, paramMap);
          this.ensureNestedTupleInstances("Eq", argType);
          this._resolvedProtocolUsages.set(node, {
            protocolName: "Eq",
            typeArgs: [argType]
          });
          return node;
        });
        let branchBody;
        if (checks.length === 0) {
          branchBody = { kind: "Var", name: "True", namespace: "upper", span };
        } else {
          branchBody = checks.reduce((acc, check) => ({
            kind: "Infix",
            left: acc,
            operator: "&&",
            right: check,
            operatorInfo: {
              precedence: 3,
              associativity: "right"
            },
            span
          }));
        }
        branches.push({ pattern, body: branchBody, span });
      }
      if (hasMultipleConstructors || decl.constructors.length === 0) {
        branches.push({
          pattern: { kind: "WildcardPattern", span },
          body: { kind: "Var", name: "False", namespace: "upper", span },
          span
        });
      }
      body = {
        kind: "Case",
        discriminant: {
          kind: "Tuple",
          elements: [
            { kind: "Var", name: xVar, namespace: "lower", span },
            { kind: "Var", name: yVar, namespace: "lower", span }
          ],
          span
        },
        branches,
        span
      };
    } else {
      body = { kind: "Var", name: "True", namespace: "upper", span };
    }
    methods.set("==", {
      kind: "Lambda",
      args: [
        { kind: "VarPattern", name: xVar, span },
        { kind: "VarPattern", name: yVar, span }
      ],
      body,
      span
    });
    return {
      protocolName: protocol.name,
      moduleName: this.getModuleName(),
      typeArgs,
      constraints,
      methods,
      explicitMethods: new Set(["=="]),
      span: decl.span
    };
  }
  validateTypeImplementsEq(type, declSpan, typeParams, checkedTypes, derivingTypeName) {
    const typeKey = JSON.stringify(type);
    if (checkedTypes.has(typeKey))
      return;
    checkedTypes.add(typeKey);
    switch (type.kind) {
      case "FunctionType":
        throw new SemanticError(`Type mismatch: cannot unify 'Int' with 'Int -> Int'. No instance of Eq for function type.`, declSpan, this.getFilePath());
      case "TypeRef": {
        const firstChar = type.name.charAt(0);
        const isTypeVariable = firstChar === firstChar.toLowerCase() && firstChar !== firstChar.toUpperCase();
        if (isTypeVariable && typeParams.has(type.name))
          return;
        if (derivingTypeName && type.name === derivingTypeName)
          return;
        const typeForLookup = {
          kind: "con",
          name: type.name,
          args: []
        };
        if (findInstanceForTypeInternal("Eq", typeForLookup, this.instances)) {
          return;
        }
        if (type.name === "List" && type.args.length === 1) {
          this.validateTypeImplementsEq(type.args[0], declSpan, typeParams, checkedTypes, derivingTypeName);
          return;
        }
        const localDecl = this.getDeclarations().find((d) => (d.kind === "TypeDeclaration" || d.kind === "TypeAliasDeclaration") && d.name === type.name);
        if (localDecl) {
          if (localDecl.kind === "TypeDeclaration") {
            const hasExplicit = this.getDeclarations().some((d) => d.kind === "ImplementationDeclaration" && d.protocolName === "Eq" && d.typeArgs.length > 0 && d.typeArgs[0].kind === "TypeRef" && d.typeArgs[0].name === type.name);
            if (hasExplicit)
              return;
            throw new SemanticError(`Type '${type.name}' does not implement 'Eq'. Implicit 'Eq' requires all fields to implement 'Eq'.`, declSpan, this.getFilePath());
          }
        }
        type.args.forEach((arg) => this.validateTypeImplementsEq(arg, declSpan, typeParams, checkedTypes, derivingTypeName));
        return;
      }
      case "TupleType":
        type.elements.forEach((elem) => this.validateTypeImplementsEq(elem, declSpan, typeParams, checkedTypes, derivingTypeName));
        return;
      case "RecordType":
        type.fields.forEach((f) => this.validateTypeImplementsEq(f.type, declSpan, typeParams, checkedTypes, derivingTypeName));
        return;
      case "QualifiedType":
        this.validateTypeImplementsEq(type.type, declSpan, typeParams, checkedTypes, derivingTypeName);
        return;
      default:
        return;
    }
  }
  autoImplementProtocols() {
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
  autoImplementProtocolForType(protocolName, decl) {
    const protocol = this.protocols[protocolName];
    if (!protocol)
      return;
    const isVibe = protocol.moduleName === "Vibe" || protocol.moduleName === "Vibe.Basics";
    if (!isVibe)
      return;
    if (!this.canDeclImplementProtocol(decl, protocolName)) {
      return;
    }
    for (const existing of this.instances) {
      if (existing.protocolName !== protocolName)
        continue;
      if (existing.typeArgs.length === 0)
        continue;
      const typeArg = existing.typeArgs[0];
      if (typeArg?.kind === "con" && typeArg.name === decl.name) {
        return;
      }
    }
    if (protocolName === "Eq") {
      return this.generateEqImplementation(decl, protocol);
    } else if (protocolName === "Show") {
      return this.generateShowImplementation(decl);
    }
    return;
  }
  registerValueDeclarations() {
    for (const decl of this.getDeclarations()) {
      if (decl.kind === "ValueDeclaration" || decl.kind === "DecoratedDeclaration") {
        if (decl.kind === "DecoratedDeclaration") {
          this.validateDecoratedDeclaration(decl);
        }
        this.registerValue(decl);
        continue;
      }
      if (decl.kind === "TypeAnnotationDeclaration") {
        if (Object.hasOwn(this.annotations, decl.name)) {
          throw new SemanticError(`Duplicate type annotation for '${decl.name}'`, decl.span, this.getFilePath());
        }
        this.annotations[decl.name] = decl;
      }
    }
  }
  canDeclImplementProtocol(decl, protocolName) {
    const typeParams = new Set(decl.params);
    if (decl.recordFields) {
      for (const field of decl.recordFields) {
        if (!this.canTypeImplementProtocol(field.type, protocolName, typeParams)) {
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
  canTypeImplementProtocol(type, protocolName, typeParams, checkedTypes = new Set) {
    const typeKey = JSON.stringify(type);
    if (checkedTypes.has(typeKey))
      return true;
    checkedTypes.add(typeKey);
    const typeMatchesInstanceArg = (instanceArg, typeName) => {
      return instanceArg.kind === "con" && instanceArg.name === typeName;
    };
    switch (type.kind) {
      case "FunctionType":
        const hasFunctionInstance = this.instances.some((inst) => inst.protocolName === protocolName && inst.typeArgs.length > 0 && inst.typeArgs[0].kind === "fun");
        return hasFunctionInstance;
      case "TypeRef": {
        const firstChar = type.name.charAt(0);
        if (firstChar === firstChar.toLowerCase() && firstChar !== firstChar.toUpperCase()) {
          return true;
        }
        const hasInstance = this.instances.some((inst) => inst.protocolName === protocolName && inst.typeArgs.length > 0 && typeMatchesInstanceArg(inst.typeArgs[0], type.name));
        if (hasInstance) {
          return type.args.every((arg) => this.canTypeImplementProtocol(arg, protocolName, typeParams, checkedTypes));
        }
        const protocol = this.protocols[protocolName];
        if (!protocol) {
          return false;
        }
        const allMethodsHaveDefaults = Array.from(protocol.methods.values()).every((method) => method.defaultImpl !== undefined);
        if (allMethodsHaveDefaults) {
          return true;
        }
        const localDecl = this.getDeclarations().find((d) => (d.kind === "TypeDeclaration" || d.kind === "TypeAliasDeclaration") && d.name === type.name);
        if (localDecl && localDecl.kind === "TypeDeclaration") {
          return type.args.every((arg) => this.canTypeImplementProtocol(arg, protocolName, typeParams, checkedTypes));
        }
        if (localDecl && localDecl.kind === "TypeAliasDeclaration") {
          return type.args.every((arg) => this.canTypeImplementProtocol(arg, protocolName, typeParams, checkedTypes));
        }
        return false;
      }
      case "TupleType":
        return type.elements.every((elem) => this.canTypeImplementProtocol(elem, protocolName, typeParams, checkedTypes));
      case "RecordType":
        return type.fields.every((f) => this.canTypeImplementProtocol(f.type, protocolName, typeParams, checkedTypes));
      case "QualifiedType":
        return this.canTypeImplementProtocol(type.type, protocolName, typeParams, checkedTypes);
      default:
        return false;
    }
  }
  validateInfixDeclarationsHaveDefinitions() {
    for (const decl of this.infixDeclarations) {
      const op = decl.operator;
      const hasLocalDefinition = Object.hasOwn(this.values, op);
      const isLocalProtocolMethod = this.localProtocolMethods.has(op);
      if (!hasLocalDefinition && !isLocalProtocolMethod) {
        throw new SemanticError(`Infix declaration for operator '${op}' has no corresponding function definition. ` + `Define the operator in this module or remove the fixity declaration.`, decl.span, this.getFilePath());
      }
    }
  }
  validateDecoratedDeclaration(decl) {
    const validDecorators = ["external", "get", "call", "val", "import"];
    if (!validDecorators.includes(decl.decorator)) {
      throw new SemanticError(`Unknown decorator '@${decl.decorator}'. Valid decorators are: @external, @get, @call, @val, @import`, decl.span, this.getFilePath());
    }
    switch (decl.decorator) {
      case "external":
        if (decl.args.length !== 2) {
          throw new SemanticError(`@external requires exactly 2 string arguments (module path and export name), got ${decl.args.length}`, decl.span, this.getFilePath());
        }
        break;
      case "get":
      case "call":
      case "val":
        if (decl.args.length !== 1) {
          throw new SemanticError(`@${decl.decorator} requires exactly 1 string argument (property key), got ${decl.args.length}`, decl.span, this.getFilePath());
        }
        break;
      case "import":
        if (decl.args.length !== 1) {
          throw new SemanticError(`@import requires exactly 1 string argument (module path), got ${decl.args.length}`, decl.span, this.getFilePath());
        }
        break;
    }
  }
  isFFIDeclaration(decl) {
    return decl.kind === "DecoratedDeclaration";
  }
  validateDecoratedPropertyType(decl, type) {
    const variant = decl.decorator;
    if (variant === "val") {
      return;
    }
    const params = flattenFunctionParams(type);
    if (variant === "get") {
      if (params.length !== 1) {
        throw new SemanticError(`@get declaration '${decl.name}' must have type A -> B (exactly one argument), got ${params.length} argument(s)`, decl.span, this.getFilePath());
      }
    } else if (variant === "call") {
      if (params.length < 1) {
        throw new SemanticError(`@call declaration '${decl.name}' must have at least one argument`, decl.span, this.getFilePath());
      }
    }
  }
  validateAnnotationsAndSeedGlobalNames() {
    this.validateInfixDeclarationsHaveDefinitions();
    for (const [name, ann] of Object.entries(this.annotations)) {
      if (!Object.hasOwn(this.values, name)) {
        throw new SemanticError(`Type annotation for '${name}' has no matching definition`, ann.span, this.getFilePath());
      }
      const value = this.values[name];
      if (value.declaration.kind === "DecoratedDeclaration") {
        throw new SemanticError(`Decorated declaration '${name}' already includes a type annotation`, ann.span, this.getFilePath());
      }
      value.annotation = ann.annotation;
    }
    for (const [name, info] of Object.entries(this.values)) {
      const annotationExpr = info.annotation ?? (info.declaration.kind === "DecoratedDeclaration" ? info.declaration.annotation : undefined);
      let annotationType;
      let annotatedConstraints;
      if (annotationExpr) {
        const typeVars = collectTypeVariables(annotationExpr);
        const validationErrors = validateTypeExpr(this, annotationExpr, typeVars, info.declaration.span);
        if (validationErrors.length > 0) {
          const err = validationErrors[0];
          const message = err.suggestion ? `${err.message}. ${err.suggestion}` : err.message;
          throw new SemanticError(message, err.span, this.getFilePath());
        }
        const result = this.typeFromAnnotationWithConstraints(annotationExpr, new Map);
        annotationType = result.type;
        annotatedConstraints = result.constraints.length > 0 ? result.constraints : undefined;
        if (annotatedConstraints) {
          info.annotatedConstraints = annotatedConstraints;
        }
        if (result.paramNames.size > 0) {
          info.annotatedParamNames = result.paramNames;
        }
      }
      const seeded = annotationType ?? this.seedValueType(info.declaration);
      this.declareSymbol(this.globalScope, name, { vars: new Set, constraints: [], type: seeded }, info.declaration.span);
      this.types[name] = seeded;
    }
  }
  typeFromAnnotationWithConstraints(annotation, context = new Map) {
    const { protocols } = this;
    if (annotation.kind === "QualifiedType") {
      const constraints = [];
      for (const astConstraint of annotation.constraints) {
        const protocol = protocols[astConstraint.protocolName];
        if (!protocol) {
          throw new SemanticError(`Unknown protocol '${astConstraint.protocolName}' in type constraint`, astConstraint.span, this.getFilePath());
        }
        if (astConstraint.typeArgs.length !== protocol.params.length) {
          throw new SemanticError(`Protocol '${astConstraint.protocolName}' expects ${protocol.params.length} type argument(s), but constraint has ${astConstraint.typeArgs.length}`, astConstraint.span, this.getFilePath());
        }
        const constraintTypeArgs = [];
        for (const typeArg of astConstraint.typeArgs) {
          constraintTypeArgs.push(this.typeFromAnnotation(typeArg, context));
        }
        for (let i = 0;i < constraintTypeArgs.length; i++) {
          const typeArg = constraintTypeArgs[i];
          if (typeArg.kind !== "var") {
            throw new SemanticError(`Constraint '${astConstraint.protocolName}' must be applied to type variables, not concrete types`, astConstraint.span, this.getFilePath());
          }
        }
        constraints.push({
          protocolName: astConstraint.protocolName,
          typeArgs: constraintTypeArgs
        });
      }
      const innerResult = this.typeFromAnnotationWithConstraints(annotation.type, context);
      const paramNames2 = invertContext(context);
      return {
        type: innerResult.type,
        constraints: [...constraints, ...innerResult.constraints],
        paramNames: paramNames2
      };
    }
    const type = this.typeFromAnnotation(annotation, context);
    const paramNames = invertContext(context);
    return { type, constraints: [], paramNames };
  }
  inferValueDeclarations() {
    this.concretizeInstanceTypeArgs();
    const depGraph = buildDependencyGraph(this.values);
    const sccs = computeSCCs(depGraph);
    for (const scc of sccs) {
      const externals = [];
      const valueDecls = [];
      for (const name of scc) {
        const info = this.values[name];
        if (this.isFFIDeclaration(info.declaration)) {
          externals.push(name);
        } else {
          valueDecls.push(name);
        }
      }
      for (const name of externals) {
        const info = this.values[name];
        const decl = info.declaration;
        if (decl.kind === "DecoratedDeclaration") {
          const result = this.typeFromAnnotationWithConstraints(decl.annotation, new Map);
          info.type = result.type;
          if (result.constraints.length > 0) {
            info.annotatedConstraints = result.constraints;
          }
          if (decl.decorator === "get" || decl.decorator === "call") {
            this.validateDecoratedPropertyType(decl, info.type);
          }
          const freeVars = getFreeTypeVars(info.type, this.substitution);
          const scheme = {
            vars: freeVars,
            constraints: result.constraints,
            type: info.type,
            paramNames: result.paramNames.size > 0 ? result.paramNames : undefined
          };
          this.globalScope.symbols.set(info.declaration.name, scheme);
          this.typeSchemes[name] = scheme;
        }
      }
      if (valueDecls.length > 0) {
        this.resetConstraintContext();
        const inferredTypes = new Map;
        for (const name of valueDecls) {
          const info = this.values[name];
          if (info.declaration.kind !== "ValueDeclaration")
            continue;
          const declaredType = this.types[name];
          const annotationType = info.annotation ? this.typeFromAnnotation(info.annotation, new Map) : undefined;
          const inferred = this.analyzeValueDeclaration(info.declaration, this.globalScope, this.substitution, declaredType, annotationType);
          inferredTypes.set(name, inferred);
          this.types[name] = inferred;
          info.type = inferred;
        }
        for (const name of valueDecls) {
          const info = this.values[name];
          const inferred = inferredTypes.get(name);
          const generalizedScheme = this.generalizeWithAnnotatedConstraints(inferred, new Scope, this.substitution, info.annotatedConstraints, info.declaration.span);
          if (info.annotatedParamNames) {
            generalizedScheme.paramNames = resolveParamNames(info.annotatedParamNames, this.substitution);
          }
          this.globalScope.define(name, generalizedScheme);
          this.typeSchemes[name] = generalizedScheme;
        }
        for (const usage of this._pendingProtocolUsages) {
          const resolvedArgs = usage.constraint.typeArgs.map((t) => applySubstitution(t, this.substitution));
          this._resolvedProtocolUsages.set(usage.node, {
            protocolName: usage.constraint.protocolName,
            typeArgs: resolvedArgs
          });
        }
      }
    }
  }
  generalizeWithAnnotatedConstraints(type, scope, substitution, annotatedConstraints, span) {
    const typeFreeVars = getFreeTypeVars(type, substitution);
    const scopeFreeVars = getFreeTypeVarsInScope(scope, substitution);
    const quantified = new Set;
    for (const v of typeFreeVars) {
      if (!scopeFreeVars.has(v)) {
        quantified.add(v);
      }
    }
    const rawConstraints = this.getCollectedConstraints();
    const resolvedConstraints = applySubstitutionToConstraints(rawConstraints, substitution);
    for (const c of resolvedConstraints) {
      const resolvedTypeArgs = c.typeArgs.map((t) => applySubstitution(t, substitution));
      const isFullyPolymorphic = resolvedTypeArgs.every((t) => {
        if (t.kind === "var" && quantified.has(t.id)) {
          return true;
        }
        return false;
      });
      if (isFullyPolymorphic) {
        continue;
      }
      const resolvedConstraint = {
        protocolName: c.protocolName,
        typeArgs: resolvedTypeArgs
      };
      const lookupResult = validateConstraintSatisfiable(resolvedConstraint, this.instances);
      if (!lookupResult.found) {
        const typeArgsStr = resolvedTypeArgs.map((t) => formatType(t)).join(", ");
        if (lookupResult.reason === "unsatisfied-constraint") {
          throw new SemanticError(`No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` + `The instance requires '${lookupResult.constraint}' for '${lookupResult.forType}', ` + `but no such instance exists.`, span, this.getFilePath());
        } else {
          throw new SemanticError(`No instance of '${c.protocolName}' for type(s) '${typeArgsStr}'. ` + `Add an implementation: implement ${c.protocolName} ${typeArgsStr} where ...`, span, this.getFilePath());
        }
      }
    }
    const inferredRelevantConstraints = [];
    for (const c of resolvedConstraints) {
      const typeArgFreeVars = [];
      for (const t of c.typeArgs) {
        const freeVars = getFreeTypeVars(t, substitution);
        for (const v of freeVars) {
          typeArgFreeVars.push(v);
        }
      }
      const hasQuantifiedVar = typeArgFreeVars.some((v) => quantified.has(v));
      const hasUnquantifiedVar = typeArgFreeVars.some((v) => !quantified.has(v));
      if (typeArgFreeVars.length === 0) {
        continue;
      }
      if (hasUnquantifiedVar && !hasQuantifiedVar) {
        const typeArgsStr = c.typeArgs.map((t) => formatType(t)).join(", ");
        throw new SemanticError(`Ambiguous type variable in '${c.protocolName}' constraint. ` + `The type '${typeArgsStr}' contains type variable(s) that do not appear ` + `in the expression's type, so they cannot be determined. ` + `Consider adding a type annotation to make the type concrete.`, span, this.getFilePath());
      }
      if (hasQuantifiedVar) {
        inferredRelevantConstraints.push(c);
      }
    }
    let allConstraints = [...inferredRelevantConstraints];
    if (annotatedConstraints && annotatedConstraints.length > 0) {
      const resolvedAnnotated = applySubstitutionToConstraints(annotatedConstraints, substitution);
      for (const c of resolvedAnnotated) {
        for (const typeArg of c.typeArgs) {
          const freeVars = getFreeTypeVars(typeArg, substitution);
          let hasQuantifiedVar = false;
          for (const v of freeVars) {
            if (quantified.has(v)) {
              hasQuantifiedVar = true;
            }
          }
          if (freeVars.size === 0) {
            throw new SemanticError(`Constraint '${c.protocolName}' is on a concrete type, which is not allowed in type annotations`, span, this.getFilePath());
          }
          if (!hasQuantifiedVar) {
            throw new SemanticError(`Constraint '${c.protocolName}' references type variables not used in the function type`, span, this.getFilePath());
          }
        }
        allConstraints.push(c);
      }
    }
    const uniqueConstraints = [];
    for (const c of allConstraints) {
      const isDuplicate = uniqueConstraints.some((uc) => uc.protocolName === c.protocolName && uc.typeArgs.length === c.typeArgs.length && uc.typeArgs.every((t, i) => typesEqual(t, c.typeArgs[i])));
      if (!isDuplicate) {
        uniqueConstraints.push(c);
      }
    }
    const constraintsToRemove = [];
    for (const c of uniqueConstraints) {
      const resolvedTypeArgs = c.typeArgs.map((t) => applySubstitution(t, substitution));
      const varPositions = [];
      const concretePositions = [];
      for (let i = 0;i < resolvedTypeArgs.length; i++) {
        const arg = resolvedTypeArgs[i];
        if (arg.kind === "var" && quantified.has(arg.id)) {
          varPositions.push(i);
        } else if (arg.kind !== "var") {
          concretePositions.push(i);
        }
      }
      if (concretePositions.length > 0 && varPositions.length > 0) {
        const matchingInstances = this.instances.filter((inst) => {
          if (inst.protocolName !== c.protocolName)
            return false;
          if (inst.typeArgs.length !== resolvedTypeArgs.length)
            return false;
          for (const pos of concretePositions) {
            const instArg = inst.typeArgs[pos];
            const constraintArg = resolvedTypeArgs[pos];
            if (!instanceTypeMatches(instArg, constraintArg))
              return false;
          }
          for (const pos of varPositions) {
            const instArg = inst.typeArgs[pos];
            if (instArg.kind === "var")
              return false;
          }
          return true;
        });
        if (matchingInstances.length === 1) {
          const matchingInst = matchingInstances[0];
          for (const pos of varPositions) {
            const typeVar = resolvedTypeArgs[pos];
            const instType = matchingInst.typeArgs[pos];
            if (typeVar.kind === "var") {
              substitution.set(typeVar.id, instType);
              quantified.delete(typeVar.id);
            }
          }
          constraintsToRemove.push(c);
        }
      }
    }
    const finalConstraints = uniqueConstraints.filter((c) => !constraintsToRemove.includes(c));
    const finalType = applySubstitution(type, substitution);
    const finalTypeFreeVars = getFreeTypeVars(finalType, substitution);
    for (const c of finalConstraints) {
      for (const typeArg of c.typeArgs) {
        const resolvedTypeArg = applySubstitution(typeArg, substitution);
        const constraintFreeVars = getFreeTypeVars(resolvedTypeArg, substitution);
        for (const v of constraintFreeVars) {
          if (!finalTypeFreeVars.has(v)) {
            throw new SemanticError(`Ambiguous type variable in '${c.protocolName}' constraint. ` + `The type variable does not appear in the expression's type, ` + `so it cannot be determined. Consider adding a type annotation ` + `to make the type concrete.`, span, this.getFilePath());
          }
        }
      }
    }
    return { vars: quantified, constraints: finalConstraints, type: finalType };
  }
  validateImplementationMethodExpressions() {
    for (const instance of this.localInstances) {
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        if (methodExpr) {
          this.validateExpressionIdentifiers(methodExpr, this.globalScope, instance.protocolName, methodName);
        }
      }
    }
  }
  validateExpressionIdentifiers(expr, scope, protocolName, methodName) {
    const validate = (e, s = scope) => this.validateExpressionIdentifiers(e, s, protocolName, methodName);
    switch (expr.kind) {
      case "Var": {
        const name = expr.name;
        if (this.constructors[name]) {
          return;
        }
        if (!symbolExists(scope, name)) {
          this.addError(`Undefined name '${name}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span);
        }
        return;
      }
      case "Lambda": {
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
        const childScope = new Scope(scope);
        for (const binding of expr.bindings) {
          validate(binding.body);
          childScope.symbols.set(binding.name, {
            vars: new Set,
            constraints: [],
            type: { kind: "var", id: -1 }
          });
        }
        validate(expr.body, childScope);
        return;
      }
      case "Case": {
        validate(expr.discriminant);
        for (const branch of expr.branches) {
          const branchScope = new Scope(scope);
          bindPatternNames(branch.pattern, branchScope);
          validate(branch.body, branchScope);
        }
        return;
      }
      case "Infix": {
        if (!symbolExists(scope, expr.operator)) {
          throw new SemanticError(`Undefined operator '${expr.operator}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
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
        if (!symbolExists(scope, expr.base)) {
          this.addError(`Undefined name '${expr.base}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span);
        }
        for (const field of expr.fields) {
          validate(field.value);
        }
        return;
      }
      case "FieldAccess": {
        const resolved = this.validateModuleFieldAccess(expr, protocolName, methodName);
        if (!resolved) {
          validate(expr.target);
        }
        return;
      }
      case "Number":
      case "String":
      case "Char":
      case "Unit":
        return;
    }
  }
  validateModuleFieldAccess(expr, protocolName, methodName) {
    const { imports, dependencies } = this;
    const parts = [];
    let current = expr;
    while (current.kind === "FieldAccess") {
      parts.unshift(current.field);
      current = current.target;
    }
    if (current.kind !== "Var") {
      return false;
    }
    const baseName = current.name;
    parts.unshift(baseName);
    for (const imp of imports) {
      if (imp.alias && baseName === imp.alias && parts.length >= 2) {
        const depModule = dependencies.get(imp.moduleName);
        if (!depModule) {
          throw new SemanticError(`Module '${imp.moduleName}' (aliased as '${imp.alias}') not found in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
        }
        const fieldName = parts[1];
        const valueInfo = depModule.values[fieldName];
        if (valueInfo) {
          if (!isExportedFromModule(depModule, fieldName, "value")) {
            throw new SemanticError(`'${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
          return true;
        }
        const ctorInfo = depModule.constructors[fieldName];
        if (ctorInfo) {
          if (!isExportedFromModule(depModule, fieldName, "constructor")) {
            throw new SemanticError(`Constructor '${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
          return true;
        }
        throw new SemanticError(`'${fieldName}' is not defined in module '${imp.moduleName}' (aliased as '${imp.alias}') in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
      }
      const importParts = imp.moduleName.split(".");
      if (!imp.alias && importParts.length <= parts.length - 1) {
        let matches = true;
        for (let i = 0;i < importParts.length; i++) {
          if (importParts[i] !== parts[i]) {
            matches = false;
            break;
          }
        }
        if (matches) {
          const depModule = dependencies.get(imp.moduleName);
          if (!depModule) {
            throw new SemanticError(`Module '${imp.moduleName}' not found in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
          const fieldParts = parts.slice(importParts.length);
          if (fieldParts.length === 1) {
            const fieldName = fieldParts[0];
            const valueInfo = depModule.values[fieldName];
            if (valueInfo) {
              if (!isExportedFromModule(depModule, fieldName, "value")) {
                throw new SemanticError(`'${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
              }
              return true;
            }
            const ctorInfo = depModule.constructors[fieldName];
            if (ctorInfo) {
              if (!isExportedFromModule(depModule, fieldName, "constructor")) {
                throw new SemanticError(`Constructor '${fieldName}' is not exported from module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
              }
              return true;
            }
            throw new SemanticError(`'${fieldName}' is not defined in module '${imp.moduleName}' in implementation of '${methodName}' for protocol '${protocolName}'`, expr.span, this.getFilePath());
          }
        }
      }
    }
    return false;
  }
  validateInstanceConstraintSatisfiability() {
    for (const instance of this.localInstances) {
      if (instance.constraints.length === 0)
        continue;
      for (const constraint of instance.constraints) {
        const constraintProtocol = this.protocols[constraint.protocolName];
        if (!constraintProtocol) {
          throw new SemanticError(`Instance constraint references unknown protocol '${constraint.protocolName}'`, instance.span, this.getFilePath());
        }
      }
    }
  }
  validateProtocolDefaultImplementations() {
    for (const [protocolName, protocol] of Object.entries(this.protocols)) {
      if (protocol.moduleName !== this.getModuleName())
        continue;
      for (const [methodName, methodInfo] of protocol.methods) {
        if (methodInfo.defaultImpl) {
          const methodScope = new Scope(this.globalScope);
          for (const arg of methodInfo.defaultImpl.args) {
            bindPatternNames(arg, methodScope);
          }
          this.validateExpressionIdentifiers(methodInfo.defaultImpl.body, methodScope, protocolName, methodName);
        }
      }
    }
  }
  computeModuleExports() {
    const moduleDecl = this.getModule();
    const {
      values,
      operators,
      adts,
      typeAliases,
      opaqueTypes,
      records,
      protocols,
      importedValues
    } = this;
    const exports = {
      values: new Set,
      operators: new Set,
      types: new Map,
      protocols: new Map,
      exportsAll: false,
      reExportedValues: new Map
    };
    if (!moduleDecl.exposing) {
      return exports;
    }
    const exposing = moduleDecl.exposing;
    if (exposing.kind === "All") {
      exports.exportsAll = true;
      for (const name of Object.keys(values)) {
        exports.values.add(name);
      }
      for (const op of operators.keys()) {
        exports.operators.add(op);
      }
      for (const [name, adt] of Object.entries(adts)) {
        exports.types.set(name, {
          allConstructors: true,
          constructors: new Set(adt.constructors)
        });
      }
      for (const name of Object.keys(typeAliases)) {
        exports.types.set(name, { allConstructors: false });
      }
      for (const name of Object.keys(opaqueTypes)) {
        exports.types.set(name, { allConstructors: false });
      }
      for (const name of Object.keys(records)) {
        exports.types.set(name, { allConstructors: false });
      }
      for (const [name, protocol] of Object.entries(protocols)) {
        exports.protocols.set(name, {
          allMethods: true,
          methods: new Set(protocol.methods.keys())
        });
      }
      return exports;
    }
    for (const spec of exposing.exports) {
      switch (spec.kind) {
        case "ExportValue": {
          const name = spec.name;
          if (Object.hasOwn(values, name)) {
            exports.values.add(name);
            continue;
          }
          if (importedValues.has(name)) {
            exports.reExportedValues.set(name, importedValues.get(name));
            continue;
          }
          if (Object.hasOwn(adts, name)) {
            exports.types.set(name, {
              allConstructors: false,
              constructors: new Set
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
          if (Object.hasOwn(protocols, name)) {
            exports.protocols.set(name, {
              allMethods: false,
              methods: new Set
            });
            continue;
          }
          throw new SemanticError(`Module exposes '${name}' which is not defined`, spec.span, this.getFilePath());
        }
        case "ExportOperator": {
          const op = spec.operator;
          if (!Object.hasOwn(values, op) && !operators.has(op) && !importedValues.has(op)) {
            throw new SemanticError(`Module exposes operator '${op}' which is not defined`, spec.span, this.getFilePath());
          }
          exports.operators.add(op);
          if (Object.hasOwn(values, op)) {
            exports.values.add(op);
          } else if (importedValues.has(op)) {
            exports.reExportedValues.set(op, importedValues.get(op));
          }
          break;
        }
        case "ExportTypeAll": {
          const name = spec.name;
          if (Object.hasOwn(adts, name)) {
            const adt = adts[name];
            exports.types.set(name, {
              allConstructors: true,
              constructors: new Set(adt.constructors)
            });
            continue;
          }
          if (Object.hasOwn(protocols, name)) {
            const protocol = protocols[name];
            exports.protocols.set(name, {
              allMethods: true,
              methods: new Set(protocol.methods.keys())
            });
            continue;
          }
          if (Object.hasOwn(typeAliases, name)) {
            throw new SemanticError(`Type alias '${name}' cannot use (..) syntax - type aliases have no constructors`, spec.span, this.getFilePath());
          }
          if (Object.hasOwn(opaqueTypes, name)) {
            throw new SemanticError(`Opaque type '${name}' cannot use (..) syntax - opaque types have no constructors`, spec.span, this.getFilePath());
          }
          if (Object.hasOwn(records, name)) {
            throw new SemanticError(`Record type '${name}' cannot use (..) syntax - records have no constructors. Use '${name}' instead`, spec.span, this.getFilePath());
          }
          throw new SemanticError(`Module exposes '${name}(..)' but '${name}' is not a type or protocol`, spec.span, this.getFilePath());
        }
        case "ExportTypeSome": {
          const name = spec.name;
          const members = spec.members;
          if (Object.hasOwn(adts, name)) {
            const adt = adts[name];
            const exportedCtors = new Set;
            for (const memberName of members) {
              if (!adt.constructors.includes(memberName)) {
                throw new SemanticError(`Constructor '${memberName}' is not defined in type '${name}'`, spec.span, this.getFilePath());
              }
              exportedCtors.add(memberName);
            }
            exports.types.set(name, {
              allConstructors: false,
              constructors: exportedCtors
            });
            continue;
          }
          if (Object.hasOwn(protocols, name)) {
            const protocol = protocols[name];
            const exportedMethods = new Set;
            for (const memberName of members) {
              if (!protocol.methods.has(memberName)) {
                throw new SemanticError(`Method '${memberName}' is not defined in protocol '${name}'`, spec.span, this.getFilePath());
              }
              exportedMethods.add(memberName);
              exports.values.add(memberName);
            }
            exports.protocols.set(name, {
              allMethods: false,
              methods: exportedMethods
            });
            continue;
          }
          throw new SemanticError(`Module exposes '${name}(...)' but '${name}' is not a type or protocol`, spec.span, this.getFilePath());
        }
      }
    }
    return exports;
  }
  buildSemanticModule() {
    const exports = this.computeModuleExports();
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
      protocolMethodUsages: this._resolvedProtocolUsages
    };
  }
  concretizeInstanceTypeArgs() {
    for (const instance of this.localInstances) {
      const protocol = this.protocols[instance.protocolName];
      if (!protocol)
        continue;
      const hasTypeVars = instance.typeArgs.some((t) => t.kind === "var");
      if (!hasTypeVars)
        continue;
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        const protocolMethodInfo = protocol.methods.get(methodName);
        if (!methodExpr || !protocolMethodInfo)
          continue;
        const expectedType = substituteTypeParams(protocolMethodInfo.type, protocol.params, instance.typeArgs);
        const inferSubstitution = new Map(this._substitution);
        const tempScope = this.createChildScope();
        try {
          const inferredType = this.analyzeExpr(methodExpr, {
            scope: tempScope,
            substitution: inferSubstitution
          });
          this.unify(inferredType, expectedType, methodExpr.span, inferSubstitution);
          for (let i = 0;i < instance.typeArgs.length; i++) {
            const typeArg = instance.typeArgs[i];
            const resolved = applySubstitution(typeArg, inferSubstitution);
            if (resolved.kind !== "var" && typeArg.kind === "var") {
              instance.typeArgs[i] = resolved;
            }
          }
          const constraintsToRemove = [];
          for (const constraint of instance.constraints) {
            let allConcrete = true;
            for (let i = 0;i < constraint.typeArgs.length; i++) {
              const typeArg = constraint.typeArgs[i];
              const resolved = applySubstitution(typeArg, inferSubstitution);
              if (resolved.kind !== "var" && typeArg.kind === "var") {
                constraint.typeArgs[i] = resolved;
              }
              if (constraint.typeArgs[i].kind === "var") {
                allConcrete = false;
              }
            }
            if (allConcrete) {
              const isSatisfied = findInstanceForConstraint(constraint.protocolName, constraint.typeArgs, this.instances);
              if (isSatisfied) {
                constraintsToRemove.push(constraint);
              }
            }
          }
          for (const toRemove of constraintsToRemove) {
            const idx = instance.constraints.indexOf(toRemove);
            if (idx !== -1) {
              instance.constraints.splice(idx, 1);
            }
          }
        } catch {}
      }
    }
  }
  validateImplementationMethodTypes() {
    for (const instance of this.localInstances) {
      const protocol = this.protocols[instance.protocolName];
      if (!protocol)
        continue;
      const paramSubstitution = new Map;
      const paramNameToId = new Map;
      for (let i = 0;i < protocol.params.length; i++) {
        const paramName = protocol.params[i];
        const typeArg = instance.typeArgs[i];
        if (typeArg) {
          const paramVar = freshType();
          paramNameToId.set(paramName, paramVar.id);
          paramSubstitution.set(paramVar.id, typeArg);
        }
      }
      for (const methodName of instance.explicitMethods) {
        const methodExpr = instance.methods.get(methodName);
        const protocolMethodInfo = protocol.methods.get(methodName);
        if (!methodExpr || !protocolMethodInfo)
          continue;
        const expectedType = substituteTypeParams(protocolMethodInfo.type, protocol.params, instance.typeArgs);
        const inferSubstitution = new Map(this._substitution);
        const tempScope = this.createChildScope();
        let inferredType;
        if (methodExpr.kind === "Lambda" && expectedType.kind === "fun") {
          const expectedParamTypes = [];
          let currentType = expectedType;
          while (currentType.kind === "fun" && expectedParamTypes.length < methodExpr.args.length) {
            expectedParamTypes.push(currentType.from);
            currentType = currentType.to;
          }
          this.bindPatterns(tempScope, methodExpr.args, expectedParamTypes, inferSubstitution);
          const bodyType = this.analyzeExpr(methodExpr.body, {
            scope: tempScope,
            substitution: inferSubstitution
          });
          inferredType = fnChain(expectedParamTypes, bodyType);
        } else {
          inferredType = this.analyzeExpr(methodExpr, {
            scope: tempScope,
            substitution: inferSubstitution,
            expectedType
          });
        }
        try {
          this.unify(inferredType, expectedType, methodExpr.span, inferSubstitution);
        } catch (e) {
          if (e instanceof SemanticError) {
            throw new SemanticError(`Implementation of '${methodName}' for '${instance.protocolName}' has type '${formatType(applySubstitution(inferredType, inferSubstitution))}' but protocol expects '${formatType(expectedType)}'`, methodExpr.span, this.getFilePath());
          }
          throw e;
        }
        for (const constraint of instance.constraints) {
          for (const constraintTypeArg of constraint.typeArgs) {
            const resolvedType = applySubstitution(constraintTypeArg, inferSubstitution);
            if (resolvedType.kind === "con") {
              const hasInstance = findInstanceForTypeInternal(constraint.protocolName, resolvedType, this.instances);
              if (!hasInstance) {
                throw new SemanticError(`Implementation of '${methodName}' for '${instance.protocolName}' ` + `requires '${constraint.protocolName}' constraint on type parameter, ` + `but the implementation uses type '${formatType(resolvedType)}' ` + `which does not implement '${constraint.protocolName}'`, methodExpr.span, this.getFilePath());
              }
            }
          }
        }
        for (let i = 0;i < instance.typeArgs.length; i++) {
          const typeArg = instance.typeArgs[i];
          const resolved = applySubstitution(typeArg, inferSubstitution);
          if (resolved.kind !== "var" && typeArg.kind === "var") {
            instance.typeArgs[i] = resolved;
          }
        }
        for (const constraint of instance.constraints) {
          for (let i = 0;i < constraint.typeArgs.length; i++) {
            const typeArg = constraint.typeArgs[i];
            const resolved = applySubstitution(typeArg, inferSubstitution);
            if (resolved.kind !== "var" && typeArg.kind === "var") {
              constraint.typeArgs[i] = resolved;
            }
          }
        }
      }
    }
  }
  validateConcreteConstraintInstances() {
    for (const [valueName, valueInfo] of Object.entries(this.values)) {
      if (valueName.startsWith("$"))
        continue;
      if (valueInfo.collectedConstraints) {
        for (const constraint of valueInfo.collectedConstraints) {
          const resolvedTypeArgs = constraint.typeArgs.map((t) => applySubstitution(t, this._substitution));
          for (const typeArg of resolvedTypeArgs) {
            if (typeArg.kind === "con") {
              const hasInstance = findInstanceForTypeInternal(constraint.protocolName, typeArg, this.instances);
              if (!hasInstance) {
                const span = valueInfo.span ?? valueInfo.declaration.span;
                throw new SemanticError(`No instance of '${constraint.protocolName}' for type '${formatType(typeArg)}'. ` + `Add an implementation: implement ${constraint.protocolName} ${formatType(typeArg)} where ...`, span, this.getFilePath());
              }
            }
          }
        }
      }
    }
  }
  validateConstraintsEagerly(substitution, span) {
    const constraints = this.getCollectedConstraints();
    if (constraints.length === 0)
      return;
    for (const c of constraints) {
      const resolvedTypeArgs = c.typeArgs.map((t) => applySubstitution(t, substitution));
      const hasConcreteNonVarArg = resolvedTypeArgs.some((t) => {
        if (t.kind === "var")
          return false;
        if (t.kind === "fun")
          return true;
        if (t.kind === "tuple")
          return true;
        if (t.kind === "record")
          return true;
        if (t.kind === "con") {
          return false;
        }
        return false;
      });
      if (!hasConcreteNonVarArg)
        continue;
      const resolvedConstraint = {
        protocolName: c.protocolName,
        typeArgs: resolvedTypeArgs
      };
      const satisfiability = checkConstraintSatisfiability(resolvedConstraint, this.instances);
      if (!satisfiability.possible) {
        for (const typeArg of resolvedTypeArgs) {
          if (typeArg.kind === "fun") {
            throw new SemanticError(`Type mismatch: cannot unify '${formatType(typeArg.from)}' with '${formatType(typeArg)}'`, span, this.getFilePath());
          }
        }
        const typeArgsStr = resolvedTypeArgs.map((t) => formatType(t)).join(", ");
        throw new SemanticError(`Type mismatch: expression cannot be used as a function (constraint '${c.protocolName}' on '${typeArgsStr}' cannot be satisfied)`, span, this.getFilePath());
      }
    }
  }
  checkTypeCollision(name, importingFrom, existing, span, kind) {
    if (!existing)
      return;
    if (kind === "protocol") {
      return;
    }
    if (existing.moduleName !== undefined && existing.moduleName !== importingFrom) {
      throw new SemanticError(`${kind === "type" ? "Type" : kind === "type alias" ? "Type alias" : kind === "record type" ? "Record type" : "Protocol"} '${name}' conflicts with ${kind} from module '${existing.moduleName}'. ` + `Consider using qualified imports or aliasing one of them.`, span, this.getFilePath());
    }
  }
  declareSymbol(scope, name, scheme, span) {
    if (scope.has(name)) {
      throw new SemanticError(`Duplicate definition for '${name}'`, span, this.getFilePath());
    }
    scope.define(name, scheme);
  }
  lookupSymbolWithConstraints(scope, name, span, substitution) {
    const sub = substitution ?? this._substitution;
    const scheme = scope.lookup(name);
    if (scheme) {
      const { type, constraints } = instantiateWithConstraints(scheme, sub);
      return { type, constraints };
    }
    this.addError(`Undefined name '${name}'`, span);
    return { type: ERROR_TYPE, constraints: [] };
  }
  lookupSymbol(scope, name, span, substitution) {
    return this.lookupSymbolWithConstraints(scope, name, span, substitution).type;
  }
  generalize(type, scope, substitution) {
    const sub = substitution ?? this._substitution;
    const typeFreeVars = getFreeTypeVars(type, sub);
    const scopeFreeVars = getFreeTypeVarsInScope(scope, sub);
    const quantified = new Set;
    for (const v of typeFreeVars) {
      if (!scopeFreeVars.has(v)) {
        quantified.add(v);
      }
    }
    const rawConstraints = this.getCollectedConstraints();
    const resolvedConstraints = applySubstitutionToConstraints(rawConstraints, sub);
    const relevantConstraints = resolvedConstraints.filter((c) => {
      return c.typeArgs.some((t) => {
        const freeVars = getFreeTypeVars(t, sub);
        for (const v of freeVars) {
          if (quantified.has(v))
            return true;
        }
        return false;
      });
    });
    const uniqueConstraints = [];
    for (const c of relevantConstraints) {
      const isDuplicate = uniqueConstraints.some((uc) => uc.protocolName === c.protocolName && uc.typeArgs.length === c.typeArgs.length && uc.typeArgs.every((t, i) => typesEqual(t, c.typeArgs[i])));
      if (!isDuplicate) {
        uniqueConstraints.push(c);
      }
    }
    return { vars: quantified, constraints: uniqueConstraints, type };
  }
}
function collectFreeVars(expr, bound = new Set) {
  const free = new Set;
  const worklist = [[expr, bound]];
  while (worklist.length > 0) {
    const [e, localBound] = worklist.pop();
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
        for (const el of e.elements)
          worklist.push([el, localBound]);
        break;
      case "List":
        for (const el of e.elements)
          worklist.push([el, localBound]);
        break;
      case "ListRange":
        worklist.push([e.start, localBound]);
        worklist.push([e.end, localBound]);
        break;
      case "Record":
        for (const f of e.fields)
          worklist.push([f.value, localBound]);
        break;
      case "RecordUpdate":
        if (!localBound.has(e.base)) {
          free.add(e.base);
        }
        for (const f of e.fields)
          worklist.push([f.value, localBound]);
        break;
      case "FieldAccess":
        worklist.push([e.target, localBound]);
        break;
      case "Apply":
        worklist.push([e.callee, localBound]);
        for (const arg of e.args)
          worklist.push([arg, localBound]);
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
        for (const p of e.args)
          collectPatternVars(p, lambdaBound);
        worklist.push([e.body, lambdaBound]);
        break;
      }
      case "LetIn": {
        const letBound = new Set(localBound);
        for (const binding of e.bindings) {
          letBound.add(binding.name);
          for (const p of binding.args)
            collectPatternVars(p, letBound);
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
function collectPatternVars(pattern, bound) {
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
function buildDependencyGraph(values) {
  const graph = new Map;
  const valueNames = new Set(Object.keys(values));
  for (const [name, info] of Object.entries(values)) {
    const deps = new Set;
    if (info.declaration.kind === "ValueDeclaration") {
      const bound = new Set;
      info.declaration.args.forEach((p) => collectPatternVars(p, bound));
      const freeVars = collectFreeVars(info.declaration.body, bound);
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
function computeSCCs(graph) {
  const index = new Map;
  const lowlink = new Map;
  const onStack = new Set;
  const stack = [];
  const sccs = [];
  let currentIndex = 0;
  function strongConnect(v) {
    index.set(v, currentIndex);
    lowlink.set(v, currentIndex);
    currentIndex++;
    stack.push(v);
    onStack.add(v);
    const deps = graph.get(v) ?? new Set;
    for (const w of deps) {
      if (!index.has(w)) {
        strongConnect(w);
        lowlink.set(v, Math.min(lowlink.get(v), lowlink.get(w)));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v), index.get(w)));
      }
    }
    if (lowlink.get(v) === index.get(v)) {
      const scc = [];
      let w;
      do {
        w = stack.pop();
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
  return sccs;
}
function isExportedFromModule(depModule, itemName, itemKind) {
  const exports = depModule.exports;
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
      for (const [, protocolExport] of exports.protocols) {
        if (protocolExport.methods?.has(itemName)) {
          return true;
        }
      }
      return false;
    }
  }
}
function analyze(program, options) {
  return new SemanticAnalyzer(program, options).analyze();
}
function containsRecordType(expr) {
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
function collectTypeVariables(expr) {
  const vars = new Set;
  function collect(e) {
    switch (e.kind) {
      case "TypeRef": {
        const name = e.name;
        if (name.length > 0 && name[0] === name[0].toLowerCase()) {
          vars.add(name);
        }
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
function validateTypeExpr(analyzer, expr, definedParams, parentSpan) {
  const { adts, typeAliases, opaqueTypes, records, imports, dependencies } = analyzer;
  const errors = [];
  function resolve(name) {
    return resolveQualifiedType(name, adts, typeAliases, opaqueTypes, records, imports, dependencies);
  }
  function findCaseSuggestion(name) {
    const nameLower = name.toLowerCase();
    for (const adtName of Object.keys(adts)) {
      if (adtName.toLowerCase() === nameLower && adtName !== name) {
        return adtName;
      }
    }
    for (const aliasName of Object.keys(typeAliases)) {
      if (aliasName.toLowerCase() === nameLower && aliasName !== name) {
        return aliasName;
      }
    }
    for (const opaqueName of Object.keys(opaqueTypes)) {
      if (opaqueName.toLowerCase() === nameLower && opaqueName !== name) {
        return opaqueName;
      }
    }
    for (const recordName of Object.keys(records)) {
      if (recordName.toLowerCase() === nameLower && recordName !== name) {
        return recordName;
      }
    }
    return;
  }
  function validate(e) {
    switch (e.kind) {
      case "TypeRef": {
        const name = e.name;
        const isLowercase = name.charAt(0) === name.charAt(0).toLowerCase();
        if (definedParams.has(name)) {
          if (e.args.length > 0) {
            errors.push({
              message: `Type parameter '${name}' cannot take type arguments`,
              span: e.span ?? parentSpan
            });
          }
          return;
        }
        const adtByName = adts[name];
        if (adtByName) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (name === "List") {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (typeAliases[name]) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (opaqueTypes[name]) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        if (records[name]) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        const resolved = resolve(name);
        if (resolved) {
          for (const arg of e.args) {
            validate(arg);
          }
          return;
        }
        const suggestion = findCaseSuggestion(name);
        if (isLowercase && name.length === 1) {
          errors.push({
            message: `Type variable '${name}' is not defined in this context`,
            span: e.span ?? parentSpan,
            suggestion: `Add '${name}' as a type parameter to the type alias`
          });
          return;
        }
        if (suggestion) {
          errors.push({
            message: `Type '${name}' is not defined`,
            span: e.span ?? parentSpan,
            suggestion: `Did you mean '${suggestion}'?`
          });
        } else {
          errors.push({
            message: `Type '${name}' is not defined`,
            span: e.span ?? parentSpan
          });
        }
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
function addProtocolMethodsToScope(protocol, scope) {
  const sharedTypeVarCtx = new Map;
  for (const param of protocol.params) {
    sharedTypeVarCtx.set(param, freshType());
  }
  const protocolConstraint = {
    protocolName: protocol.name,
    typeArgs: protocol.params.map((p) => sharedTypeVarCtx.get(p))
  };
  const quantifiedVars = new Set;
  for (const tv of sharedTypeVarCtx.values()) {
    quantifiedVars.add(tv.id);
  }
  const paramNames = invertContext(sharedTypeVarCtx);
  const methodSchemes = new Map;
  for (const [methodName, methodInfo] of protocol.methods) {
    if (!scope.symbols.has(methodName)) {
      const refreshedType = refreshType(methodInfo.type, sharedTypeVarCtx);
      const scheme = {
        vars: new Set(quantifiedVars),
        constraints: [protocolConstraint],
        type: refreshedType,
        paramNames
      };
      scope.symbols.set(methodName, scheme);
      methodSchemes.set(methodName, scheme);
    }
  }
  return methodSchemes;
}
function refreshType(type, newVarMap) {
  const oldVarIds = collectTypeVarIds(type);
  const newVars = Array.from(newVarMap.values());
  const varSubst = new Map;
  const oldVarArray = Array.from(oldVarIds);
  for (let i = 0;i < Math.min(oldVarArray.length, newVars.length); i++) {
    varSubst.set(oldVarArray[i], newVars[i]);
  }
  return applyVarSubstitution(type, varSubst);
}
function substituteProtocolVars(type, protocolVarCtx) {
  return type;
}
function findInstanceForConstraint(protocolName, typeArgs, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    if (inst.typeArgs.length !== typeArgs.length)
      continue;
    let allMatch = true;
    for (let i = 0;i < typeArgs.length; i++) {
      if (!instanceTypeMatches(inst.typeArgs[i], typeArgs[i])) {
        allMatch = false;
        break;
      }
    }
    if (allMatch)
      return true;
  }
  if (typeArgs.length > 0) {
    if (trySynthesizeInstance(protocolName, typeArgs[0], instances)) {
      return true;
    }
  }
  return false;
}
function substituteTypeParams(type, params, typeArgs) {
  const varIds = [];
  collectTypeVarIdsOrdered(type, varIds, new Set);
  const substitution = new Map;
  for (let i = 0;i < Math.min(varIds.length, typeArgs.length); i++) {
    substitution.set(varIds[i], typeArgs[i]);
  }
  return applyTypeSubstitution(type, substitution);
}
function instanceTypeMatches(instType, concreteType) {
  if (instType.kind === "var") {
    return true;
  }
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
    for (let i = 0;i < instType.args.length; i++) {
      if (!instanceTypeMatches(instType.args[i], concreteType.args[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return instanceTypeMatches(instType.from, concreteType.from) && instanceTypeMatches(instType.to, concreteType.to);
  }
  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0;i < instType.elements.length; i++) {
      if (!instanceTypeMatches(instType.elements[i], concreteType.elements[i])) {
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
      if (!instanceTypeMatches(instType.fields[key], concreteType.fields[key])) {
        return false;
      }
    }
    return true;
  }
  return typesEqual(instType, concreteType);
}
function checkConstraintSatisfiability(constraint, instances) {
  const hasFreeVars = constraint.typeArgs.some((t) => {
    const freeVars = getFreeTypeVars(t, new Map);
    return freeVars.size > 0;
  });
  if (!hasFreeVars) {
    const result = validateConstraintSatisfiable(constraint, instances);
    return { possible: result.found };
  }
  for (const typeArg of constraint.typeArgs) {
    const shape = getTypeShape(typeArg);
    if (shape === "fun") {
      const hasFunctionInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "fun" || t.kind === "var");
      });
      if (!hasFunctionInstance) {
        return { possible: false };
      }
    }
    if (shape === "tuple") {
      if (constraint.protocolName === "Eq" || constraint.protocolName === "Show") {
        return { possible: true };
      }
      const hasTupleInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "tuple" || t.kind === "var");
      });
      if (!hasTupleInstance) {
        return { possible: false };
      }
    }
    if (shape === "record") {
      if (constraint.protocolName === "Eq" || constraint.protocolName === "Show") {
        return { possible: true };
      }
      const hasRecordInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "record" || t.kind === "var");
      });
      if (!hasRecordInstance) {
        return { possible: false };
      }
    }
    if (shape === "con" && typeArg.kind === "con") {
      const hasMatchingInstance = instances.some((inst) => {
        if (inst.protocolName !== constraint.protocolName)
          return false;
        return inst.typeArgs.some((t) => t.kind === "var" || t.kind === "con" && t.name === typeArg.name);
      });
      if (!hasMatchingInstance) {
        return { possible: false };
      }
    }
  }
  return { possible: true };
}
function getTypeShape(type) {
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
      return "con";
  }
}
function validateConstraintSatisfiable(constraint, instances) {
  let firstUnsatisfiedConstraint = null;
  for (const inst of instances) {
    if (inst.protocolName !== constraint.protocolName)
      continue;
    if (inst.typeArgs.length !== constraint.typeArgs.length)
      continue;
    const instSubstitution = new Map;
    let allArgsMatch = true;
    for (let i = 0;i < inst.typeArgs.length; i++) {
      const instArg = inst.typeArgs[i];
      const constraintArg = constraint.typeArgs[i];
      if (!matchTypeArgForInstance(instArg, constraintArg, instSubstitution)) {
        allArgsMatch = false;
        break;
      }
    }
    if (!allArgsMatch)
      continue;
    if (inst.constraints.length === 0) {
      return { found: true };
    }
    let allConstraintsSatisfied = true;
    for (const instConstraint of inst.constraints) {
      const substitutedTypeArgs = instConstraint.typeArgs.map((t) => applySubstitution(t, instSubstitution));
      const substitutedConstraint = {
        protocolName: instConstraint.protocolName,
        typeArgs: substitutedTypeArgs
      };
      const canBeSatisfied = checkConstraintSatisfiability(substitutedConstraint, instances);
      if (!canBeSatisfied.possible) {
        allConstraintsSatisfied = false;
        if (!firstUnsatisfiedConstraint) {
          firstUnsatisfiedConstraint = {
            constraint: instConstraint.protocolName,
            forType: substitutedTypeArgs.map((t) => formatType(t)).join(", ")
          };
        }
        break;
      }
    }
    if (allConstraintsSatisfied) {
      return { found: true };
    }
  }
  if (firstUnsatisfiedConstraint) {
    return {
      found: false,
      reason: "unsatisfied-constraint",
      constraint: firstUnsatisfiedConstraint.constraint,
      forType: firstUnsatisfiedConstraint.forType
    };
  }
  if (constraint.typeArgs.length === 1) {
    if (trySynthesizeInstance(constraint.protocolName, constraint.typeArgs[0], instances)) {
      return { found: true };
    }
  }
  return { found: false, reason: "no-instance" };
}
function matchTypeArgForInstance(instArg, constraintArg, substitution) {
  if (instArg.kind === "var") {
    const existing = substitution.get(instArg.id);
    if (existing) {
      return typesEqual(existing, constraintArg);
    }
    substitution.set(instArg.id, constraintArg);
    return true;
  }
  if (constraintArg.kind === "var") {
    return true;
  }
  if (instArg.kind !== constraintArg.kind)
    return false;
  if (instArg.kind === "con" && constraintArg.kind === "con") {
    if (instArg.name !== constraintArg.name)
      return false;
    if (instArg.args.length !== constraintArg.args.length)
      return false;
    for (let i = 0;i < instArg.args.length; i++) {
      if (!matchTypeArgForInstance(instArg.args[i], constraintArg.args[i], substitution)) {
        return false;
      }
    }
    return true;
  }
  if (instArg.kind === "fun" && constraintArg.kind === "fun") {
    return matchTypeArgForInstance(instArg.from, constraintArg.from, substitution) && matchTypeArgForInstance(instArg.to, constraintArg.to, substitution);
  }
  if (instArg.kind === "tuple" && constraintArg.kind === "tuple") {
    if (instArg.elements.length !== constraintArg.elements.length)
      return false;
    for (let i = 0;i < instArg.elements.length; i++) {
      if (!matchTypeArgForInstance(instArg.elements[i], constraintArg.elements[i], substitution)) {
        return false;
      }
    }
    return true;
  }
  if (instArg.kind === "record" && constraintArg.kind === "record") {
    const instKeys = Object.keys(instArg.fields).sort();
    const constraintKeys = Object.keys(constraintArg.fields).sort();
    if (instKeys.length !== constraintKeys.length)
      return false;
    for (let i = 0;i < instKeys.length; i++) {
      const key = instKeys[i];
      if (key !== constraintKeys[i])
        return false;
      if (!matchTypeArgForInstance(instArg.fields[key], constraintArg.fields[key], substitution)) {
        return false;
      }
    }
    return true;
  }
  return typesEqual(instArg, constraintArg);
}
function findInstanceForTypeWithReason(protocolName, concreteType, instances) {
  let hasPolymorphicInstance = false;
  let firstUnsatisfiedConstraint = null;
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg)
      continue;
    let matchesStructure = false;
    if (instTypeArg.kind === concreteType.kind) {
      if (instTypeArg.kind === "con" && concreteType.kind === "con") {
        matchesStructure = instTypeArg.name === concreteType.name && instTypeArg.args.length === concreteType.args.length;
      } else if (instTypeArg.kind === "tuple" && concreteType.kind === "tuple") {
        matchesStructure = instTypeArg.elements.length === concreteType.elements.length;
      } else if (instTypeArg.kind === "record" && concreteType.kind === "record") {
        const k1 = Object.keys(instTypeArg.fields).sort();
        const k2 = Object.keys(concreteType.fields).sort();
        matchesStructure = k1.length === k2.length && k1.every((k, i) => k === k2[i]);
      } else {
        matchesStructure = true;
      }
    }
    if (matchesStructure) {
      if (instanceTypeMatches(instTypeArg, concreteType)) {
        if (inst.constraints.length === 0) {
          return { found: true };
        }
        const typeVarSubst = new Map;
        buildTypeVarSubstitution(instTypeArg, concreteType, typeVarSubst);
        let allConstraintsSatisfied = true;
        for (const constraint of inst.constraints) {
          const substitutedTypeArgs = constraint.typeArgs.map((t) => applySubstitutionToType(t, typeVarSubst));
          for (const typeArg of substitutedTypeArgs) {
            const constraintResult = findInstanceForTypeWithReason(constraint.protocolName, typeArg, instances);
            if (!constraintResult.found) {
              allConstraintsSatisfied = false;
              hasPolymorphicInstance = true;
              if (!firstUnsatisfiedConstraint) {
                firstUnsatisfiedConstraint = {
                  constraint: constraint.protocolName,
                  forType: formatType(typeArg)
                };
              }
              break;
            }
          }
          if (!allConstraintsSatisfied)
            break;
        }
        if (allConstraintsSatisfied) {
          return { found: true };
        }
      }
    }
    if (instTypeArg.kind === "var") {
      hasPolymorphicInstance = true;
      if (inst.constraints.length === 0) {
        return { found: true };
      }
      let allConstraintsSatisfied = true;
      for (const constraint of inst.constraints) {
        const constraintResult = findInstanceForTypeWithReason(constraint.protocolName, concreteType, instances);
        if (!constraintResult.found) {
          allConstraintsSatisfied = false;
          if (!firstUnsatisfiedConstraint) {
            firstUnsatisfiedConstraint = {
              constraint: constraint.protocolName,
              forType: formatType(concreteType)
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
  if (trySynthesizeInstance(protocolName, concreteType, instances)) {
    return { found: true };
  }
  if (hasPolymorphicInstance && firstUnsatisfiedConstraint) {
    return {
      found: false,
      reason: "unsatisfied-constraint",
      constraint: firstUnsatisfiedConstraint.constraint,
      forType: firstUnsatisfiedConstraint.forType
    };
  }
  return { found: false, reason: "no-instance" };
}
function buildTypeVarSubstitution(instType, concreteType, subst) {
  if (instType.kind === "var") {
    subst.set(instType.id, concreteType);
    return;
  }
  if (instType.kind === "con" && concreteType.kind === "con") {
    for (let i = 0;i < instType.args.length; i++) {
      buildTypeVarSubstitution(instType.args[i], concreteType.args[i], subst);
    }
  } else if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    for (let i = 0;i < instType.elements.length; i++) {
      buildTypeVarSubstitution(instType.elements[i], concreteType.elements[i], subst);
    }
  } else if (instType.kind === "fun" && concreteType.kind === "fun") {
    buildTypeVarSubstitution(instType.from, concreteType.from, subst);
    buildTypeVarSubstitution(instType.to, concreteType.to, subst);
  } else if (instType.kind === "record" && concreteType.kind === "record") {
    for (const key of Object.keys(instType.fields)) {
      if (key in concreteType.fields) {
        buildTypeVarSubstitution(instType.fields[key], concreteType.fields[key], subst);
      }
    }
  }
}
function applySubstitutionToType(type, subst) {
  if (type.kind === "var") {
    return subst.get(type.id) ?? type;
  }
  if (type.kind === "con") {
    return {
      ...type,
      args: type.args.map((a) => applySubstitutionToType(a, subst))
    };
  }
  if (type.kind === "tuple") {
    return {
      ...type,
      elements: type.elements.map((e) => applySubstitutionToType(e, subst))
    };
  }
  if (type.kind === "fun") {
    return {
      ...type,
      from: applySubstitutionToType(type.from, subst),
      to: applySubstitutionToType(type.to, subst)
    };
  }
  if (type.kind === "record") {
    const newFields = {};
    for (const [k, v] of Object.entries(type.fields)) {
      newFields[k] = applySubstitutionToType(v, subst);
    }
    return { ...type, fields: newFields };
  }
  return type;
}
function findInstanceForTypeInternal(protocolName, concreteType, instances) {
  return findInstanceForTypeWithReason(protocolName, concreteType, instances).found;
}
var syntheticSpan = {
  start: { offset: 0, line: 0, column: 0 },
  end: { offset: 0, line: 0, column: 0 }
};
var _recordRegistry = {};
var _protocolMethodUsages;
var _currentModuleName;
function getRecordFieldTypes(type) {
  if (type.kind === "record") {
    return type.fields;
  }
  if (type.kind === "con") {
    const info = _recordRegistry[type.name];
    if (info) {
      const fields = {};
      const resolveCtx = new Map;
      info.params.forEach((p, i) => {
        const v = type.args[i]?.kind === "var" ? type.args[i] : freshType();
        resolveCtx.set(p, v);
      });
      for (const field of info.fields) {
        fields[field.name] = resolveTypeExprForRecord(field.typeExpr, resolveCtx);
      }
      return fields;
    }
  }
  return;
}
function resolveTypeExprForRecord(typeExpr, context) {
  switch (typeExpr.kind) {
    case "TypeRef": {
      const tv = context.get(typeExpr.name);
      if (tv)
        return tv;
      return {
        kind: "con",
        name: typeExpr.name,
        args: typeExpr.args.map((a) => resolveTypeExprForRecord(a, context))
      };
    }
    case "FunctionType":
      return {
        kind: "fun",
        from: resolveTypeExprForRecord(typeExpr.from, context),
        to: resolveTypeExprForRecord(typeExpr.to, context)
      };
    case "TupleType":
      return {
        kind: "tuple",
        elements: typeExpr.elements.map((e) => resolveTypeExprForRecord(e, context))
      };
    default:
      return freshType();
  }
}
function trySynthesizeInstance(protocolName, type, instances) {
  if (protocolName !== "Eq" && protocolName !== "Show")
    return false;
  if (type.kind === "tuple") {
    for (const elem of type.elements) {
      if (!findInstanceForTypeInternal(protocolName, elem, instances)) {
        return false;
      }
    }
    const instance = generateSyntheticInstance(protocolName, type, _protocolMethodUsages);
    instances.push(instance);
    return true;
  }
  const fieldTypes = getRecordFieldTypes(type);
  if (fieldTypes) {
    for (const fieldType of Object.values(fieldTypes)) {
      if (!findInstanceForTypeInternal(protocolName, fieldType, instances)) {
        return false;
      }
    }
    const instance = generateSyntheticInstance(protocolName, type, _protocolMethodUsages);
    instances.push(instance);
    return true;
  }
  return false;
}
function generateSyntheticInstance(protocolName, type, usages) {
  const methods = new Map;
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
    span
  };
}
function generateSyntheticEq(type, usages) {
  const span = syntheticSpan;
  if (type.kind === "tuple") {
    const arity = type.elements.length;
    const xVars = Array.from({ length: arity }, (_, i) => `x${i}`);
    const yVars = Array.from({ length: arity }, (_, i) => `y${i}`);
    let body = { kind: "Var", name: "True", namespace: "upper", span };
    if (arity > 0) {
      const checks = xVars.map((xv, i) => {
        const node = {
          kind: "Infix",
          left: { kind: "Var", name: xv, namespace: "lower", span },
          operator: "==",
          right: { kind: "Var", name: yVars[i], namespace: "lower", span },
          span
        };
        if (usages) {
          usages.set(node, {
            protocolName: "Eq",
            typeArgs: [type.elements[i]]
          });
        }
        return node;
      });
      body = checks.reduce((acc, check) => ({
        kind: "Infix",
        left: acc,
        operator: "&&",
        right: check,
        span
      }));
    }
    const pattern = {
      kind: "TuplePattern",
      elements: [
        {
          kind: "TuplePattern",
          elements: xVars.map((v) => ({ kind: "VarPattern", name: v, span })),
          span
        },
        {
          kind: "TuplePattern",
          elements: yVars.map((v) => ({ kind: "VarPattern", name: v, span })),
          span
        }
      ],
      span
    };
    return {
      kind: "Lambda",
      args: [
        { kind: "VarPattern", name: "x_impl", span },
        { kind: "VarPattern", name: "y_impl", span }
      ],
      body: {
        kind: "Case",
        discriminant: {
          kind: "Tuple",
          elements: [
            { kind: "Var", name: "x_impl", namespace: "lower", span },
            { kind: "Var", name: "y_impl", namespace: "lower", span }
          ],
          span
        },
        branches: [{ pattern, body, span }],
        span
      },
      span
    };
  } else {
    const fieldTypes = getRecordFieldTypes(type);
    if (fieldTypes) {
      const fields = Object.keys(fieldTypes).sort();
      let body = { kind: "Var", name: "True", namespace: "upper", span };
      if (fields.length > 0) {
        const checks = fields.map((f) => {
          const node = {
            kind: "Infix",
            left: {
              kind: "FieldAccess",
              target: { kind: "Var", name: "x_impl", namespace: "lower", span },
              field: f,
              span
            },
            operator: "==",
            right: {
              kind: "FieldAccess",
              target: { kind: "Var", name: "y_impl", namespace: "lower", span },
              field: f,
              span
            },
            span
          };
          if (usages) {
            usages.set(node, {
              protocolName: "Eq",
              typeArgs: [fieldTypes[f]]
            });
          }
          return node;
        });
        body = checks.reduce((acc, check) => ({
          kind: "Infix",
          left: acc,
          operator: "&&",
          right: check,
          span
        }));
      }
      return {
        kind: "Lambda",
        args: [
          { kind: "VarPattern", name: "x_impl", span },
          { kind: "VarPattern", name: "y_impl", span }
        ],
        body,
        span
      };
    }
  }
  throw new Error("Unsupported type for synthetic Eq");
}
function generateSyntheticShow(type, usages) {
  const span = syntheticSpan;
  if (type.kind === "tuple") {
    const arity = type.elements.length;
    const vars = Array.from({ length: arity }, (_, i) => `x${i}`);
    const str = (s) => ({
      kind: "String",
      value: `"${s}"`,
      span
    });
    const append = (a, b) => ({
      kind: "Infix",
      left: a,
      operator: "++",
      right: b,
      span
    });
    const toStringCall = (valName, elemType) => {
      const callee = {
        kind: "Var",
        name: "toString",
        namespace: "lower",
        span
      };
      const node = {
        kind: "Apply",
        callee,
        args: [{ kind: "Var", name: valName, namespace: "lower", span }],
        span
      };
      if (usages) {
        usages.set(callee, { protocolName: "Show", typeArgs: [elemType] });
      }
      return node;
    };
    let body = str("(");
    vars.forEach((v, i) => {
      if (i > 0)
        body = append(body, str(", "));
      body = append(body, toStringCall(v, type.elements[i]));
    });
    body = append(body, str(")"));
    const pattern = {
      kind: "TuplePattern",
      elements: vars.map((v) => ({ kind: "VarPattern", name: v, span })),
      span
    };
    return {
      kind: "Lambda",
      args: [{ kind: "VarPattern", name: "x_impl", span }],
      body: {
        kind: "Case",
        discriminant: { kind: "Var", name: "x_impl", namespace: "lower", span },
        branches: [{ pattern, body, span }],
        span
      },
      span
    };
  } else {
    const fieldTypes = getRecordFieldTypes(type);
    if (fieldTypes) {
      const fields = Object.keys(fieldTypes).sort();
      const typeName = type.kind === "con" ? type.name : null;
      const str = (s) => ({
        kind: "String",
        value: `"${s}"`,
        span
      });
      const append = (a, b) => ({
        kind: "Infix",
        left: a,
        operator: "++",
        right: b,
        span
      });
      const toStringField = (f) => {
        const callee = {
          kind: "Var",
          name: "toString",
          namespace: "lower",
          span
        };
        const node = {
          kind: "Apply",
          callee,
          args: [
            {
              kind: "FieldAccess",
              target: { kind: "Var", name: "x_impl", namespace: "lower", span },
              field: f,
              span
            }
          ],
          span
        };
        if (usages) {
          usages.set(callee, {
            protocolName: "Show",
            typeArgs: [fieldTypes[f]]
          });
        }
        return node;
      };
      let body = str(typeName ? `${typeName} { ` : "{ ");
      fields.forEach((f, i) => {
        if (i > 0)
          body = append(body, str(", "));
        body = append(body, str(`${f} = `));
        body = append(body, toStringField(f));
      });
      body = append(body, str(" }"));
      return {
        kind: "Lambda",
        args: [{ kind: "VarPattern", name: "x_impl", span }],
        body,
        span
      };
    }
  }
  throw new Error("Unsupported type for synthetic Show");
}
function symbolExists(scope, name) {
  return scope.lookup(name) !== undefined;
}
function bindPatternNames(pattern, scope) {
  switch (pattern.kind) {
    case "VarPattern":
      scope.define(pattern.name, {
        vars: new Set,
        constraints: [],
        type: { kind: "var", id: -1 }
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
function typeArgsEqual(args1, args2) {
  if (args1.length !== args2.length)
    return false;
  return args1.every((t, i) => typesEqual(t, args2[i]));
}
function instancesOverlap(inst1, inst2, instances) {
  if (!typesOverlap(inst1.typeArgs, inst2.typeArgs)) {
    return false;
  }
  for (let i = 0;i < inst1.typeArgs.length; i++) {
    const type1 = inst1.typeArgs[i];
    const type2 = inst2.typeArgs[i];
    if (type1.kind === "var" && type2.kind === "con") {
      const constraintsForVar = inst1.constraints.filter((c) => c.typeArgs.some((t) => t.kind === "var" && t.id === type1.id));
      for (const constraint of constraintsForVar) {
        const substitutedType = substituteTypeInConstraint(constraint, type1.id, type2);
        if (!canSatisfyConstraint(substitutedType, constraint.protocolName, instances)) {
          return false;
        }
      }
    }
    if (type2.kind === "var" && type1.kind === "con") {
      const constraintsForVar = inst2.constraints.filter((c) => c.typeArgs.some((t) => t.kind === "var" && t.id === type2.id));
      for (const constraint of constraintsForVar) {
        const substitutedType = substituteTypeInConstraint(constraint, type2.id, type1);
        if (!canSatisfyConstraint(substitutedType, constraint.protocolName, instances)) {
          return false;
        }
      }
    }
  }
  return true;
}
function substituteTypeInConstraint(constraint, varId, replacement) {
  for (const typeArg of constraint.typeArgs) {
    if (typeArg.kind === "var" && typeArg.id === varId) {
      return replacement;
    }
    if (typeArg.kind === "con") {
      const substitutedArgs = typeArg.args.map((arg) => arg.kind === "var" && arg.id === varId ? replacement : arg);
      return { kind: "con", name: typeArg.name, args: substitutedArgs };
    }
  }
  return constraint.typeArgs[0] || replacement;
}
function canSatisfyConstraint(concreteType, protocolName, instances) {
  if (concreteType.kind !== "con") {
    return true;
  }
  return findInstanceForTypeInternal(protocolName, concreteType, instances);
}
function typesOverlap(types1, types2) {
  if (types1.length !== types2.length)
    return false;
  for (let i = 0;i < types1.length; i++) {
    if (!typeOverlaps(types1[i], types2[i])) {
      return false;
    }
  }
  return true;
}
function typeOverlaps(type1, type2) {
  if (type1.kind === "var" || type2.kind === "var")
    return true;
  if (type1.kind === "con" && type2.kind === "con") {
    if (type1.name !== type2.name)
      return false;
    return typesOverlap(type1.args, type2.args);
  }
  if (type1.kind === "fun" && type2.kind === "fun") {
    return typeOverlaps(type1.from, type2.from) && typeOverlaps(type1.to, type2.to);
  }
  if (type1.kind === "tuple" && type2.kind === "tuple") {
    return typesOverlap(type1.elements, type2.elements);
  }
  if (type1.kind === "record" && type2.kind === "record") {
    const fields1 = Object.keys(type1.fields);
    const fields2 = Object.keys(type2.fields);
    if (fields1.length !== fields2.length)
      return false;
    for (const field of fields1) {
      if (!type2.fields[field])
        return false;
      if (!typeOverlaps(type1.fields[field], type2.fields[field])) {
        return false;
      }
    }
    return true;
  }
  return false;
}
function resolveModuleField(depModule, field, substitution) {
  if (Object.hasOwn(depModule.typeSchemes, field)) {
    const scheme = depModule.typeSchemes[field];
    return instantiate(scheme, substitution);
  }
  if (Object.hasOwn(depModule.values, field)) {
    const valueInfo = depModule.values[field];
    const valueType = valueInfo.type || depModule.types[field];
    if (valueType) {
      return valueType;
    }
  }
  if (Object.hasOwn(depModule.constructorTypes, field)) {
    const ctorScheme = depModule.constructorTypes[field];
    return instantiate(ctorScheme, substitution);
  }
  return null;
}
function tryResolveModuleFieldAccess(expr, imports, dependencies, substitution) {
  const parts = [];
  let current = expr;
  while (current.kind === "FieldAccess") {
    parts.unshift(current.field);
    current = current.target;
  }
  if (current.kind !== "Var") {
    return null;
  }
  const baseName = current.name;
  parts.unshift(baseName);
  for (const imp of imports) {
    const importParts = imp.moduleName.split(".");
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = dependencies.get(imp.moduleName);
      if (!depModule) {
        continue;
      }
      const fieldParts = parts.slice(1);
      if (fieldParts.length === 1) {
        const field = fieldParts[0];
        const resolved = resolveModuleField(depModule, field, substitution);
        if (resolved)
          return resolved;
      }
    }
    if (importParts.length <= parts.length - 1) {
      let matches = true;
      for (let i = 0;i < importParts.length; i++) {
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
        const fieldParts = parts.slice(importParts.length);
        if (fieldParts.length === 1) {
          const field = fieldParts[0];
          const resolved = resolveModuleField(depModule, field, substitution);
          if (resolved)
            return resolved;
        }
      }
    }
  }
  return null;
}
function instantiateWithConstraints(scheme, substitution) {
  if (scheme.vars.size === 0) {
    return { type: scheme.type, constraints: scheme.constraints };
  }
  const instantiationMap = new Map;
  for (const varId of scheme.vars) {
    instantiationMap.set(varId, freshType());
  }
  const instantiatedType = instantiateType(scheme.type, instantiationMap, substitution);
  const instantiatedConstraints = scheme.constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => instantiateType(t, instantiationMap, substitution))
  }));
  return { type: instantiatedType, constraints: instantiatedConstraints };
}
function instantiate(scheme, substitution) {
  return instantiateWithConstraints(scheme, substitution).type;
}
function instantiateType(type, instantiationMap, substitution) {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
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
      args: concrete.args.map((arg) => instantiateType(arg, instantiationMap, substitution))
    };
  }
  if (concrete.kind === "fun") {
    return {
      kind: "fun",
      from: instantiateType(concrete.from, instantiationMap, substitution),
      to: instantiateType(concrete.to, instantiationMap, substitution)
    };
  }
  if (concrete.kind === "tuple") {
    return {
      kind: "tuple",
      elements: concrete.elements.map((el) => instantiateType(el, instantiationMap, substitution))
    };
  }
  if (concrete.kind === "record") {
    const fields = {};
    for (const [key, fieldType] of Object.entries(concrete.fields)) {
      fields[key] = instantiateType(fieldType, instantiationMap, substitution);
    }
    return { kind: "record", fields };
  }
  return concrete;
}
function extractAnnotationParams(annotation, argCount, span) {
  const params = flattenFunctionParams(annotation);
  const result = params.slice(0, argCount);
  while (result.length < argCount) {
    result.push(freshType());
  }
  return result;
}
function extractAnnotationReturn(annotation, argCount) {
  let result = annotation;
  for (let i = 0;i < argCount; i++) {
    if (result.kind !== "fun") {
      return result;
    }
    result = result.to;
  }
  return result;
}
function isTypeVariable(name) {
  if (name.length === 0)
    return false;
  const firstChar = name[0];
  return firstChar === firstChar.toLowerCase();
}
function occursIn(id, type, substitution) {
  const concrete = applySubstitution(type, substitution);
  if (concrete.kind === "var") {
    return concrete.id === id;
  }
  switch (concrete.kind) {
    case "fun":
      return occursIn(id, concrete.from, substitution) || occursIn(id, concrete.to, substitution);
    case "tuple":
      return concrete.elements.some((t) => occursIn(id, t, substitution));
    case "record":
      return Object.values(concrete.fields).some((t) => occursIn(id, t, substitution));
    case "con":
      return concrete.args.some((t) => occursIn(id, t, substitution));
    default:
      return false;
  }
}
function resolveQualifiedType(name, adts, typeAliases, opaqueTypes, records, imports, dependencies) {
  if (adts[name])
    return { kind: "adt", name, info: adts[name] };
  if (typeAliases[name])
    return { kind: "alias", name, info: typeAliases[name] };
  if (opaqueTypes[name])
    return { kind: "opaque", name, info: opaqueTypes[name] };
  if (records[name])
    return { kind: "record", name, info: records[name] };
  const parts = name.split(".");
  if (parts.length > 1) {
    const typeName = parts.pop();
    const moduleName = parts.join(".");
    for (const imp of imports) {
      if (imp.alias === moduleName || imp.moduleName === moduleName) {
        const depModule = dependencies.get(imp.moduleName);
        if (depModule) {
          if (depModule.adts[typeName])
            return {
              kind: "adt",
              name: typeName,
              info: depModule.adts[typeName]
            };
          if (depModule.typeAliases[typeName])
            return {
              kind: "alias",
              name: typeName,
              info: depModule.typeAliases[typeName]
            };
          if (depModule.opaqueTypes[typeName])
            return {
              kind: "opaque",
              name: typeName,
              info: depModule.opaqueTypes[typeName]
            };
          if (depModule.records[typeName])
            return {
              kind: "record",
              name: typeName,
              info: depModule.records[typeName]
            };
        }
      }
    }
  }
  return null;
}
function resolveQualifiedConstructor(name, constructors, adts, imports, dependencies, moduleContext) {
  if (moduleContext) {
    const depModule = dependencies.get(moduleContext);
    if (depModule && depModule.constructors[name]) {
      const info = depModule.constructors[name];
      const adt = depModule.adts[info.parentType];
      if (adt) {
        return { name, info, adt };
      }
    }
  }
  if (constructors[name]) {
    const info = constructors[name];
    const adt = adts[info.parentType];
    if (adt) {
      return { name, info, adt };
    }
  }
  const parts = name.split(".");
  if (parts.length > 1) {
    const ctorName = parts.pop();
    const moduleName = parts.join(".");
    for (const imp of imports) {
      if (imp.alias === moduleName || imp.moduleName === moduleName) {
        const depModule = dependencies.get(imp.moduleName);
        if (depModule) {
          if (depModule.constructors[ctorName]) {
            const info = depModule.constructors[ctorName];
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
export {
  formatTypeSchemeForDisplay,
  formatTypeForDisplay,
  formatConstraintsForDisplay,
  buildNormalizedNames,
  analyze,
  SemanticError,
  Scope,
  RegistryManager,
  MultipleSemanticErrors,
  ImplementingProtocolError
};
