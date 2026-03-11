// @bun
// src/utils.ts
function formatTypeKey(type) {
  if (!type)
    return "unknown";
  switch (type.kind) {
    case "con":
      if (type.args.length === 0)
        return type.name;
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
function isTypeVariable(type) {
  switch (type.kind) {
    case "var":
      return true;
    case "con":
      return false;
    case "list":
      return isTypeVariable(type.element);
    case "tuple":
      return type.elements.every(isTypeVariable);
    case "fun":
      return isTypeVariable(type.from) && isTypeVariable(type.to);
    case "record":
      return Object.values(type.fields).every(isTypeVariable);
    default:
      return false;
  }
}
function buildTypeVarSubstitution(instanceTypeArgs, concreteTypes) {
  const subst = new Map;
  function collectSubst(instType, concreteType) {
    if (instType.kind === "var") {
      subst.set(instType.id, concreteType);
    } else if (instType.kind === "con" && concreteType.kind === "con") {
      for (let i = 0;i < instType.args.length && i < concreteType.args.length; i++) {
        collectSubst(instType.args[i], concreteType.args[i]);
      }
    } else if (instType.kind === "list" && concreteType.kind === "list") {
      collectSubst(instType.element, concreteType.element);
    } else if (instType.kind === "tuple" && concreteType.kind === "tuple") {
      for (let i = 0;i < instType.elements.length && i < concreteType.elements.length; i++) {
        collectSubst(instType.elements[i], concreteType.elements[i]);
      }
    } else if (instType.kind === "fun" && concreteType.kind === "fun") {
      collectSubst(instType.from, concreteType.from);
      collectSubst(instType.to, concreteType.to);
    }
  }
  for (let i = 0;i < instanceTypeArgs.length && i < concreteTypes.length; i++) {
    collectSubst(instanceTypeArgs[i], concreteTypes[i]);
  }
  return subst;
}
function applyTypeSubstitution(type, subst) {
  if (type.kind === "var") {
    return subst.get(type.id) ?? type;
  }
  return type;
}
function typeStructureMatches(instType, concreteType) {
  if (instType.kind === "var") {
    return true;
  }
  if (instType.kind === "con" && concreteType.kind === "con") {
    if (instType.name !== concreteType.name) {
      return false;
    }
    if (instType.args.length !== concreteType.args.length) {
      return false;
    }
    for (let i = 0;i < instType.args.length; i++) {
      if (!typeStructureMatches(instType.args[i], concreteType.args[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "list" && concreteType.kind === "list") {
    return typeStructureMatches(instType.element, concreteType.element);
  }
  if (instType.kind === "tuple" && concreteType.kind === "tuple") {
    if (instType.elements.length !== concreteType.elements.length) {
      return false;
    }
    for (let i = 0;i < instType.elements.length; i++) {
      if (!typeStructureMatches(instType.elements[i], concreteType.elements[i])) {
        return false;
      }
    }
    return true;
  }
  if (instType.kind === "fun" && concreteType.kind === "fun") {
    return typeStructureMatches(instType.from, concreteType.from) && typeStructureMatches(instType.to, concreteType.to);
  }
  return false;
}
function findPolymorphicInstance(protocolName, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const firstTypeArg = inst.typeArgs[0];
    if (firstTypeArg && firstTypeArg.kind === "var") {
      const typeKey = formatTypeKey(firstTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst
      };
    }
  }
  return null;
}
function findMatchingInstance(protocolName, concreteType, instances) {
  for (const inst of instances) {
    if (inst.protocolName !== protocolName)
      continue;
    const instTypeArg = inst.typeArgs[0];
    if (!instTypeArg)
      continue;
    if (instTypeArg.kind === "var")
      continue;
    if (typeStructureMatches(instTypeArg, concreteType)) {
      const typeKey = formatTypeKey(instTypeArg);
      return {
        key: `${protocolName}_${typeKey}`,
        instance: inst
      };
    }
  }
  return null;
}

// src/dependency.ts
function buildDependencyGraph(values, instances, protocols = {}, constructors = {}) {
  const graph = new Map;
  const availableNames = new Set(Object.keys(values));
  const protocolMethodMap = new Map;
  for (const protocol of Object.values(protocols)) {
    for (const method of protocol.methods) {
      protocolMethodMap.set(method.name, protocol.name);
    }
  }
  for (const inst of instances) {
    if (!inst.typeArgs[0])
      continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const instName = `$dict_${inst.protocolName}_${typeKey}`;
    availableNames.add(instName);
  }
  for (const [name, value] of Object.entries(values)) {
    const deps = new Set;
    collectDependencies(value.body, deps, availableNames, instances, protocolMethodMap, constructors);
    for (const param of value.params) {
      collectPatternDependencies(param, deps, availableNames);
    }
    graph.set(name, deps);
  }
  for (const inst of instances) {
    if (!inst.typeArgs[0])
      continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const instName = `$dict_${inst.protocolName}_${typeKey}`;
    const deps = new Set;
    for (const implName of Object.values(inst.methods)) {
      if (availableNames.has(implName)) {
        deps.add(implName);
      }
    }
    for (const constraint of inst.constraints) {
      const typeArg = constraint.typeArgs[0];
      if (typeArg && typeArg.kind !== "var") {
        const constraintKey = formatTypeKey(typeArg);
        const constraintDict = `$dict_${constraint.protocolName}_${constraintKey}`;
        if (availableNames.has(constraintDict)) {
          deps.add(constraintDict);
        }
      }
    }
    graph.set(instName, deps);
  }
  return graph;
}
function collectDependencies(expr, deps, availableNames, instances, protocolMethodMap, constructors) {
  switch (expr.kind) {
    case "IRVar":
      if (expr.namespace === "value") {
        if (availableNames.has(expr.name)) {
          deps.add(expr.name);
        }
        if (expr.constraint) {
          const typeArg = expr.constraint.typeArgs[0];
          if (typeArg && typeArg.kind !== "var") {
            const dictName = resolveDictionaryName(expr.constraint.protocolName, typeArg, instances);
            if (dictName && availableNames.has(dictName)) {
              deps.add(dictName);
            }
          }
        }
      }
      break;
    case "IRLiteral":
    case "IRUnit":
      break;
    case "IRLambda":
      collectDependencies(expr.body, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRApply":
      if (expr.callee.kind === "IRVar" && protocolMethodMap.has(expr.callee.name)) {
        const protocolName = protocolMethodMap.get(expr.callee.name);
        if (protocolName && expr.args.length > 0) {
          const firstArg = expr.args[0];
          let inferenceType;
          if (firstArg.kind === "IRLiteral") {
            const litType = firstArg.literalType;
            const typeName = litType.charAt(0).toUpperCase() + litType.slice(1);
            if (typeName === "Int" || typeName === "Float" || typeName === "String" || typeName === "Bool") {
              inferenceType = { kind: "con", name: typeName, args: [] };
            } else if (litType === "char") {
              inferenceType = { kind: "con", name: "Char", args: [] };
            }
          } else if (firstArg.kind === "IRVar" && firstArg.type) {
            inferenceType = firstArg.type;
          } else if (firstArg.kind === "IRConstructor") {
            const ctorInfo = constructors[firstArg.name];
            if (ctorInfo) {
              inferenceType = {
                kind: "con",
                name: ctorInfo.parentType,
                args: []
              };
            }
          }
          if (inferenceType) {
            const dictName = resolveDictionaryName(protocolName, inferenceType, instances);
            if (dictName && availableNames.has(dictName)) {
              deps.add(dictName);
            }
          }
        }
      }
      collectDependencies(expr.callee, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRIf":
      collectDependencies(expr.condition, deps, availableNames, instances, protocolMethodMap, constructors);
      collectDependencies(expr.thenBranch, deps, availableNames, instances, protocolMethodMap, constructors);
      collectDependencies(expr.elseBranch, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRCase":
      collectDependencies(expr.discriminant, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const branch of expr.branches) {
        collectDependencies(branch.body, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRTuple":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRList":
      for (const elem of expr.elements) {
        collectDependencies(elem, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRRecord":
      for (const field of expr.fields) {
        collectDependencies(field.value, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRRecordUpdate":
      collectDependencies(expr.base, deps, availableNames, instances, protocolMethodMap, constructors);
      for (const field of expr.updates) {
        collectDependencies(field.value, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRFieldAccess":
      collectDependencies(expr.target, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRConstructor":
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
    case "IRSelfLoop":
      collectDependencies(expr.body, deps, availableNames, instances, protocolMethodMap, constructors);
      break;
    case "IRLoopContinue":
      for (const arg of expr.args) {
        collectDependencies(arg, deps, availableNames, instances, protocolMethodMap, constructors);
      }
      break;
  }
}
function resolveDictionaryName(protocolName, type, instances) {
  const typeKey = formatTypeKey(type);
  const exactKey = `${protocolName}_${typeKey}`;
  const match = findMatchingInstance(protocolName, type, instances);
  if (match) {
    return `$dict_${match.key}`;
  }
  const polyMatch = findPolymorphicInstance(protocolName, instances);
  if (polyMatch) {
    return `$dict_${polyMatch.key}`;
  }
  return `$dict_${exactKey}`;
}
function collectPatternDependencies(pattern, deps, availableNames) {
  switch (pattern.kind) {
    case "IRVarPattern":
    case "IRWildcardPattern":
    case "IRLiteralPattern":
      break;
    case "IRConstructorPattern":
      for (const arg of pattern.args) {
        collectPatternDependencies(arg, deps, availableNames);
      }
      break;
    case "IRTuplePattern":
      for (const elem of pattern.elements) {
        collectPatternDependencies(elem, deps, availableNames);
      }
      break;
  }
}
function findSCCs(graph, declarationOrder) {
  const state = {
    index: 0,
    stack: [],
    onStack: new Set,
    indices: new Map,
    lowlinks: new Map,
    sccs: []
  };
  for (const name of declarationOrder) {
    if (!state.indices.has(name)) {
      strongconnect(name, graph, state);
    }
  }
  const result = [];
  for (const scc of state.sccs) {
    const orderMap = new Map(declarationOrder.map((name, idx) => [name, idx]));
    const sortedValues = [...scc].sort((a, b) => {
      const aIdx = orderMap.get(a) ?? Infinity;
      const bIdx = orderMap.get(b) ?? Infinity;
      return aIdx - bIdx;
    });
    result.push({
      values: sortedValues,
      isMutuallyRecursive: scc.length > 1
    });
  }
  return result;
}
function strongconnect(v, graph, state) {
  state.indices.set(v, state.index);
  state.lowlinks.set(v, state.index);
  state.index++;
  state.stack.push(v);
  state.onStack.add(v);
  const successors = graph.get(v) ?? new Set;
  for (const w of successors) {
    if (!state.indices.has(w)) {
      strongconnect(w, graph, state);
      state.lowlinks.set(v, Math.min(state.lowlinks.get(v), state.lowlinks.get(w)));
    } else if (state.onStack.has(w)) {
      state.lowlinks.set(v, Math.min(state.lowlinks.get(v), state.indices.get(w)));
    }
  }
  if (state.lowlinks.get(v) === state.indices.get(v)) {
    const scc = [];
    let w;
    do {
      w = state.stack.pop();
      state.onStack.delete(w);
      scc.push(w);
    } while (w !== v);
    state.sccs.push(scc);
  }
}
function validateTopologicalOrder(sccs, graph) {
  const errors = [];
  const emitted = new Set;
  for (const scc of sccs) {
    const sccSet = new Set(scc.values);
    for (const name of scc.values) {
      const deps = graph.get(name) ?? new Set;
      for (const dep of deps) {
        if (!sccSet.has(dep) && !emitted.has(dep)) {
          errors.push(`Value '${name}' depends on '${dep}' which has not been emitted yet`);
        }
      }
    }
    for (const name of scc.values) {
      emitted.add(name);
    }
  }
  return { valid: errors.length === 0, errors };
}

// src/types.ts
class IRError extends Error {
  span;
  filePath;
  constructor(message, span, filePath) {
    super(message);
    this.span = span;
    this.filePath = filePath;
    this.name = "IRError";
  }
}
// src/lowering/context.ts
function createLoweringContext(semantics, imports = [], dependencies = new Map) {
  const ctx = {
    semantics,
    liftedBindings: [],
    nameCounter: 0,
    constructorTags: new Map,
    recordFields: new Map,
    imports,
    dependencies,
    namespaceImportedNames: new Map
  };
  assignConstructorTags(ctx);
  buildRecordFieldInfo(ctx);
  buildNamespaceImportedNames(ctx);
  return ctx;
}
function assignConstructorTags(ctx) {
  for (const [adtName, adt] of Object.entries(ctx.semantics.adts)) {
    for (let i = 0;i < adt.constructors.length; i++) {
      const ctorName = adt.constructors[i];
      if (ctorName) {
        ctx.constructorTags.set(ctorName, i);
      }
    }
  }
  for (const imp of ctx.imports) {
    const depModule = ctx.dependencies.get(imp.moduleName);
    if (!depModule)
      continue;
    for (const [adtName, adt] of Object.entries(depModule.adts)) {
      for (let i = 0;i < adt.constructors.length; i++) {
        const ctorName = adt.constructors[i];
        if (ctorName) {
          const fullQualified = `${imp.moduleName}.${ctorName}`;
          ctx.constructorTags.set(fullQualified, i);
          if (imp.alias) {
            const aliasQualified = `${imp.alias}.${ctorName}`;
            ctx.constructorTags.set(aliasQualified, i);
          }
        }
      }
    }
  }
}
function buildRecordFieldInfo(ctx) {
  for (const [name, alias] of Object.entries(ctx.semantics.typeAliases)) {
    if (alias.value.kind === "RecordType") {
      ctx.recordFields.set(name, alias.value.fields.map((f) => f.name));
    }
  }
}
function buildNamespaceImportedNames(ctx) {
  const allExposingModules = new Set;
  for (const imp of ctx.imports) {
    if (imp.exposing?.kind === "All") {
      allExposingModules.add(imp.moduleName);
    }
  }
  if (allExposingModules.size === 0)
    return;
  for (const [name, moduleName] of ctx.semantics.importedValues) {
    if (allExposingModules.has(moduleName)) {
      ctx.namespaceImportedNames.set(name, moduleName);
    }
  }
  for (const imp of ctx.imports) {
    if (imp.exposing?.kind !== "All")
      continue;
    const dep = ctx.dependencies.get(imp.moduleName);
    if (!dep)
      continue;
    for (const ctorName of Object.keys(ctx.semantics.constructors)) {
      if (dep.constructors[ctorName] && isConstructorExported(dep, ctorName)) {
        ctx.namespaceImportedNames.set(ctorName, imp.moduleName);
      }
    }
  }
}
function isConstructorExported(dep, ctorName) {
  const exports = dep.exports;
  if (exports.exportsAll)
    return true;
  for (const [, typeExport] of exports.types) {
    if (typeExport.constructors?.has(ctorName))
      return true;
  }
  return false;
}
// ../syntax/src/operators.ts
var OPERATOR_CHARS = new Set(Array.from("!#$%&*+./<=>?@%\\^|~:-"));
var CHAR_TO_IDENTIFIER = {
  ".": "_DOT",
  "+": "_PLUS",
  "-": "_MINUS",
  "*": "_STAR",
  "/": "_SLASH",
  "%": "_PERCENT",
  "^": "_CARET",
  "<": "_LT",
  ">": "_GT",
  "=": "_EQ",
  "|": "_PIPE",
  "&": "_AMP",
  "!": "_BANG",
  ":": "_COLON",
  "~": "_TILDE",
  $: "_DOLLAR",
  "#": "_HASH",
  "@": "_AT",
  "?": "_QUESTION",
  "\\": "_BACKSLASH"
};
function sanitizeOperator(lexeme) {
  if (lexeme.length === 0)
    return lexeme;
  const parts = [];
  for (const char of lexeme) {
    const mapping = CHAR_TO_IDENTIFIER[char];
    if (mapping) {
      parts.push(mapping);
    } else {
      return lexeme;
    }
  }
  return parts.join("");
}
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
var BOOL_TYPE_NAME = "Bool";

// src/lowering/patterns.ts
function lowerPattern(pattern, ctx) {
  switch (pattern.kind) {
    case "VarPattern":
      return {
        kind: "IRVarPattern",
        name: pattern.name,
        span: pattern.span
      };
    case "WildcardPattern":
      return {
        kind: "IRWildcardPattern",
        span: pattern.span
      };
    case "IntPattern":
      return {
        kind: "IRLiteralPattern",
        value: Number(pattern.value),
        literalType: "int",
        span: pattern.span
      };
    case "FloatPattern":
      return {
        kind: "IRLiteralPattern",
        value: Number(pattern.value),
        literalType: "float",
        span: pattern.span
      };
    case "StringPattern":
      return {
        kind: "IRLiteralPattern",
        value: pattern.value,
        literalType: "string",
        span: pattern.span
      };
    case "CharPattern":
      return {
        kind: "IRLiteralPattern",
        value: pattern.value,
        literalType: "char",
        span: pattern.span
      };
    case "ConstructorPattern": {
      const tag = ctx.constructorTags.get(pattern.name);
      if (tag === undefined) {
        throw new IRError(`Unknown constructor '${pattern.name}' - no tag found. ` + `This is a compiler bug: the constructor may be from an unresolved import.`, pattern.span);
      }
      return {
        kind: "IRConstructorPattern",
        name: pattern.name,
        args: pattern.args.map((p) => lowerPattern(p, ctx)),
        tag,
        span: pattern.span
      };
    }
    case "TuplePattern":
      return {
        kind: "IRTuplePattern",
        elements: pattern.elements.map((p) => lowerPattern(p, ctx)),
        span: pattern.span
      };
    case "ListPattern":
      return {
        kind: "IRListPattern",
        elements: pattern.elements.map((p) => lowerPattern(p, ctx)),
        span: pattern.span
      };
    case "ConsPattern":
      return {
        kind: "IRConsPattern",
        head: lowerPattern(pattern.head, ctx),
        tail: lowerPattern(pattern.tail, ctx),
        span: pattern.span
      };
    case "RecordPattern":
      return {
        kind: "IRRecordPattern",
        fields: pattern.fields.map((f) => ({
          name: f.name,
          pattern: f.pattern ? lowerPattern(f.pattern, ctx) : undefined
        })),
        span: pattern.span
      };
    default:
      const _exhaustive = pattern;
      throw new IRError(`Unknown pattern kind: ${pattern.kind}`, pattern.span);
  }
}

// src/lowering/types.ts
function convertType(type) {
  switch (type.kind) {
    case "var":
      return { kind: "var", id: type.id };
    case "con":
      return { kind: "con", name: type.name, args: type.args.map(convertType) };
    case "fun":
      return {
        kind: "fun",
        from: convertType(type.from),
        to: convertType(type.to)
      };
    case "tuple":
      return { kind: "tuple", elements: type.elements.map(convertType) };
    case "record":
      const fields = {};
      for (const [k, v] of Object.entries(type.fields)) {
        fields[k] = convertType(v);
      }
      return { kind: "record", fields };
    case "list":
      return { kind: "list", element: convertType(type.element) };
    default:
      return type;
  }
}
function convertConstraints(constraints) {
  return constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map(convertType)
  }));
}

// src/lowering/tco.ts
function rewriteSelfTailCalls(funcName, params, body, dictParamCount) {
  const paramNames = [];
  for (const p of params) {
    if (p.kind !== "IRVarPattern")
      return body;
    paramNames.push(p.name);
  }
  if (paramNames.length === 0)
    return body;
  const expectedArgCount = dictParamCount + paramNames.length;
  let foundTailCall = false;
  function rewrite(expr, inTailPos) {
    switch (expr.kind) {
      case "IRApply": {
        if (inTailPos && expr.callee.kind === "IRVar" && expr.callee.name === funcName && expr.callee.namespace === "value" && expr.args.length === expectedArgCount) {
          foundTailCall = true;
          return {
            kind: "IRLoopContinue",
            args: expr.args.slice(dictParamCount),
            span: expr.span
          };
        }
        if (inTailPos && expr.callee.kind === "IRLambda" && expr.args.length === 1) {
          const innerBody = rewrite(expr.callee.body, true);
          if (innerBody === expr.callee.body)
            return expr;
          return {
            ...expr,
            callee: { ...expr.callee, body: innerBody }
          };
        }
        return expr;
      }
      case "IRIf": {
        const thenBranch = rewrite(expr.thenBranch, inTailPos);
        const elseBranch = rewrite(expr.elseBranch, inTailPos);
        if (thenBranch === expr.thenBranch && elseBranch === expr.elseBranch) {
          return expr;
        }
        return { ...expr, thenBranch, elseBranch };
      }
      case "IRCase": {
        let changed = false;
        const branches = expr.branches.map((b) => {
          const body2 = rewrite(b.body, inTailPos);
          if (body2 !== b.body)
            changed = true;
          return body2 === b.body ? b : { ...b, body: body2 };
        });
        if (!changed)
          return expr;
        return { ...expr, branches };
      }
      case "IRLambda":
        return expr;
      case "IRSelfLoop":
      case "IRLoopContinue":
        return expr;
      default:
        return expr;
    }
  }
  const rewritten = rewrite(body, true);
  if (!foundTailCall)
    return body;
  return {
    kind: "IRSelfLoop",
    paramNames,
    body: rewritten,
    span: body.span
  };
}

// src/lowering/expressions.ts
function lowerExpr(exprArg, ctx) {
  let expr = exprArg;
  while (true) {
    switch (expr.kind) {
      case "Var":
        return lowerVar(expr, ctx);
      case "Number":
        return lowerNumber(expr);
      case "String":
        return {
          kind: "IRLiteral",
          value: expr.value,
          literalType: "string",
          span: expr.span
        };
      case "Char":
        return {
          kind: "IRLiteral",
          value: expr.value,
          literalType: "char",
          span: expr.span
        };
      case "Lambda":
        return {
          kind: "IRLambda",
          params: expr.args.map((p) => lowerPattern(p, ctx)),
          body: lowerExpr(expr.body, ctx),
          span: expr.span
        };
      case "Apply":
        return {
          kind: "IRApply",
          callee: lowerExpr(expr.callee, ctx),
          args: expr.args.map((a) => lowerExpr(a, ctx)),
          span: expr.span
        };
      case "If":
        return {
          kind: "IRIf",
          condition: lowerExpr(expr.condition, ctx),
          thenBranch: lowerExpr(expr.thenBranch, ctx),
          elseBranch: lowerExpr(expr.elseBranch, ctx),
          span: expr.span
        };
      case "LetIn":
        return lowerLetIn(expr, ctx);
      case "Case":
        return lowerCase(expr, ctx);
      case "Infix":
        return lowerInfix(expr, ctx);
      case "Unary":
        return {
          kind: "IRUnary",
          operator: expr.operator,
          operand: lowerExpr(expr.operand, ctx),
          span: expr.span
        };
      case "Paren":
        expr = expr.expression;
        continue;
      case "Tuple":
        return {
          kind: "IRTuple",
          elements: expr.elements.map((e) => lowerExpr(e, ctx)),
          span: expr.span
        };
      case "Unit":
        return {
          kind: "IRUnit",
          span: expr.span
        };
      case "List":
        return {
          kind: "IRList",
          elements: expr.elements.map((e) => lowerExpr(e, ctx)),
          span: expr.span
        };
      case "ListRange":
        return {
          kind: "IRApply",
          callee: {
            kind: "IRApply",
            callee: {
              kind: "IRVar",
              name: "range",
              namespace: "value",
              span: expr.span
            },
            args: [lowerExpr(expr.start, ctx)],
            span: expr.span
          },
          args: [lowerExpr(expr.end, ctx)],
          span: expr.span
        };
      case "Record":
        return {
          kind: "IRRecord",
          fields: expr.fields.map((f) => ({
            name: f.name,
            value: lowerExpr(f.value, ctx),
            span: f.span
          })),
          span: expr.span
        };
      case "RecordUpdate":
        return lowerRecordUpdate(expr, ctx);
      case "FieldAccess": {
        const moduleAccess = tryResolveModuleAccess(expr, ctx);
        if (moduleAccess) {
          return moduleAccess;
        }
        return {
          kind: "IRFieldAccess",
          target: lowerExpr(expr.target, ctx),
          field: expr.field,
          span: expr.span
        };
      }
      default:
        const _exhaustive = expr;
        throw new IRError(`Unknown expression kind: ${expr.kind}`, expr.span);
    }
  }
}
function getModuleValueConstraints(depModule, valueName) {
  const scheme = depModule.typeSchemes[valueName];
  if (!scheme || scheme.constraints.length === 0) {
    return [];
  }
  return scheme.constraints.map((c) => ({
    protocolName: c.protocolName,
    typeArgs: c.typeArgs.map((t) => convertType(t))
  }));
}
function getInstantiatedConstraints(expr, depModule, valueName, ctx) {
  const usages = ctx.semantics.constrainedCallUsages?.get(expr);
  if (usages && usages.length > 0) {
    return usages.map((u) => ({
      protocolName: u.protocolName,
      typeArgs: u.typeArgs.map((t) => convertType(t))
    }));
  }
  return getModuleValueConstraints(depModule, valueName);
}
function tryResolveModuleAccess(expr, ctx) {
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
  for (const imp of ctx.imports) {
    const importParts = imp.moduleName.split(".");
    if (imp.alias && baseName === imp.alias && parts.length >= 2) {
      const depModule = ctx.dependencies.get(imp.moduleName);
      if (!depModule) {
        continue;
      }
      const fieldParts = parts.slice(1);
      if (fieldParts.length === 1) {
        const field = fieldParts[0];
        const valueInfo = depModule.values[field];
        if (valueInfo) {
          const decl = valueInfo.declaration;
          let externalName;
          if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
            externalName = decl.args[1];
          }
          const constraints = getInstantiatedConstraints(expr, depModule, field, ctx);
          return {
            kind: "IRModuleAccess",
            importAlias: imp.alias,
            moduleName: imp.moduleName,
            valueName: field,
            externalName,
            ...constraints.length > 0 ? { constraints } : {},
            span: expr.span
          };
        }
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
        const depModule = ctx.dependencies.get(imp.moduleName);
        if (!depModule) {
          continue;
        }
        const fieldParts = parts.slice(importParts.length);
        if (fieldParts.length === 1) {
          const field = fieldParts[0];
          const valueInfo = depModule.values[field];
          if (valueInfo) {
            const decl = valueInfo.declaration;
            let externalName;
            if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
              externalName = decl.args[1];
            }
            const alias = imp.alias || importParts[importParts.length - 1];
            const constraints = getInstantiatedConstraints(expr, depModule, field, ctx);
            return {
              kind: "IRModuleAccess",
              importAlias: alias,
              moduleName: imp.moduleName,
              valueName: field,
              externalName,
              ...constraints.length > 0 ? { constraints } : {},
              span: expr.span
            };
          }
        }
      }
    }
  }
  return null;
}
function lowerVar(expr, ctx) {
  if (expr.namespace === "upper") {
    const ctorInfo = ctx.semantics.constructors[expr.name];
    if (ctorInfo) {
      const tag = ctx.constructorTags.get(expr.name) ?? 0;
      const ctorModuleName = ctx.namespaceImportedNames.get(expr.name);
      const isImported = ctorInfo.moduleName !== undefined && ctorInfo.moduleName !== ctx.semantics.module.name && ctorInfo.moduleName !== BUILTIN_MODULE_NAME;
      if (ctorInfo.arity === 0) {
        if (isImported) {
          return {
            kind: "IRVar",
            name: expr.name,
            namespace: "constructor",
            ...ctorModuleName ? { moduleName: ctorModuleName } : {},
            span: expr.span
          };
        }
        return {
          kind: "IRConstructor",
          name: expr.name,
          args: [],
          tag,
          span: expr.span
        };
      }
      return {
        kind: "IRVar",
        name: expr.name,
        namespace: "constructor",
        ...ctorModuleName ? { moduleName: ctorModuleName } : {},
        span: expr.span
      };
    }
  }
  const moduleName = ctx.namespaceImportedNames.get(expr.name);
  let constraint;
  const usage = ctx.semantics.protocolMethodUsages?.get(expr);
  if (usage) {
    constraint = {
      protocolName: usage.protocolName,
      typeArgs: usage.typeArgs.map((t) => convertType(t))
    };
  }
  return {
    kind: "IRVar",
    name: expr.name,
    namespace: "value",
    ...moduleName ? { moduleName } : {},
    ...constraint ? { constraint } : {},
    span: expr.span
  };
}
function lowerNumber(expr) {
  const isFloat = expr.value.includes(".") || expr.value.includes("e") || expr.value.includes("E");
  return {
    kind: "IRLiteral",
    value: isFloat ? parseFloat(expr.value) : parseInt(expr.value, 10),
    literalType: isFloat ? "float" : "int",
    span: expr.span
  };
}
function lowerLetIn(exprArg, ctx) {
  const levels = [];
  let current = exprArg;
  while (current.kind === "LetIn") {
    levels.push({ bindings: current.bindings, span: current.span });
    current = current.body;
  }
  let result = lowerExpr(current, ctx);
  for (let level = levels.length - 1;level >= 0; level--) {
    const { bindings, span } = levels[level];
    for (let i = bindings.length - 1;i >= 0; i--) {
      const binding = bindings[i];
      const bindingValue = lowerValueBody(binding, ctx);
      const pattern = {
        kind: "IRVarPattern",
        name: binding.name,
        span: binding.span
      };
      const lambda = {
        kind: "IRLambda",
        params: [pattern],
        body: result,
        span
      };
      let valueExpr;
      if (binding.args.length > 0) {
        const bindingParams = binding.args.map((p) => lowerPattern(p, ctx));
        const tcoBody = rewriteSelfTailCalls(binding.name, bindingParams, bindingValue, 0);
        valueExpr = {
          kind: "IRLambda",
          params: bindingParams,
          body: tcoBody,
          span: binding.span
        };
      } else {
        valueExpr = bindingValue;
      }
      result = {
        kind: "IRApply",
        callee: lambda,
        args: [valueExpr],
        span
      };
    }
  }
  return result;
}
function lowerValueBody(decl, ctx) {
  return lowerExpr(decl.body, ctx);
}
function lowerCase(expr, ctx) {
  const discriminant = lowerExpr(expr.discriminant, ctx);
  const branches = expr.branches;
  if (isBoolCase(branches, ctx)) {
    return lowerBoolCase(discriminant, branches, ctx, expr.span);
  }
  if (allLiteralPatterns(branches)) {
    return lowerLiteralCase(discriminant, branches, ctx, expr.span);
  }
  return {
    kind: "IRCase",
    discriminant,
    branches: branches.map((b) => ({
      pattern: lowerPattern(b.pattern, ctx),
      body: lowerExpr(b.body, ctx),
      span: b.span
    })),
    span: expr.span
  };
}
function isBoolCase(branches, ctx) {
  if (branches.length !== 2)
    return false;
  const patterns = branches.map((b) => b.pattern);
  const truePattern = patterns.find((p) => p.kind === "ConstructorPattern" && p.name === "True" && p.args.length === 0);
  const falsePattern = patterns.find((p) => p.kind === "ConstructorPattern" && p.name === "False" && p.args.length === 0);
  if (!truePattern || !falsePattern)
    return false;
  const trueCtorInfo = ctx.semantics.constructors["True"];
  const falseCtorInfo = ctx.semantics.constructors["False"];
  return trueCtorInfo?.moduleName === BUILTIN_MODULE_NAME && trueCtorInfo?.parentType === BOOL_TYPE_NAME && falseCtorInfo?.moduleName === BUILTIN_MODULE_NAME && falseCtorInfo?.parentType === BOOL_TYPE_NAME;
}
function lowerBoolCase(discriminant, branches, ctx, span) {
  let trueBranch = null;
  let falseBranch = null;
  for (const branch of branches) {
    if (branch.pattern.kind === "ConstructorPattern" && branch.pattern.name === "True") {
      trueBranch = lowerExpr(branch.body, ctx);
    } else if (branch.pattern.kind === "ConstructorPattern" && branch.pattern.name === "False") {
      falseBranch = lowerExpr(branch.body, ctx);
    }
  }
  if (!trueBranch || !falseBranch) {
    throw new IRError("Bool case missing True or False branch", span);
  }
  return {
    kind: "IRIf",
    condition: discriminant,
    thenBranch: trueBranch,
    elseBranch: falseBranch,
    span
  };
}
function allLiteralPatterns(branches) {
  return false;
}
function lowerLiteralCase(discriminant, branches, ctx, span) {
  return {
    kind: "IRCase",
    discriminant,
    branches: branches.map((b) => ({
      pattern: lowerPattern(b.pattern, ctx),
      body: lowerExpr(b.body, ctx),
      span: b.span
    })),
    span
  };
}
function lowerRecordUpdate(expr, ctx) {
  const baseType = ctx.semantics.types[expr.base];
  const baseVar = {
    kind: "IRVar",
    name: expr.base,
    namespace: "value",
    span: expr.span
  };
  const updatedFields = new Set(expr.fields.map((f) => f.name));
  let allFields = [];
  if (baseType && baseType.kind === "record") {
    allFields = Object.keys(baseType.fields);
  }
  const irFields = [];
  for (const fieldName of allFields) {
    if (!updatedFields.has(fieldName)) {
      irFields.push({
        name: fieldName,
        value: {
          kind: "IRFieldAccess",
          target: baseVar,
          field: fieldName,
          span: expr.span
        },
        span: expr.span
      });
    }
  }
  for (const field of expr.fields) {
    irFields.push({
      name: field.name,
      value: lowerExpr(field.value, ctx),
      span: field.span
    });
  }
  if (allFields.length === 0) {
    return {
      kind: "IRRecordUpdate",
      base: baseVar,
      updates: expr.fields.map((f) => ({
        name: f.name,
        value: lowerExpr(f.value, ctx),
        span: f.span
      })),
      span: expr.span
    };
  }
  return {
    kind: "IRRecord",
    fields: irFields,
    span: expr.span
  };
}
function lowerInfix(expr, ctx) {
  const left = lowerExpr(expr.left, ctx);
  const right = lowerExpr(expr.right, ctx);
  const opModuleName = ctx.namespaceImportedNames.get(expr.operator);
  let constraint;
  const usage = ctx.semantics.protocolMethodUsages?.get(expr);
  if (usage) {
    constraint = {
      protocolName: usage.protocolName,
      typeArgs: usage.typeArgs.map((t) => convertType(t))
    };
  }
  const opVar = {
    kind: "IRVar",
    name: expr.operator,
    namespace: "value",
    ...opModuleName ? { moduleName: opModuleName } : {},
    ...constraint ? { constraint } : {},
    span: expr.span
  };
  const rightArg = SHORT_CIRCUIT_OPERATORS.has(expr.operator) ? {
    kind: "IRLambda",
    params: [],
    body: right,
    span: expr.right.span
  } : right;
  return {
    kind: "IRApply",
    callee: {
      kind: "IRApply",
      callee: opVar,
      args: [left],
      span: expr.span
    },
    args: [rightArg],
    span: expr.span
  };
}
// src/printer.ts
var defaultOptions = {
  indent: "  ",
  showTypes: true,
  showConstraints: true,
  showSpans: false,
  maxWidth: 80
};
function printProgram(program, options = {}) {
  const opts = { ...defaultOptions, ...options };
  const lines = [];
  lines.push(`module ${program.moduleName}`);
  lines.push("");
  if (program.externalImports.size > 0) {
    lines.push("-- External Imports");
    for (const imp of program.externalImports) {
      lines.push(`import "${imp}"`);
    }
    lines.push("");
  }
  if (program.defaultImports.length > 0) {
    lines.push("-- Default Imports");
    for (const imp of program.defaultImports) {
      lines.push(`import ${imp.name} from "${imp.modulePath}"`);
    }
    lines.push("");
  }
  if (Object.keys(program.adts).length > 0) {
    lines.push("-- Types");
    for (const [name, adt] of Object.entries(program.adts)) {
      const params = adt.params.length > 0 ? " " + adt.params.join(" ") : "";
      const ctors = adt.constructors.map((c) => {
        const info = program.constructors[c];
        const tag = info ? `[${info.tag}]` : "";
        return `${c}${tag}`;
      }).join(" | ");
      lines.push(`type ${name}${params} = ${ctors}`);
    }
    lines.push("");
  }
  if (Object.keys(program.protocols).length > 0) {
    lines.push("-- Protocols");
    for (const [name, proto] of Object.entries(program.protocols)) {
      const params = proto.params.join(" ");
      lines.push(`protocol ${name} ${params} where`);
      for (const method of proto.methods) {
        const def = method.hasDefault ? " {default}" : "";
        if (opts.showTypes) {
          lines.push(`${opts.indent}${method.name} : ${printType(method.type)}${def}`);
        } else {
          lines.push(`${opts.indent}${method.name}${def}`);
        }
      }
    }
    lines.push("");
  }
  if (program.instances.length > 0) {
    lines.push("-- Instances");
    for (const inst of program.instances) {
      const typeArgs = inst.typeArgs.map(printType).join(" ");
      const constraints = inst.constraints.length > 0 ? `(${inst.constraints.map(printConstraint).join(", ")}) => ` : "";
      lines.push(`implement ${constraints}${inst.protocolName} ${typeArgs} where`);
      for (const [method, impl] of Object.entries(inst.methods)) {
        lines.push(`${opts.indent}${method} = ${impl}`);
      }
    }
    lines.push("");
  }
  lines.push("-- Values (in dependency order)");
  for (const scc of program.dependencyOrder) {
    if (scc.isMutuallyRecursive) {
      lines.push(`-- SCC (mutually recursive): ${scc.values.join(", ")}`);
    }
    for (const valueName of scc.values) {
      const value = program.values[valueName];
      if (value) {
        lines.push(printValue(value, opts));
        lines.push("");
      }
    }
  }
  if (program.liftedBindings.length > 0) {
    lines.push("-- Lifted Bindings");
    for (const binding of program.liftedBindings) {
      lines.push(printValue(binding, opts));
      lines.push("");
    }
  }
  return lines.join(`
`);
}
function printValue(value, options = {}) {
  const opts = { ...defaultOptions, ...options };
  const lines = [];
  if (opts.showConstraints && value.constraints.length > 0) {
    const constraints = value.constraints.map(printConstraint).join(", ");
    lines.push(`-- Constraints: ${constraints}`);
  }
  if (opts.showTypes) {
    lines.push(`${value.name} : ${printType(value.type)}`);
  }
  if (value.isExternal && value.externalTarget) {
    lines.push(`@external "${value.externalTarget.modulePath}" "${value.externalTarget.exportName}"`);
  }
  const params = value.params.length > 0 ? " " + value.params.map((p) => printPattern(p)).join(" ") : "";
  if (value.isExternal) {
    lines.push(`${value.name}${params}`);
  } else {
    const body = printExpr(value.body, opts.indent, 0);
    lines.push(`${value.name}${params} =`);
    lines.push(`${opts.indent}${body}`);
  }
  return lines.join(`
`);
}
function printExpr(expr, indent = "  ", depth = 0) {
  const ind = indent.repeat(depth);
  const ind1 = indent.repeat(depth + 1);
  switch (expr.kind) {
    case "IRVar":
      return expr.name;
    case "IRModuleAccess":
      return `${expr.importAlias}.${expr.externalName || expr.valueName}`;
    case "IRLiteral":
      if (expr.literalType === "string") {
        return `"${expr.value}"`;
      } else if (expr.literalType === "char") {
        return `'${expr.value}'`;
      } else if (expr.literalType === "bool") {
        return expr.value ? "True" : "False";
      }
      return String(expr.value);
    case "IRLambda": {
      const params = expr.params.map(printPattern).join(" ");
      const body = printExpr(expr.body, indent, 0);
      return `\\${params} -> ${body}`;
    }
    case "IRApply": {
      const callee = printExpr(expr.callee, indent, 0);
      const args = expr.args.map((a) => printExpr(a, indent, 0));
      const calleeStr = expr.callee.kind === "IRLambda" ? `(${callee})` : callee;
      const argStrs = args.map((a, i) => {
        const arg = expr.args[i];
        if (arg && (arg.kind === "IRApply" || arg.kind === "IRLambda" || arg.kind === "IRIf" || arg.kind === "IRCase")) {
          return `(${a})`;
        }
        return a;
      });
      return `${calleeStr} ${argStrs.join(" ")}`;
    }
    case "IRIf":
      return `if ${printExpr(expr.condition, indent, 0)}
` + `${ind1}then ${printExpr(expr.thenBranch, indent, depth + 1)}
` + `${ind1}else ${printExpr(expr.elseBranch, indent, depth + 1)}`;
    case "IRCase": {
      const disc = printExpr(expr.discriminant, indent, 0);
      const branches = expr.branches.map((b) => {
        const pat = printPattern(b.pattern);
        const body = printExpr(b.body, indent, depth + 2);
        return `${ind1}${pat} -> ${body}`;
      }).join(`
`);
      return `case ${disc} of
${branches}`;
    }
    case "IRTuple": {
      const elems = expr.elements.map((e) => printExpr(e, indent, 0));
      return `(${elems.join(", ")})`;
    }
    case "IRUnit":
      return "()";
    case "IRList": {
      const elems = expr.elements.map((e) => printExpr(e, indent, 0));
      return `[${elems.join(", ")}]`;
    }
    case "IRRecord": {
      const fields = expr.fields.map((f) => `${f.name} = ${printExpr(f.value, indent, 0)}`);
      return `{ ${fields.join(", ")} }`;
    }
    case "IRRecordUpdate": {
      const base = printExpr(expr.base, indent, 0);
      const updates = expr.updates.map((f) => `${f.name} = ${printExpr(f.value, indent, 0)}`);
      return `{ ${base} | ${updates.join(", ")} }`;
    }
    case "IRFieldAccess":
      return `${printExpr(expr.target, indent, 0)}.${expr.field}`;
    case "IRConstructor": {
      if (expr.args.length === 0) {
        return expr.name;
      }
      const args = expr.args.map((a) => {
        const s = printExpr(a, indent, 0);
        return a.kind === "IRApply" || a.kind === "IRConstructor" ? `(${s})` : s;
      });
      return `${expr.name} ${args.join(" ")}`;
    }
    case "IRUnary":
      return `-${printExpr(expr.operand, indent, 0)}`;
    case "IRSelfLoop": {
      const params = expr.paramNames.join(", ");
      const body = printExpr(expr.body, indent, depth + 1);
      return `loop (${params}) {
${ind1}${body}
${ind}}`;
    }
    case "IRLoopContinue": {
      const args = expr.args.map((a) => printExpr(a, indent, 0));
      return `continue (${args.join(", ")})`;
    }
    default:
      return `<unknown: ${expr.kind}>`;
  }
}
function printPattern(pattern) {
  switch (pattern.kind) {
    case "IRVarPattern":
      return pattern.name;
    case "IRWildcardPattern":
      return "_";
    case "IRConstructorPattern": {
      if (pattern.args.length === 0) {
        return pattern.name;
      }
      const args = pattern.args.map(printPattern);
      return `(${pattern.name} ${args.join(" ")})`;
    }
    case "IRTuplePattern": {
      const elems = pattern.elements.map(printPattern);
      return `(${elems.join(", ")})`;
    }
    case "IRLiteralPattern":
      if (pattern.literalType === "string") {
        return `"${pattern.value}"`;
      } else if (pattern.literalType === "char") {
        return `'${pattern.value}'`;
      }
      return String(pattern.value);
    case "IRListPattern": {
      const elems = pattern.elements.map(printPattern);
      return `[${elems.join(", ")}]`;
    }
    case "IRConsPattern":
      return `(${printPattern(pattern.head)} :: ${printPattern(pattern.tail)})`;
    default:
      return `<unknown: ${pattern.kind}>`;
  }
}
function printType(type) {
  switch (type.kind) {
    case "var":
      if (type.id >= 0 && type.id < 26) {
        return String.fromCharCode(97 + type.id);
      }
      return `t${type.id}`;
    case "con":
      if (type.args.length === 0) {
        return type.name;
      }
      const args = type.args.map((a) => {
        const s = printType(a);
        return a.kind === "fun" ? `(${s})` : s;
      });
      return `${type.name} ${args.join(" ")}`;
    case "fun": {
      const from = printType(type.from);
      const to = printType(type.to);
      const fromStr = type.from.kind === "fun" ? `(${from})` : from;
      return `${fromStr} -> ${to}`;
    }
    case "tuple": {
      const elems = type.elements.map(printType);
      return `(${elems.join(", ")})`;
    }
    case "record": {
      const fields = Object.entries(type.fields).map(([k, v]) => `${k} : ${printType(v)}`).join(", ");
      return `{ ${fields} }`;
    }
    case "list":
      return `List ${printType(type.element)}`;
    default:
      return `<unknown>`;
  }
}
function printConstraint(constraint) {
  const args = constraint.typeArgs.map(printType).join(" ");
  return `${constraint.protocolName} ${args}`;
}
function printSCC(scc) {
  const kind = scc.isMutuallyRecursive ? "recursive" : "single";
  return `[${kind}] ${scc.values.join(", ")}`;
}
function printDependencyOrder(sccs) {
  return sccs.map((scc, i) => `${i + 1}. ${printSCC(scc)}`).join(`
`);
}
function printExprCompact(expr) {
  switch (expr.kind) {
    case "IRVar":
      return expr.name;
    case "IRModuleAccess":
      return `${expr.importAlias}.${expr.externalName || expr.valueName}`;
    case "IRLiteral":
      return String(expr.value);
    case "IRLambda":
      return `(\\... -> ...)`;
    case "IRApply":
      return `(${printExprCompact(expr.callee)} ...)`;
    case "IRIf":
      return `(if ... then ... else ...)`;
    case "IRCase":
      return `(case ... of ...)`;
    case "IRTuple":
      return `(${expr.elements.length} elems)`;
    case "IRUnit":
      return "()";
    case "IRList":
      return `[${expr.elements.length} elems]`;
    case "IRRecord":
      return `{${expr.fields.length} fields}`;
    case "IRFieldAccess":
      return `_.${expr.field}`;
    case "IRRecordUpdate":
      return `{ ... | ${expr.updates.length} fields }`;
    case "IRConstructor":
      return expr.args.length > 0 ? `${expr.name} ...` : expr.name;
    case "IRUnary":
      return `-${printExprCompact(expr.operand)}`;
    case "IRSelfLoop":
      return `(loop ...)`;
    case "IRLoopContinue":
      return `(continue ...)`;
    default:
      return "?";
  }
}
// src/internal/helpers.ts
function substituteProtocolMethods(expr, methodSubstitutions) {
  if (methodSubstitutions.size === 0)
    return expr;
  switch (expr.kind) {
    case "Var":
      if (methodSubstitutions.has(expr.name)) {
        return methodSubstitutions.get(expr.name);
      }
      return expr;
    case "Infix": {
      if (methodSubstitutions.has(expr.operator)) {
        const newOp = methodSubstitutions.get(expr.operator);
        const left = substituteProtocolMethods(expr.left, methodSubstitutions);
        const right = substituteProtocolMethods(expr.right, methodSubstitutions);
        return {
          kind: "Apply",
          callee: {
            kind: "Apply",
            callee: newOp,
            args: [left],
            span: expr.span
          },
          args: [right],
          span: expr.span
        };
      }
      return {
        kind: "Infix",
        left: substituteProtocolMethods(expr.left, methodSubstitutions),
        operator: expr.operator,
        right: substituteProtocolMethods(expr.right, methodSubstitutions),
        span: expr.span
      };
    }
    case "Lambda":
      return {
        kind: "Lambda",
        args: expr.args,
        body: substituteProtocolMethods(expr.body, methodSubstitutions),
        span: expr.span
      };
    case "Apply":
      return {
        kind: "Apply",
        callee: substituteProtocolMethods(expr.callee, methodSubstitutions),
        args: expr.args.map((arg) => substituteProtocolMethods(arg, methodSubstitutions)),
        span: expr.span
      };
    case "If":
      return {
        kind: "If",
        condition: substituteProtocolMethods(expr.condition, methodSubstitutions),
        thenBranch: substituteProtocolMethods(expr.thenBranch, methodSubstitutions),
        elseBranch: substituteProtocolMethods(expr.elseBranch, methodSubstitutions),
        span: expr.span
      };
    case "LetIn":
      return {
        kind: "LetIn",
        bindings: expr.bindings.map((b) => ({
          ...b,
          body: substituteProtocolMethods(b.body, methodSubstitutions)
        })),
        body: substituteProtocolMethods(expr.body, methodSubstitutions),
        span: expr.span
      };
    case "Case":
      return {
        kind: "Case",
        discriminant: substituteProtocolMethods(expr.discriminant, methodSubstitutions),
        branches: expr.branches.map((branch) => ({
          ...branch,
          body: substituteProtocolMethods(branch.body, methodSubstitutions)
        })),
        span: expr.span
      };
    case "Paren":
      return {
        kind: "Paren",
        expression: substituteProtocolMethods(expr.expression, methodSubstitutions),
        span: expr.span
      };
    case "Tuple":
      return {
        kind: "Tuple",
        elements: expr.elements.map((e) => substituteProtocolMethods(e, methodSubstitutions)),
        span: expr.span
      };
    case "List":
      return {
        kind: "List",
        elements: expr.elements.map((e) => substituteProtocolMethods(e, methodSubstitutions)),
        span: expr.span
      };
    case "ListRange":
      return {
        kind: "ListRange",
        start: substituteProtocolMethods(expr.start, methodSubstitutions),
        end: substituteProtocolMethods(expr.end, methodSubstitutions),
        span: expr.span
      };
    case "Record":
      return {
        kind: "Record",
        fields: expr.fields.map((f) => ({
          ...f,
          value: substituteProtocolMethods(f.value, methodSubstitutions)
        })),
        span: expr.span
      };
    case "RecordUpdate":
      return {
        kind: "RecordUpdate",
        base: expr.base,
        fields: expr.fields.map((f) => ({
          ...f,
          value: substituteProtocolMethods(f.value, methodSubstitutions)
        })),
        span: expr.span
      };
    case "FieldAccess":
      return {
        kind: "FieldAccess",
        target: substituteProtocolMethods(expr.target, methodSubstitutions),
        field: expr.field,
        span: expr.span
      };
    case "Number":
    case "String":
    case "Char":
    case "Unit":
      return expr;
    case "Unary":
      return {
        kind: "Unary",
        operator: expr.operator,
        operand: substituteProtocolMethods(expr.operand, methodSubstitutions),
        span: expr.span
      };
  }
}

// src/impl/lower.ts
function substituteTypeArg(typeArg, paramSubst) {
  if (typeArg.kind === "var") {
    const values = Array.from(paramSubst.values());
    if (values.length === 1) {
      return values[0];
    }
    return typeArg;
  }
  if (typeArg.kind === "con") {
    return {
      kind: "con",
      name: typeArg.name,
      args: typeArg.args.map((arg) => substituteTypeArg(arg, paramSubst))
    };
  }
  if (typeArg.kind === "fun") {
    return {
      kind: "fun",
      from: substituteTypeArg(typeArg.from, paramSubst),
      to: substituteTypeArg(typeArg.to, paramSubst)
    };
  }
  if (typeArg.kind === "tuple") {
    return {
      kind: "tuple",
      elements: typeArg.elements.map((el) => substituteTypeArg(el, paramSubst))
    };
  }
  if (typeArg.kind === "record") {
    const newFields = {};
    for (const [key, val] of Object.entries(typeArg.fields)) {
      newFields[key] = substituteTypeArg(val, paramSubst);
    }
    return {
      kind: "record",
      fields: newFields
    };
  }
  return typeArg;
}
function lower(program, semantics, options = {}) {
  const imports = program.imports || [];
  const dependencies = options.dependencies || new Map;
  const ctx = createLoweringContext(semantics, imports, dependencies);
  const values = {};
  const declarationOrder = [];
  for (const [name, valueInfo] of Object.entries(semantics.values)) {
    declarationOrder.push(name);
    const decl = valueInfo.declaration;
    const type = semantics.types[name];
    const isExternal = decl.kind === "DecoratedDeclaration";
    let body;
    let params = [];
    if (isExternal) {
      body = {
        kind: "IRUnit",
        span: decl.span
      };
    } else {
      body = lowerExpr(decl.body, ctx);
      params = decl.args.map((p) => lowerPattern(p, ctx));
    }
    const irType = type ? convertType(type) : { kind: "var", id: -1 };
    const typeScheme = semantics.typeSchemes[name];
    const constraints = typeScheme ? convertConstraints(typeScheme.constraints) : [];
    let propertyAccess;
    if (decl.kind === "DecoratedDeclaration" && (decl.decorator === "get" || decl.decorator === "call" || decl.decorator === "val")) {
      const key = decl.args[0];
      if (decl.decorator === "val") {
        propertyAccess = {
          variant: "val",
          key,
          callArity: 0
        };
      } else {
        let argCount = 0;
        let t = irType;
        while (t.kind === "fun") {
          argCount++;
          t = t.to;
        }
        propertyAccess = {
          variant: decl.decorator,
          key,
          callArity: Math.max(0, argCount - 1)
        };
      }
    }
    let externalTarget;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "external") {
      let argCount = 0;
      let t = irType;
      while (t.kind === "fun") {
        argCount++;
        t = t.to;
      }
      externalTarget = {
        modulePath: decl.args[0],
        exportName: decl.args[1],
        callArity: argCount
      };
    }
    if (params.length > 0 && !isExternal) {
      const seenProtos = new Set;
      let dictParamCount = 0;
      for (const c of constraints) {
        const ta = c.typeArgs[0];
        if (ta && ta.kind === "var" && !seenProtos.has(c.protocolName)) {
          seenProtos.add(c.protocolName);
          dictParamCount++;
        }
      }
      body = rewriteSelfTailCalls(name, params, body, dictParamCount);
    }
    const irValue = {
      name,
      params,
      body,
      type: irType,
      constraints,
      isExternal: decl.kind === "DecoratedDeclaration" && (decl.decorator === "external" || decl.decorator === "import"),
      externalTarget,
      propertyAccess,
      span: decl.span
    };
    values[name] = irValue;
  }
  const constructors = {};
  for (const [name, info] of Object.entries(semantics.constructors)) {
    constructors[name] = {
      name,
      parentType: info.parentType,
      arity: info.arity,
      tag: ctx.constructorTags.get(name) ?? 0,
      moduleName: info.moduleName
    };
  }
  const protocols = {};
  for (const [name, proto] of Object.entries(semantics.protocols)) {
    const methods = [];
    for (const [methodName, methodInfo] of proto.methods) {
      methods.push({
        name: methodName,
        type: methodInfo.type ? convertType(methodInfo.type) : { kind: "var", id: -1 },
        hasDefault: methodInfo.defaultImpl !== undefined
      });
    }
    const superclassConstraints = proto.superclassConstraints.map((c) => ({
      protocolName: c.protocolName,
      typeArgs: c.typeArgs.map((t) => convertType(t))
    }));
    protocols[name] = {
      name: proto.name,
      params: proto.params,
      superclassConstraints,
      methods
    };
  }
  const syntheticValues = {};
  const syntheticOrder = [];
  const currentModuleName = semantics.module.name;
  const isLocalInstance = (inst) => !inst.moduleName || inst.moduleName === currentModuleName;
  const instances = semantics.instances.map((inst) => {
    const methodsObj = {};
    for (const [methodName, methodExpr] of inst.methods) {
      if (methodExpr.kind === "Var") {
        methodsObj[methodName] = methodExpr.name;
      } else if (methodExpr.kind === "FieldAccess") {
        const parts = [methodExpr.field];
        let current = methodExpr.target;
        while (current.kind === "FieldAccess") {
          parts.unshift(current.field);
          current = current.target;
        }
        if (current.kind === "Var") {
          parts.unshift(current.name);
        }
        methodsObj[methodName] = parts.join(".");
      } else if (methodExpr.kind === "Lambda") {
        if (!isLocalInstance(inst)) {
          const typeKey2 = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
          const sanitizedMethodName2 = sanitizeOperator(methodName);
          const isExplicit2 = inst.explicitMethods.has(methodName);
          const prefix2 = isExplicit2 ? "$impl" : "$default";
          const syntheticName2 = `${prefix2}_${inst.protocolName}_${typeKey2}_${sanitizedMethodName2}`;
          methodsObj[methodName] = syntheticName2;
          continue;
        }
        const isExplicit = inst.explicitMethods.has(methodName);
        const typeKey = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const prefix = isExplicit ? "$impl" : "$default";
        const syntheticName = `${prefix}_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;
        const methodSubstitutions = new Map;
        for (const [otherMethodName, otherMethodExpr] of inst.methods) {
          if (otherMethodExpr.kind === "Var" || otherMethodExpr.kind === "FieldAccess") {
            methodSubstitutions.set(otherMethodName, otherMethodExpr);
          }
        }
        const substitutedExpr = substituteProtocolMethods(methodExpr, methodSubstitutions);
        const irLambda = lowerExpr(substitutedExpr, ctx);
        const protocol = semantics.protocols[inst.protocolName];
        const methodInfo = protocol?.methods.get(methodName);
        let methodType = { kind: "var", id: -1 };
        if (methodInfo?.type) {
          const paramSubst = new Map;
          if (protocol) {
            for (let pi = 0;pi < protocol.params.length; pi++) {
              const param = protocol.params[pi];
              const instTypeArg = inst.typeArgs[pi];
              if (param && instTypeArg) {
                paramSubst.set(param, instTypeArg);
              }
            }
          }
          const specializedType = substituteTypeArg(methodInfo.type, paramSubst);
          methodType = convertType(specializedType);
        }
        const instanceConstraints = inst.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => convertType(t))
        }));
        const selfConstraint = {
          protocolName: inst.protocolName,
          typeArgs: inst.typeArgs.map((t) => convertType(t))
        };
        const superclassConstraints = [];
        if (protocol?.superclassConstraints) {
          const paramSubst = new Map;
          for (let i = 0;i < protocol.params.length; i++) {
            const param = protocol.params[i];
            const instTypeArg = inst.typeArgs[i];
            if (param && instTypeArg) {
              paramSubst.set(param, instTypeArg);
            }
          }
          for (const superConstraint of protocol.superclassConstraints) {
            const substitutedTypeArgs = superConstraint.typeArgs.map((typeArg) => {
              const substituted = substituteTypeArg(typeArg, paramSubst);
              return convertType(substituted);
            });
            superclassConstraints.push({
              protocolName: superConstraint.protocolName,
              typeArgs: substitutedTypeArgs
            });
          }
        }
        const allConstraints = [
          selfConstraint,
          ...instanceConstraints,
          ...superclassConstraints
        ];
        const syntheticValue = {
          name: syntheticName,
          params: [],
          body: irLambda,
          type: methodType,
          constraints: allConstraints,
          isExternal: false,
          span: methodExpr.span
        };
        syntheticValues[syntheticName] = syntheticValue;
        syntheticOrder.push(syntheticName);
        methodsObj[methodName] = syntheticName;
      } else {
        if (!isLocalInstance(inst)) {
          const typeKey2 = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
          const sanitizedMethodName2 = sanitizeOperator(methodName);
          const syntheticName2 = `$impl_${inst.protocolName}_${typeKey2}_${sanitizedMethodName2}`;
          methodsObj[methodName] = syntheticName2;
          continue;
        }
        const typeKey = inst.typeArgs.length > 0 ? formatTypeKey(convertType(inst.typeArgs[0])) : "Unknown";
        const sanitizedMethodName = sanitizeOperator(methodName);
        const syntheticName = `$impl_${inst.protocolName}_${typeKey}_${sanitizedMethodName}`;
        const irExpr = lowerExpr(methodExpr, ctx);
        const protocol = semantics.protocols[inst.protocolName];
        const methodInfo = protocol?.methods.get(methodName);
        let methodType = { kind: "var", id: -1 };
        if (methodInfo?.type) {
          const paramSubst2 = new Map;
          if (protocol) {
            for (let pi = 0;pi < protocol.params.length; pi++) {
              const param = protocol.params[pi];
              const instTypeArg = inst.typeArgs[pi];
              if (param && instTypeArg) {
                paramSubst2.set(param, instTypeArg);
              }
            }
          }
          const specializedType = substituteTypeArg(methodInfo.type, paramSubst2);
          methodType = convertType(specializedType);
        }
        const instanceConstraints = inst.constraints.map((c) => ({
          protocolName: c.protocolName,
          typeArgs: c.typeArgs.map((t) => convertType(t))
        }));
        const selfConstraint2 = {
          protocolName: inst.protocolName,
          typeArgs: inst.typeArgs.map((t) => convertType(t))
        };
        const superclassConstraints2 = [];
        if (protocol?.superclassConstraints) {
          const paramSubst = new Map;
          for (let i = 0;i < protocol.params.length; i++) {
            const param = protocol.params[i];
            const instTypeArg = inst.typeArgs[i];
            if (param && instTypeArg) {
              paramSubst.set(param, instTypeArg);
            }
          }
          for (const superConstraint of protocol.superclassConstraints) {
            const substitutedTypeArgs = superConstraint.typeArgs.map((typeArg) => {
              const substituted = substituteTypeArg(typeArg, paramSubst);
              return convertType(substituted);
            });
            superclassConstraints2.push({
              protocolName: superConstraint.protocolName,
              typeArgs: substitutedTypeArgs
            });
          }
        }
        const allConstraints2 = [
          selfConstraint2,
          ...instanceConstraints,
          ...superclassConstraints2
        ];
        const syntheticValue = {
          name: syntheticName,
          params: [],
          body: irExpr,
          type: methodType,
          constraints: allConstraints2,
          isExternal: false,
          span: methodExpr.span
        };
        syntheticValues[syntheticName] = syntheticValue;
        syntheticOrder.push(syntheticName);
        methodsObj[methodName] = syntheticName;
      }
    }
    return {
      protocolName: inst.protocolName,
      typeArgs: inst.typeArgs.map((t) => convertType(t)),
      constraints: inst.constraints.map((c) => ({
        protocolName: c.protocolName,
        typeArgs: c.typeArgs.map((t) => convertType(t))
      })),
      methods: methodsObj
    };
  });
  const mergedValues = { ...values, ...syntheticValues };
  const instanceNames = [];
  for (const inst of instances) {
    if (inst.typeArgs[0]) {
      const key = formatTypeKey(inst.typeArgs[0]);
      instanceNames.push(`$dict_${inst.protocolName}_${key}`);
    }
  }
  const mergedOrder = [
    ...declarationOrder,
    ...syntheticOrder,
    ...instanceNames
  ];
  const depGraph = buildDependencyGraph(mergedValues, instances, protocols, constructors);
  const sccs = findSCCs(depGraph, mergedOrder);
  if (options.validateDependencies) {
    const validation = validateTopologicalOrder(sccs, depGraph);
    if (!validation.valid) {
      console.warn("Dependency order validation failed:", validation.errors);
    }
  }
  const syntheticDefaultImpls = Object.values(syntheticValues);
  const constraintMetadata = new Map;
  for (const [name, value] of Object.entries(mergedValues)) {
    if (value.constraints.length > 0) {
      constraintMetadata.set(name, value.constraints);
    }
  }
  const externalImports = new Set;
  for (const value of Object.values(values)) {
    if (value.isExternal && value.externalTarget) {
      externalImports.add(value.externalTarget.modulePath);
    }
  }
  const importAliases = [];
  for (const imp of imports) {
    const alias = imp.alias || imp.moduleName.split(".").pop();
    importAliases.push({
      alias,
      moduleName: imp.moduleName
    });
  }
  const importedModuleNames = new Set(imports.map((imp) => imp.moduleName));
  for (const semInst of semantics.instances) {
    const srcModule = semInst.moduleName;
    if (srcModule && srcModule !== currentModuleName && !importedModuleNames.has(srcModule) && !importAliases.some((a) => a.moduleName === srcModule)) {
      const alias = "$inst_" + srcModule.split(".").pop();
      importAliases.push({ alias, moduleName: srcModule });
    }
  }
  const resolvedImports = resolveImports(imports, dependencies, semantics);
  augmentImportsFromDefaults(syntheticValues, mergedValues, dependencies, semantics, resolvedImports);
  const packageName = options.packageName ?? semantics.module.name.split(".")[0];
  const defaultImports = [];
  for (const valueInfo of Object.values(semantics.values)) {
    const decl = valueInfo.declaration;
    if (decl.kind === "DecoratedDeclaration" && decl.decorator === "import") {
      defaultImports.push({
        name: decl.name,
        modulePath: decl.args[0]
      });
    }
  }
  return {
    moduleName: semantics.module.name,
    packageName,
    values: mergedValues,
    dependencyOrder: sccs,
    liftedBindings: ctx.liftedBindings,
    syntheticDefaultImpls,
    adts: semantics.adts,
    opaqueTypes: semantics.opaqueTypes,
    constructors,
    protocols,
    instances,
    constraintMetadata,
    externalImports,
    importAliases,
    resolvedImports,
    sourceModule: semantics,
    sourceProgram: program,
    exports: semantics.exports,
    defaultImports
  };
}
function resolveImports(imports, dependencies, semantics) {
  const resolved = [];
  for (const imp of imports) {
    const depModule = dependencies.get(imp.moduleName);
    const alias = imp.alias || imp.moduleName.split(".").pop() || imp.moduleName;
    if (imp.alias) {
      resolved.push({
        moduleName: imp.moduleName,
        namespaceImport: alias,
        namedImports: []
      });
    }
    if (imp.exposing?.kind === "All") {
      if (!imp.alias) {
        resolved.push({
          moduleName: imp.moduleName,
          namespaceImport: alias,
          namedImports: []
        });
      }
    } else if (imp.exposing?.kind === "Explicit") {
      const names = [];
      for (const spec of imp.exposing.exports) {
        switch (spec.kind) {
          case "ExportValue": {
            if (isTypeOnly(spec.name, depModule, semantics))
              break;
            names.push(spec.name);
            break;
          }
          case "ExportOperator":
            names.push(sanitizeOperator(spec.operator));
            break;
          case "ExportTypeAll": {
            const isProtocol = depModule?.protocols && Object.hasOwn(depModule.protocols, spec.name) || Object.hasOwn(semantics.protocols, spec.name);
            const isOpaque = depModule?.opaqueTypes && Object.hasOwn(depModule.opaqueTypes, spec.name) || Object.hasOwn(semantics.opaqueTypes, spec.name);
            if (isProtocol || isOpaque)
              break;
            const adtInfo = depModule?.adts[spec.name] ?? semantics.adts[spec.name];
            if (adtInfo && adtInfo.constructors.length > 0) {
              for (const ctorName of adtInfo.constructors) {
                names.push(ctorName);
              }
            }
            break;
          }
          case "ExportTypeSome": {
            if (spec.members) {
              for (const ctorName of spec.members) {
                names.push(ctorName);
              }
            }
            break;
          }
        }
      }
      if (names.length > 0) {
        const existing = imp.alias ? resolved.find((r) => r.moduleName === imp.moduleName) : undefined;
        if (existing) {
          existing.namedImports = names;
        } else {
          resolved.push({
            moduleName: imp.moduleName,
            namedImports: names
          });
        }
      } else if (!imp.alias) {}
    } else if (!imp.alias) {
      resolved.push({
        moduleName: imp.moduleName,
        namespaceImport: alias,
        namedImports: []
      });
    }
  }
  const importedModuleNames = new Set(imports.map((i) => i.moduleName));
  for (const semInst of semantics.instances) {
    const srcModule = semInst.moduleName;
    if (srcModule && srcModule !== semantics.module.name && !importedModuleNames.has(srcModule) && !resolved.some((r) => r.moduleName === srcModule)) {
      const instAlias = "$inst_" + srcModule.split(".").pop();
      resolved.push({
        moduleName: srcModule,
        namespaceImport: instAlias,
        namedImports: []
      });
    }
  }
  return resolved;
}
function collectBareVarNames(expr, out) {
  switch (expr.kind) {
    case "IRVar":
      if (expr.namespace === "value" && !expr.moduleName) {
        out.add(expr.name);
      }
      break;
    case "IRLambda":
      collectBareVarNames(expr.body, out);
      break;
    case "IRApply":
      collectBareVarNames(expr.callee, out);
      for (const arg of expr.args)
        collectBareVarNames(arg, out);
      break;
    case "IRIf":
      collectBareVarNames(expr.condition, out);
      collectBareVarNames(expr.thenBranch, out);
      collectBareVarNames(expr.elseBranch, out);
      break;
    case "IRCase":
      collectBareVarNames(expr.discriminant, out);
      for (const branch of expr.branches)
        collectBareVarNames(branch.body, out);
      break;
    case "IRList":
    case "IRTuple":
      for (const el of expr.elements)
        collectBareVarNames(el, out);
      break;
    case "IRRecord":
      for (const f of expr.fields)
        collectBareVarNames(f.value, out);
      break;
    case "IRRecordUpdate":
      collectBareVarNames(expr.base, out);
      for (const f of expr.updates)
        collectBareVarNames(f.value, out);
      break;
    case "IRFieldAccess":
      collectBareVarNames(expr.target, out);
      break;
    case "IRUnary":
      collectBareVarNames(expr.operand, out);
      break;
    case "IRConstructor":
      for (const arg of expr.args)
        collectBareVarNames(arg, out);
      break;
    case "IRSelfLoop":
      collectBareVarNames(expr.body, out);
      break;
    case "IRLoopContinue":
      for (const arg of expr.args)
        collectBareVarNames(arg, out);
      break;
  }
}
function augmentImportsFromDefaults(syntheticValues, localValues, dependencies, semantics, resolvedImports) {
  const bareNames = new Set;
  for (const [name, value] of Object.entries(syntheticValues)) {
    if (!name.startsWith("$default_"))
      continue;
    collectBareVarNames(value.body, bareNames);
  }
  const missingNames = new Set;
  for (const name of bareNames) {
    if (localValues[name])
      continue;
    if (syntheticValues[name])
      continue;
    missingNames.add(name);
  }
  if (missingNames.size === 0)
    return;
  const nameToModule = new Map;
  for (const [moduleName, dep] of dependencies) {
    for (const missingName of missingNames) {
      if (nameToModule.has(missingName))
        continue;
      if (dep.values[missingName]) {
        nameToModule.set(missingName, moduleName);
      }
    }
  }
  const moduleToNames = new Map;
  for (const [name, moduleName] of nameToModule) {
    if (!moduleToNames.has(moduleName))
      moduleToNames.set(moduleName, []);
    moduleToNames.get(moduleName).push(name);
  }
  for (const [moduleName, names] of moduleToNames) {
    const existing = resolvedImports.find((r) => r.moduleName === moduleName);
    if (existing) {
      for (const name of names) {
        if (!existing.namedImports.includes(name)) {
          existing.namedImports.push(name);
        }
      }
    } else {
      resolvedImports.push({
        moduleName,
        namedImports: names
      });
    }
  }
}
function isTypeOnly(name, depModule, semantics) {
  const isProtocol = depModule?.protocols && Object.hasOwn(depModule.protocols, name) || Object.hasOwn(semantics.protocols, name);
  const isOpaque = depModule?.opaqueTypes && Object.hasOwn(depModule.opaqueTypes, name) || Object.hasOwn(semantics.opaqueTypes, name);
  const isADT = depModule?.adts && Object.hasOwn(depModule.adts, name) || Object.hasOwn(semantics.adts, name);
  return isProtocol || isOpaque || isADT;
}

// src/index.ts
function getIRStats(ir) {
  return {
    valueCount: Object.keys(ir.values).length,
    sccCount: ir.dependencyOrder.length,
    mutualRecursionGroups: ir.dependencyOrder.filter((s) => s.isMutuallyRecursive).length,
    liftedBindingCount: ir.liftedBindings.length,
    externalImportCount: ir.externalImports.size,
    protocolCount: Object.keys(ir.protocols).length,
    instanceCount: ir.instances.length
  };
}
function dependsOn(ir, valueName, dependencyName) {
  const depGraph = buildDependencyGraph(ir.values, ir.instances, ir.protocols, ir.constructors);
  const deps = depGraph.get(valueName);
  return deps?.has(dependencyName) ?? false;
}
export {
  validateTopologicalOrder,
  typeStructureMatches,
  printValue,
  printType,
  printSCC,
  printProgram,
  printPattern,
  printExprCompact,
  printExpr,
  printDependencyOrder,
  printConstraint,
  lowerPattern,
  lowerExpr,
  lower,
  isTypeVariable,
  getIRStats,
  formatTypeKey,
  findSCCs,
  findPolymorphicInstance,
  findMatchingInstance,
  dependsOn,
  createLoweringContext,
  convertType,
  convertConstraints,
  buildTypeVarSubstitution,
  buildDependencyGraph,
  applyTypeSubstitution,
  IRError
};
