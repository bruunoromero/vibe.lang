// @bun
// src/index.ts
import fs from "fs";
import path from "path";

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
var UNIT_TYPE_NAME = "Unit";

// src/sanitize.ts
var RESERVED_WORDS = new Set([
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
  "yield"
]);
function sanitizeIdentifier(name) {
  if (name.startsWith("(") && name.endsWith(")")) {
    name = name.slice(1, -1);
  }
  if (/^[+\-*/<>=!&|^%:.~$#@?]+$/.test(name)) {
    return sanitizeOperator(name);
  }
  if (RESERVED_WORDS.has(name)) {
    return `$${name}`;
  }
  return name;
}

// src/imports.ts
function generateDefaultImports(program) {
  const lines = [];
  for (const { name, modulePath } of program.defaultImports) {
    lines.push(`import ${name} from "${modulePath}";`);
  }
  return lines;
}
function generateExternalImports(externalBindings) {
  const lines = [];
  for (const [modulePath, bindings] of externalBindings) {
    if (bindings.size > 0) {
      const importSpecifiers = [];
      const sortedEntries = Array.from(bindings.entries()).sort(([a], [b]) => a.localeCompare(b));
      for (const [vibeName, { runtimeName, callArity }] of sortedEntries) {
        const safeVibeName = sanitizeIdentifier(vibeName);
        if (callArity > 0) {
          importSpecifiers.push(`${runtimeName} as $$${safeVibeName}`);
        } else if (runtimeName === vibeName || runtimeName === safeVibeName) {
          importSpecifiers.push(runtimeName);
        } else {
          importSpecifiers.push(`${runtimeName} as ${safeVibeName}`);
        }
      }
      lines.push(`import { ${importSpecifiers.join(", ")} } from "${modulePath}";`);
    }
  }
  return lines;
}
function generateDependencyImports(program, modulePackages) {
  const lines = [];
  const currentPackage = program.packageName;
  const currentDepth = program.moduleName.split(".").length - 1;
  for (const resolved of program.resolvedImports) {
    const importedPackage = modulePackages.get(resolved.moduleName) || resolved.moduleName;
    const importPath = calculateImportPath(currentPackage, currentDepth, importedPackage, resolved.moduleName);
    if (resolved.namespaceImport) {
      lines.push(`import * as ${resolved.namespaceImport} from "${importPath}";`);
    }
    if (resolved.namedImports.length > 0) {
      const sanitized = resolved.namedImports.map((n) => sanitizeIdentifier(n));
      lines.push(`import { ${sanitized.join(", ")} } from "${importPath}";`);
    }
  }
  return lines;
}
function calculateImportPath(currentPackage, currentDepth, importedPackage, importedModule) {
  const moduleSegments = importedModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath = moduleSegments.length > 1 ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js` : `${fileName}.js`;
  if (currentPackage === importedPackage) {
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${importedPackage}/${modulePath}`;
  }
}
function calculateReExportPath(program, targetModule, modulePackages) {
  const currentModule = program.moduleName;
  const currentPackage = program.packageName;
  const currentDepth = currentModule.split(".").length - 1;
  const targetPackage = modulePackages.get(targetModule) || targetModule.split(".")[0];
  const moduleSegments = targetModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath = moduleSegments.length > 1 ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js` : `${fileName}.js`;
  if (currentPackage === targetPackage) {
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${targetPackage}/${modulePath}`;
  }
}

// src/instances.ts
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
function getImportAliasForModule(moduleName, importAliases) {
  for (const alias of importAliases) {
    if (alias.moduleName === moduleName) {
      return alias.alias;
    }
  }
  const segments = moduleName.split(".");
  return segments[0] || moduleName;
}
function resolveDictReference(protocolName, typeKey, ctx, concreteType, allConcreteTypes) {
  const key = `${protocolName}_${typeKey}`;
  let dictRef = null;
  let instanceKey = key;
  if (ctx.localInstanceKeys.has(key)) {
    dictRef = `$dict_${key}`;
  } else {
    const sourceModule = ctx.instanceModules.get(key);
    if (sourceModule) {
      const importAlias = getImportAliasForModule(sourceModule, ctx.importAliases);
      dictRef = `${importAlias}.$dict_${key}`;
    }
  }
  if (!dictRef && concreteType) {
    const structuralMatch = findMatchingInstance(protocolName, concreteType, ctx.instances);
    if (structuralMatch) {
      instanceKey = structuralMatch.key;
      if (ctx.localInstanceKeys.has(instanceKey)) {
        dictRef = `$dict_${instanceKey}`;
      } else {
        const sourceModule = ctx.instanceModules.get(instanceKey);
        if (sourceModule) {
          const importAlias = getImportAliasForModule(sourceModule, ctx.importAliases);
          dictRef = `${importAlias}.$dict_${instanceKey}`;
        }
      }
    }
  }
  if (!dictRef) {
    const polymorphicMatch = findPolymorphicInstance(protocolName, ctx.instances);
    if (polymorphicMatch) {
      instanceKey = polymorphicMatch.key;
      if (ctx.localInstanceKeys.has(instanceKey)) {
        dictRef = `$dict_${instanceKey}`;
      } else {
        const sourceModule = ctx.instanceModules.get(instanceKey);
        if (sourceModule) {
          const importAlias = getImportAliasForModule(sourceModule, ctx.importAliases);
          dictRef = `${importAlias}.$dict_${instanceKey}`;
        }
      }
    }
  }
  if (!dictRef) {
    const typeName = typeKey.startsWith("v") ? "a type variable" : `'${typeKey}'`;
    throw new Error(`No instance of '${protocolName}' found for ${typeName}. ` + `You may need to add: implement ${protocolName} ${typeKey} where ...`);
  }
  const constraints = ctx.constrainedInstances.get(instanceKey);
  const matchedInstance = ctx.instances.find((inst) => inst.protocolName === protocolName && `${protocolName}_${formatTypeKey(inst.typeArgs[0])}` === instanceKey);
  if (constraints && constraints.length > 0) {
    const constraintDicts = [];
    const seenProtocols = new Set;
    const typeVarSubst = allConcreteTypes && matchedInstance ? buildTypeVarSubstitution(matchedInstance.typeArgs, allConcreteTypes) : new Map;
    for (const constraint of constraints) {
      if (!seenProtocols.has(constraint.protocolName)) {
        seenProtocols.add(constraint.protocolName);
        const constraintTypeArg = constraint.typeArgs[0];
        let resolvedType;
        if (constraintTypeArg && constraintTypeArg.kind === "var") {
          resolvedType = typeVarSubst.get(constraintTypeArg.id);
        } else if (constraintTypeArg) {
          resolvedType = constraintTypeArg;
        }
        if (resolvedType && (resolvedType.kind === "con" || resolvedType.kind === "list" || resolvedType.kind === "tuple")) {
          constraintDicts.push(resolveDictionaryForType(constraint.protocolName, resolvedType, ctx));
        } else {
          constraintDicts.push(`$dict_${constraint.protocolName}`);
        }
      }
    }
    return `${dictRef}(${constraintDicts.join(", ")})`;
  }
  return dictRef;
}
function resolveDictionaryForType(protocolName, type, ctx) {
  if (!type) {
    return `$dict_${protocolName}`;
  }
  if (type.kind === "con") {
    const typeKey = formatTypeKey(type);
    return resolveDictReference(protocolName, typeKey, ctx, type);
  }
  if (type.kind === "list") {
    const listAsCon = {
      kind: "con",
      name: "List",
      args: [type.element]
    };
    const typeKey = formatTypeKey(listAsCon);
    return resolveDictReference(protocolName, typeKey, ctx, listAsCon);
  }
  if (type.kind === "tuple") {
    const typeKey = formatTypeKey(type);
    return resolveDictReference(protocolName, typeKey, ctx, type);
  }
  return `$dict_${protocolName}`;
}

// src/index.ts
function createCodegenContext(program) {
  const ctx = {
    program,
    indentLevel: 0,
    instances: program.instances,
    protocols: program.protocols,
    constructors: program.constructors,
    externalImports: new Set(program.externalImports),
    externalBindings: new Map,
    instanceDictNames: new Map,
    localInstanceKeys: new Set,
    instanceModules: new Map,
    uniqueCounter: 0,
    dictParamsInScope: new Set,
    concreteConstraints: new Map,
    generatedDictNames: [],
    protocolMethodMap: new Map,
    varTypes: new Map,
    constrainedInstances: new Map,
    expectedReturnType: undefined,
    usedShortCircuitOps: new Set,
    instanceMap: new Map,
    tcoLoopParams: null
  };
  for (const [protocolName, protocol] of Object.entries(program.protocols)) {
    for (const method of protocol.methods) {
      ctx.protocolMethodMap.set(method.name, protocolName);
    }
  }
  for (const [vibeName, value] of Object.entries(program.values)) {
    if (value.isExternal && value.externalTarget) {
      const { modulePath, exportName, callArity } = value.externalTarget;
      if (!ctx.externalBindings.has(modulePath)) {
        ctx.externalBindings.set(modulePath, new Map);
      }
      ctx.externalBindings.get(modulePath).set(vibeName, { runtimeName: exportName, callArity });
    }
  }
  const currentModule = program.moduleName;
  const sourceInstances = program.sourceModule.instances;
  for (let i = 0;i < program.instances.length; i++) {
    const inst = program.instances[i];
    if (!inst || inst.typeArgs.length === 0)
      continue;
    const typeKey = formatTypeKey(inst.typeArgs[0]);
    const key = `${inst.protocolName}_${typeKey}`;
    ctx.instanceDictNames.set(key, `$dict_${key}`);
    const sourceInst = sourceInstances[i];
    const instanceModule = sourceInst?.moduleName ? sourceInst.moduleName : currentModule;
    ctx.instanceModules.set(key, instanceModule);
    if (instanceModule === currentModule) {
      ctx.localInstanceKeys.add(key);
    }
    if (inst.constraints.length > 0) {
      ctx.constrainedInstances.set(key, inst.constraints);
    }
    ctx.instanceMap.set(`$dict_${key}`, inst);
  }
  return ctx;
}
function freshName(ctx, base) {
  const name = `$${base}_${ctx.uniqueCounter}`;
  ctx.uniqueCounter++;
  return name;
}
function toInstanceContext(ctx) {
  return {
    instances: ctx.instances,
    instanceDictNames: ctx.instanceDictNames,
    localInstanceKeys: ctx.localInstanceKeys,
    instanceModules: ctx.instanceModules,
    constrainedInstances: ctx.constrainedInstances,
    importAliases: ctx.program.importAliases,
    dictParamsInScope: ctx.dictParamsInScope,
    concreteConstraints: ctx.concreteConstraints,
    expectedReturnType: ctx.expectedReturnType
  };
}
function resolveDictReferenceCtx(protocolName, typeKey, ctx, concreteType, allConcreteTypes) {
  return resolveDictReference(protocolName, typeKey, toInstanceContext(ctx), concreteType, allConcreteTypes);
}
function resolveDictionaryForTypeCtx(protocolName, type, ctx) {
  return resolveDictionaryForType(protocolName, type, toInstanceContext(ctx));
}
function generate(program, options = {}) {
  const ctx = createCodegenContext(program);
  const { modulePackages = new Map } = options;
  const headerLines = [];
  const bodyLines = [];
  const defaultImportLines = generateDefaultImports(program);
  if (defaultImportLines.length > 0) {
    headerLines.push(...defaultImportLines);
    headerLines.push("");
  }
  const importLines = generateImports(ctx);
  if (importLines.length > 0) {
    headerLines.push(...importLines);
    headerLines.push("");
  }
  const depImportLines = generateDependencyImports(program, modulePackages);
  if (depImportLines.length > 0) {
    headerLines.push(...depImportLines);
    headerLines.push("");
  }
  const ctorLines = generateConstructors(ctx);
  if (ctorLines.length > 0) {
    bodyLines.push("// ADT Constructors");
    bodyLines.push(...ctorLines);
    bodyLines.push("");
  }
  bodyLines.push("// Values");
  for (const scc of program.dependencyOrder) {
    const sccLines = generateSCC(scc, ctx);
    bodyLines.push(...sccLines);
  }
  const exportLines = generateExports(program, ctx, modulePackages);
  if (exportLines.length > 0) {
    bodyLines.push("");
    bodyLines.push(...exportLines);
  }
  const helperLines = [];
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
    code: [...headerLines, ...helperLines, ...bodyLines].join(`
`),
    imports: ctx.externalBindings,
    exports: Object.keys(program.values).filter((name) => !name.startsWith("$"))
  };
}
function generateImports(ctx) {
  return generateExternalImports(ctx.externalBindings);
}
function generateConstructors(ctx) {
  const lines = [];
  const generated = new Set;
  const currentModule = ctx.program.moduleName;
  const byParent = new Map;
  for (const ctor of Object.values(ctx.constructors)) {
    if (!byParent.has(ctor.parentType)) {
      byParent.set(ctor.parentType, []);
    }
    byParent.get(ctor.parentType).push(ctor);
  }
  for (const [parentType, ctors] of byParent) {
    if (parentType === BOOL_TYPE_NAME)
      continue;
    if (parentType === UNIT_TYPE_NAME)
      continue;
    const adtInfo = ctx.program.adts[parentType];
    if (adtInfo?.moduleName && adtInfo.moduleName !== currentModule && adtInfo.moduleName !== BUILTIN_MODULE_NAME) {
      continue;
    }
    ctors.sort((a, b) => a.tag - b.tag);
    for (const ctor of ctors) {
      if (generated.has(ctor.name))
        continue;
      generated.add(ctor.name);
      const safeName = sanitizeIdentifier(ctor.name);
      if (ctor.arity === 0) {
        lines.push(`const ${safeName} = { $tag: ${ctor.tag} };`);
      } else {
        const params = Array.from({ length: ctor.arity }, (_, i) => `$${i}`);
        const fields = params.map((p) => p).join(", ");
        let body = `({ $tag: ${ctor.tag}, ${fields} })`;
        for (let i = params.length - 1;i >= 0; i--) {
          body = `(${params[i]}) => ${body}`;
        }
        lines.push(`const ${safeName} = ${body};`);
      }
    }
  }
  return lines;
}
function generateInstanceDictionary(inst, ctx) {
  const lines = [];
  const currentModule = ctx.program.moduleName;
  const typeKey = formatTypeKey(inst.typeArgs[0]);
  const key = `${inst.protocolName}_${typeKey}`;
  const dictName = ctx.instanceDictNames.get(key);
  if (!dictName)
    return [];
  const instanceModule = ctx.instanceModules.get(key);
  if (instanceModule && instanceModule !== currentModule) {
    return [];
  }
  const constraints = ctx.constrainedInstances.get(key);
  const methodEntries = [];
  for (const [methodName, implName] of Object.entries(inst.methods)) {
    const safeName = sanitizeIdentifier(methodName);
    const safeImpl = sanitizeIdentifier(implName);
    if (constraints && constraints.length > 0) {
      const dictParams = [];
      const seenProtocols = new Set;
      for (const constraint of constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      }
      const dictPasses = dictParams.map((d) => `(${d})`).join("");
      methodEntries.push(`  ${safeName}: ${safeImpl}${dictPasses}`);
    } else {
      methodEntries.push(`  ${safeName}: ${safeImpl}`);
    }
  }
  if (methodEntries.length > 0) {
    if (constraints && constraints.length > 0) {
      const dictParams = [];
      const seenProtocols = new Set;
      for (const constraint of constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      }
      lines.push(`const ${dictName} = (${dictParams.join(", ")}) => ({`);
      lines.push(methodEntries.join(`,
`));
      lines.push(`});`);
    } else {
      lines.push(`const ${dictName} = {`);
      lines.push(methodEntries.join(`,
`));
      lines.push(`};`);
    }
    ctx.generatedDictNames.push(dictName);
  }
  return lines;
}
function generateSCC(scc, ctx) {
  const lines = [];
  if (scc.isMutuallyRecursive) {
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
        const inst = ctx.instanceMap.get(name);
        const dictLines = generateInstanceDictionary(inst, ctx);
        for (const line of dictLines) {
          lines.push(line.startsWith("const ") ? line.slice(6) : line);
        }
      }
    }
  } else {
    for (const name of scc.values) {
      if (ctx.program.values[name]) {
        const value = ctx.program.values[name];
        if (value.isExternal && (!value.externalTarget || value.externalTarget.callArity === 0))
          continue;
        const safeName = sanitizeIdentifier(name);
        if (value.isExternal && value.externalTarget && value.externalTarget.callArity > 0) {
          lines.push(`const ${safeName} = ${generateExternalWrapper(value)};`);
          continue;
        }
        if (value.propertyAccess) {
          lines.push(`const ${safeName} = ${generatePropertyAccess(value)};`);
          continue;
        }
        const body = generateValue(value, ctx);
        lines.push(`const ${safeName} = ${body};`);
      } else if (ctx.instanceMap.has(name)) {
        const inst = ctx.instanceMap.get(name);
        lines.push(...generateInstanceDictionary(inst, ctx));
      }
    }
  }
  return lines;
}
function generatePropertyAccess(value) {
  const { variant, key, callArity } = value.propertyAccess;
  if (variant === "val") {
    return key;
  }
  if (variant === "get") {
    return `($recv) => $recv.${key}`;
  }
  const argNames = Array.from({ length: callArity }, (_, i) => `$a${i}`);
  let result = `$recv.${key}(${argNames.join(", ")})`;
  for (let i = argNames.length - 1;i >= 0; i--) {
    result = `(${argNames[i]}) => ${result}`;
  }
  result = `($recv) => ${result}`;
  return result;
}
function generateExternalWrapper(value) {
  const { callArity } = value.externalTarget;
  const safeName = sanitizeIdentifier(value.name);
  const privateName = `$$${safeName}`;
  const argNames = Array.from({ length: callArity }, (_, i) => `$a${i}`);
  let result = `${privateName}(${argNames.join(", ")})`;
  for (let i = argNames.length - 1;i >= 0; i--) {
    result = `(${argNames[i]}) => ${result}`;
  }
  return result;
}
function generateValue(value, ctx) {
  if (value.isExternal) {
    return `/* external: ${value.externalTarget?.exportName} */`;
  }
  const dictParams = [];
  const seenConstraints = new Set;
  const concreteConstraints = new Map;
  for (const constraint of value.constraints) {
    const typeArg = constraint.typeArgs[0];
    if (typeArg) {
      if (typeArg.kind === "var") {
        const dictKey = `${constraint.protocolName}`;
        if (!seenConstraints.has(dictKey)) {
          seenConstraints.add(dictKey);
          dictParams.push(`$dict_${constraint.protocolName}`);
        }
      } else if (typeArg.kind === "con") {
        concreteConstraints.set(constraint.protocolName, typeArg);
      }
    }
  }
  const varTypes = new Map;
  let returnType = value.type;
  if (value.params.length > 0 && value.type.kind === "fun") {
    let currentType = value.type;
    for (let i = 0;i < value.params.length; i++) {
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
  if (value.body.kind === "IRLambda" && returnType.kind === "fun") {
    let currentType = returnType;
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
  const prevDictParams = ctx.dictParamsInScope;
  const prevConcreteConstraints = ctx.concreteConstraints;
  const prevVarTypes = ctx.varTypes;
  const prevExpectedReturnType = ctx.expectedReturnType;
  ctx.dictParamsInScope = new Set(dictParams);
  ctx.concreteConstraints = concreteConstraints;
  ctx.varTypes = varTypes;
  ctx.expectedReturnType = returnType;
  if (value.params.length > 0 || dictParams.length > 0) {
    let body2 = generateExpr(value.body, ctx);
    for (let i = value.params.length - 1;i >= 0; i--) {
      const param = value.params[i];
      if (!param)
        continue;
      const paramCode = generatePattern(param, ctx);
      body2 = `(${paramCode}) => ${body2}`;
    }
    for (let i = dictParams.length - 1;i >= 0; i--) {
      body2 = `(${dictParams[i]}) => ${body2}`;
    }
    ctx.dictParamsInScope = prevDictParams;
    ctx.concreteConstraints = prevConcreteConstraints;
    ctx.varTypes = prevVarTypes;
    ctx.expectedReturnType = prevExpectedReturnType;
    return body2;
  }
  const body = generateExpr(value.body, ctx);
  ctx.dictParamsInScope = prevDictParams;
  ctx.concreteConstraints = prevConcreteConstraints;
  ctx.varTypes = prevVarTypes;
  ctx.expectedReturnType = prevExpectedReturnType;
  return body;
}
function generateExpr(expr, ctx) {
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
      return `-${generateExpr(expr.operand, ctx)}`;
    case "IRSelfLoop":
      return generateSelfLoop(expr, ctx);
    case "IRLoopContinue":
      throw new Error("IRLoopContinue found outside IRSelfLoop \u2014 compiler bug");
    default:
      const _exhaustive = expr;
      throw new Error(`Unknown expression kind: ${expr.kind}`);
  }
}
function isFullyConcrete(type) {
  switch (type.kind) {
    case "var":
      return false;
    case "con":
      return type.args.every(isFullyConcrete);
    case "list":
      return isFullyConcrete(type.element);
    case "tuple":
      return type.elements.every(isFullyConcrete);
    case "fun":
      return isFullyConcrete(type.from) && isFullyConcrete(type.to);
    case "record":
      return Object.values(type.fields).every(isFullyConcrete);
    default:
      return true;
  }
}
function generateVar(expr, ctx) {
  const currentModule = ctx.program.moduleName;
  if (SHORT_CIRCUIT_OPERATORS.has(expr.name)) {
    const helper = SHORT_CIRCUIT_HELPERS[expr.name];
    if (helper) {
      ctx.usedShortCircuitOps.add(expr.name);
      return helper.name;
    }
  }
  const value = ctx.program.values[expr.name];
  if (value?.isExternal && value.externalTarget) {
    return sanitizeIdentifier(expr.name);
  }
  const protocolName = ctx.protocolMethodMap.get(expr.name);
  if (protocolName) {
    const sanitizedName = sanitizeIdentifier(expr.name);
    const dictParam = `$dict_${protocolName}`;
    if (expr.constraint) {
      const typeArg = expr.constraint.typeArgs[0];
      if (typeArg && (typeArg.kind === "con" || typeArg.kind === "tuple" || typeArg.kind === "list") && isFullyConcrete(typeArg)) {
        const typeKey = formatTypeKey(typeArg);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, typeArg, expr.constraint.typeArgs);
        return `${dictRef}.${sanitizedName}`;
      }
    }
    if (ctx.dictParamsInScope.has(dictParam)) {
      return `${dictParam}.${sanitizedName}`;
    }
    if (expr.constraint) {
      const typeArg = expr.constraint.typeArgs[0];
      if (typeArg && (typeArg.kind === "con" || typeArg.kind === "tuple" || typeArg.kind === "list")) {
        const typeKey = formatTypeKey(typeArg);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, typeArg, expr.constraint.typeArgs);
        return `${dictRef}.${sanitizedName}`;
      }
    }
    if (expr.type && expr.type.kind === "fun") {
      const argType = expr.type.from;
      if (argType.kind === "con") {
        const typeKey = formatTypeKey(argType);
        const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx);
        return `${dictRef}.${sanitizedName}`;
      }
    }
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType && constraintType.kind === "con") {
      const typeKey = formatTypeKey(constraintType);
      const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx);
      return `${dictRef}.${sanitizedName}`;
    }
    if (ctx.expectedReturnType && !isTypeVariable(ctx.expectedReturnType)) {
      const typeKey = formatTypeKey(ctx.expectedReturnType);
      const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, ctx.expectedReturnType);
      return `${dictRef}.${sanitizedName}`;
    }
    throw new Error(`Cannot resolve protocol method '${expr.name}' in monomorphic context without type info.`);
  }
  if (expr.moduleName && expr.moduleName !== currentModule && expr.moduleName !== BUILTIN_MODULE_NAME) {
    const importAlias = getImportAliasForModule(expr.moduleName, ctx.program.importAliases);
    return `${importAlias}.${sanitizeIdentifier(expr.name)}`;
  }
  const safeName = sanitizeIdentifier(expr.name);
  return safeName;
}
function generateModuleAccess(expr, ctx) {
  const valueName = sanitizeIdentifier(expr.valueName);
  let result = `${expr.importAlias}.${valueName}`;
  if (expr.constraints && expr.constraints.length > 0) {
    const seenProtocols = new Set;
    for (const constraint of expr.constraints) {
      if (!seenProtocols.has(constraint.protocolName)) {
        seenProtocols.add(constraint.protocolName);
        const typeArg = constraint.typeArgs[0];
        const dictName = resolveDictionaryForTypeCtx(constraint.protocolName, typeArg, ctx);
        result = `${result}(${dictName})`;
      }
    }
  }
  return result;
}
function interpretEscapes(raw) {
  let result = "";
  for (let i = 0;i < raw.length; i++) {
    if (raw[i] === "\\" && i + 1 < raw.length) {
      const next = raw[i + 1];
      switch (next) {
        case "n":
          result += `
`;
          i++;
          break;
        case "t":
          result += "\t";
          i++;
          break;
        case "r":
          result += "\r";
          i++;
          break;
        case "\\":
          result += "\\";
          i++;
          break;
        case "'":
          result += "'";
          i++;
          break;
        case '"':
          result += '"';
          i++;
          break;
        default:
          result += raw[i];
      }
    } else {
      result += raw[i];
    }
  }
  return result;
}
function generateLiteral(expr) {
  switch (expr.literalType) {
    case "int":
    case "float":
      return String(expr.value);
    case "string": {
      let strVal = String(expr.value);
      if (strVal.startsWith('"') && strVal.endsWith('"')) {
        strVal = strVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(strVal));
    }
    case "char": {
      let charVal = String(expr.value);
      if (charVal.startsWith("'") && charVal.endsWith("'")) {
        charVal = charVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(charVal));
    }
    case "bool":
      return expr.value ? "true" : "false";
  }
}
function generateLambda(expr, ctx) {
  const body = generateExpr(expr.body, ctx);
  if (expr.params.length === 0) {
    return `() => ${body}`;
  }
  let result = body;
  for (let i = expr.params.length - 1;i >= 0; i--) {
    const param = expr.params[i];
    if (!param)
      continue;
    const paramCode = generatePattern(param, ctx);
    result = `(${paramCode}) => ${result}`;
  }
  return result;
}
function generateApply(expr, ctx) {
  const protocolMethodResult = tryGenerateProtocolMethodApply(expr, ctx);
  if (protocolMethodResult !== null) {
    return protocolMethodResult;
  }
  let callee = generateExpr(expr.callee, ctx);
  if (expr.callee.kind === "IRLambda") {
    callee = `(${callee})`;
  }
  let dictPasses = [];
  if (expr.callee.kind === "IRVar") {
    const calleeValue = ctx.program.values[expr.callee.name];
    if (calleeValue && calleeValue.constraints && calleeValue.constraints.length > 0) {
      const seenProtocols = new Set;
      for (const constraint of calleeValue.constraints) {
        if (!seenProtocols.has(constraint.protocolName)) {
          seenProtocols.add(constraint.protocolName);
          const typeArg = constraint.typeArgs[0];
          const dictName = resolveDictionaryForTypeCtx(constraint.protocolName, typeArg, ctx);
          dictPasses.push(dictName);
        }
      }
    }
  }
  let result = callee;
  for (const dict of dictPasses) {
    result = `${result}(${dict})`;
  }
  for (const arg of expr.args) {
    const argCode = generateExpr(arg, ctx);
    result = `${result}(${argCode})`;
  }
  return result;
}
function tryGenerateProtocolMethodApply(expr, ctx) {
  const { methodName, methodVar, operands } = extractMethodAndOperands(expr);
  if (!methodName) {
    return null;
  }
  const protocolName = ctx.protocolMethodMap.get(methodName);
  if (!protocolName) {
    return null;
  }
  if (methodVar?.constraint) {
    const typeArg = methodVar.constraint.typeArgs[0];
    if (typeArg && typeArg.kind === "con" && isFullyConcrete(typeArg)) {
      const sanitizedName2 = sanitizeIdentifier(methodName);
      const typeKey2 = formatTypeKey(typeArg);
      const dictRef2 = resolveDictReferenceCtx(protocolName, typeKey2, ctx, typeArg, methodVar.constraint.typeArgs);
      let result2 = `${dictRef2}.${sanitizedName2}`;
      for (const operand of operands) {
        const argCode = generateExpr(operand, ctx);
        result2 = `${result2}(${argCode})`;
      }
      return result2;
    }
  }
  const dictParam = `$dict_${protocolName}`;
  if (ctx.dictParamsInScope.has(dictParam)) {
    return null;
  }
  let concreteType = inferConcreteTypeFromOperands(operands, ctx);
  let allOperandTypes = inferAllTypesFromOperands(operands, ctx);
  if (ctx.expectedReturnType && !isTypeVariable(ctx.expectedReturnType)) {
    allOperandTypes = [...allOperandTypes, ctx.expectedReturnType];
  }
  if (!concreteType) {
    const constraintType = ctx.concreteConstraints.get(protocolName);
    if (constraintType) {
      concreteType = constraintType;
    }
  }
  if (!concreteType) {
    return null;
  }
  const sanitizedName = sanitizeIdentifier(methodName);
  const typeKey = formatTypeKey(concreteType);
  const dictRef = resolveDictReferenceCtx(protocolName, typeKey, ctx, concreteType, allOperandTypes.length > 0 ? allOperandTypes : undefined);
  let result = `${dictRef}.${sanitizedName}`;
  for (const operand of operands) {
    const argCode = generateExpr(operand, ctx);
    result = `${result}(${argCode})`;
  }
  return result;
}
function extractMethodAndOperands(expr) {
  const operands = [];
  let current = expr;
  while (current.kind === "IRApply") {
    operands.unshift(...current.args);
    current = current.callee;
  }
  if (current.kind === "IRVar" && current.namespace === "value") {
    return { methodName: current.name, methodVar: current, operands };
  }
  return { methodName: null, methodVar: null, operands: [] };
}
function inferConcreteTypeFromOperands(operands, ctx) {
  for (const operand of operands) {
    const type = inferExprType(operand, ctx);
    if (type && type.kind === "con") {
      return type;
    }
  }
  return null;
}
function inferAllTypesFromOperands(operands, ctx) {
  const types = [];
  for (const operand of operands) {
    const type = inferExprType(operand, ctx);
    if (type) {
      types.push(type);
    }
  }
  return types;
}
function inferExprType(expr, ctx) {
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
      const ctorInfo = ctx.constructors[expr.name];
      if (ctorInfo) {
        return { kind: "con", name: ctorInfo.parentType, args: [] };
      }
      break;
    }
    case "IRVar":
      if (expr.type) {
        return expr.type;
      }
      const varType = ctx.varTypes.get(expr.name);
      if (varType) {
        return varType;
      }
      break;
    case "IRFieldAccess":
      const targetType = inferExprType(expr.target, ctx);
      if (targetType && targetType.kind === "record") {
        const fieldType = targetType.fields[expr.field];
        if (fieldType) {
          return fieldType;
        }
      }
      break;
    case "IRApply": {
      const { methodName, operands } = extractMethodAndOperands(expr);
      if (methodName) {
        const protocolName = ctx.protocolMethodMap.get(methodName);
        if (protocolName) {
          const protocol = ctx.protocols[protocolName];
          if (protocol) {
            const method = protocol.methods.find((m) => m.name === methodName);
            if (method && method.type.kind === "fun") {
              let resultType = method.type;
              for (let i = 0;i < operands.length && resultType.kind === "fun"; i++) {
                resultType = resultType.to;
              }
              if (resultType.kind === "con") {
                return resultType;
              }
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
      for (const operand of operands) {
        const opType = inferExprType(operand, ctx);
        if (opType && opType.kind === "con") {
          return opType;
        }
      }
      break;
    }
    case "IRUnary":
      return inferExprType(expr.operand, ctx);
    case "IRList": {
      if (expr.elements.length > 0) {
        const elemType = inferExprType(expr.elements[0], ctx);
        if (elemType) {
          return { kind: "con", name: "List", args: [elemType] };
        }
      }
      return { kind: "con", name: "List", args: [{ kind: "var", id: -1 }] };
    }
    case "IRTuple": {
      const elemTypes = [];
      for (const elem of expr.elements) {
        const elemType = inferExprType(elem, ctx);
        if (elemType) {
          elemTypes.push(elemType);
        } else {
          elemTypes.push({ kind: "var", id: -1 });
        }
      }
      return { kind: "tuple", elements: elemTypes };
    }
  }
  return null;
}
function generateSelfLoop(expr, ctx) {
  const prevParams = ctx.tcoLoopParams;
  ctx.tcoLoopParams = expr.paramNames.map((n) => sanitizeIdentifier(n));
  const stmts = generateExprAsStmt(expr.body, ctx);
  ctx.tcoLoopParams = prevParams;
  return `{ while (true) { ${stmts} } }`;
}
function generateExprAsStmt(expr, ctx) {
  switch (expr.kind) {
    case "IRLoopContinue": {
      const paramNames = ctx.tcoLoopParams;
      const argExprs = expr.args.map((a) => generateExpr(a, ctx));
      if (paramNames.length === 1) {
        return `${paramNames[0]} = ${argExprs[0]}; continue;`;
      }
      return `[${paramNames.join(", ")}] = [${argExprs.join(", ")}]; continue;`;
    }
    case "IRIf": {
      const cond = generateExpr(expr.condition, ctx);
      const thenStmt = generateExprAsStmt(expr.thenBranch, ctx);
      const elseStmt = generateExprAsStmt(expr.elseBranch, ctx);
      return `if (${cond}) { ${thenStmt} } else { ${elseStmt} }`;
    }
    case "IRCase": {
      const matchName = freshName(ctx, "match");
      const discriminant = generateExpr(expr.discriminant, ctx);
      const branches = [];
      for (const branch of expr.branches) {
        const { condition, bindings } = generatePatternMatchCode(branch.pattern, matchName, ctx);
        const bodyStmt = generateExprAsStmt(branch.body, ctx);
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
        code += `${bodyStmt} }`;
        branches.push(code);
      }
      branches.push(`throw new Error("Pattern match failed");`);
      return `{ const ${matchName} = ${discriminant}; ${branches.join(" ")} }`;
    }
    case "IRSelfLoop":
      return generateSelfLoop(expr, ctx);
    case "IRApply": {
      if (expr.callee.kind === "IRLambda" && expr.callee.params.length === 1 && expr.args.length === 1) {
        const paramCode = generatePattern(expr.callee.params[0], ctx);
        const valueCode = generateExpr(expr.args[0], ctx);
        const bodyStmt = generateExprAsStmt(expr.callee.body, ctx);
        return `{ const ${paramCode} = ${valueCode}; ${bodyStmt} }`;
      }
      return `return ${generateExpr(expr, ctx)};`;
    }
    default:
      return `return ${generateExpr(expr, ctx)};`;
  }
}
function generateIf(expr, ctx) {
  const cond = generateExpr(expr.condition, ctx);
  const then_ = generateExpr(expr.thenBranch, ctx);
  const else_ = generateExpr(expr.elseBranch, ctx);
  return `(${cond} ? ${then_} : ${else_})`;
}
function generateCase(expr, ctx) {
  const matchName = freshName(ctx, "match");
  const discriminant = generateExpr(expr.discriminant, ctx);
  const branches = [];
  for (const branch of expr.branches) {
    const { condition, bindings, body } = generateBranchCode(branch.pattern, matchName, branch.body, ctx);
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
  branches.push(`throw new Error("Pattern match failed");`);
  return `((${matchName}) => { ${branches.join(" ")} })(${discriminant})`;
}
function generateBranchCode(pattern, matchName, body, ctx) {
  const bindings = [];
  function buildConditionAndBindings(pat, accessor) {
    switch (pat.kind) {
      case "IRVarPattern":
        const safeName = sanitizeIdentifier(pat.name);
        bindings.push(`const ${safeName} = ${accessor};`);
        return null;
      case "IRWildcardPattern":
        return null;
      case "IRConstructorPattern": {
        const ctor = ctx.constructors[pat.name];
        if (ctor?.parentType === BOOL_TYPE_NAME) {
          const boolValue = pat.name === "True" ? "true" : "false";
          return `${accessor} === ${boolValue}`;
        }
        const tag = pat.tag;
        const conditions = [`${accessor}.$tag === ${tag}`];
        for (let i = 0;i < pat.args.length; i++) {
          const argPat = pat.args[i];
          if (!argPat)
            continue;
          const argAccessor = `${accessor}.$${i}`;
          const argCondition = buildConditionAndBindings(argPat, argAccessor);
          if (argCondition) {
            conditions.push(argCondition);
          }
        }
        return conditions.join(" && ");
      }
      case "IRTuplePattern": {
        const conditions = [];
        for (let i = 0;i < pat.elements.length; i++) {
          const elemPat = pat.elements[i];
          if (!elemPat)
            continue;
          const elemAccessor = `${accessor}[${i}]`;
          const elemCondition = buildConditionAndBindings(elemPat, elemAccessor);
          if (elemCondition) {
            conditions.push(elemCondition);
          }
        }
        return conditions.length > 0 ? conditions.join(" && ") : null;
      }
      case "IRLiteralPattern":
        const lit = generateLiteralPatternValue(pat);
        return `${accessor} === ${lit}`;
      case "IRListPattern": {
        const conditions = [
          `Array.isArray(${accessor})`,
          `${accessor}.length === ${pat.elements.length}`
        ];
        for (let i = 0;i < pat.elements.length; i++) {
          const elemPat = pat.elements[i];
          if (!elemPat)
            continue;
          const elemAccessor = `${accessor}[${i}]`;
          const elemCondition = buildConditionAndBindings(elemPat, elemAccessor);
          if (elemCondition) {
            conditions.push(elemCondition);
          }
        }
        return conditions.join(" && ");
      }
      case "IRConsPattern": {
        const conditions = [
          `Array.isArray(${accessor})`,
          `${accessor}.length >= 1`
        ];
        const headCondition = buildConditionAndBindings(pat.head, `${accessor}[0]`);
        if (headCondition) {
          conditions.push(headCondition);
        }
        const tailCondition = buildConditionAndBindings(pat.tail, `${accessor}.slice(1)`);
        if (tailCondition) {
          conditions.push(tailCondition);
        }
        return conditions.join(" && ");
      }
      case "IRRecordPattern": {
        const conditions = [];
        for (const field of pat.fields) {
          const fieldAccessor = `${accessor}.${field.name}`;
          if (field.pattern) {
            const fieldCondition = buildConditionAndBindings(field.pattern, fieldAccessor);
            if (fieldCondition) {
              conditions.push(fieldCondition);
            }
          } else {
            const safeName2 = sanitizeIdentifier(field.name);
            bindings.push(`const ${safeName2} = ${fieldAccessor};`);
          }
        }
        return conditions.length > 0 ? conditions.join(" && ") : null;
      }
      default:
        const _exhaustive = pat;
        throw new Error(`Unknown pattern kind: ${pat.kind}`);
    }
  }
  const condition = buildConditionAndBindings(pattern, matchName);
  const bodyCode = generateExpr(body, ctx);
  return { condition, bindings, body: bodyCode };
}
function generatePatternMatchCode(pattern, matchName, ctx) {
  const dummySpan = {
    start: { offset: 0, line: 0, column: 0 },
    end: { offset: 0, line: 0, column: 0 }
  };
  const result = generateBranchCode(pattern, matchName, { kind: "IRUnit", span: dummySpan }, ctx);
  return { condition: result.condition, bindings: result.bindings };
}
function generateLiteralPatternValue(pat) {
  switch (pat.literalType) {
    case "int":
    case "float":
      return String(pat.value);
    case "string": {
      let strVal = String(pat.value);
      if (strVal.startsWith('"') && strVal.endsWith('"')) {
        strVal = strVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(strVal));
    }
    case "char": {
      let charVal = String(pat.value);
      if (charVal.startsWith("'") && charVal.endsWith("'")) {
        charVal = charVal.slice(1, -1);
      }
      return JSON.stringify(interpretEscapes(charVal));
    }
    case "bool":
      return pat.value ? "true" : "false";
  }
}
function generateTuple(expr, ctx) {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}
function generateList(expr, ctx) {
  const elements = expr.elements.map((e) => generateExpr(e, ctx));
  return `[${elements.join(", ")}]`;
}
function generateRecord(expr, ctx) {
  const fields = expr.fields.map((f) => {
    const value = generateExpr(f.value, ctx);
    return `${f.name}: ${value}`;
  });
  return `({ ${fields.join(", ")} })`;
}
function generateRecordUpdate(expr, ctx) {
  const base = generateExpr(expr.base, ctx);
  const updates = expr.updates.map((f) => {
    const value = generateExpr(f.value, ctx);
    return `${f.name}: ${value}`;
  });
  return `({ ...${base}, ${updates.join(", ")} })`;
}
function generateFieldAccess(expr, ctx) {
  const target = generateExpr(expr.target, ctx);
  return `${target}.${expr.field}`;
}
function generateConstructorExpr(expr, ctx) {
  const ctor = ctx.constructors[expr.name];
  const currentModule = ctx.program.moduleName;
  if (ctor?.parentType === BOOL_TYPE_NAME) {
    return expr.name === "True" ? "true" : "false";
  }
  if (expr.moduleName && expr.moduleName !== currentModule && expr.moduleName !== BUILTIN_MODULE_NAME) {
    const importAlias = getImportAliasForModule(expr.moduleName, ctx.program.importAliases);
    const ctorName = sanitizeIdentifier(expr.name);
    if (expr.args.length === 0) {
      return `${importAlias}.${ctorName}`;
    }
    let result = `${importAlias}.${ctorName}`;
    for (const arg of expr.args) {
      const argCode = generateExpr(arg, ctx);
      result = `${result}(${argCode})`;
    }
    return result;
  }
  if (expr.args.length === 0) {
    return `{ $tag: ${expr.tag} }`;
  }
  const fields = expr.args.map((arg, i) => {
    const argCode = generateExpr(arg, ctx);
    return `$${i}: ${argCode}`;
  });
  return `{ $tag: ${expr.tag}, ${fields.join(", ")} }`;
}
function generatePattern(pattern, ctx) {
  switch (pattern.kind) {
    case "IRVarPattern":
      return sanitizeIdentifier(pattern.name);
    case "IRWildcardPattern":
      return "_";
    case "IRConstructorPattern":
      if (pattern.args.length === 0) {
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
      return freshName(ctx, "lit");
    case "IRListPattern":
      const listElements = pattern.elements.map((e) => generatePattern(e, ctx));
      return `[${listElements.join(", ")}]`;
    case "IRConsPattern":
      const head = generatePattern(pattern.head, ctx);
      const tail = generatePattern(pattern.tail, ctx);
      return `[${head}, ...${tail}]`;
    case "IRRecordPattern":
      const recordBindings = pattern.fields.map((f) => {
        if (f.pattern) {
          const fieldPattern = generatePattern(f.pattern, ctx);
          return `${f.name}: ${fieldPattern}`;
        } else {
          return f.name;
        }
      });
      return `{ ${recordBindings.join(", ")} }`;
    default:
      const _exhaustive = pattern;
      throw new Error(`Unknown pattern kind: ${pattern.kind}`);
  }
}
function generateExports(program, ctx, modulePackages) {
  const lines = [];
  const currentModule = program.moduleName;
  const moduleExports = program.exports;
  const localExports = [];
  const reExports = new Map;
  function addReExport(moduleName, name) {
    if (!reExports.has(moduleName)) {
      reExports.set(moduleName, new Set);
    }
    reExports.get(moduleName).add(sanitizeIdentifier(name));
  }
  function addConstructorExport(ctorName) {
    const ctor = program.constructors[ctorName];
    if (!ctor)
      return;
    if (ctor.moduleName && ctor.moduleName !== currentModule && ctor.moduleName !== BUILTIN_MODULE_NAME) {
      addReExport(ctor.moduleName, ctorName);
    } else {
      localExports.push(sanitizeIdentifier(ctorName));
    }
  }
  function addValueExport(name, value, checkLocal = true) {
    if (!value && checkLocal)
      return;
    if (value) {
      localExports.push(sanitizeIdentifier(name));
    }
  }
  if (moduleExports.exportsAll) {
    for (const [name, value] of Object.entries(program.values)) {
      if (name.startsWith("$"))
        continue;
      addValueExport(name, value);
    }
    for (const [ctorName, ctor] of Object.entries(program.constructors)) {
      if (ctor.parentType === BOOL_TYPE_NAME || ctor.parentType === UNIT_TYPE_NAME)
        continue;
      const adtInfo = program.adts[ctor.parentType];
      if (adtInfo?.moduleName && adtInfo.moduleName !== currentModule && adtInfo.moduleName !== BUILTIN_MODULE_NAME) {
        continue;
      }
      localExports.push(sanitizeIdentifier(ctorName));
    }
    for (const dictName of ctx.generatedDictNames) {
      localExports.push(dictName);
    }
  } else {
    for (const valueName of moduleExports.values) {
      const value = program.values[valueName];
      if (value) {
        addValueExport(valueName, value);
      }
    }
    for (const opName of moduleExports.operators) {
      const value = program.values[opName];
      if (value) {
        addValueExport(opName, value);
      }
    }
    for (const [typeName, typeExport] of moduleExports.types) {
      if (typeExport.allConstructors) {
        for (const [ctorName, ctor] of Object.entries(program.constructors)) {
          if (ctor.parentType === typeName) {
            addConstructorExport(ctorName);
          }
        }
      } else if (typeExport.constructors) {
        for (const ctorName of typeExport.constructors) {
          if (program.constructors[ctorName]) {
            addConstructorExport(ctorName);
          }
        }
      }
    }
    for (const dictName of ctx.generatedDictNames) {
      localExports.push(dictName);
    }
    for (const [protocolName, protocolExport] of moduleExports.protocols) {
      for (const [instanceKey, sourceModule] of ctx.instanceModules) {
        if (instanceKey.startsWith(`${protocolName}_`) && sourceModule !== currentModule) {
          const dictName = ctx.instanceDictNames.get(instanceKey);
          if (dictName) {
            addReExport(sourceModule, dictName);
          }
        }
      }
    }
    for (const [name, sourceModule] of moduleExports.reExportedValues) {
      addReExport(sourceModule, name);
    }
  }
  for (const [moduleName, names] of reExports) {
    const sortedNames = [...names].sort();
    if (sortedNames.length > 0) {
      const importPath = calculateReExportPath(program, moduleName, modulePackages);
      lines.push(`export { ${sortedNames.join(", ")} } from "${importPath}";`);
    }
  }
  const uniqueLocalExports = [...new Set(localExports)].sort();
  if (uniqueLocalExports.length > 0) {
    lines.push(`export { ${uniqueLocalExports.join(", ")} };`);
  }
  return lines;
}
function writeModule(module, options) {
  const { distDir, packageName, createDirs = true } = options;
  const moduleSegments = module.moduleName.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const moduleDir = path.join(distDir, packageName, ...moduleSegments.slice(0, -1));
  const filePath = path.join(moduleDir, `${fileName}.js`);
  if (createDirs) {
    fs.mkdirSync(moduleDir, { recursive: true });
  }
  fs.writeFileSync(filePath, module.code, "utf8");
  if (options.sourceFilePath) {
    const sourceDir = path.dirname(options.sourceFilePath);
    for (const modulePath of module.imports.keys()) {
      if (modulePath.startsWith("./") || modulePath.startsWith("../")) {
        const srcFile = path.resolve(sourceDir, modulePath);
        const destFile = path.resolve(moduleDir, modulePath);
        if (fs.existsSync(srcFile)) {
          fs.mkdirSync(path.dirname(destFile), { recursive: true });
          fs.copyFileSync(srcFile, destFile);
        }
      }
    }
  }
  return filePath;
}
function buildProject(programs, options) {
  const outputs = new Map;
  const errors = [];
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
export {
  writeModule,
  sanitizeIdentifier,
  resolveDictionaryForType,
  resolveDictReference,
  isTypeVariable,
  generate,
  formatTypeKey,
  createCodegenContext,
  buildProject
};
