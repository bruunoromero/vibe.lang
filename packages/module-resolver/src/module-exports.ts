import { parseSource } from "@vibe/parser";
import type { Value } from "@vibe/interpreter";
import {
  NodeKind,
  type ExpressionNode,
  type ListNode,
  type NamespaceImportNode,
  type ProgramNode,
  type ScopeId,
  type SymbolNode,
  type VectorNode,
} from "@vibe/syntax";
import {
  analyzeProgram,
  type ModuleResolver,
  type ModuleExportEntry,
  type ModuleExportedMacroClause,
  type ModuleExportsLookup,
  type ModuleMacroDependency,
} from "@vibe/semantics";
import { Dirent, existsSync, readFileSync } from "node:fs";
import { readFile, readdir } from "node:fs/promises";
import path from "node:path";
import type { PackageMetadata } from "./module-resolver";
import { parseVibeConfig, resolveVibePackageConfig } from "./workspace-config";
import { LANG_EXTENSION } from "./specifiers";

export class ModuleExportsTable implements ModuleExportsLookup {
  private readonly cache = new Map<string, readonly ModuleExportEntry[]>();

  getExports(moduleId: string): readonly ModuleExportEntry[] | undefined {
    return this.cache.get(path.resolve(moduleId));
  }

  has(moduleId: string): boolean {
    return this.cache.has(path.resolve(moduleId));
  }

  register(moduleId: string, symbols: readonly ModuleExportEntry[]): void {
    const normalized = path.resolve(moduleId);
    this.cache.set(normalized, dedupeExports(symbols));
  }
}

const PUBLIC_DEF_FORMS = new Set(["def"]);
const PRIVATE_DEF_FORMS = new Set(["defp"]);

export interface ExportExtractionOptions {
  readonly moduleId?: string;
  readonly moduleResolver?: ModuleResolver;
  readonly moduleExports?: ModuleExportsLookup;
}

interface ExportDiscoveryOptions extends ExportExtractionOptions {}

interface SeedModuleExportsOptions {
  readonly moduleResolver?: ModuleResolver;
}

export const extractTopLevelExports = async (
  program: ProgramNode,
  options: ExportExtractionOptions = {}
): Promise<readonly ModuleExportEntry[]> => {
  const analysis = await analyzeProgram(program, {
    moduleId: options.moduleId,
    moduleResolver: options.moduleResolver,
    moduleExports: options.moduleExports,
  });
  // Build a map of analyzer-provided export identifiers (emitted aliases)
  const analyzerExportMap = new Map<string, string>();
  try {
    for (const e of analysis.graph.exports ?? []) {
      if (e && (e as any).name && (e as any).identifier) {
        analyzerExportMap.set((e as any).name, (e as any).identifier);
      }
    }
  } catch {
    // ignore
  }

  if (process.env.VIBE_DEBUG) {
    try {
      console.debug(
        `[extractTopLevelExports] module=${
          options.moduleId
        } analyzerExports=${JSON.stringify(analysis.graph.exports)}`
      );
    } catch {}
  }

  const raw = collectExportsFromProgram(program, analysis.macroRuntime);
  // Attach analyzer-assigned identifier when available so importers can
  // reference the actual emitted property name.
  const decorated = raw.map((entry) =>
    analyzerExportMap.has(entry.name)
      ? { ...entry, identifier: analyzerExportMap.get(entry.name) }
      : entry
  );
  return decorated;
};

const collectExportsFromProgram = (
  program: ProgramNode,
  macroRuntime?: ReadonlyMap<string, Value>
): ModuleExportEntry[] => {
  const exports: ModuleExportEntry[] = [];
  const seen = new Set<string>();
  const namespaceAliases: ModuleMacroDependency[] = [];
  for (const form of program.body) {
    if (!form || form.kind !== NodeKind.List) {
      if (form && form.kind === NodeKind.NamespaceImport) {
        recordNamespaceImportAlias(namespaceAliases, form);
      }
      continue;
    }
    const list = form as ListNode;
    const head = list.elements[0];
    if (!head || head.kind !== NodeKind.Symbol) {
      continue;
    }
    const headValue = head.value;
    if (headValue === "external" || headValue === "require") {
      recordNamespaceAlias(namespaceAliases, list, headValue);
      continue;
    }
    if (PRIVATE_DEF_FORMS.has(headValue)) {
      continue;
    }
    if (!PUBLIC_DEF_FORMS.has(headValue)) {
      continue;
    }
    const binding = list.elements[1];
    if (!binding || binding.kind !== NodeKind.Symbol) {
      continue;
    }
    if (seen.has(binding.value)) {
      continue;
    }
    seen.add(binding.value);
    const valueNode = list.elements[2];
    if (isMacroLiteralNode(valueNode)) {
      const macro = createMacroExport(valueNode as ListNode, binding, [
        ...namespaceAliases,
      ]);
      exports.push(macro ?? { name: binding.value, kind: "macro" });
      continue;
    }
    const runtimeValue = macroRuntime?.get(binding.value);
    exports.push({
      name: binding.value,
      kind: "var",
      ...(runtimeValue ? { macroRuntimeValue: runtimeValue } : {}),
    });
  }
  return exports;
};

const dedupeExports = (
  entries: readonly ModuleExportEntry[]
): ModuleExportEntry[] => {
  const byName = new Map<string, ModuleExportEntry>();
  for (const entry of entries) {
    if (!byName.has(entry.name)) {
      byName.set(entry.name, entry);
    }
  }
  return [...byName.values()];
};

const createMacroExport = (
  macroNode: ListNode,
  binding: SymbolNode,
  namespaceAliases: readonly ModuleMacroDependency[]
): ModuleExportEntry | null => {
  const clauseNodes = extractMacroClauses(macroNode);
  if (clauseNodes.length === 0) {
    return null;
  }

  const clauses = clauseNodes.map((clause) => {
    const paramsNode = clause.paramsNode;
    if (!paramsNode || paramsNode.kind !== NodeKind.Vector) {
      return null;
    }
    const parsedParams = parseMacroParams(paramsNode);
    if (!parsedParams) {
      return null;
    }
    const bodyNode = clause.bodyNodes.find((expr): expr is ExpressionNode =>
      Boolean(expr)
    );
    if (!bodyNode) {
      return null;
    }
    return {
      params: parsedParams.params,
      ...(parsedParams.rest ? { rest: parsedParams.rest } : {}),
      body: cloneExpressionWithoutScopes(bodyNode),
    } satisfies ModuleExportedMacroClause;
  });

  if (clauses.some((clause) => clause === null)) {
    return null;
  }

  return {
    name: binding.value,
    kind: "macro",
    macro: {
      clauses: clauses as ModuleExportedMacroClause[],
      dependencies:
        namespaceAliases && namespaceAliases.length > 0
          ? namespaceAliases.map((dep) => ({ ...dep }))
          : undefined,
    },
  } satisfies ModuleExportEntry;
};

const recordNamespaceAlias = (
  aliases: ModuleMacroDependency[],
  form: ListNode,
  kind: "external" | "require"
): void => {
  const aliasNode = form.elements[1];
  const specifierNode = form.elements[2];
  if (!aliasNode || aliasNode.kind !== NodeKind.Symbol) {
    return;
  }
  if (!specifierNode || specifierNode.kind !== NodeKind.String) {
    return;
  }
  aliases.push({
    kind,
    alias: aliasNode.value,
    specifier: specifierNode.value,
  });
};

const recordNamespaceImportAlias = (
  aliases: ModuleMacroDependency[],
  node: NamespaceImportNode
): void => {
  if (node.importKind !== "external" && node.importKind !== "require") {
    return;
  }
  const aliasNode = node.alias;
  const sourceNode = node.source;
  if (!aliasNode || aliasNode.kind !== NodeKind.Symbol) {
    return;
  }
  if (!sourceNode || sourceNode.kind !== NodeKind.String) {
    return;
  }
  aliases.push({
    kind: node.importKind,
    alias: aliasNode.value,
    specifier: sourceNode.value,
  });
};

interface MacroClauseNodeDescriptor {
  readonly paramsNode: ExpressionNode | null;
  readonly bodyNodes: readonly ExpressionNode[];
}

const isMacroLiteralNode = (
  node: ExpressionNode | null | undefined
): node is ListNode => {
  if (!node || node.kind !== NodeKind.List) {
    return false;
  }
  const head = node.elements[0];
  return Boolean(
    head && head.kind === NodeKind.Symbol && head.value === "macro+"
  );
};

const extractMacroClauses = (node: ListNode): MacroClauseNodeDescriptor[] => {
  const paramsNode = node.elements[1];
  if (paramsNode && paramsNode.kind === NodeKind.Vector) {
    return [
      {
        paramsNode,
        bodyNodes: node.elements
          .slice(2)
          .filter((expr): expr is ExpressionNode => Boolean(expr)),
      },
    ];
  }

  const tail = node.elements.slice(1).filter(Boolean) as ExpressionNode[];
  if (tail.length === 0) {
    return [];
  }

  const clauseLists = tail.filter(
    (expr): expr is ListNode => expr.kind === NodeKind.List
  );
  if (clauseLists.length !== tail.length) {
    return [];
  }

  return clauseLists.map((clause) => ({
    paramsNode: clause.elements[0] ?? null,
    bodyNodes: clause.elements
      .slice(1)
      .filter((expr): expr is ExpressionNode => Boolean(expr)),
  }));
};

const parseMacroParams = (
  node: VectorNode
): { params: string[]; rest?: string } | null => {
  const params: string[] = [];
  let rest: string | undefined;
  let sawRest = false;
  for (let index = 0; index < node.elements.length; index += 1) {
    const element = node.elements[index];
    if (!element) {
      continue;
    }
    if (element.kind !== NodeKind.Symbol) {
      return null;
    }
    if (element.value === "&") {
      if (sawRest) {
        return null;
      }
      sawRest = true;
      const next = node.elements[index + 1];
      if (!next || next.kind !== NodeKind.Symbol) {
        return null;
      }
      rest = next.value;
      index += 1;
      continue;
    }
    if (sawRest) {
      return null;
    }
    params.push(element.value);
  }
  return { params, rest };
};

const cloneExpression = <T extends ExpressionNode>(node: T): T => {
  const structured = (globalThis as { structuredClone?: <S>(input: S) => S })
    .structuredClone;
  if (structured) {
    return structured(node);
  }
  return JSON.parse(JSON.stringify(node)) as T;
};

const cloneExpressionWithoutScopes = <T extends ExpressionNode>(node: T): T => {
  const cloned = cloneExpression(node);
  stripScopeMetadata(cloned);
  return cloned;
};

const stripScopeMetadata = (value: unknown): void => {
  if (!value || typeof value !== "object") {
    return;
  }
  if (Array.isArray(value)) {
    for (const element of value) {
      stripScopeMetadata(element);
    }
    return;
  }
  if (Object.prototype.hasOwnProperty.call(value, "scopeId")) {
    delete (value as { scopeId?: ScopeId }).scopeId;
  }
  for (const child of Object.values(value as Record<string, unknown>)) {
    stripScopeMetadata(child);
  }
};

export const discoverModuleExports = async (
  modulePath: string,
  options: ExportDiscoveryOptions = {}
): Promise<readonly ModuleExportEntry[] | undefined> => {
  if (!modulePath.endsWith(LANG_EXTENSION)) {
    return undefined;
  }
  try {
    const source = await readFile(modulePath, "utf8");
    const result = await parseSource(source);
    return extractTopLevelExports(result.program, {
      moduleId: modulePath,
      ...options,
    });
  } catch {
    return undefined;
  }
};

export const seedModuleExportsFromMetadata = async (
  metadata: PackageMetadata,
  table: ModuleExportsTable,
  options: SeedModuleExportsOptions = {}
): Promise<void> => {
  const resolved = resolveVibePackageConfig(metadata.rootDir, metadata.vibe);
  const moduleCandidates = new Set<string>();
  if (resolved.entry) {
    moduleCandidates.add(resolved.entry);
  }
  for (const sourceRoot of resolved.sourceRoots) {
    const langFiles = await collectLangFiles(sourceRoot);
    for (const file of langFiles) {
      moduleCandidates.add(file);
    }
  }
  for (const modulePath of moduleCandidates) {
    if (table.has(modulePath)) {
      continue;
    }
    const exports = await discoverModuleExports(modulePath, {
      moduleResolver: options.moduleResolver,
      moduleExports: table,
    });
    if (exports !== undefined) {
      table.register(modulePath, exports);
    }
  }
};

export const seedModuleExportsFromPackageJson = async (
  packageRoot: string,
  table: ModuleExportsTable,
  options: SeedModuleExportsOptions = {}
): Promise<void> => {
  const metadata = readPackageMetadata(packageRoot);
  if (!metadata) {
    return;
  }
  await seedModuleExportsFromMetadata(metadata, table, options);
};

const readPackageMetadata = (dir: string): PackageMetadata | null => {
  const manifestPath = path.join(dir, "package.json");
  if (!existsSync(manifestPath)) {
    return null;
  }
  try {
    const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
    const name =
      typeof manifest.name === "string" && manifest.name.length > 0
        ? manifest.name
        : path.basename(dir);
    const vibe = parseVibeConfig(manifest.vibe);
    return {
      name,
      rootDir: dir,
      ...(vibe ? { vibe } : {}),
    } satisfies PackageMetadata;
  } catch {
    return null;
  }
};

const collectLangFiles = async (root: string): Promise<string[]> => {
  const files: string[] = [];
  const visit = async (dir: string): Promise<void> => {
    let entries: Dirent[];
    try {
      entries = await readdir(dir, { withFileTypes: true });
    } catch (error) {
      if (isEnoent(error)) {
        return;
      }
      throw error;
    }
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        await visit(fullPath);
      } else if (entry.isFile() && entry.name.endsWith(LANG_EXTENSION)) {
        files.push(fullPath);
      }
    }
  };
  await visit(root);
  return files;
};

const isEnoent = (error: unknown): error is NodeJS.ErrnoException => {
  return (
    typeof error === "object" &&
    error !== null &&
    "code" in error &&
    (error as NodeJS.ErrnoException).code === "ENOENT"
  );
};
