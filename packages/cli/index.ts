import type { Diagnostic } from "@vibe/syntax";
import { parseSource, type ParseResult } from "@vibe/parser";
import { tokenizeStream, type TokenStream } from "@vibe/lexer";
import {
  analyzeProgram,
  type AnalyzeResult,
  type ModuleResolver,
} from "@vibe/semantics";
import { generateModule } from "@vibe/codegen";
import { existsSync } from "node:fs";
import { mkdir, readFile, readdir, stat, writeFile } from "node:fs/promises";
import type { Stats } from "node:fs";
import path from "node:path";
import sade from "sade";
import {
  createProjectModuleResolver,
  findWorkspaceRoot,
  PackageRegistry,
  buildPackageGraph,
  resolveVibePackageConfig,
  ModuleExportsTable,
  seedModuleExportsFromMetadata,
  seedModuleExportsFromPackageJson,
  LANG_EXTENSION,
} from "@vibe/module-resolver";
// Re-export selected internals as a stable public API surface for other workspace packages

type TokenizeOptions = {
  file?: string;
  source?: string;
  out?: string;
};

type AnalyzeCliOptions = TokenizeOptions & {
  debugMacros?: boolean;
  pretty?: number;
  showAst?: boolean;
  showIr?: boolean;
};

type CompileAllCliOptions = {
  debugMacros?: boolean;
  pretty?: number;
  outDir?: string;
};

type BuildCliOptions = CompileAllCliOptions & {
  force?: boolean;
};

type CliOptionBag = TokenizeOptions & Record<string, unknown>;

interface FrontendResult {
  readonly sourceName: string;
  readonly sourceText: string;
  readonly parse: ParseResult;
  readonly analysis: AnalyzeResult;
  readonly ok: boolean;
}

const DEFAULT_PRETTY = 2;

const WORKSPACE_ROOT = findWorkspaceRoot(process.cwd());
const PACKAGE_REGISTRY = PackageRegistry.create(WORKSPACE_ROOT);
const defaultModuleResolver = createProjectModuleResolver({
  packageRegistry: PACKAGE_REGISTRY,
  workspaceRoot: WORKSPACE_ROOT,
});
const MODULE_EXPORTS = new ModuleExportsTable();
const seededPackageRoots = new Set<string>();
const workspaceExportsPromise = seedWorkspaceModuleExports();

async function seedWorkspaceModuleExports(): Promise<void> {
  if (!WORKSPACE_ROOT) {
    return;
  }
  const workspacePackages = PACKAGE_REGISTRY.getWorkspacePackages();
  await Promise.all(
    workspacePackages.map(async (metadata) => {
      await seedModuleExportsFromMetadata(metadata, MODULE_EXPORTS, {
        moduleResolver: defaultModuleResolver,
      });
      seededPackageRoots.add(metadata.rootDir);
    })
  );
}

const findPackageRootForFile = (filePath: string): string | null => {
  let currentDir = path.dirname(path.resolve(filePath));
  while (true) {
    const manifestPath = path.join(currentDir, "package.json");
    if (existsSync(manifestPath)) {
      return currentDir;
    }
    const parent = path.dirname(currentDir);
    if (parent === currentDir) {
      return null;
    }
    currentDir = parent;
  }
};

const ensureModuleExportsReady = async (modulePath?: string): Promise<void> => {
  await workspaceExportsPromise;
  if (!modulePath) {
    return;
  }
  const packageRoot = findPackageRootForFile(modulePath);
  if (!packageRoot || seededPackageRoots.has(packageRoot)) {
    return;
  }
  await seedModuleExportsFromPackageJson(packageRoot, MODULE_EXPORTS, {
    moduleResolver: defaultModuleResolver,
  });
  seededPackageRoots.add(packageRoot);
};

const replaceLangExtension = (filePath: string): string => {
  return filePath.endsWith(LANG_EXTENSION)
    ? `${filePath.slice(0, -LANG_EXTENSION.length)}.js`
    : `${filePath}.js`;
};

const relativeToRoot = (root: string, filePath: string): string => {
  const relative = path.relative(root, filePath);
  if (!relative || relative.startsWith("..")) {
    return path.basename(filePath);
  }
  return relative;
};

const collectLangFiles = async (root: string): Promise<string[]> => {
  const files: string[] = [];
  const visit = async (dir: string): Promise<void> => {
    const entries = await readdir(dir, { withFileTypes: true });
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
  return files.sort((a, b) => a.localeCompare(b));
};

interface LangFileDescriptor {
  readonly absolutePath: string;
  readonly relativePath: string;
  readonly sourceRoot: string;
}

const collectLangFilesFromRoots = async (
  roots: readonly string[]
): Promise<LangFileDescriptor[]> => {
  const descriptors: LangFileDescriptor[] = [];
  for (const root of roots) {
    const absoluteRoot = path.resolve(root);
    const files = await collectLangFiles(absoluteRoot);
    for (const file of files) {
      descriptors.push({
        absolutePath: file,
        relativePath: relativeToRoot(absoluteRoot, file),
        sourceRoot: absoluteRoot,
      });
    }
  }
  return descriptors.sort((a, b) =>
    a.absolutePath.localeCompare(b.absolutePath)
  );
};

const safeStat = async (filePath: string): Promise<Stats | null> => {
  try {
    return await stat(filePath);
  } catch (error) {
    if (isEnoent(error)) {
      return null;
    }
    throw error;
  }
};

const descriptorNeedsRebuild = async (
  descriptor: LangFileDescriptor,
  outDir: string
): Promise<boolean> => {
  const outputPath = path.join(
    outDir,
    replaceLangExtension(descriptor.relativePath)
  );
  const [sourceStats, outputStats] = await Promise.all([
    safeStat(descriptor.absolutePath),
    safeStat(outputPath),
  ]);
  if (!sourceStats || !outputStats) {
    return true;
  }
  return sourceStats.mtimeMs > outputStats.mtimeMs;
};

const packageNeedsRebuild = async (
  descriptors: readonly LangFileDescriptor[],
  outDir: string
): Promise<boolean> => {
  for (const descriptor of descriptors) {
    if (await descriptorNeedsRebuild(descriptor, outDir)) {
      return true;
    }
  }
  return false;
};

const isEnoent = (error: unknown): boolean => {
  return Boolean(
    error &&
      typeof error === "object" &&
      "code" in error &&
      (error as { code?: string }).code === "ENOENT"
  );
};

interface CompileSourcesOptions {
  readonly packageName?: string;
  readonly sourceRoots: readonly string[];
  readonly outDir: string;
  readonly pretty: number;
  readonly debugMacros?: boolean;
  readonly logTag?: string;
  readonly skipIfUpToDate?: boolean;
}

interface CompileSourcesResult {
  readonly ok: boolean;
  readonly fileCount: number;
}

const compileSources = async (
  options: CompileSourcesOptions
): Promise<CompileSourcesResult> => {
  const logTag = options.logTag ?? "[vibe][build]";
  const sourceRoots = options.sourceRoots.map((root) => path.resolve(root));
  const descriptors = await collectLangFilesFromRoots(sourceRoots);
  if (descriptors.length === 0) {
    console.error(
      `${logTag} No .lang files found under ${sourceRoots.join(", ")}`
    );
    return { ok: false, fileCount: 0 };
  }
  const outDir = path.resolve(options.outDir);
  if (options.skipIfUpToDate) {
    const needsBuild = await packageNeedsRebuild(descriptors, outDir);
    if (!needsBuild) {
      console.info(
        `${logTag} up to date; skipping (${descriptors.length} sources)`
      );
      return { ok: true, fileCount: descriptors.length };
    }
  }
  const knownModules = new Set(
    descriptors.map((descriptor) => descriptor.absolutePath)
  );
  const moduleResolver = createProjectModuleResolver({
    knownModules,
    packageRegistry: PACKAGE_REGISTRY,
    workspaceRoot: WORKSPACE_ROOT,
  });
  await mkdir(outDir, { recursive: true });
  let hadFailure = false;
  for (const descriptor of descriptors) {
    const moduleContext: FrontendContext = {
      moduleId: descriptor.absolutePath,
      moduleResolver,
    };
    const frontend = await runFrontend(
      { file: descriptor.absolutePath },
      moduleContext
    );
    if (options.debugMacros) {
      logMacroDebug(frontend.analysis, options.pretty);
    }
    if (!frontend.ok) {
      hadFailure = true;
      continue;
    }
    const targetFileName = replaceLangExtension(descriptor.relativePath);
    const outputPath = path.join(outDir, targetFileName);
    const codegen = generateModule(
      frontend.parse.program,
      frontend.analysis.graph,
      {
        sourceName: descriptor.relativePath,
        sourceContent: frontend.sourceText,
        targetFileName,
      }
    );
    emitDiagnostics(codegen.diagnostics);
    if (!codegen.ok) {
      hadFailure = true;
      continue;
    }
    await mkdir(path.dirname(outputPath), { recursive: true });
    await writeFile(outputPath, codegen.moduleText, "utf8");
    console.info(`${logTag} wrote ${path.relative(process.cwd(), outputPath)}`);
  }
  return {
    ok: !hadFailure,
    fileCount: descriptors.length,
  };
};

interface FrontendContext {
  readonly moduleId?: string;
  readonly moduleResolver?: ModuleResolver;
}

const createFileModuleContext = (filePath?: string): FrontendContext => {
  if (!filePath) {
    return {};
  }
  const moduleId = path.resolve(filePath);
  return {
    moduleId,
    moduleResolver: defaultModuleResolver,
  } satisfies FrontendContext;
};

const camelize = (key: string): string =>
  key.replace(/-([a-z])/g, (_, char: string) => char.toUpperCase());

const getOptionValue = <T>(
  options: CliOptionBag,
  key: string
): T | undefined => {
  const camelKey = camelize(key);
  return (
    (options as Record<string, T | undefined>)[camelKey] ??
    (options as Record<string, T | undefined>)[key]
  );
};

const getBooleanOption = (
  options: CliOptionBag,
  key: string
): boolean | undefined => {
  const value = getOptionValue<unknown>(options, key);
  if (value === undefined) {
    return undefined;
  }
  if (typeof value === "boolean") {
    return value;
  }
  if (typeof value === "string") {
    if (value.length === 0) {
      return true;
    }
    const normalized = value.toLowerCase();
    if (normalized === "false" || normalized === "0") {
      return false;
    }
    if (normalized === "true" || normalized === "1") {
      return true;
    }
  }
  return Boolean(value);
};

const getNumberOption = (
  options: CliOptionBag,
  key: string
): number | undefined => {
  const value = getOptionValue<unknown>(options, key);
  if (value === undefined) {
    return undefined;
  }
  if (typeof value === "number" && Number.isFinite(value)) {
    return value;
  }
  if (typeof value === "string" && value.length > 0) {
    const parsed = Number(value);
    if (!Number.isNaN(parsed)) {
      return parsed;
    }
  }
  return undefined;
};

const formatDiagnostic = (diagnostic: Diagnostic): string => {
  const { line, column } = diagnostic.span.start;
  const position = `${line + 1}:${column + 1}`;
  return (
    `${diagnostic.severity.toUpperCase()} ${diagnostic.code ?? ""}`.trim() +
    ` ${position} ${diagnostic.message}`
  );
};

const emitDiagnostics = (diagnostics: readonly Diagnostic[]): void => {
  if (diagnostics.length === 0) {
    return;
  }
  for (const diagnostic of diagnostics) {
    console.error(formatDiagnostic(diagnostic));
  }
};

const resolveSource = async (options: TokenizeOptions): Promise<string> => {
  if (options.file) {
    try {
      return await readFile(options.file, "utf8");
    } catch (error) {
      if (isEnoent(error)) {
        throw new Error(`Cannot read file: ${options.file}`);
      }
      throw error;
    }
  }

  if (options.source !== undefined) {
    return options.source;
  }

  return "";
};

const runFrontend = async (
  options: TokenizeOptions,
  context: FrontendContext = {}
): Promise<FrontendResult> => {
  const sourceText = await resolveSource(options);
  const sourceName = options.file ?? "<inline>";
  const defaultContext = createFileModuleContext(options.file);
  const mergedContext: FrontendContext = {
    ...defaultContext,
    ...context,
  };
  const targetModuleId =
    mergedContext.moduleId ??
    (options.file ? path.resolve(options.file) : undefined);
  await ensureModuleExportsReady(targetModuleId);
  const parse = await parseSource(sourceText);
  const analysis = await analyzeProgram(parse.program, {
    moduleId: mergedContext.moduleId,
    moduleResolver: mergedContext.moduleResolver,
    moduleExports: MODULE_EXPORTS,
  });
  emitDiagnostics([...parse.diagnostics, ...analysis.diagnostics]);
  return {
    sourceName,
    sourceText,
    parse,
    analysis,
    ok: parse.ok && analysis.ok,
  } satisfies FrontendResult;
};

const logMacroDebug = (analysis: AnalyzeResult, pretty: number): void => {
  const macroSymbols = analysis.graph.symbols.filter(
    (symbol) => symbol.kind === "macro"
  );
  const macroSymbolIds = new Set(macroSymbols.map((symbol) => symbol.id));
  const macroNodes = analysis.graph.nodes.filter((node) => {
    const symbolId = node.symbol?.symbolId;
    return Boolean(symbolId && macroSymbolIds.has(symbolId));
  });
  const macroScopes = analysis.graph.scopes.filter((scope) =>
    scope.symbols.some((symbolId) => macroSymbolIds.has(symbolId))
  );
  const macroDiagnostics = analysis.diagnostics.filter((diagnostic) =>
    (diagnostic.code ?? "").startsWith("SEM_MACRO")
  );
  if (
    macroSymbols.length === 0 &&
    macroNodes.length === 0 &&
    macroDiagnostics.length === 0
  ) {
    console.error("[vibe][macros] No macro metadata available.");
    return;
  }
  console.error("[vibe][macros]");
  console.error(
    JSON.stringify(
      {
        macros: {
          diagnostics: macroDiagnostics,
          symbols: macroSymbols,
          scopes: macroScopes,
          nodes: macroNodes,
          graph: analysis.graph,
        },
      },
      null,
      pretty
    )
  );
};

const logJsonBlock = (
  label: string,
  payload: unknown,
  pretty: number
): void => {
  console.error(`[vibe][${label}]`);
  console.error(JSON.stringify(payload, null, pretty));
};

const runTokenize = async (options: TokenizeOptions): Promise<number> => {
  try {
    const source = await resolveSource(options);
    const stream: TokenStream = tokenizeStream(source);
    const tokens = [];
    for await (const token of stream) {
      tokens.push(token);
    }
    const summary = await stream.result;
    emitDiagnostics(summary.diagnostics);
    console.log(JSON.stringify(tokens, null, 2));
    return summary.ok ? 0 : 1;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const runParse = async (options: TokenizeOptions): Promise<number> => {
  try {
    const source = await resolveSource(options);
    const result = await parseSource(source);
    emitDiagnostics(result.diagnostics);
    console.log(JSON.stringify(result.program, null, 2));
    return result.ok ? 0 : 1;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const runAnalyze = async (options: AnalyzeCliOptions): Promise<number> => {
  try {
    const frontend = await runFrontend(options);
    const pretty = options.pretty ?? DEFAULT_PRETTY;
    console.log(
      JSON.stringify(
        {
          program: frontend.parse.program,
          analysis: {
            ok: frontend.analysis.ok,
            graph: frontend.analysis.graph,
          },
        },
        null,
        pretty
      )
    );
    if (options.debugMacros) {
      logMacroDebug(frontend.analysis, pretty);
    }
    return frontend.ok ? 0 : 1;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const executeGeneratedModule = async (code: string): Promise<unknown> => {
  const blob = new Blob([code], { type: "text/javascript" });
  const url = URL.createObjectURL(blob);
  try {
    const module = await import(url);
    return module.default ?? module;
  } finally {
    URL.revokeObjectURL(url);
  }
};

const runRun = async (options: AnalyzeCliOptions): Promise<number> => {
  try {
    const frontend = await runFrontend(options);
    const pretty = options.pretty ?? DEFAULT_PRETTY;
    if (options.debugMacros) {
      logMacroDebug(frontend.analysis, pretty);
    }
    if (options.showAst) {
      logJsonBlock("ast", frontend.parse.program, pretty);
    }
    if (!frontend.ok) {
      return 1;
    }
    const codegen = generateModule(
      frontend.parse.program,
      frontend.analysis.graph,
      {
        sourceName: frontend.sourceName,
        sourceContent: frontend.sourceText,
      }
    );
    emitDiagnostics(codegen.diagnostics);
    if (!codegen.ok) {
      return 1;
    }
    if (options.showIr) {
      logJsonBlock("ir", codegen.ir, pretty);
    }
    const runtimeValue = await executeGeneratedModule(codegen.moduleText);
    console.log(
      JSON.stringify(
        {
          ir: codegen.ir,
          runtime: runtimeValue,
        },
        null,
        pretty
      )
    );
    return 0;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const runCompile = async (options: AnalyzeCliOptions): Promise<number> => {
  try {
    const frontend = await runFrontend(options);
    const pretty = options.pretty ?? DEFAULT_PRETTY;
    if (options.debugMacros) {
      logMacroDebug(frontend.analysis, pretty);
    }
    if (options.showAst) {
      logJsonBlock("ast", frontend.parse.program, pretty);
    }
    if (!frontend.ok) {
      return 1;
    }
    const codegen = generateModule(
      frontend.parse.program,
      frontend.analysis.graph,
      {
        sourceName: frontend.sourceName,
        sourceContent: frontend.sourceText,
        targetFileName: options.out ?? `${frontend.sourceName}.js`,
      }
    );
    emitDiagnostics(codegen.diagnostics);
    if (!codegen.ok) {
      return 1;
    }
    if (options.showIr) {
      logJsonBlock("ir", codegen.ir, pretty);
    }
    if (options.out) {
      const outputPath = path.resolve(options.out);
      await mkdir(path.dirname(outputPath), { recursive: true });
      await writeFile(outputPath, codegen.moduleText, "utf8");
    } else {
      console.log(codegen.moduleText);
    }
    return 0;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const runCompileAll = async (
  directory: string | undefined,
  options: CompileAllCliOptions
): Promise<number> => {
  try {
    const sourceRoot = path.resolve(directory ?? ".");
    const outDir = path.resolve(options.outDir ?? "dist");
    const pretty = options.pretty ?? DEFAULT_PRETTY;
    const debugMacros = options.debugMacros ?? false;
    const result = await compileSources({
      sourceRoots: [sourceRoot],
      outDir,
      pretty,
      debugMacros,
      logTag: "[vibe][compile-all]",
    });
    return result.ok ? 0 : 1;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const resolveBuildTargetDir = (target: string | undefined): string => {
  if (!target) {
    return process.cwd();
  }
  const candidate = path.resolve(target);
  const manifestPath = path.join(candidate, "package.json");
  if (existsSync(manifestPath)) {
    return candidate;
  }
  const metadata = PACKAGE_REGISTRY.getPackageMetadata(
    target,
    WORKSPACE_ROOT ?? process.cwd()
  );
  if (metadata) {
    return metadata.rootDir;
  }
  throw new Error(
    `Unable to resolve build target ${target}. Provide a package name or directory.`
  );
};

const runBuild = async (
  target: string | undefined,
  options: BuildCliOptions
): Promise<number> => {
  try {
    const pretty = options.pretty ?? DEFAULT_PRETTY;
    const debugMacros = options.debugMacros ?? false;
    const force = options.force ?? false;
    const targetDir = resolveBuildTargetDir(target);
    const graph = buildPackageGraph(targetDir, PACKAGE_REGISTRY);
    if (graph.topoOrder.length === 0) {
      console.error(
        `[vibe][build] No Vibe sources found starting from ${graph.entry.name}`
      );
      return 1;
    }
    let hadFailure = false;
    for (const node of graph.topoOrder) {
      const resolved = resolveVibePackageConfig(node.rootDir, node.vibe);
      if (resolved.sourceRoots.length === 0) {
        continue;
      }
      const outDir = resolved.outDir ?? path.resolve(node.rootDir, "dist");
      const result = await compileSources({
        packageName: node.name,
        sourceRoots: resolved.sourceRoots,
        outDir,
        pretty,
        debugMacros,
        logTag: `[vibe][build:${node.name}]`,
        skipIfUpToDate: !force,
      });
      if (!result.ok) {
        hadFailure = true;
      }
    }
    return hadFailure ? 1 : 0;
  } catch (error) {
    console.error(String(error));
    return 1;
  }
};

const bootstrapCli = (): void => {
  const version = process.env.npm_package_version ?? "0.0.0";
  const cli = sade("vibe").version(version);

  cli
    .command("tokenize [source]")
    .describe("Tokenize inline source or a file and print tokens as JSON")
    .option("--file, -f", "Path to a source file to read")
    .option("--source, -s", "Inline source provided via flag")
    .example('tokenize "(+ 1 2)"')
    .example("tokenize --file program.lang")
    .example('tokenize --source "(println :ok)"')
    .action(
      async (
        positionalSource: string | undefined,
        opts: TokenizeOptions
      ): Promise<void> => {
        const exitCode = await runTokenize({
          file: opts.file,
          source: opts.source ?? positionalSource,
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("parse [source]")
    .describe("Parse inline source or a file and print the AST as JSON")
    .option("--file, -f", "Path to a source file to read")
    .option("--source, -s", "Inline source provided via flag")
    .example('parse "(def foo 1)"')
    .example("parse --file program.lang")
    .example('parse --source "(println :ok)"')
    .action(
      async (
        positionalSource: string | undefined,
        opts: TokenizeOptions
      ): Promise<void> => {
        const exitCode = await runParse({
          file: opts.file,
          source: opts.source ?? positionalSource,
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("analyze [source]")
    .describe(
      "Run parser + semantic analyzer and print the AST with semantic metadata"
    )
    .option("--file, -f", "Path to a source file to read")
    .option("--source, -s", "Inline source provided via flag")
    .option("--pretty, -p <n>", "Pretty-print JSON output (default 2)", "2")
    .option(
      "--debug-macros",
      "Dump macro-expansion metadata to stderr alongside diagnostics"
    )
    .example('analyze "(def foo 1) foo"')
    .example("analyze --file program.lang")
    .action(
      async (
        positionalSource: string | undefined,
        opts: CliOptionBag
      ): Promise<void> => {
        // If positional looks like a file (has .lang extension), treat it as a file
        const isFileArg =
          positionalSource?.endsWith(".lang") ||
          (positionalSource && existsSync(positionalSource));
        const exitCode = await runAnalyze({
          file: opts.file || (isFileArg ? positionalSource : undefined),
          source: opts.source ?? (isFileArg ? undefined : positionalSource),
          pretty: getNumberOption(opts, "pretty"),
          debugMacros: getBooleanOption(opts, "debug-macros"),
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("run [source]")
    .describe(
      "Parse, analyze, codegen, and execute the program with stub IR output"
    )
    .option("--file, -f", "Path to a source file to read")
    .option("--source, -s", "Inline source provided via flag")
    .option("--pretty, -p <n>", "Pretty-print JSON output (default 2)", "2")
    .option(
      "--debug-macros",
      "Dump macro-expansion metadata to stderr alongside diagnostics"
    )
    .option("--show-ast", "Print the parsed AST JSON to stderr")
    .option("--show-ir", "Print the generated IR JSON to stderr")
    .example('run "(def foo 1) foo"')
    .example("run --file program.lang")
    .action(
      async (
        positionalSource: string | undefined,
        opts: CliOptionBag
      ): Promise<void> => {
        // If positional looks like a file (has .lang extension), treat it as a file
        const isFileArg =
          positionalSource?.endsWith(".lang") ||
          (positionalSource && existsSync(positionalSource));
        const exitCode = await runRun({
          file: opts.file || (isFileArg ? positionalSource : undefined),
          source: opts.source ?? (isFileArg ? undefined : positionalSource),
          pretty: getNumberOption(opts, "pretty"),
          debugMacros: getBooleanOption(opts, "debug-macros"),
          showAst: getBooleanOption(opts, "show-ast"),
          showIr: getBooleanOption(opts, "show-ir"),
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("compile [source]")
    .describe("Compile source to JavaScript and print or write to --out")
    .option("--file, -f", "Path to a source file to read")
    .option("--source, -s", "Inline source provided via flag")
    .option("--out, -o", "Destination file for generated JavaScript")
    .option(
      "--pretty, -p <n>",
      "Pretty-print debug JSON output (default 2)",
      "2"
    )
    .option(
      "--debug-macros",
      "Dump macro-expansion metadata to stderr alongside diagnostics"
    )
    .option("--show-ast", "Print the parsed AST JSON to stderr")
    .option("--show-ir", "Print the generated IR JSON to stderr")
    .example('compile "(def foo 1) foo" --out foo.js')
    .action(
      async (
        positionalSource: string | undefined,
        opts: CliOptionBag
      ): Promise<void> => {
        // If positional looks like a file (has .lang extension), treat it as a file
        const isFileArg =
          positionalSource?.endsWith(".lang") ||
          (positionalSource && existsSync(positionalSource));
        const exitCode = await runCompile({
          file: opts.file || (isFileArg ? positionalSource : undefined),
          source: opts.source ?? (isFileArg ? undefined : positionalSource),
          out: opts.out,
          pretty: getNumberOption(opts, "pretty"),
          debugMacros: getBooleanOption(opts, "debug-macros"),
          showAst: getBooleanOption(opts, "show-ast"),
          showIr: getBooleanOption(opts, "show-ir"),
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("compile-all [dir]")
    .describe("Compile every .lang file within a directory into --out-dir")
    .option("--out-dir, -o", "Directory for generated JavaScript", "dist")
    .option(
      "--pretty, -p <n>",
      "Pretty-print debug JSON output (default 2)",
      "2"
    )
    .option(
      "--debug-macros",
      "Dump macro-expansion metadata to stderr alongside diagnostics"
    )
    .example("compile-all src --out-dir dist")
    .action(
      async (
        directory: string | undefined,
        opts: CliOptionBag
      ): Promise<void> => {
        const exitCode = await runCompileAll(directory, {
          outDir: getOptionValue<string>(opts, "out-dir"),
          pretty: getNumberOption(opts, "pretty"),
          debugMacros: getBooleanOption(opts, "debug-macros"),
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("build [target]")
    .describe(
      "Recursively compile Vibe packages starting from a package name or directory"
    )
    .option(
      "--pretty, -p <n>",
      "Pretty-print debug JSON output (default 2)",
      "2"
    )
    .option(
      "--debug-macros",
      "Dump macro-expansion metadata to stderr alongside diagnostics"
    )
    .option("--force", "Rebuild every package even when outputs are fresh")
    .example("build packages/example-app")
    .example("build @vibe/example-app")
    .action(
      async (target: string | undefined, opts: CliOptionBag): Promise<void> => {
        const exitCode = await runBuild(target, {
          pretty: getNumberOption(opts, "pretty"),
          debugMacros: getBooleanOption(opts, "debug-macros"),
          force: getBooleanOption(opts, "force"),
        });
        process.exit(exitCode);
      }
    );

  cli
    .command("repl")
    .describe("Interactive REPL: parse → analyze → codegen → eval")
    .option("--pretty, -p <n>", "Pretty-print JSON result (default 2)", "2")
    .option(
      "--debug-macros",
      "Dump macro-expansion metadata to stderr alongside diagnostics"
    )
    .example("repl")
    .action(async (opts: CliOptionBag): Promise<void> => {
      const pretty = getNumberOption(opts, "pretty");
      const debugMacros = getBooleanOption(opts, "debug-macros");
      // Lazy import to avoid circular module issues in some runtimes
      const { runRepl } = await import("./src/repl");
      await runRepl({
        pretty: pretty ?? undefined,
        debugMacros: debugMacros ?? false,
      });
      process.exit(0);
    });

  cli.parse(process.argv);
};

bootstrapCli();
