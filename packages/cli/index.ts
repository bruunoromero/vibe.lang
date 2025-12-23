import type { Diagnostic } from "@vibe/syntax";
import { parseSource, type ParseResult } from "@vibe/parser";
import { tokenizeStream, type TokenStream } from "@vibe/lexer";
import {
  analyzeProgram,
  type AnalyzeResult,
  type ModuleResolver,
  type ModuleResolutionRequest,
  type ModuleResolutionResult,
} from "@vibe/semantics";
import { generateModule } from "@vibe/codegen";
import { existsSync } from "node:fs";
import { mkdir, readdir } from "node:fs/promises";
import path from "node:path";
import sade from "sade";

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

type CliOptionBag = TokenizeOptions & Record<string, unknown>;

interface FrontendResult {
  readonly sourceName: string;
  readonly sourceText: string;
  readonly parse: ParseResult;
  readonly analysis: AnalyzeResult;
  readonly ok: boolean;
}

const DEFAULT_PRETTY = 2;
const LANG_EXTENSION = ".lang";

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

interface FrontendContext {
  readonly moduleId?: string;
  readonly moduleResolver?: ModuleResolver;
}

const isRelativeSpecifier = (specifier: string): boolean =>
  specifier.startsWith("./") || specifier.startsWith("../");

const ensureLangSpecifier = (specifier: string): string | null => {
  if (specifier.endsWith(".lang")) {
    return specifier;
  }
  if (specifier.endsWith(".js")) {
    return null;
  }
  return `${specifier}.lang`;
};

const normalizeRequireTarget = (
  fromModuleId: string,
  specifier: string
): string | null => {
  if (!isRelativeSpecifier(specifier)) {
    return null;
  }
  const langSpecifier = ensureLangSpecifier(specifier);
  if (!langSpecifier) {
    return null;
  }
  const fromDir = path.dirname(fromModuleId);
  return path.resolve(fromDir, langSpecifier);
};

class FileSystemModuleResolver implements ModuleResolver {
  resolve({
    fromModuleId,
    specifier,
    kind,
  }: ModuleResolutionRequest): ModuleResolutionResult {
    if (kind !== "require") {
      return { ok: true };
    }
    const normalized = normalizeRequireTarget(fromModuleId, specifier);
    if (!normalized) {
      return {
        ok: false,
        reason: "require expects a relative .lang specifier",
      };
    }
    if (existsSync(normalized)) {
      return { ok: true, moduleId: normalized };
    }
    return { ok: false, reason: `module not found (${specifier})` };
  }
}

class WorkspaceModuleResolver implements ModuleResolver {
  constructor(private readonly knownModules: Set<string>) {}

  resolve({
    fromModuleId,
    specifier,
    kind,
  }: ModuleResolutionRequest): ModuleResolutionResult {
    if (kind !== "require") {
      return { ok: true };
    }
    const normalized = normalizeRequireTarget(fromModuleId, specifier);
    if (!normalized) {
      return {
        ok: false,
        reason: "require expects a relative .lang specifier",
      };
    }
    if (this.knownModules.has(normalized)) {
      return { ok: true, moduleId: normalized };
    }
    return { ok: false, reason: `module not found (${specifier})` };
  }
}

const fileSystemModuleResolver = new FileSystemModuleResolver();

const createFileModuleContext = (filePath?: string): FrontendContext => {
  if (!filePath) {
    return {};
  }
  const moduleId = path.resolve(filePath);
  return {
    moduleId,
    moduleResolver: fileSystemModuleResolver,
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
    const file = Bun.file(options.file);
    if (!(await file.exists())) {
      throw new Error(`Cannot read file: ${options.file}`);
    }
    return file.text();
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
  const parse = await parseSource(sourceText);
  const analysis = analyzeProgram(parse.program, {
    moduleId: mergedContext.moduleId,
    moduleResolver: mergedContext.moduleResolver,
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
      await Bun.write(options.out, codegen.moduleText);
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
    const langFiles = await collectLangFiles(sourceRoot);
    if (langFiles.length === 0) {
      console.error(
        `[vibe][compile-all] No .lang files found under ${sourceRoot}`
      );
      return 1;
    }
    const knownModules = new Set(langFiles.map((file) => path.resolve(file)));
    const workspaceResolver = new WorkspaceModuleResolver(knownModules);
    await mkdir(outDir, { recursive: true });
    let hadFailure = false;
    for (const filePath of langFiles) {
      const moduleContext: FrontendContext = {
        moduleId: path.resolve(filePath),
        moduleResolver: workspaceResolver,
      };
      const frontend = await runFrontend({ file: filePath }, moduleContext);
      if (options.debugMacros) {
        logMacroDebug(frontend.analysis, pretty);
      }
      if (!frontend.ok) {
        hadFailure = true;
        continue;
      }
      const relativeSource = relativeToRoot(sourceRoot, frontend.sourceName);
      const targetFileName = replaceLangExtension(relativeSource);
      const outputPath = path.join(outDir, targetFileName);
      const codegen = generateModule(
        frontend.parse.program,
        frontend.analysis.graph,
        {
          sourceName: relativeSource,
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
      await Bun.write(outputPath, codegen.moduleText);
      console.info(
        `[vibe][compile-all] wrote ${path.relative(process.cwd(), outputPath)}`
      );
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
        const exitCode = await runAnalyze({
          file: opts.file,
          source: opts.source ?? positionalSource,
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
        const exitCode = await runRun({
          file: opts.file,
          source: opts.source ?? positionalSource,
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
        const exitCode = await runCompile({
          file: opts.file,
          source: opts.source ?? positionalSource,
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
