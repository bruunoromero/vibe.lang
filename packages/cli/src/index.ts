import fs from "node:fs";
import path from "node:path";
import { Command } from "commander";
import { lex } from "@vibe/lexer";
import { parse, ParseError, collectInfixDeclarations } from "@vibe/parser";
import {
  analyze,
  SemanticError,
  MultipleSemanticErrors,
  type SemanticModule,
} from "@vibe/semantics";
import { lower, printProgram, IRError, type IRProgram } from "@vibe/ir";
import { loadConfig } from "@vibe/config";
import {
  resolveModule,
  discoverModuleGraph,
  discoverAllModules,
  type DiscoverOptions,
} from "@vibe/module-resolver";
import { generate, writeModule, type GeneratedModule } from "@vibe/codegen";

/**
 * Create discovery options for module graph functions.
 * This provides the parser and infix declaration collector needed
 * to properly handle operator precedence across modules.
 */
function createDiscoverOptions(
  overrides: Partial<DiscoverOptions> = {},
): DiscoverOptions {
  return {
    collectInfixDeclarations,
    parseFunction: parse,
    preferDist: false,
    ...overrides,
  };
}

interface CliOptions {
  cwd?: string;
  stdout?: NodeJS.WritableStream;
  stderr?: NodeJS.WritableStream;
}

interface CommandOptions {
  module?: string;
  config?: string;
  pretty?: string;
  watch?: boolean;
}

interface ExecuteCommandOptions {
  command: string;
  moduleName?: string;
  configPath?: string;
  pretty: number;
  json?: boolean;
}

interface ExecuteOptions {
  cwd: string;
  stdout: NodeJS.WritableStream;
  stderr: NodeJS.WritableStream;
}

let exitCode = 0;
let isHandled = false;

export async function run(
  args: string[],
  options: CliOptions = {},
): Promise<number> {
  const stdout = options.stdout ?? process.stdout;
  const stderr = options.stderr ?? process.stderr;
  const cwd = options.cwd ?? process.cwd();

  const program = new Command();

  program
    .version("0.0.0")
    .description("Vibe language compiler and tooling")
    .option("-c, --config <path>", "Path to vibe.json config file")
    .option(
      "-p, --pretty <n>",
      "Pretty-print JSON output with <n> spaces",
      "2",
    );

  program
    .command("tokenize <module>")
    .description("Tokenize source code")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .action((module: string, opts: CommandOptions) => {
      isHandled = true;
      handleCommand("tokenize", module, { ...opts, cwd, stdout, stderr });
    });

  program
    .command("parse <module>")
    .description("Parse source code into AST")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .action((module: string, opts: CommandOptions) => {
      isHandled = true;
      handleCommand("parse", module, { ...opts, cwd, stdout, stderr });
    });

  program
    .command("analyze <module>")
    .description("Type-check code (requires parsing)")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .action((module: string, opts: CommandOptions) => {
      isHandled = true;
      handleCommand("analyze", module, { ...opts, cwd, stdout, stderr });
    });

  program
    .command("ir <module>")
    .description("Lower to intermediate representation (requires analysis)")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .option("--json", "Output IR as JSON instead of pretty-printed format")
    .action((module: string, opts: CommandOptions & { json?: boolean }) => {
      isHandled = true;
      handleCommand("ir", module, { ...opts, cwd, stdout, stderr });
    });

  program
    .command("build")
    .description("Build all modules in the source directory to JavaScript")
    .option("-w, --watch", "Watch files for changes and rebuild")
    .action((opts: CommandOptions) => {
      isHandled = true;
      handleCommand("build", undefined, { ...opts, cwd, stdout, stderr });
    });

  try {
    await program.parseAsync(args, { from: "user" });
    return isHandled ? exitCode : 0;
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    return 1;
  }
}

function handleCommand(
  command: "tokenize" | "parse" | "analyze" | "ir" | "build",
  module: string | undefined,
  opts: CommandOptions & {
    cwd: string;
    stdout: NodeJS.WritableStream;
    stderr: NodeJS.WritableStream;
    json?: boolean;
  },
): void {
  const {
    cwd,
    stdout,
    stderr,
    config: configPath,
    pretty: prettyStr,
    watch,
    json,
  } = opts;

  let config;
  try {
    config = loadConfig({ cwd, path: configPath });
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    exitCode = 1;
    return;
  }

  const targetModule = module;
  const prettySpaces = Math.max(0, Number.parseInt(prettyStr ?? "2", 10) || 2);

  if (watch) {
    watchMode(
      {
        command,
        moduleName: targetModule,
        configPath,
        pretty: prettySpaces,
        json,
      },
      { cwd, stdout, stderr },
    );
  } else {
    exitCode = executeCommand(
      {
        command,
        moduleName: targetModule,
        configPath,
        pretty: prettySpaces,
        json,
      },
      { cwd, stdout, stderr },
    );
  }
}

function executeCommand(
  opts: ExecuteCommandOptions,
  exec: ExecuteOptions,
): number {
  const { command, moduleName, configPath, pretty, json } = opts;
  const { cwd, stdout, stderr } = exec;

  let config;
  try {
    config = loadConfig({ cwd, path: configPath });
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    return 1;
  }

  try {
    if (command === "tokenize") {
      const resolved = resolveModule({ config, moduleName: moduleName! });
      const source = fs.readFileSync(resolved.filePath, "utf8");
      const tokens = lex(source);
      stdout.write(`${JSON.stringify(tokens, null, pretty)}\n`);
      return 0;
    }

    if (command === "parse") {
      const moduleGraph = discoverModuleGraph(
        config,
        moduleName!,
        createDiscoverOptions(),
      );
      const moduleNode = moduleGraph.modules.get(moduleName!);
      if (!moduleNode) {
        throw new Error(`Module "${moduleName}" not found in graph`);
      }
      stdout.write(`${JSON.stringify(moduleNode.ast, null, pretty)}\n`);
      return 0;
    }

    if (command === "analyze" || command === "ir") {
      const moduleGraph = discoverModuleGraph(
        config,
        moduleName!,
        createDiscoverOptions(),
      );

      const analyzedModules = new Map<string, SemanticModule>();

      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode = moduleGraph.modules.get(currentModuleName);
        if (!moduleNode) {
          throw new Error(`Module "${currentModuleName}" not found in graph`);
        }

        const semantic = analyze(moduleNode.ast, {
          dependencies: analyzedModules,
          fileContext: {
            filePath: moduleNode.filePath,
            srcDir: moduleNode.srcDir,
          },
        });

        analyzedModules.set(currentModuleName, semantic);
      }

      const result = analyzedModules.get(moduleName!);
      if (!result) {
        throw new Error(`Module "${moduleName}" not found in analyzed modules`);
      }

      if (command === "analyze") {
        stdout.write(`${JSON.stringify(result, null, pretty)}\n`);
        return 0;
      }

      // IR command
      const moduleNode = moduleGraph.modules.get(moduleName!);
      if (!moduleNode) {
        throw new Error(`Module "${moduleName}" not found in graph`);
      }

      const ir = lower(moduleNode.ast, result, {
        validateDependencies: true,
        dependencies: analyzedModules,
      });

      if (json) {
        const jsonIr = {
          ...ir,
          externalImports: Array.from(ir.externalImports),
          constraintMetadata: Object.fromEntries(ir.constraintMetadata),
        };
        stdout.write(`${JSON.stringify(jsonIr, null, pretty)}\n`);
      } else {
        const printed = printProgram(ir);
        stdout.write(`${printed}\n`);
      }
      return 0;
    }

    if (command === "build") {
      const moduleGraph = discoverAllModules(config, createDiscoverOptions());

      const analyzedModules = new Map<string, SemanticModule>();

      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode = moduleGraph.modules.get(currentModuleName);
        if (!moduleNode) {
          throw new Error(`Module "${currentModuleName}" not found in graph`);
        }

        const semantic = analyze(moduleNode.ast, {
          dependencies: analyzedModules,
          fileContext: {
            filePath: moduleNode.filePath,
            srcDir: moduleNode.srcDir,
          },
        });

        analyzedModules.set(currentModuleName, semantic);
      }

      const modulePackages = new Map<string, string>();
      for (const [modName, modNode] of moduleGraph.modules) {
        modulePackages.set(modName, modNode.packageName);
      }

      const irPrograms: IRProgram[] = [];

      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode = moduleGraph.modules.get(currentModuleName);
        const semantic = analyzedModules.get(currentModuleName);

        if (!moduleNode || !semantic) {
          throw new Error(`Module "${currentModuleName}" missing data`);
        }

        const ir = lower(moduleNode.ast, semantic, {
          validateDependencies: false,
          packageName: moduleNode.packageName,
          dependencies: analyzedModules,
        });

        irPrograms.push(ir);
      }

      const distDir = config.distDir;
      const generatedFiles: string[] = [];

      for (const ir of irPrograms) {
        const generated = generate(ir, { modulePackages });
        const moduleNode = moduleGraph.modules.get(ir.moduleName);
        const outputPath = writeModule(generated, {
          distDir,
          packageName: generated.packageName,
          sourceFilePath: moduleNode?.filePath,
        });
        generatedFiles.push(outputPath);
        stderr.write(`📦 Built ${generated.moduleName} -> ${outputPath}\n`);
      }

      stdout.write(
        `\n✅ Build complete: ${generatedFiles.length} module(s) compiled to ${distDir}\n`,
      );
      return 0;
    }

    return 0;
  } catch (error) {
    if (error instanceof Error && error.message.includes("call stack")) {
      stderr.write(`${error.stack}\n`);
    }
    formatAndWriteError(error, "", "", stderr);
    return 1;
  }
}

function formatAndWriteError(
  error: unknown,
  fallbackFilePath: string,
  fallbackSource: string,
  stderr: NodeJS.WritableStream,
): void {
  // Handle multiple semantic errors (Elm-style error accumulation)
  if (error instanceof MultipleSemanticErrors) {
    for (const err of error.errors) {
      formatSingleError(err, fallbackFilePath, fallbackSource, stderr);
    }
    return;
  }

  if (
    error instanceof ParseError ||
    error instanceof SemanticError ||
    error instanceof IRError
  ) {
    formatSingleError(error, fallbackFilePath, fallbackSource, stderr);
  } else {
    stderr.write(`${(error as Error).message}\n`);
  }
}

function formatSingleError(
  error: ParseError | SemanticError | IRError,
  fallbackFilePath: string,
  fallbackSource: string,
  stderr: NodeJS.WritableStream,
): void {
  const { span, message } = error;
  const filePath = (error as any).filePath || fallbackFilePath;

  let actualSource = fallbackSource;
  if ((error as any).filePath && (error as any).filePath !== fallbackFilePath) {
    actualSource = fs.readFileSync((error as any).filePath, "utf8");
  }

  const lines = actualSource.split("\n");
  const line = lines[span.start.line - 1] || "";

  stderr.write(
    `${filePath}:${span.start.line}:${span.start.column}: error: ${message}\n`,
  );
  stderr.write(`${line}\n`);

  // Write caret indicator
  const caretPos = span.start.column - 1;
  if (caretPos >= 0) {
    stderr.write(`${" ".repeat(caretPos)}^\n`);
  }
}

function watchMode(opts: ExecuteCommandOptions, exec: ExecuteOptions): never {
  const { cwd, stdout, stderr } = exec;

  let config;
  try {
    config = loadConfig({ cwd, path: opts.configPath });
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    process.exit(1);
  }

  function runBuild(): void {
    executeCommand({ ...opts, command: "build" }, { cwd, stdout, stderr });
  }

  function runSingleModule(): void {
    executeCommand(opts, { cwd, stdout, stderr });
  }

  const isBuild = opts.command === "build";

  try {
    if (isBuild) {
      stderr.write(`👀 Watching ${config.srcDir} for changes...\n`);

      runBuild();

      fs.watch(config.srcDir, { recursive: true }, (eventType, filename) => {
        if (!filename?.endsWith(".vibe")) return;
        if (eventType === "change" || eventType === "rename") {
          stderr.write(
            `\n📝 ${new Date().toLocaleTimeString()} - ${filename} changed\n`,
          );
          runBuild();
        }
      });
    } else {
      const resolved = resolveModule({ config, moduleName: opts.moduleName! });
      const filePath = resolved.filePath;
      const fileDir = path.dirname(filePath);

      stderr.write(`👀 Watching ${filePath} for changes...\n`);

      runSingleModule();

      fs.watch(fileDir, (eventType, filename) => {
        if (filename !== path.basename(filePath)) return;
        if (eventType === "change") {
          stderr.write(
            `\n📝 ${new Date().toLocaleTimeString()} - File changed\n`,
          );
          runSingleModule();
        }
      });
    }

    return new Promise(() => {}) as never;
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    process.exit(1);
  }
}

if (import.meta.main) {
  run(process.argv.slice(2)).then((code) => {
    process.exit(code);
  });
}
