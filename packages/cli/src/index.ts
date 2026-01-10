import fs from "node:fs";
import path from "node:path";
import { Command } from "commander";
import { lex } from "@vibe/lexer";
import { parse, ParseError } from "@vibe/parser";
import { analyze, SemanticError, type SemanticModule } from "@vibe/semantics";
import { loadConfig } from "@vibe/config";
import { resolveModule, discoverModuleGraph } from "@vibe/module-resolver";

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
  moduleName: string;
  configPath?: string;
  pretty: number;
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
  options: CliOptions = {}
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
      "2"
    );

  program
    .command("tokenize [module]")
    .description("Tokenize source code")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .action((module: string | undefined, opts: CommandOptions) => {
      isHandled = true;
      handleCommand("tokenize", module, { ...opts, cwd, stdout, stderr });
    });

  program
    .command("parse [module]")
    .description("Parse source code into AST")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .action((module: string | undefined, opts: CommandOptions) => {
      isHandled = true;
      handleCommand("parse", module, { ...opts, cwd, stdout, stderr });
    });

  program
    .command("analyze [module]")
    .description("Type-check code (requires parsing)")
    .option("-w, --watch", "Watch file for changes and re-run command")
    .action((module: string | undefined, opts: CommandOptions) => {
      isHandled = true;
      handleCommand("analyze", module, { ...opts, cwd, stdout, stderr });
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
  command: "tokenize" | "parse" | "analyze",
  module: string | undefined,
  opts: CommandOptions & {
    cwd: string;
    stdout: NodeJS.WritableStream;
    stderr: NodeJS.WritableStream;
  }
): void {
  const {
    cwd,
    stdout,
    stderr,
    config: configPath,
    pretty: prettyStr,
    watch,
  } = opts;

  let config;
  try {
    config = loadConfig({ cwd, path: configPath });
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    exitCode = 1;
    return;
  }

  const targetModule = module ?? config.name;
  const prettySpaces = Math.max(0, Number.parseInt(prettyStr ?? "2", 10) || 2);

  if (watch) {
    watchMode(
      { command, moduleName: targetModule, configPath, pretty: prettySpaces },
      { cwd, stdout, stderr }
    );
  } else {
    exitCode = executeCommand(
      {
        command,
        moduleName: targetModule,
        configPath,
        pretty: prettySpaces,
      } as ExecuteCommandOptions,
      { cwd, stdout, stderr }
    );
  }
}

function executeCommand(
  opts: ExecuteCommandOptions,
  exec: ExecuteOptions
): number {
  const { command, moduleName, configPath, pretty } = opts;
  const { cwd, stdout, stderr } = exec;

  let config;
  try {
    config = loadConfig({ cwd, path: configPath });
  } catch (error) {
    stderr.write(`${(error as Error).message}\n`);
    return 1;
  }

  let filePath = "";
  let source = "";

  try {
    const resolved = resolveModule({ config, moduleName });
    filePath = resolved.filePath;
    source = fs.readFileSync(filePath, "utf8");

    if (command === "tokenize") {
      const tokens = lex(source);
      stdout.write(`${JSON.stringify(tokens, null, pretty)}\n`);
      return 0;
    }

    if (command === "parse") {
      const ast = parse(source);
      stdout.write(`${JSON.stringify(ast, null, pretty)}\n`);
      return 0;
    }

    if (command === "analyze") {
      const ast = parse(source);

      // Discover all modules and their dependencies, sorted topologically
      const moduleGraph = discoverModuleGraph(config, moduleName, parse);

      // Analyze modules in topological order
      const analyzedModules = new Map<string, SemanticModule>();

      for (const currentModuleName of moduleGraph.sortedModuleNames) {
        const moduleNode = moduleGraph.modules.get(currentModuleName);
        if (!moduleNode) {
          throw new Error(`Module "${currentModuleName}" not found in graph`);
        }

        // Analyze with pre-analyzed dependencies
        const semantic = analyze(moduleNode.ast, {
          dependencies: analyzedModules,
        });

        analyzedModules.set(currentModuleName, semantic);
      }

      // Return the result for the requested module
      const result = analyzedModules.get(moduleName);
      if (!result) {
        throw new Error(`Module "${moduleName}" not found in analyzed modules`);
      }

      stdout.write(`${JSON.stringify(result, null, pretty)}\n`);
      return 0;
    }

    return 0;
  } catch (error) {
    formatAndWriteError(error, filePath, source, stderr);
    return 1;
  }
}

function formatAndWriteError(
  error: unknown,
  filePath: string,
  source: string,
  stderr: NodeJS.WritableStream
): void {
  if (error instanceof ParseError || error instanceof SemanticError) {
    const { span, message } = error;
    const lines = source.split("\n");
    const line = lines[span.start.line - 1] || "";

    stderr.write(
      `${filePath}:${span.start.line}:${span.start.column}: error: ${message}\n`
    );
    stderr.write(`${line}\n`);

    // Write caret indicator
    const caretPos = span.start.column - 1;
    if (caretPos >= 0) {
      stderr.write(`${" ".repeat(caretPos)}^\n`);
    }
  } else {
    stderr.write(`${(error as Error).message}\n`);
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

  try {
    const resolved = resolveModule({ config, moduleName: opts.moduleName });
    const filePath = resolved.filePath;
    const fileDir = path.dirname(filePath);

    stderr.write(`👀 Watching ${filePath} for changes...\n`);

    const watcher = fs.watch(fileDir, (eventType, filename) => {
      // Only watch the specific file we're interested in
      if (filename !== path.basename(filePath)) {
        return;
      }

      if (eventType === "change") {
        stderr.write(
          `\n📝 ${new Date().toLocaleTimeString()} - File changed\n`
        );

        try {
          const watchSource = fs.readFileSync(filePath, "utf8");

          if (opts.command === "analyze") {
            // Use topological analysis in watch mode too
            const moduleGraph = discoverModuleGraph(
              config,
              opts.moduleName,
              parse
            );
            const analyzedModules = new Map<string, SemanticModule>();

            for (const currentModuleName of moduleGraph.sortedModuleNames) {
              const moduleNode = moduleGraph.modules.get(currentModuleName);
              if (!moduleNode) {
                throw new Error(
                  `Module "${currentModuleName}" not found in graph`
                );
              }

              const semantic = analyze(moduleNode.ast, {
                dependencies: analyzedModules,
              });

              analyzedModules.set(currentModuleName, semantic);
            }

            stderr.write(`✅ Type checking passed\n`);
          } else if (opts.command === "parse") {
            parse(watchSource);
            stderr.write(`✅ Parsing successful\n`);
          } else if (opts.command === "tokenize") {
            lex(watchSource);
            stderr.write(`✅ Tokenization successful\n`);
          }
        } catch (error) {
          stderr.write(`❌ Error detected:\n`);
          const watchErrorSource = fs.readFileSync(filePath, "utf8");
          formatAndWriteError(error, filePath, watchErrorSource ?? "", stderr);
        }
      }
    });

    // Run once initially
    try {
      const initialSource = fs.readFileSync(filePath, "utf8");

      if (opts.command === "analyze") {
        // Use topological analysis for initial run too
        const moduleGraph = discoverModuleGraph(config, opts.moduleName, parse);
        const analyzedModules = new Map<string, SemanticModule>();

        for (const currentModuleName of moduleGraph.sortedModuleNames) {
          const moduleNode = moduleGraph.modules.get(currentModuleName);
          if (!moduleNode) {
            throw new Error(`Module "${currentModuleName}" not found in graph`);
          }

          const semantic = analyze(moduleNode.ast, {
            dependencies: analyzedModules,
          });

          analyzedModules.set(currentModuleName, semantic);
        }

        stderr.write(`✅ Initial type checking passed\n`);
      } else if (opts.command === "parse") {
        parse(initialSource);
        stderr.write(`✅ Initial parsing successful\n`);
      } else if (opts.command === "tokenize") {
        lex(initialSource);
        stderr.write(`✅ Initial tokenization successful\n`);
      }
    } catch (error) {
      stderr.write(`❌ Initial error detected:\n`);
      const initialErrorSource = fs.readFileSync(filePath, "utf8");
      formatAndWriteError(error, filePath, initialErrorSource ?? "", stderr);
    }

    // Keep the process alive
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
