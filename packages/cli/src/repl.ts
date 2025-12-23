import { parseSource } from "@vibe/parser";
import { BUILTIN_SYMBOLS } from "@vibe/syntax";
import { analyzeProgram } from "@vibe/semantics";
import { generateModule, type GenerateModuleOptions } from "@vibe/codegen";
import type { Diagnostic } from "@vibe/syntax";
import { writeFileSync, unlinkSync, mkdirSync } from "node:fs";
import { join } from "node:path";
import { randomUUID } from "node:crypto";

export type ReplIO = {
  readLine?: () => Promise<string | null>;
  writeOut?: (s: string) => void;
  writeErr?: (s: string) => void;
};

const DEFAULT_PROMPT = "vibe> ";
const CONT_PROMPT = "... ";

const UNTERMINATED_CODES = new Set([
  "PARSE_LIST_UNTERMINATED",
  "PARSE_VECTOR_UNTERMINATED",
  "PARSE_MAP_UNTERMINATED",
  "PARSE_SET_UNTERMINATED",
  "PARSE_MACRO_MISSING_TARGET",
]);

const formatDiagnostic = (diagnostic: Diagnostic): string => {
  const { line, column } = diagnostic.span.start;
  const position = `${line + 1}:${column + 1}`;
  return (
    `${diagnostic.severity.toUpperCase()} ${diagnostic.code ?? ""}`.trim() +
    ` ${position} ${diagnostic.message}`
  );
};

const emitDiagnostics = (
  diagnostics: readonly Diagnostic[],
  writeErr: (s: string) => void
) => {
  if (diagnostics.length === 0) return;
  for (const d of diagnostics) {
    writeErr(formatDiagnostic(d));
  }
};

const executeGeneratedModule = async (code: string): Promise<unknown> => {
  const tempDir = join(process.cwd(), "tmp");
  mkdirSync(tempDir, { recursive: true });
  const fileName = join(tempDir, `vibe-repl-${randomUUID()}.mjs`);
  writeFileSync(fileName, code, "utf8");
  try {
    const mod = await import(`file://${fileName}`);
    return mod.default ?? mod;
  } finally {
    try {
      unlinkSync(fileName);
    } catch {
      // ignore cleanup errors
    }
  }
};

export const runRepl = async (
  opts: {
    pretty?: number;
    debugMacros?: boolean;
  } & ReplIO = {}
): Promise<void> => {
  const pretty = opts.pretty ?? 2;
  const debugMacros = opts.debugMacros ?? false;
  const readLine = opts.readLine;
  const writeOut = opts.writeOut ?? ((s: string) => console.log(s));
  const writeErr = opts.writeErr ?? ((s: string) => console.error(s));

  // If no readLine provided, use Node/Bun readline and show continuation prompts correctly
  let rl: any | null = null;
  if (!readLine) {
    // Lazy require so tests in Bun can import this file without side effects
    // eslint-disable-next-line @typescript-eslint/no-var-requires
    const readline = require("node:readline");
    rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
    });
  }

  const prompt = async (isContinuation: boolean) => {
    if (rl) {
      return new Promise<string | null>((resolve) => {
        rl.question(
          isContinuation ? CONT_PROMPT : DEFAULT_PROMPT,
          (line: string) => resolve(line)
        );
      });
    }
    // If a custom readLine was provided (tests), delegate to it
    return null;
  };

  const readNextLine = async (
    isContinuation = false
  ): Promise<string | null> => {
    if (readLine) return readLine();
    return prompt(isContinuation);
  };

  let buffer = "";
  let history = "";
  let resultCounter = 0;

  // Helper that checks whether parse produced a continuation-worthy diagnostic
  const isIncompleteParse = (
    parseResult: Awaited<ReturnType<typeof parseSource>>
  ) => {
    return parseResult.diagnostics.some(
      (d) => (d.code && UNTERMINATED_CODES.has(d.code)) ?? false
    );
  };

  try {
    while (true) {
      const isContinuation = buffer.length > 0;
      const line = await readNextLine(isContinuation);
      if (line === null) {
        // EOF
        break;
      }
      const trimmed = line.trim();
      if (trimmed === "" && !isContinuation) {
        continue;
      }
      if (trimmed === "exit" || trimmed === ".exit") {
        break;
      }

      buffer += (buffer.length > 0 ? "\n" : "") + line;

      const parse = await parseSource(buffer);
      if (isIncompleteParse(parse)) {
        // need more lines
        continue;
      }

      // If parse had diagnostics (errors), print and discard this statement (do not add to history)
      emitDiagnostics(parse.diagnostics, writeErr);
      if (!parse.ok) {
        buffer = "";
        continue;
      }

      // Candidate history including the new buffer but do not commit until evaluation succeeds
      const candidateHistory =
        history + (history.length > 0 ? "\n" : "") + buffer;

      // Build an eval source that assigns the last expression to a unique temporary binding
      resultCounter += 1;
      const resultName = `__repl_result_${resultCounter}`;

      // Determine last expression text by reparsing the candidate history and locating the last node span
      const fullParse = await parseSource(candidateHistory);
      if (!fullParse.ok || fullParse.program.body.length === 0) {
        buffer = "";
        continue;
      }
      const lastNode =
        fullParse.program.body[fullParse.program.body.length - 1]!;
      const start = lastNode.span.start.offset;
      const end = lastNode.span.end.offset;
      const lastExprText = candidateHistory.slice(start, end);

      // Debug print of lastNode when debugging is enabled
      if (process.env.LANG_REPL_DEBUG) {
        try {
          writeErr(
            "[vibe][repl] lastNode:" + JSON.stringify(lastNode, null, 2)
          );
        } catch {}
      }

      // If the last expression is a top-level def like `(def a ...)`, don't
      // nest a `def` inside another `def` (which would make `(def x (def a ...))`).
      // Instead capture the symbol (e.g., `a`) as the value to return.
      let captureExpr = lastExprText;
      try {
        if (lastNode.kind === "list") {
          const head = (lastNode as any).elements[0];
          const second = (lastNode as any).elements[1];
          if (
            head &&
            head.kind === "symbol" &&
            head.value === "def" &&
            second &&
            second.kind === "symbol"
          ) {
            captureExpr = second.value;
          }
        }
      } catch {
        captureExpr = lastExprText;
      }

      if (process.env.LANG_REPL_DEBUG) {
        writeErr("[vibe][repl] captureExpr: " + captureExpr);
      }

      // If the last expression is a bare symbol that is a builtin/special form,
      // don't attempt to evaluate it as a value — print a friendly message and
      // discard the statement. This avoids the codegen error that arises when
      // referencing builtins as values.
      const builtinSet = new Set(BUILTIN_SYMBOLS as readonly string[]);
      if (lastNode.kind === "symbol") {
        const sym = (lastNode as any).value;
        if (builtinSet.has(sym)) {
          writeErr(
            `ERROR: cannot evaluate builtin symbol '${String(sym)}' as a value`
          );
          buffer = "";
          continue;
        }
      }

      const evalSource =
        candidateHistory + `\n(def ${resultName} ${captureExpr})`;

      // Debug hook: output evalSource when LANG_REPL_DEBUG is set
      if (process.env.LANG_REPL_DEBUG) {
        writeErr("[vibe][repl] evalSource:\n" + evalSource);
      }

      const evalParse = await parseSource(evalSource);
      emitDiagnostics(evalParse.diagnostics, writeErr);
      if (!evalParse.ok) {
        buffer = "";
        continue;
      }

      const evalAnalysis = analyzeProgram(evalParse.program);
      emitDiagnostics(evalAnalysis.diagnostics, writeErr);
      if (!evalAnalysis.ok) {
        buffer = "";
        continue;
      }

      const codegen = generateModule(evalParse.program, evalAnalysis.graph, {
        sourceName: "repl.lang",
        sourceContent: evalSource,
      });

      emitDiagnostics(codegen.diagnostics, writeErr);
      if (!codegen.ok) {
        buffer = "";
        continue;
      }

      // Execute and print the result
      const runtime = await executeGeneratedModule(codegen.moduleText);
      const value = (runtime as any)[resultName];

      // Commit the candidate into history only after successful execution
      history = candidateHistory;

      // Safe serializer for REPL output that pretty-prints functions, errors,
      // and avoids failing on circular structures.
      const serializeResult = (val: unknown): string => {
        const seen = new WeakSet();
        const replacer = (_key: string, v: unknown) => {
          if (typeof v === "function") {
            const fn = v as Function;
            return `<fn ${fn.name || "anonymous"} arity=${fn.length}>`;
          }
          if (v instanceof Error) {
            return { __error: v.message, stack: v.stack };
          }
          if (v && typeof v === "object") {
            // WeakSet only accepts objects
            try {
              if (seen.has(v as object)) return "[Circular]";
              seen.add(v as object);
            } catch {
              // ignore failures for non-extensible objects
            }
          }
          return v;
        };
        try {
          return JSON.stringify({ result: val }, replacer, pretty);
        } catch (e) {
          try {
            return JSON.stringify({ result: String(val) }, null, pretty);
          } catch {
            return String(val);
          }
        }
      };

      writeOut(serializeResult(value));

      // Optionally log macro metadata
      if (debugMacros) {
        // dump analysis.graph subset for macros to stderr
        const macroSymbols = evalAnalysis.graph.symbols.filter(
          (s) => s.kind === "macro"
        );
        if (macroSymbols.length > 0) {
          writeErr("[vibe][macros] macros present in session");
        }
      }

      buffer = "";
    }
  } finally {
    if (rl) {
      rl.close();
    }
  }
};
