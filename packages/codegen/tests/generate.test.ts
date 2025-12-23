import { describe, expect, test } from "bun:test";
import { randomUUID } from "node:crypto";
import { join } from "node:path";
import { writeFileSync, unlinkSync, mkdirSync } from "node:fs";
import { parseSource } from "@vibe/parser";
import { analyzeProgram } from "@vibe/semantics";
import { generateModule, type GenerateModuleOptions } from "../src";

const compile = async (source: string) => {
  const parseResult = await parseSource(source);
  if (!parseResult.ok) {
    throw new Error(
      `Parse failed: ${parseResult.diagnostics
        .map((d) => d.message)
        .join("\n")}`
    );
  }
  const analysis = analyzeProgram(parseResult.program);
  if (!analysis.ok) {
    throw new Error(
      `Analyze failed: ${analysis.diagnostics.map((d) => d.message).join("\n")}`
    );
  }
  return { source, program: parseResult.program, graph: analysis.graph };
};

const evaluateModule = async (code: string) => {
  const tempDir = join(process.cwd(), "tmp");
  mkdirSync(tempDir, { recursive: true });
  const fileName = join(tempDir, `vibe-codegen-${randomUUID()}.mjs`);
  writeFileSync(fileName, code, "utf8");
  try {
    const mod = await import(`file://${fileName}`);
    return mod;
  } finally {
    unlinkSync(fileName);
  }
};

const generateFromSource = async (
  source: string,
  options?: GenerateModuleOptions
) => {
  const { program, graph } = await compile(source);
  return generateModule(program, graph, {
    sourceContent: source,
    ...options,
  });
};

const runProgram = async (source: string, options?: GenerateModuleOptions) => {
  const result = await generateFromSource(source, options);
  const runtime = await evaluateModule(result.moduleText);
  return { result, runtime };
};

const macroPipelineSource = `
  (def build-pipeline
    (fn [seed]
      (fn [value]
        (let [tmp (+ value seed)]
          (fn [extra]
            (let [tmp (+ tmp extra)
                  final (+ tmp seed)]
              (println "macro-stage" final)
              final))))))

  (def total
    (let [stage-a ((build-pipeline 2) 3)
          stage-b ((build-pipeline 1) 4)
          first (stage-a 5)
          second (stage-b 6)]
      (+ first second)))

  total
`.trim();

const macroPipelineOptions: GenerateModuleOptions = {
  sourceName: "snapshots/macros.lang",
  targetFileName: "snapshot-macros.js",
};

describe("generateModule", () => {
  test("exports defs as named bindings", async () => {
    const fixture = "(def foo 1)";
    const { result, runtime } = await runProgram(fixture, {
      sourceName: "repl.lang",
    });

    expect(result.ok).toBeTrue();
    expect(result.ir.stats.symbolCount).toBeGreaterThanOrEqual(1);
    expect(result.moduleText).toContain("export const foo = 1");
    expect(result.moduleText).toContain(
      "//# sourceMappingURL=data:application/json;base64,"
    );

    expect(runtime.foo).toBe(1);
    expect(runtime.default).toBeUndefined();
    expect(result.sourceMap.sources[0]).toBe("repl.lang");
  });

  test("preserves sanitized export names", async () => {
    const fixture = "(def foo-bar 2)";
    const { result, runtime } = await runProgram(fixture, {
      sourceName: "env.lang",
    });
    expect(result.moduleText).toContain("export const foo_bar = 2");
    expect(runtime.foo_bar).toBe(2);
  });

  test("lowers let bindings and arithmetic", async () => {
    const fixture = "(def answer (let [x 1 y (+ x 2)] (* y 3)))";
    const { runtime } = await runProgram(fixture, {
      sourceName: "calc.lang",
    });
    expect(runtime.answer).toBe(9);
  });

  test("can export the AST payload", async () => {
    const fixture = "(println :ok)";
    const result = await generateFromSource(fixture, {
      includeAst: true,
      pretty: 0,
    });

    expect(result.moduleText).toContain("export const ast");
    expect(result.moduleText).toContain("println");
  });

  test("emits functions, vectors, sets, and maps", async () => {
    const fixture = `
      (def builder
        (fn [x]
          (let [nums [x (+ x 1)]]
            {:nums nums
             :unique #{x}
             :echo (println "value" x)})))
      (def result (builder 5))
    `.trim();

    const { runtime } = await runProgram(fixture);
    expect(runtime.result).toBeInstanceOf(Map);
    const record = runtime.result as Map<string, unknown>;
    expect(Array.isArray(record.get("nums"))).toBeTrue();
    const unique = record.get("unique");
    expect(unique).toBeInstanceOf(Set);
    expect((unique as Set<number>).has(5)).toBeTrue();
    expect(record.get("echo")).toBe(5);
    expect(typeof runtime.builder).toBe("function");
  });

  test("sanitizes reserved identifiers and deduplicates collisions", async () => {
    const fixture = `
      (def class 1)
      (def class! 2)
      (def class? 3)
      class?
    `.trim();

    const { result, runtime } = await runProgram(fixture, {
      sourceName: "ident.lang",
    });

    const exportMatches = [
      ...result.moduleText.matchAll(/export const ([a-zA-Z0-9_]+) =/g),
    ].map(([, identifier]) => identifier);

    expect(exportMatches).toContain("_class");
    expect(new Set(exportMatches).size).toBe(exportMatches.length);
    expect(
      exportMatches.map((id) => runtime[id as keyof typeof runtime])
    ).toEqual(expect.arrayContaining([1, 2, 3]));
  });

  test("exposes sanitized identifiers in the IR", async () => {
    const fixture = `
      (def foo 1)
      (let [x foo]
        x)
    `.trim();

    const { result } = await runProgram(fixture);
    const fooSymbol = result.ir.symbols.find((symbol) => symbol.name === "foo");
    expect(fooSymbol?.identifier).toBe("foo");

    const localSymbol = result.ir.symbols.find(
      (symbol) => symbol.name === "x" && symbol.scopeId !== fooSymbol?.scopeId
    );
    expect(localSymbol?.identifier).toMatch(/^x__/);
  });

  test("emits namespace imports and property access", async () => {
    const fixture = `
      (require math "./math.lang")
      (external path "node:path")
      (def compute (fn [x] (math/add x 1)))
      path/sep
      (get path path-separator)
      (compute 41)
    `.trim();

    const result = await generateFromSource(fixture, {
      sourceName: "imports.lang",
      targetFileName: "imports.js",
    });

    expect(result.moduleText).toContain('import * as math from "./math.js";');
    expect(result.moduleText).toContain('import * as path from "node:path";');
    expect(result.moduleText).toContain("math.add");
    expect(result.moduleText).toContain('path["path-separator"]');
    expect(result.moduleText).not.toContain("export { math };");
    expect(result.moduleText).not.toContain("export { path };");
  });
});

describe("generateModule IR summaries", () => {
  test("serializes macro pipeline IR", async () => {
    const result = await generateFromSource(
      macroPipelineSource,
      macroPipelineOptions
    );

    expect(result.ir.stats.symbolCount).toBeGreaterThan(0);
    expect(
      result.ir.symbols.some((symbol) => symbol.name === "total")
    ).toBeTrue();
    expect(result.ir.nodes.length).toBeGreaterThan(0);
  });
});

describe("generateModule errors", () => {
  test("throws when def appears inside an expression", async () => {
    const { program, graph } = await compile("(let [] (def foo 1))");
    expect(() => generateModule(program, graph)).toThrow(
      "def is only supported at the top level"
    );
  });

  test("throws when arithmetic operators are referenced as values", async () => {
    const { program, graph } = await compile("(let [plus +] plus)");
    expect(() => generateModule(program, graph)).toThrow(
      "Cannot reference arithmetic operator + as a value yet"
    );
  });
});
