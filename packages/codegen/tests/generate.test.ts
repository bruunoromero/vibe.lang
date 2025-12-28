import { describe, expect, test } from "bun:test";
import { randomUUID } from "node:crypto";
import { join } from "node:path";
import { writeFileSync, unlinkSync, mkdirSync } from "node:fs";
import { parseSource } from "@vibe/parser";
import {
  analyzeProgram,
  type AnalyzeOptions,
  type ModuleResolver,
  type ModuleExportsLookup,
} from "@vibe/semantics";
import { BUILTIN_SYMBOLS } from "@vibe/syntax";
import { keyword_STAR as runtimeKeyword } from "@vibe/runtime";
import { generateModule, type GenerateModuleOptions } from "../src";

const RUNTIME_PRELUDE = `
  (external runtime "@vibe/runtime")

  (def println runtime/println)
`.trim();

const withRuntimePrelude = (source: string) => `${RUNTIME_PRELUDE}\n${source}`;

const PRELUDE_JS = join(process.cwd(), "packages/prelude/src/prelude.js");

const ARITHMETIC_PRELUDE = `
${RUNTIME_PRELUDE}

  (require prelude "${PRELUDE_JS}")

  (def reduce (fn [f init coll]
    (if (runtime/eq* (runtime/count coll) 0)
      init
      (reduce f (f init (prelude/first coll)) (prelude/rest coll)))))

  (def + (fn [& xs]
    (reduce runtime/add* 0 xs)))

  (def - (fn [& xs]
    (let [cnt (runtime/count xs)]
      (if (runtime/eq* cnt 0)
        0
        (if (runtime/eq* cnt 1)
          (runtime/sub* 0 (prelude/first xs))
          (reduce runtime/sub* (prelude/first xs) (prelude/rest xs)))))))

  (def * (fn [& xs]
    (reduce runtime/mul* 1 xs)))

  (def / (fn [& xs]
    (let [cnt (runtime/count xs)]
      (if (runtime/eq* cnt 0)
        0
        (if (runtime/eq* cnt 1)
          (runtime/div* 1 (prelude/first xs))
          (reduce runtime/div* (prelude/first xs) (prelude/rest xs)))))))
`.trim();

const withArithmeticPrelude = (source: string) =>
  `${ARITHMETIC_PRELUDE}\n${source}`;

const compile = async (source: string, options?: AnalyzeOptions) => {
  const parseResult = await parseSource(source);
  if (!parseResult.ok) {
    throw new Error(
      `Parse failed: ${parseResult.diagnostics
        .map((d) => d.message)
        .join("\n")}`
    );
  }
  // Provide a default module resolver and exports lookup for tests so
  // requires of the prelude resolve to the local prelude source.
  const preludeModuleId = join(
    process.cwd(),
    "packages/prelude/src/prelude.lang"
  );
  const defaultResolver: ModuleResolver = {
    resolve: ({ specifier }) => {
      if (
        specifier === "@vibe/prelude" ||
        specifier.startsWith("@vibe/prelude/")
      ) {
        return { ok: true, moduleId: preludeModuleId };
      }
      return { ok: false, reason: "unmapped" };
    },
  };

  const defaultModuleExports: ModuleExportsLookup = {
    getExports: (moduleId: string) => {
      if (moduleId !== preludeModuleId) return undefined;
      try {
        const text = require("fs").readFileSync(preludeModuleId, "utf8");
        const exports: { name: string; kind: "var" | "macro" }[] = [];
        for (const line of text.split(/\r?\n/)) {
          const m = line.match(
            /^\s*\((defmacro|defmacrop|defn|defnp|def)\s+([^\s\)]+)/
          );
          if (m) {
            const kind = m[1].startsWith("defmacro") ? "macro" : "var";
            const name = m[2];
            exports.push({ name, kind });
          }
        }
        return exports.map((e) => ({ name: e.name, kind: e.kind }));
      } catch {
        return undefined;
      }
    },
  };

  const analysis = await analyzeProgram(parseResult.program, {
    builtins: options?.builtins ?? [...BUILTIN_SYMBOLS],
    moduleResolver: options?.moduleResolver ?? defaultResolver,
    moduleExports: options?.moduleExports ?? defaultModuleExports,
    ...options,
  });
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
  // Create a small runtime shim so generated modules that import
  // "@vibe/runtime" can resolve legacy named imports to the
  // current runtime export names without changing source runtime.
  const shimName = "__vibe_runtime_shim.mjs";
  const shimPath = join(tempDir, shimName);
  const runtimeDist = join(process.cwd(), "packages/runtime/dist/index.js");
  const shim = `export * from "file://${runtimeDist}";
import * as R from "file://${runtimeDist}";
export const keyword = R.keyword_STAR;
export const symbol = R.symbol_STAR;
export const get = R.get_STAR;
export const assoc = R.assoc_STAR;
export const dissoc = R.dissoc_STAR;
export const keys = R.keys_STAR;
export const vals = R.vals_STAR;
export const add = R.add_STAR;
export const sub = R.sub_STAR;
export const mul = R.mul_STAR;
export const div = R.div_STAR;
export const mod = R.mod_STAR;
export const eq = R.eq_STAR;
export const lt = R.lt_STAR;
export const gt = R.gt_STAR;
export const lte = R.lte_STAR;
export const gte = R.gte_STAR;
export const symbol_STAR = R.symbol_STAR;
export const keyword_STAR = R.keyword_STAR;
export default R;`;
  writeFileSync(shimPath, shim, "utf8");

  // Rewrite imports of @vibe/runtime (single or double quotes) to the local shim
  const rewritten = code.replace(
    /from\s+['"]@vibe\/runtime['"]/g,
    `from "./${shimName}"`
  );
  writeFileSync(fileName, rewritten, "utf8");
  try {
    const mod = await import(`file://${fileName}`);
    return mod;
  } finally {
    unlinkSync(fileName);
    try {
      unlinkSync(shimPath);
    } catch {}
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

const macroPipelineSource = withArithmeticPrelude(
  `
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
`.trim()
);

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

  test("defp emits private const bindings", async () => {
    const fixture = "(defp hidden 7) (def shown 8) shown";
    const { result, runtime } = await runProgram(fixture, {
      sourceName: "private.lang",
    });

    expect(result.moduleText).toContain("const hidden = 7");
    expect(result.moduleText).not.toContain("export const hidden");
    expect(runtime.hidden).toBeUndefined();
    expect(runtime.shown).toBe(8);
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
    const fixture = withArithmeticPrelude(
      "(def answer (let [x 1 y (+ x 2)] (* y 3)))"
    );
    const { runtime } = await runProgram(fixture, {
      sourceName: "calc.lang",
    });
    expect(runtime.answer).toBe(9);
  });

  test("can export the AST payload", async () => {
    const fixture = withRuntimePrelude("(println :ok)");
    const result = await generateFromSource(fixture, {
      includeAst: true,
      pretty: 0,
    });

    expect(result.moduleText).toContain("export const ast");
    expect(result.moduleText).toContain("println");
  });

  test("emits functions, vectors, sets, and maps", async () => {
    const fixture = withArithmeticPrelude(
      `
      (def builder
        (fn [x]
          (let [nums [x (+ x 1)]]
            {:nums nums
             :unique #{x}
             :echo (println "value" x)})))
      (def result (builder 5))
    `.trim()
    );

    const { runtime } = await runProgram(fixture);
    expect(runtime.result).toBeInstanceOf(Map);
    const record = runtime.result as Map<unknown, unknown>;
    expect(Array.isArray(record.get(runtimeKeyword("nums")))).toBeTrue();
    const unique = record.get(runtimeKeyword("unique"));
    expect(unique).toBeInstanceOf(Set);
    expect((unique as Set<number>).has(5)).toBeTrue();
    expect(record.get(runtimeKeyword("echo"))).toBe(5);
    expect(typeof runtime.builder).toBe("function");
  });

  test("emits destructuring for let bindings and function parameters", async () => {
    const fixture = withRuntimePrelude(
      `
      (def describe
        (fn [[x y & tail :as original]
             {:keys [bonus extra] :or {bonus (runtime/add* x y) extra 5} :as opts}]
          {:original original
           :tail tail
           :bonus bonus
           :extra extra
           :opts opts}))

      (def output
        (let [[a b & rest :as raw] [10 20 30 40]
              {:keys [note] :or {note (runtime/add* a 1)}} {}]
          (describe raw {:bonus note})))

      output
    `.trim()
    );

    const { runtime } = await runProgram(fixture);
    expect(runtime.output).toBeInstanceOf(Map);
    const record = runtime.output as Map<unknown, unknown>;
    expect(record.get(runtimeKeyword("original"))).toEqual([10, 20, 30, 40]);
    expect(record.get(runtimeKeyword("tail"))).toEqual([30, 40]);
    expect(record.get(runtimeKeyword("bonus"))).toBe(11);
    expect(record.get(runtimeKeyword("extra"))).toBe(5);
    const optsEntry = record.get(runtimeKeyword("opts"));
    expect(optsEntry).toBeInstanceOf(Map);
    const opts = optsEntry as Map<unknown, unknown>;
    expect(opts.get(runtimeKeyword("bonus"))).toBe(11);
  });

  test("skips macro bindings inside let expressions", async () => {
    const fixture = `
      (def answer
        (let [local-macro
                (macro
                  ([x] \`(+ ~x ~x)))
              value 21]
          value))
      answer
    `.trim();

    const { runtime } = await runProgram(fixture, {
      sourceName: "let-macro.lang",
    });

    expect(runtime.answer).toBe(21);
  });

  test("emits multi-arity functions with runtime dispatch", async () => {
    const fixture = withArithmeticPrelude(
      `
      (def picker
        (fn
          ([x] 1)
          ([x y] 2)
          ([x y & rest] 3)))
      (def one (picker 5))
      (def two (picker 5 6))
      (def three (picker 5 6 7))
    `.trim()
    );

    const { runtime } = await runProgram(fixture);
    expect(runtime.one).toBe(1);
    expect(runtime.two).toBe(2);
    expect(runtime.three).toBe(3);
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
      (require prelude "@vibe/prelude")
      (external path "node:path")
      (def compute (fn [x] (math/add x 1)))
      (def path-info path)
      path/sep
      path-info/path-separator
      (prelude/get path "path-separator")
      (prelude/println "noop")
      (compute 41)
    `.trim();

    const { program, graph } = await compile(fixture, {
      disableMacroRuntime: true,
    });
    const result = generateModule(program, graph, {
      sourceName: "imports.lang",
      targetFileName: "imports.js",
      sourceContent: fixture,
    });

    expect(result.moduleText).toContain('import * as math from "./math.js";');
    expect(result.moduleText).toContain(
      'import * as prelude from "@vibe/prelude";'
    );
    expect(result.moduleText).toContain('import * as path from "node:path";');
    expect(result.moduleText).toContain("math.add");
    expect(result.moduleText).toContain('path_info["path-separator"]');
    expect(result.moduleText).toContain('prelude.get(path, "path-separator")');
    expect(result.moduleText).not.toContain("export { math };");
    expect(result.moduleText).not.toContain("export { path };");
  });

  test("invokes runtime symbol helpers via namespace access", async () => {
    const fixture = `
      (external runtime "@vibe/runtime")
      (def result
        (let [sym (runtime/symbol* "delta")
              ok (runtime/symbol? sym)
              eq (runtime/eq* sym (runtime/symbol* "delta"))]
          (runtime/str sym ok eq)))
      result
    `.trim();

    const { runtime } = await runProgram(fixture);
    expect(runtime.result).toBe("deltatruetrue");
  });

  test("rewrites package specifiers that point at .lang files", async () => {
    const fixture = `
      (require prelude "@vibe/prelude/src/prelude.lang")
      (def answer (prelude/plus 1 2))
      answer
    `.trim();

    const { program, graph } = await compile(fixture, {
      disableMacroRuntime: true,
    });
    const generated = generateModule(program, graph, {
      sourceName: "package-import.lang",
      targetFileName: "package-import.js",
      sourceContent: fixture,
    });

    expect(generated.moduleText).toContain(
      'import * as prelude from "@vibe/prelude/src/prelude.js";'
    );
  });

  test("import statements flatten bindings in generated modules", async () => {
    const resolver: ModuleResolver = {
      resolve: () => ({ ok: true, moduleId: "/workspace/prelude.lang" }),
    };
    const moduleExports: ModuleExportsLookup = {
      getExports: (moduleId: string) =>
        moduleId === "/workspace/prelude.lang"
          ? [{ name: "frob", kind: "var" as const }]
          : undefined,
    };
    const fixture = `
      (import "./prelude.lang")
      (def use-frob frob)
      use-frob
    `.trim();
    const { program, graph } = await compile(fixture, {
      moduleId: "/workspace/main.lang",
      moduleResolver: resolver,
      moduleExports,
      disableMacroRuntime: true,
    });
    const generated = generateModule(program, graph, {
      sourceName: "flatten.lang",
    });

    expect(generated.moduleText).toContain("import * as __import__");
    expect(generated.moduleText).toContain("const frob = __import__");
    expect(generated.moduleText).toContain("export const use_frob");
  });

  test("multiple import forms allocate unique anonymous aliases", async () => {
    const resolver: ModuleResolver = {
      resolve: ({ specifier }) => {
        if (specifier === "./foo.lang") {
          return { ok: true, moduleId: "/workspace/foo.lang" };
        }
        if (specifier === "./bar.lang") {
          return { ok: true, moduleId: "/workspace/bar.lang" };
        }
        return { ok: false, reason: "unknown module" };
      },
    };
    const moduleExports: ModuleExportsLookup = {
      getExports: (moduleId: string) => {
        if (moduleId === "/workspace/foo.lang") {
          return [{ name: "fooValue", kind: "var" as const }];
        }
        if (moduleId === "/workspace/bar.lang") {
          return [{ name: "barValue", kind: "var" as const }];
        }
        return undefined;
      },
    };

    const fixture = `
      (import "./foo.lang")
      (import "./bar.lang")
      fooValue
      barValue
    `;

    const { program, graph } = await compile(fixture, {
      moduleId: "/workspace/main.lang",
      moduleResolver: resolver,
      moduleExports,
    });
    const generated = generateModule(program, graph, {
      sourceName: "multi-import.lang",
    });

    expect(generated.moduleText).toContain(
      'import * as __import__ from "./foo.js";'
    );
    expect(generated.moduleText).toContain(
      'import * as __import___1 from "./bar.js";'
    );
  });

  test("allows referencing arithmetic operators as values", async () => {
    const fixture = withArithmeticPrelude("(def alias +)\nalias");
    const { runtime } = await runProgram(fixture);
    expect(typeof runtime.alias).toBe("function");
  });

  test("spread operator compiles to JavaScript spread", async () => {
    const fixture = withRuntimePrelude(`
      (def arr [1 2 3])
      (def combined [0 (spread arr) 4])
      combined
    `);
    const { result, runtime } = await runProgram(fixture);
    expect(result.ok).toBeTrue();
    expect(result.moduleText).toContain("...arr");
    expect(runtime.combined).toEqual([0, 1, 2, 3, 4]);
  });

  test("spread works in nested vectors", async () => {
    const fixture = withRuntimePrelude(`
      (def nums [10 20])
      (def more [30 40])
      (def all [(spread nums) 25 (spread more)])
      all
    `);
    const { result, runtime } = await runProgram(fixture);
    expect(result.ok).toBeTrue();
    expect(result.moduleText).toContain("...nums");
    expect(result.moduleText).toContain("...more");
    expect(runtime.all).toEqual([10, 20, 25, 30, 40]);
  });

  test("spread with empty array", async () => {
    const fixture = withRuntimePrelude(`
      (def empty [])
      (def result [1 (spread empty) 2])
      result
    `);
    const { result, runtime } = await runProgram(fixture);
    expect(result.ok).toBeTrue();
    expect(runtime.result).toEqual([1, 2]);
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
});
