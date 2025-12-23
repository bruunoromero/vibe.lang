import { describe, expect, test } from "bun:test";
import {
  analyzeProgram,
  type AnalyzeOptions,
  type ModuleResolver,
} from "../src";
import { parseSource } from "@vibe/parser";
import { NodeKind } from "@vibe/syntax";

const analyzeSource = async (source: string, options?: AnalyzeOptions) => {
  const parseResult = await parseSource(source);
  if (!parseResult.ok) {
    const messages = parseResult.diagnostics.map((d) => d.message).join("\n");
    throw new Error(`Failed to parse fixture: ${messages}`);
  }
  return analyzeProgram(parseResult.program, options);
};

type AnalysisResult = Awaited<ReturnType<typeof analyzeSource>>;

const getDiagnosticCodes = (analysis: AnalysisResult) =>
  analysis.diagnostics.map((diagnostic) => diagnostic.code);

const expectDiagnostic = (analysis: AnalysisResult, code: string) => {
  expect(getDiagnosticCodes(analysis)).toContain(code);
};

describe("analyzeProgram", () => {
  test("aligns semantic scopes with parser annotations", async () => {
    const parseResult = await parseSource("(let [x 1] (fn [y] y) x)");
    if (!parseResult.ok) {
      throw new Error("Failed to parse scope fixture");
    }

    const program = parseResult.program;
    const letNode = program.body[0];
    if (!letNode || letNode.kind !== NodeKind.List) {
      throw new Error("Expected let node");
    }
    const bindingsVector = letNode.elements[1];
    if (!bindingsVector || bindingsVector.kind !== NodeKind.Vector) {
      throw new Error("Expected bindings vector");
    }
    const bindingSymbol = bindingsVector.elements[0];
    if (!bindingSymbol || bindingSymbol.kind !== NodeKind.Symbol) {
      throw new Error("Expected binding symbol");
    }

    const parserScopeId = bindingSymbol.scopeId;
    expect(parserScopeId).toBeDefined();

    const analysis = analyzeProgram(program);
    expect(analysis.ok).toBeTrue();

    const semanticScope = analysis.graph.scopes.find(
      (scope) => scope.id === parserScopeId
    );
    expect(semanticScope).toBeDefined();

    const usageNode = analysis.graph.nodes.find(
      (node) =>
        node.symbol?.name === "x" &&
        node.symbol.role === "usage" &&
        node.scopeId === parserScopeId
    );
    expect(usageNode).toBeDefined();
  });

  test("links definitions with usages", async () => {
    const analysis = await analyzeSource("(def foo 1) foo");

    expect(analysis.ok).toBeTrue();
    const fooSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "foo"
    );
    expect(fooSymbol?.kind).toBe("var");

    const definitionNode = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "foo" && node.symbol.role === "definition"
    );
    const usageNode = analysis.graph.nodes.find(
      (node) =>
        node.symbol?.name === "foo" &&
        node.symbol.role === "usage" &&
        node !== definitionNode
    );

    expect(definitionNode?.symbol?.symbolId).toBe(fooSymbol?.id);
    expect(usageNode?.symbol?.symbolId).toBe(fooSymbol?.id);
  });

  test("records top-level defs as module exports", async () => {
    const analysis = await analyzeSource("(def foo 1) (def bar 2)");

    expect(analysis.graph.exports).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ name: "foo", identifier: "foo" }),
        expect.objectContaining({ name: "bar", identifier: "bar" }),
      ])
    );
  });

  test("creates scoped bindings for let and fn", async () => {
    const analysis = await analyzeSource("(let [x 1] (fn [y] (+ x y)))");

    expect(analysis.ok).toBeTrue();
    expect(analysis.graph.scopes.length).toBeGreaterThan(1);

    const parameterNode = analysis.graph.nodes.find(
      (node) => node.symbol?.role === "parameter" && node.symbol.name === "y"
    );
    expect(parameterNode).toBeDefined();

    const letBindingNode = analysis.graph.nodes.find(
      (node) => node.symbol?.role === "definition" && node.symbol.name === "x"
    );
    expect(letBindingNode).toBeDefined();

    expect(parameterNode?.scopeId).not.toBe(letBindingNode?.scopeId);
  });

  test("reports unresolved symbols", async () => {
    const analysis = await analyzeSource("(+ mystery)");

    expect(analysis.ok).toBeFalse();
    expect(analysis.diagnostics.map((d) => d.code)).toContain(
      "SEM_UNRESOLVED_SYMBOL"
    );
  });

  test("attaches spans to unresolved symbol diagnostics", async () => {
    const analysis = await analyzeSource("(+ mystery)");

    const diagnostic = analysis.diagnostics.find(
      (d) => d.code === "SEM_UNRESOLVED_SYMBOL"
    );
    const mysteryUsage = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "mystery" && node.symbol.role === "usage"
    );

    expect(diagnostic).toBeDefined();
    expect(mysteryUsage).toBeDefined();
    expect(diagnostic?.span).toEqual(mysteryUsage?.span);
  });

  test("resolves shadowed bindings to the innermost scope", async () => {
    const analysis = await analyzeSource(`
      (def x 0)
      (let [x 1]
        x)
      x
    `);

    expect(analysis.ok).toBeTrue();
    const rootScopeId = analysis.graph.scopes.find(
      (scope) => scope.parentId === null
    )?.id;
    expect(rootScopeId).toBeDefined();

    const globalSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "x" && symbol.scopeId === rootScopeId
    );
    const innerSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "x" && symbol.scopeId !== rootScopeId
    );

    expect(globalSymbol).toBeDefined();
    expect(innerSymbol).toBeDefined();

    const innerUsage = analysis.graph.nodes.find(
      (node) =>
        node.symbol?.name === "x" &&
        node.symbol.role === "usage" &&
        node.scopeId === innerSymbol?.scopeId
    );
    const outerUsage = analysis.graph.nodes.find(
      (node) =>
        node.symbol?.name === "x" &&
        node.symbol.role === "usage" &&
        node.scopeId === rootScopeId
    );

    expect(innerUsage?.symbol?.symbolId).toBe(innerSymbol?.id);
    expect(outerUsage?.symbol?.symbolId).toBe(globalSymbol?.id);
  });

  test("resolves identifiers across deeply nested scopes", async () => {
    const analysis = await analyzeSource(`
      (def root 0)
      (let [outer 1]
        (fn [mid]
          (let [inner (+ outer mid root)]
            (fn [leaf]
              (+ root outer mid inner leaf)))))
    `);

    expect(analysis.ok).toBeTrue();

    const getSymbol = (name: string, kind?: string) =>
      analysis.graph.symbols.find(
        (symbol) => symbol.name === name && (kind ? symbol.kind === kind : true)
      );

    const rootSymbol = getSymbol("root");
    const outerSymbol = getSymbol("outer");
    const innerSymbol = getSymbol("inner");
    const midSymbol = getSymbol("mid", "parameter");
    const leafSymbol = getSymbol("leaf", "parameter");

    if (
      !rootSymbol ||
      !outerSymbol ||
      !innerSymbol ||
      !midSymbol ||
      !leafSymbol
    ) {
      throw new Error("Missing expected symbol in nested scope fixture");
    }

    const expectUsagesToReference = (name: string, symbolId: string) => {
      const usages = analysis.graph.nodes.filter(
        (node) => node.symbol?.name === name && node.symbol.role === "usage"
      );
      expect(usages.length).toBeGreaterThan(0);
      for (const usage of usages) {
        expect(usage.symbol?.symbolId).toBe(symbolId);
      }
    };

    expectUsagesToReference("root", rootSymbol.id);
    expectUsagesToReference("outer", outerSymbol.id);
    expectUsagesToReference("inner", innerSymbol.id);
    expectUsagesToReference("mid", midSymbol.id);
    expectUsagesToReference("leaf", leafSymbol.id);
  });

  test("emits alias metadata for roots and locals", async () => {
    const analysis = await analyzeSource(`
      (def foo-bar 1)
      (def class 2)
      (let [tmp-value foo-bar]
        tmp-value)
    `);

    expect(analysis.ok).toBeTrue();

    const fooSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "foo-bar"
    );
    const classSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "class"
    );
    const tmpSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "tmp-value" && symbol.kind === "var"
    );

    expect(fooSymbol?.alias).toBe("foo_bar");
    expect(classSymbol?.alias).toBe("_class");
    expect(tmpSymbol?.alias).toMatch(/^tmp_value__symbol_/);

    const tmpUsage = analysis.graph.nodes.find(
      (node) =>
        node.symbol?.name === "tmp-value" && node.symbol.role === "usage"
    );
    expect(tmpUsage?.symbol?.symbolId).toBe(tmpSymbol?.id);
  });

  test("duplicate symbols shadow within inner scope", async () => {
    const analysis = await analyzeSource(`
      (let [outer 1]
        (let [inner 2 inner 3]
          inner)
        outer)
    `);

    // No duplicate symbol diagnostic; shadowing is allowed.
    expect(getDiagnosticCodes(analysis)).not.toContain("SEM_DUPLICATE_SYMBOL");

    // The inner usage should resolve to a defined symbol id.
    const innerUsage = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "inner" && node.symbol.role === "usage"
    );
    expect(innerUsage?.symbol?.symbolId).toBeDefined();

    const outerSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "outer"
    );
    expect(outerSymbol).toBeDefined();

    const outerUsage = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "outer" && node.symbol.role === "usage"
    );
    expect(outerUsage?.symbol?.symbolId).toBe(outerSymbol?.id);
  });

  test("records gensym-generated bindings with unique names", async () => {
    const analysis = await analyzeSource(`
      (defmacro with-unique [expr]
        \`(let [~(gensym "tmp") ~expr]
           42))

      (with-unique 10)
    `);

    expect(analysis.ok).toBeTrue();
    const generatedSymbol = analysis.graph.symbols.find((symbol) =>
      symbol.name.startsWith("tmp__")
    );
    expect(generatedSymbol).toBeDefined();

    const definitionNode = analysis.graph.nodes.find(
      (node) => node.symbol?.symbolId === generatedSymbol?.id
    );
    expect(definitionNode?.symbol?.role).toBe("definition");
  });

  test("assigns unique hygiene tags to macro-generated bindings", async () => {
    const analysis = await analyzeSource(`
      (defmacro with-temp [value]
        \`(let [tmp ~value]
           tmp))

      (let [a (with-temp 1)
            b (with-temp 2)]
        (+ a b))
    `);

    expect(analysis.ok).toBeTrue();
    expect(getDiagnosticCodes(analysis)).not.toContain("SEM_DUPLICATE_SYMBOL");

    const tmpSymbols = analysis.graph.symbols.filter(
      (symbol) => symbol.name === "tmp" && symbol.kind === "var"
    );
    expect(tmpSymbols.length).toBe(2);

    const scopeIds = new Set(tmpSymbols.map((symbol) => symbol.scopeId));
    const hygieneTags = new Set(tmpSymbols.map((symbol) => symbol.hygieneTag));

    expect(scopeIds.size).toBe(tmpSymbols.length);
    expect(hygieneTags.size).toBe(tmpSymbols.length);
  });

  test("expands macros and analyzes introduced bindings", async () => {
    const analysis = await analyzeSource(`
      (defmacro with-temp [expr]
        \`(let [tmp ~expr]
           (println tmp)
           tmp))

      (def answer (with-temp (+ 1 2)))
    `);

    expect(analysis.ok).toBeTrue();
    const tmpDefinition = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "tmp" && node.symbol.role === "definition"
    );
    expect(tmpDefinition).toBeDefined();

    const answerSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "answer"
    );
    expect(answerSymbol?.kind).toBe("var");
  });

  test("detects recursive macros", async () => {
    const analysis = await analyzeSource(`
      (defmacro looped [] \`(looped))
      (looped)
    `);

    expect(analysis.ok).toBeFalse();
    expect(analysis.diagnostics.map((d) => d.code)).toContain(
      "SEM_MACRO_RECURSION"
    );
  });

  describe("special form diagnostics", () => {
    test("reports when let bindings are not provided as a vector", async () => {
      const analysis = await analyzeSource("(let 42 1)");

      expectDiagnostic(analysis, "SEM_LET_EXPECTS_VECTOR");
    });

    test("reports odd numbers of let binding forms", async () => {
      const analysis = await analyzeSource("(let [x 1 y] y)");

      expectDiagnostic(analysis, "SEM_LET_ODD_BINDINGS");
    });

    test("requires binding targets inside let to be symbols", async () => {
      const analysis = await analyzeSource("(let [42 1] 42)");

      expectDiagnostic(analysis, "SEM_BINDING_REQUIRES_SYMBOL");
    });

    test("requires fn parameters to be declared via vectors", async () => {
      const analysis = await analyzeSource("(fn 42 1)");

      expectDiagnostic(analysis, "SEM_FN_EXPECTS_VECTOR");
    });

    test("requires fn parameters to be symbols", async () => {
      const analysis = await analyzeSource("(fn [42] 42)");

      expectDiagnostic(analysis, "SEM_BINDING_REQUIRES_SYMBOL");
    });

    test("allows duplicate symbols in the same scope by shadowing", async () => {
      const analysis = await analyzeSource("(let [x 1 x 2] x)");

      // Redefinition is allowed and should not produce a duplicate-symbol error
      expect(getDiagnosticCodes(analysis)).not.toContain(
        "SEM_DUPLICATE_SYMBOL"
      );
      // The final usage should resolve to a defined symbol
      const usage = analysis.graph.nodes.find(
        (n) => n.symbol?.name === "x" && n.symbol.role === "usage"
      );
      expect(usage?.symbol?.symbolId).toBeDefined();
    });
  });

  describe("defmacro validation", () => {
    test("requires macro names to be symbols", async () => {
      const analysis = await analyzeSource("(defmacro 1 [] `42)");

      expectDiagnostic(analysis, "SEM_MACRO_REQUIRES_SYMBOL");
    });

    test("requires macro parameters to be provided via vectors", async () => {
      const analysis = await analyzeSource("(defmacro foo 1 `42)");

      expectDiagnostic(analysis, "SEM_MACRO_EXPECTS_VECTOR");
    });

    test("requires macro parameters to be symbols", async () => {
      const analysis = await analyzeSource("(defmacro foo [1] `42)");

      expectDiagnostic(analysis, "SEM_MACRO_PARAM_SYMBOL");
    });

    test("reports duplicate macro parameters", async () => {
      const analysis = await analyzeSource("(defmacro foo [x x] `(list ~x))");

      expectDiagnostic(analysis, "SEM_MACRO_DUPLICATE_PARAM");
    });

    test("requires macro bodies", async () => {
      const analysis = await analyzeSource("(defmacro foo [x])");

      expectDiagnostic(analysis, "SEM_MACRO_REQUIRES_BODY");
    });

    test("requires macro bodies to be syntax quoted", async () => {
      const analysis = await analyzeSource("(defmacro foo [x] (list x))");

      expectDiagnostic(analysis, "SEM_MACRO_EXPECTS_SYNTAX_QUOTE");
    });

    test("supports only a single macro body expression", async () => {
      const analysis = await analyzeSource(
        "(defmacro foo [x] `(list ~x) `(list ~x))"
      );

      expectDiagnostic(analysis, "SEM_MACRO_SINGLE_BODY");
    });
  });

  describe("macro expansion diagnostics", () => {
    test("reports macro arity mismatches and missing args", async () => {
      const analysis = await analyzeSource(`
        (defmacro pair [a b] \`(vector ~a ~b))
        (pair 1)
      `);

      expectDiagnostic(analysis, "SEM_MACRO_ARITY_MISMATCH");
      expectDiagnostic(analysis, "SEM_MACRO_ARG_MISSING");
    });

    test("reports unknown parameters referenced via unquote", async () => {
      const analysis = await analyzeSource(`
        (defmacro uses-missing [] \`(~ missing))
        (uses-missing)
      `);

      expectDiagnostic(analysis, "SEM_MACRO_UNKNOWN_PARAM");
    });

    test("rejects unsupported expressions inside unquote", async () => {
      const analysis = await analyzeSource(`
        (defmacro unsupported [] \`(~(+ 1 2)))
        (unsupported)
      `);

      expectDiagnostic(analysis, "SEM_MACRO_UNQUOTE_UNSUPPORTED");
    });

    test("rejects unquote splicing at the top level", async () => {
      const analysis = await analyzeSource(`
        (defmacro spread [items] \`~@items)
        (spread [1 2])
      `);

      expectDiagnostic(analysis, "SEM_MACRO_SPLICE_CONTEXT");
    });

    test("requires unquote splicing targets to produce sequences", async () => {
      const analysis = await analyzeSource(`
        (defmacro spread [item] \`(list ~@item))
        (spread 42)
      `);

      expectDiagnostic(analysis, "SEM_MACRO_SPLICE_SEQUENCE");
    });
  });

  describe("namespace imports", () => {
    test("allows namespaced access via require/external", async () => {
      const analysis = await analyzeSource(`
        (require math "./math.lang")
        (external paths "node:path")
        math/add
        (get paths sep)
      `);

      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).not.toContain(
        "SEM_UNRESOLVED_SYMBOL"
      );
      expect(getDiagnosticCodes(analysis)).not.toContain(
        "SEM_UNRESOLVED_NAMESPACE_ALIAS"
      );
    });

    test("reports unresolved namespace aliases", async () => {
      const analysis = await analyzeSource("missing/ns");

      expectDiagnostic(analysis, "SEM_UNRESOLVED_NAMESPACE_ALIAS");
    });

    test("validates require arguments", async () => {
      const analysis = await analyzeSource("(require dep 42)");

      expectDiagnostic(analysis, "SEM_REQUIRE_EXPECTS_STRING");
    });

    test("validates external arguments", async () => {
      const analysis = await analyzeSource("(external dep :fs)");

      expectDiagnostic(analysis, "SEM_EXTERNAL_EXPECTS_STRING");
    });

    test("rejects require forms outside the top level", async () => {
      const analysis = await analyzeSource(`
        (let []
          (require dep "./math.lang")
          dep)
      `);

      expectDiagnostic(analysis, "SEM_REQUIRE_TOP_LEVEL");
    });

    test("rejects external forms outside the top level", async () => {
      const analysis = await analyzeSource(`
        (let []
          (external dep "node:path")
          dep)
      `);

      expectDiagnostic(analysis, "SEM_EXTERNAL_TOP_LEVEL");
    });

    test("requires alias symbols", async () => {
      const analysis = await analyzeSource('(require 42 "./math.lang")');

      expectDiagnostic(analysis, "SEM_REQUIRE_EXPECTS_ALIAS");
    });

    test("records module imports with resolver metadata", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId: "/workspace/math.lang" }),
      };
      const analysis = await analyzeSource('(require math "./math.lang")', {
        moduleId: "/workspace/main.lang",
        moduleResolver: resolver,
      });

      expect(analysis.graph.imports).toHaveLength(1);
      const record = analysis.graph.imports[0];
      expect(record?.alias).toBe("math");
      expect(record?.specifier).toBe("./math.lang");
      expect(record?.kind).toBe("require");
      expect(record?.moduleId).toBe("/workspace/math.lang");
    });

    test("emits diagnostics when module resolution fails", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: false, reason: "missing" }),
      };
      const analysis = await analyzeSource(
        '(require missing "./missing.lang")',
        {
          moduleId: "/workspace/main.lang",
          moduleResolver: resolver,
        }
      );

      expectDiagnostic(analysis, "SEM_REQUIRE_RESOLVE_FAILED");
      expect(analysis.graph.imports).toHaveLength(1);
      expect(analysis.graph.imports[0]?.moduleId).toBeUndefined();
    });

    test("validates namespace members against known exports", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId: "/workspace/math.lang" }),
      };
      const moduleExports = {
        getExports: (moduleId: string) =>
          moduleId === "/workspace/math.lang" ? ["add"] : undefined,
      };

      const okAnalysis = await analyzeSource(
        `
        (require math "./math.lang")
        math/add
      `,
        {
          moduleId: "/workspace/main.lang",
          moduleResolver: resolver,
          moduleExports,
        }
      );

      expect(getDiagnosticCodes(okAnalysis)).not.toContain(
        "SEM_UNRESOLVED_NAMESPACE_MEMBER"
      );
      expect(getDiagnosticCodes(okAnalysis)).not.toContain(
        "SEM_UNRESOLVED_SYMBOL"
      );
      const failure = await analyzeSource(
        `
        (require math "./math.lang")
        math/sub
      `,
        {
          moduleId: "/workspace/main.lang",
          moduleResolver: resolver,
          moduleExports,
        }
      );

      expectDiagnostic(failure, "SEM_UNRESOLVED_NAMESPACE_MEMBER");
    });
  });
});
