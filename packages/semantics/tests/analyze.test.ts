import { describe, expect, test } from "bun:test";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  analyzeProgram,
  type AnalyzeOptions,
  type ModuleResolver,
  type ModuleExportsLookup,
  type SymbolId,
} from "../src";
import { createDefaultTestResolvers } from "@vibe/test-helpers";
import { parseSource } from "@vibe/parser";
import { NodeKind, type ExpressionNode, type ProgramNode } from "@vibe/syntax";
import { TEST_BUILTINS } from "./test-builtins";

const ARITHMETIC_STUB = `
  (def + (fn [& xs] xs))
  (def - (fn [& xs] xs))
  (def * (fn [& xs] xs))
  (def / (fn [& xs] xs))
`;

const withArithmeticPrelude = (source: string) =>
  `${ARITHMETIC_STUB}\n${source}`;

const analyzeSource = async (source: string, options?: AnalyzeOptions) => {
  const parseResult = await parseSource(source);
  if (!parseResult.ok) {
    const messages = parseResult.diagnostics.map((d) => d.message).join("\n");
    throw new Error(`Failed to parse fixture: ${messages}`);
  }
  return await analyzeProgram(parseResult.program, {
    ...options,
    builtins: options?.builtins ?? TEST_BUILTINS,
  });
};

type AnalysisResult = Awaited<ReturnType<typeof analyzeProgram>>;

const getDiagnosticCodes = (analysis: AnalysisResult) =>
  analysis.diagnostics.map((d) => d.code);
const debugDiagnostics = (label: string, analysis: AnalysisResult) => {
  if (analysis.ok || !process.env.LANG_DEBUG_SEMANTICS) {
    return;
  }
  // eslint-disable-next-line no-console
  console.error(`[semantics][${label}]`, analysis.diagnostics);
};

const expectDiagnostic = (analysis: AnalysisResult, code: string) => {
  expect(getDiagnosticCodes(analysis)).toContain(code);
};

const expectBooleanNode = (node: ExpressionNode | null | undefined) => {
  expect(node?.kind).toBe(NodeKind.Boolean);
  if (!node || node.kind !== NodeKind.Boolean) {
    throw new Error("Expected boolean node");
  }
  return node;
};

const cloneExpression = <T>(node: T): T =>
  JSON.parse(JSON.stringify(node)) as T;

const renameSymbolInNode = (
  node: ExpressionNode | null | undefined,
  from: string,
  to: string
): ExpressionNode | null | undefined => {
  if (!node) {
    return node;
  }
  if (node.kind === NodeKind.Symbol) {
    if (node.value !== from) {
      return node;
    }
    return {
      ...node,
      value: to,
      lexeme: to,
    };
  }
  if (node.kind === NodeKind.List || node.kind === NodeKind.Vector) {
    const elements = node.elements.map(
      (element) => renameSymbolInNode(element, from, to) ?? element
    );
    return {
      ...node,
      elements,
    };
  }

  if (
    node.kind === NodeKind.Dispatch ||
    node.kind === NodeKind.Quote ||
    node.kind === NodeKind.SyntaxQuote ||
    node.kind === NodeKind.Unquote ||
    node.kind === NodeKind.UnquoteSplicing
  ) {
    const originalTarget =
      (node as { target?: ExpressionNode | null }).target ?? null;
    const updatedTarget = renameSymbolInNode(originalTarget, from, to);
    const target = updatedTarget !== undefined ? updatedTarget : originalTarget;
    return {
      ...node,
      target,
    };
  }
  return node;
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

    const analysis = await analyzeProgram(program);
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

  test("ignores stale parser scope metadata when reusing nodes under new parents", async () => {
    const parseResult = await parseSource(`
      (let [outer 1]
        outer)

      (let [shadow 2]
        shadow)
    `);
    if (!parseResult.ok) {
      throw new Error("Failed to parse stale scope fixture");
    }
    const program = parseResult.program;
    const firstExpr = program.body[0];
    const secondExpr = program.body[1];
    if (!firstExpr || firstExpr.kind !== NodeKind.List) {
      throw new Error("Expected first let expression");
    }
    if (!secondExpr || secondExpr.kind !== NodeKind.List) {
      throw new Error("Expected second let expression");
    }
    const originalBody = firstExpr.elements[2];
    if (!originalBody) {
      throw new Error("Expected first let body expression");
    }
    const staleBody = cloneExpression(originalBody);
    const renamedBody =
      renameSymbolInNode(staleBody, "outer", "shadow") ?? staleBody;
    const mutatedSecondElements = [...secondExpr.elements];
    mutatedSecondElements[2] = renamedBody;
    const mutatedSecondExpr = {
      ...secondExpr,
      elements: mutatedSecondElements,
    };
    const mutatedProgram: ProgramNode = {
      ...program,
      body: [firstExpr, mutatedSecondExpr],
    };
    const analysis = await analyzeProgram(mutatedProgram, {
      builtins: TEST_BUILTINS,
    });

    debugDiagnostics("stale parser scope metadata", analysis);

    expect(analysis.ok).toBeTrue();
    expect(getDiagnosticCodes(analysis)).not.toContain("SEM_UNRESOLVED_SYMBOL");

    const shadowSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "shadow" && symbol.kind === "var"
    );
    const shadowUsage = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "shadow" && node.symbol.role === "usage"
    );

    expect(shadowSymbol).toBeDefined();
    expect(shadowUsage?.symbol?.symbolId).toBe(shadowSymbol?.id);
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

  test("defp binds locally but remains private", async () => {
    const analysis = await analyzeSource(
      "(defp secret 41) secret (def public 42)"
    );

    expect(analysis.ok).toBeTrue();
    expect(analysis.graph.exports.map((entry) => entry.name)).toEqual([
      "public",
    ]);
    const secretSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "secret"
    );
    expect(secretSymbol).toBeDefined();
  });

  test("creates scoped bindings for let and fn", async () => {
    const analysis = await analyzeSource(
      withArithmeticPrelude("(let [x 1] (fn [y] (+ x y)))")
    );

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

  test("binds symbols from destructured let patterns", async () => {
    const analysis = await analyzeSource(
      "(let [[x y & rest :as full] [1 2 3 4]] [x y rest full])"
    );

    expect(analysis.ok).toBeTrue();
    const definitions = analysis.graph.nodes
      .filter((node) => node.symbol?.role === "definition")
      .map((node) => node.symbol?.name);
    expect(definitions).toEqual(
      expect.arrayContaining(["x", "y", "rest", "full"])
    );
  });

  test("binds symbols from destructured fn parameters", async () => {
    const analysis = await analyzeSource(
      withArithmeticPrelude("(fn [[x y] z] (+ x y z))")
    );

    expect(analysis.ok).toBeTrue();
    const parameters = analysis.graph.nodes
      .filter((node) => node.symbol?.role === "parameter")
      .map((node) => node.symbol?.name);
    expect(parameters).toEqual(expect.arrayContaining(["x", "y", "z"]));
  });

  test("analyzes multi-arity fn clauses", async () => {
    const analysis = await analyzeSource(
      withArithmeticPrelude(`
        (def picker
          (fn
            ([x] (+ x 1))
            ([x y] (+ x y))
            ([x y & rest] x)))
        picker
      `)
    );

    expect(analysis.ok).toBeTrue();
    expect(getDiagnosticCodes(analysis)).not.toContain(
      "SEM_FN_DUPLICATE_ARITY"
    );
  });

  test("rejects duplicate fixed-arity fn clauses", async () => {
    const analysis = await analyzeSource(
      withArithmeticPrelude(`
        (fn
          ([x] x)
          ([y] y))
      `)
    );

    expectDiagnostic(analysis, "SEM_FN_DUPLICATE_ARITY");
  });

  test("requires variadic fn clause to be last", async () => {
    const analysis = await analyzeSource(
      withArithmeticPrelude(`
        (fn
          ([x & rest] x)
          ([x y] y))
      `)
    );

    expectDiagnostic(analysis, "SEM_FN_REST_POSITION");
  });

  test("reports unresolved symbols", async () => {
    const analysis = await analyzeSource(withArithmeticPrelude("(+ mystery)"));

    expect(analysis.ok).toBeFalse();
    expect(analysis.diagnostics.map((d) => d.code)).toContain(
      "SEM_UNRESOLVED_SYMBOL"
    );
  });

  test("attaches spans to unresolved symbol diagnostics", async () => {
    const analysis = await analyzeSource(withArithmeticPrelude("(+ mystery)"));

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
    const analysis = await analyzeSource(
      withArithmeticPrelude(`
        (def root 0)
        (let [outer 1]
          (fn [mid]
            (let [inner (+ outer mid root)]
              (fn [leaf]
                (+ root outer mid inner leaf)))))
      `)
    );

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

    const expectUsagesToReference = (name: string, symbolId: SymbolId) => {
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
      (def with-unique
        (macro [expr]
          \`(let [~(gensym "tmp") ~expr]
             42)))

      (with-unique 10)
    `);

    debugDiagnostics("gensym-generated bindings", analysis);
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

  test("analyzes try/catch binding scopes", async () => {
    const analysis = await analyzeSource(`
      (def noop (fn [] 0))
      (try
        (noop)
        (catch err err)
        (finally (noop)))
    `);

    expect(analysis.ok).toBeTrue();
    const errSymbol = analysis.graph.symbols.find(
      (symbol) => symbol.name === "err"
    );
    expect(errSymbol?.kind).toBe("var");

    const errUsage = analysis.graph.nodes.find(
      (node) => node.symbol?.name === "err" && node.symbol.role === "usage"
    );
    expect(errUsage?.symbol?.symbolId).toBe(errSymbol?.id);
  });

  test("reports non-symbol catch bindings", async () => {
    const analysis = await analyzeSource(`
      (try
        1
        (catch [oops] oops))
    `);

    expectDiagnostic(analysis, "SEM_TRY_CATCH_REQUIRES_SYMBOL");
  });

  test("validates throw arity", async () => {
    const missingArg = await analyzeSource("(throw)");
    expectDiagnostic(missingArg, "SEM_THROW_REQUIRES_VALUE");

    const extraArgs = await analyzeSource("(throw 1 2)");
    expectDiagnostic(extraArgs, "SEM_THROW_TOO_MANY_ARGS");
  });

  test("instantiates auto gensym placeholders inside syntax quotes", async () => {
    const analysis = await analyzeSource(`
      (def capture
        (macro [expr]
          \`(let [foo# ~expr]
             foo#)))

      (capture 99)
    `);

    debugDiagnostics("auto gensym placeholders", analysis);
    expect(getDiagnosticCodes(analysis)).not.toContain(
      "SEM_GENSYM_PLACEHOLDER_CONTEXT"
    );
    const gensymSymbol = analysis.graph.symbols.find((symbol) =>
      symbol.name.startsWith("foo__")
    );
    expect(gensymSymbol).toBeDefined();
    const usages = analysis.graph.nodes.filter(
      (node) => node.symbol?.symbolId === gensymSymbol?.id
    );
    expect(
      usages.some((node) => node.symbol?.role === "definition")
    ).toBeTrue();
    expect(usages.some((node) => node.symbol?.role === "usage")).toBeTrue();
  });

  test("rejects gensym placeholders outside syntax quotes", async () => {
    const analysis = await analyzeSource("(def bad foo#)");

    expect(getDiagnosticCodes(analysis)).toContain(
      "SEM_GENSYM_PLACEHOLDER_CONTEXT"
    );
  });

  test("assigns unique hygiene tags to macro-generated bindings", async () => {
    const analysis = await analyzeSource(
      withArithmeticPrelude(`
        (def with-temp
          (macro [value]
            \`(let [tmp ~value]
               tmp)))

        (let [a (with-temp 1)
              b (with-temp 2)]
          (+ a b))
      `)
    );
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
    const analysis = await analyzeSource(
      withArithmeticPrelude(`
        (def with-temp
          (macro [expr]
            \`(let [tmp ~expr]
               tmp)))

        (def answer (with-temp (+ 1 2)))
      `)
    );

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
      (def looped (macro [] \`(looped)))
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

      expectDiagnostic(analysis, "SEM_PATTERN_UNSUPPORTED");
    });

    test("requires fn parameters to be declared via vectors", async () => {
      const analysis = await analyzeSource("(fn 42 1)");

      expectDiagnostic(analysis, "SEM_FN_EXPECTS_VECTOR");
    });

    test("requires fn parameters to be symbols", async () => {
      const analysis = await analyzeSource("(fn [42] 42)");

      expectDiagnostic(analysis, "SEM_PATTERN_UNSUPPORTED");
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

  describe("macro literal validation", () => {
    test("requires macro names to be symbols", async () => {
      const analysis = await analyzeSource("(def 1 (macro [] `42))");

      expectDiagnostic(analysis, "SEM_BINDING_REQUIRES_SYMBOL");
    });

    test("requires macro parameters to be provided via vectors", async () => {
      const analysis = await analyzeSource("(def foo (macro 1 `42))");

      expectDiagnostic(analysis, "SEM_MACRO_EXPECTS_VECTOR");
    });

    test("requires macro parameters to be symbols", async () => {
      const analysis = await analyzeSource("(def foo (macro [1] `42))");

      expectDiagnostic(analysis, "SEM_MACRO_PARAM_SYMBOL");
    });

    test("reports duplicate macro parameters", async () => {
      const analysis = await analyzeSource(
        "(def foo (macro [x x] `(list ~x)))"
      );

      expectDiagnostic(analysis, "SEM_MACRO_DUPLICATE_PARAM");
    });

    test("requires macro bodies", async () => {
      const analysis = await analyzeSource("(def foo (macro [x]))");

      expectDiagnostic(analysis, "SEM_MACRO_REQUIRES_BODY");
    });

    test("allows macro bodies to return raw forms", async () => {
      const analysis = await analyzeSource(`
        (def passthrough (macro [x] x))
        (passthrough 42)
      `);

      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).toEqual([]);
    });

    test("evaluates syntax-quoted templates produced by interpreter logic", async () => {
      const analysis = await analyzeSource(`
        (def wrap
          (macro [expr]
            (let [tmp (gensym "wrap")]
              \`(let [~tmp ~expr]
                 ~tmp))))
        (wrap 1)
      `);

      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).toEqual([]);
    });

    test("supports only a single macro body expression", async () => {
      const analysis = await analyzeSource(
        "(def foo (macro [x] `(list ~x) `(list ~x)))"
      );

      expectDiagnostic(analysis, "SEM_MACRO_SINGLE_BODY");
    });

    test("supports multi-clause macros", async () => {
      const analysis = await analyzeSource(`
        (def pick
          (macro
            ([x] x)
            ([x y] y)))
        (pick 1)
        (pick 1 2)
      `);

      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).not.toContain(
        "SEM_MACRO_DUPLICATE_ARITY"
      );
    });

    test("rejects duplicate macro clause arities", async () => {
      const analysis = await analyzeSource(`
        (def dup
          (macro
            ([x] x)
            ([y] y)))
      `);

      expectDiagnostic(analysis, "SEM_MACRO_DUPLICATE_ARITY");
    });

    test("requires variadic macro clause to be last", async () => {
      const analysis = await analyzeSource(`
        (def reorder
          (macro
            ([x & rest] x)
            ([x y] y)))
      `);

      expectDiagnostic(analysis, "SEM_MACRO_REST_POSITION");
    });
  });

  describe("macro expansion diagnostics", () => {
    test("reports macro arity mismatches and missing args", async () => {
      const analysis = await analyzeSource(`
        (def pair (macro [a b] \`(vector ~a ~b)))
        (pair 1)
      `);

      expectDiagnostic(analysis, "SEM_MACRO_ARITY_MISMATCH");
      expectDiagnostic(analysis, "SEM_MACRO_ARG_MISSING");
    });

    test("reports unknown parameters referenced via unquote", async () => {
      const analysis = await analyzeSource(`
        (def uses-missing (macro [] \`(~ missing)))
        (uses-missing)
      `);

      expectDiagnostic(analysis, "SEM_MACRO_UNKNOWN_PARAM");
    });

    test("evaluates full expressions inside unquote at compile time", async () => {
      const analysis = await analyzeSource(`
        (def simple (macro [] \`~(if true 1 2)))
        (simple)
      `);

      debugDiagnostics("macro unquote evaluation", analysis);
      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).toEqual([]);
    });

    test("rejects unquote splicing at the top level", async () => {
      const analysis = await analyzeSource(`
        (def spread (macro [items] \`~@items))
        (spread [1 2])
      `);

      expectDiagnostic(analysis, "SEM_MACRO_SPLICE_CONTEXT");
    });

    test("requires unquote splicing targets to produce sequences", async () => {
      const analysis = await analyzeSource(`
        (def spread (macro [item] \`(list ~@item)))
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
        paths/path-separator
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
          moduleId === "/workspace/math.lang"
            ? [{ name: "add", kind: "var" as const }]
            : undefined,
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

    test("imports flatten exported bindings into the current module", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId: "/workspace/prelude.lang" }),
      };
      const { moduleExports } = await createDefaultTestResolvers();

      const analysis = await analyzeSource(
        `
        (import "./prelude.lang")
        first
      `,
        {
          moduleId: "/workspace/main.lang",
          moduleResolver: resolver,
          moduleExports,
        }
      );

      debugDiagnostics("import flatten", analysis);
      expect(analysis.ok).toBeTrue();
      const importedSymbol = analysis.graph.symbols.find(
        (symbol) => symbol.name === "first"
      );
      expect(importedSymbol).toBeDefined();
      expect(analysis.graph.imports[0]?.kind).toBe("import");
      const usage = analysis.graph.nodes.find(
        (node) => node.symbol?.name === "first" && node.symbol.role === "usage"
      );
      expect(usage?.symbol?.name).toBe(importedSymbol?.name);
    });

    test("imports register macro exports for expansion", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId: "/workspace/macros.lang" }),
      };

      const macroParse = await parseSource("(def inline-true (macro [] true))");
      if (!macroParse.ok) {
        throw new Error("Failed to parse macro fixture");
      }
      const macroForm = macroParse.program.body[0];
      if (!macroForm || macroForm.kind !== NodeKind.List) {
        throw new Error("Expected macro list form");
      }
      const macroLiteral = macroForm.elements[2];
      if (!macroLiteral || macroLiteral.kind !== NodeKind.List) {
        throw new Error("Macro literal missing in def form");
      }
      const macroBody = macroLiteral.elements[2];
      if (!macroBody) {
        throw new Error("Macro body missing in fixture");
      }

      const moduleExports: ModuleExportsLookup = {
        getExports: (moduleId: string) =>
          moduleId === "/workspace/macros.lang"
            ? [
                {
                  name: "inline-true",
                  kind: "macro",
                  macro: {
                    clauses: [
                      {
                        params: [],
                        body: macroBody,
                      },
                    ],
                  },
                },
              ]
            : undefined,
      };

      const source = `
        (import "./macros.lang")
        (inline-true)
      `;
      const parseResult = await parseSource(source);
      if (!parseResult.ok) {
        throw new Error("Failed to parse macro usage fixture");
      }

      const analysis = await analyzeProgram(parseResult.program, {
        moduleId: "/workspace/main.lang",
        moduleResolver: resolver,
        moduleExports,
        builtins: TEST_BUILTINS,
      });

      expect(analysis.ok).toBeTrue();
      const invocation = expectBooleanNode(parseResult.program.body[1]);
      expect(invocation.value).toBeTrue();
      expect(analysis.graph.imports[0]?.flatten).toBeUndefined();
    });

    test("imports load external dependencies for macro expansion", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId: "/workspace/macro-deps.lang" }),
      };

      const runtimeDepSpecifier = new URL(
        "./fixtures/runtime-macro-dep.js",
        import.meta.url
      ).href;

      const macroBody = await parseSource(
        "(if (runtime/alwaysTrue) true false)"
      );
      if (!macroBody.ok) {
        throw new Error("Failed to parse macro dependency body");
      }
      const macroNode = macroBody.program.body[0];
      if (!macroNode) {
        throw new Error("Missing macro body node");
      }

      const moduleExports: ModuleExportsLookup = {
        getExports: (moduleId: string) =>
          moduleId === "/workspace/macro-deps.lang"
            ? [
                {
                  name: "runtime-macro",
                  kind: "macro",
                  macro: {
                    clauses: [
                      {
                        params: [],
                        body: macroNode,
                      },
                    ],
                    dependencies: [
                      {
                        kind: "external",
                        alias: "runtime",
                        specifier: runtimeDepSpecifier,
                      },
                    ],
                  },
                },
              ]
            : undefined,
      } satisfies ModuleExportsLookup;

      const source = `
        (import "./macro-deps.lang")
        (runtime-macro)
      `;
      const parseResult = await parseSource(source);
      if (!parseResult.ok) {
        throw new Error("Failed to parse macro dependency fixture");
      }

      const analysis = await analyzeProgram(parseResult.program, {
        moduleId: "/workspace/main.lang",
        moduleResolver: resolver,
        moduleExports,
        builtins: TEST_BUILTINS,
      });

      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).not.toContain(
        "SEM_MACRO_DEPENDENCY_FAILED"
      );
      const invocation = expectBooleanNode(parseResult.program.body[1]);
      expect(invocation.value).toBeTrue();
    });

    test("imports load require dependencies for macro expansion", async () => {
      const fixturesDir = fileURLToPath(
        new URL("./fixtures/", import.meta.url)
      );
      const moduleId = path.join(fixturesDir, "macro-require.lang");
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId }),
      };

      const macroBody = await parseSource(
        "(if (runtime/alwaysTrue) true false)"
      );
      if (!macroBody.ok) {
        throw new Error("Failed to parse macro dependency body");
      }
      const macroNode = macroBody.program.body[0];
      if (!macroNode) {
        throw new Error("Missing macro body node");
      }

      const moduleExports: ModuleExportsLookup = {
        getExports: (candidate) =>
          candidate === moduleId
            ? [
                {
                  name: "runtime-macro",
                  kind: "macro",
                  macro: {
                    clauses: [
                      {
                        params: [],
                        body: macroNode,
                      },
                    ],
                    dependencies: [
                      {
                        kind: "require",
                        alias: "runtime",
                        specifier: "./runtime-macro-dep.js",
                      },
                    ],
                  },
                },
              ]
            : undefined,
      } satisfies ModuleExportsLookup;

      const source = `
        (import "./macro-deps.lang")
        (runtime-macro)
      `;
      const parseResult = await parseSource(source);
      if (!parseResult.ok) {
        throw new Error("Failed to parse macro dependency fixture");
      }

      const analysis = await analyzeProgram(parseResult.program, {
        moduleId: "/workspace/main.lang",
        moduleResolver: resolver,
        moduleExports,
        builtins: TEST_BUILTINS,
      });

      expect(analysis.ok).toBeTrue();
      expect(getDiagnosticCodes(analysis)).not.toContain(
        "SEM_MACRO_DEPENDENCY_FAILED"
      );
      const invocation = expectBooleanNode(parseResult.program.body[1]);
      expect(invocation.value).toBeTrue();
    });

    test("reports diagnostics when imports lack export metadata", async () => {
      const resolver: ModuleResolver = {
        resolve: () => ({ ok: true, moduleId: "/workspace/prelude.lang" }),
      };

      const analysis = await analyzeSource('(import "./prelude.lang") frob', {
        moduleId: "/workspace/main.lang",
        moduleResolver: resolver,
      });

      expectDiagnostic(analysis, "SEM_IMPORT_MISSING_EXPORTS");
    });
  });

  describe("operator identifier sanitization", () => {
    test("generates distinct readable names for comparison operators", async () => {
      const analysis = await analyzeSource(`
        (def <= (fn [a b] a))
        (def >= (fn [a b] b))
        (def < (fn [a b] a))
        (def > (fn [a b] b))
      `);

      expect(analysis.ok).toBeTrue();
      const symbols = analysis.graph.symbols;

      // Find the aliases for each operator
      const lte = symbols.find((s) => s.name === "<=");
      const gte = symbols.find((s) => s.name === ">=");
      const lt = symbols.find((s) => s.name === "<");
      const gt = symbols.find((s) => s.name === ">");

      // Aliases include the operator name and symbol ID
      expect(lte?.alias).toMatch(/^_LT_EQ/);
      expect(gte?.alias).toMatch(/^_GT_EQ/);
      expect(lt?.alias).toMatch(/^_LT/);
      expect(gt?.alias).toMatch(/^_GT/);

      // Verify they're all distinct
      const aliases = [lte?.alias, gte?.alias, lt?.alias, gt?.alias];
      const uniqueAliases = new Set(aliases);
      expect(uniqueAliases.size).toBe(4);
    });

    test("generates readable names for arithmetic operators", async () => {
      const analysis = await analyzeSource(`
        (def + (fn [a b] a))
        (def - (fn [a b] b))
        (def * (fn [a b] a))
        (def / (fn [a b] b))
      `);

      expect(analysis.ok).toBeTrue();
      const symbols = analysis.graph.symbols;

      const plus = symbols.find((s) => s.name === "+");
      const minus = symbols.find((s) => s.name === "-");
      const mul = symbols.find((s) => s.name === "*");
      const div = symbols.find((s) => s.name === "/");

      // Single operators get readable names as base identifier
      expect(plus?.alias).toMatch(/^_PLUS/);
      expect(minus?.alias).toMatch(/^_DASH/);
      expect(mul?.alias).toMatch(/^_STAR/);
      expect(div?.alias).toMatch(/^_SLASH/);
    });

    test("handles compound names with special characters", async () => {
      const analysis = await analyzeSource(`
        (def is-valid? (fn [x] true))
        (def set-value! (fn [v] v))
        (def map* (fn [f c] c))
      `);

      expect(analysis.ok).toBeTrue();
      const symbols = analysis.graph.symbols;

      const isValid = symbols.find((s) => s.name === "is-valid?");
      const setValue = symbols.find((s) => s.name === "set-value!");
      const mapVar = symbols.find((s) => s.name === "map*");

      // Compound names get sanitized with underscores
      expect(isValid?.alias).toBe("is_valid_QMARK");
      expect(setValue?.alias).toBe("set_value_BANG");
      expect(mapVar?.alias).toBe("map_STAR");
    });
  });
});
