import { describe, expect, test } from "bun:test";
import { parseSource } from "../src";
import { NodeKind, type ReaderMacroKind, type SymbolNode } from "@vibe/syntax";

describe("parseSource", () => {
  test("parses nested collections", async () => {
    const result = await parseSource("(def foo [:bar [:baz 1]])");

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);

    const program = result.program;
    expect(program.body).toHaveLength(1);

    const list = program.body[0];
    expect(list?.kind).toBe(NodeKind.List);
    if (list?.kind !== NodeKind.List) {
      throw new Error("Expected list node");
    }

    const [defSym, fooSym, vector] = list.elements;
    expect(defSym?.kind).toBe(NodeKind.Symbol);
    expect(fooSym?.kind).toBe(NodeKind.Symbol);
    expect(vector?.kind).toBe(NodeKind.Vector);

    if (vector?.kind !== NodeKind.Vector) {
      throw new Error("Expected vector node");
    }

    const [keyword, innerVector] = vector.elements;
    expect(keyword?.kind).toBe(NodeKind.Keyword);
    expect(innerVector?.kind).toBe(NodeKind.Vector);

    if (innerVector?.kind !== NodeKind.Vector) {
      throw new Error("Expected inner vector node");
    }

    expect(innerVector.elements[0]?.kind).toBe(NodeKind.Keyword);
    expect(innerVector.elements[1]?.kind).toBe(NodeKind.Number);
  });

  test("parses atom literals with correct values", async () => {
    const source = String.raw`42 "hi" \newline :ns/foo ::auto true false nil`;
    const result = await parseSource(source);

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);

    const [num, str, charNode, keyword, autoKeyword, boolTrue, boolFalse, nil] =
      result.program.body;

    expect(num?.kind).toBe(NodeKind.Number);
    if (num?.kind !== NodeKind.Number) {
      throw new Error("Expected number node");
    }
    expect(num.value).toBe(42);
    expect(num.lexeme).toBe("42");

    expect(str?.kind).toBe(NodeKind.String);
    if (str?.kind !== NodeKind.String) {
      throw new Error("Expected string node");
    }
    expect(str.value).toBe("hi");

    expect(charNode?.kind).toBe(NodeKind.Character);
    if (charNode?.kind !== NodeKind.Character) {
      throw new Error("Expected character node");
    }
    expect(charNode.value).toBe("\n");

    expect(keyword?.kind).toBe(NodeKind.Keyword);
    if (keyword?.kind !== NodeKind.Keyword) {
      throw new Error("Expected keyword node");
    }
    expect(keyword.value).toBe("ns/foo");
    expect(keyword.lexeme).toBe(":ns/foo");

    expect(autoKeyword?.kind).toBe(NodeKind.Keyword);
    if (autoKeyword?.kind !== NodeKind.Keyword) {
      throw new Error("Expected auto keyword node");
    }
    expect(autoKeyword.value).toBe("auto");
    expect(autoKeyword.lexeme).toBe("::auto");

    expect(boolTrue?.kind).toBe(NodeKind.Boolean);
    expect(boolFalse?.kind).toBe(NodeKind.Boolean);
    if (
      boolTrue?.kind !== NodeKind.Boolean ||
      boolFalse?.kind !== NodeKind.Boolean
    ) {
      throw new Error("Expected boolean nodes");
    }
    expect(boolTrue.value).toBeTrue();
    expect(boolFalse.value).toBeFalse();

    expect(nil?.kind).toBe(NodeKind.Nil);
    if (nil?.kind !== NodeKind.Nil) {
      throw new Error("Expected nil node");
    }
    expect(nil.value).toBeNull();
  });

  test("handles reader macros", async () => {
    const source = "'(println ~x ~@xs :a :b)";
    const result = await parseSource(source);

    expect(result.ok).toBeTrue();

    const quote = result.program.body[0];
    expect(quote?.kind).toBe(NodeKind.Quote);
    if (quote?.kind !== NodeKind.Quote || !quote.target) {
      throw new Error("Expected quote node");
    }

    const quotedList = quote.target;
    if (quotedList.kind !== NodeKind.List) {
      throw new Error("Expected quoted list");
    }

    expect(quotedList.elements.length).toBeGreaterThan(3);
    const unquoteNode = quotedList.elements[1];
    const splicingNode = quotedList.elements[2];

    expect(unquoteNode?.kind).toBe(NodeKind.Unquote);
    expect(splicingNode?.kind).toBe(NodeKind.UnquoteSplicing);
  });

  // Map literals removed from the language; related tests deleted.

  test("reports unexpected closing tokens", async () => {
    const result = await parseSource(")");

    expect(result.ok).toBeFalse();
    expect(result.diagnostics.map((d) => d.code)).toContain(
      "PARSE_UNEXPECTED_CLOSING"
    );
  });

  test("reports unterminated sequences", async () => {
    const list = await parseSource("(foo");
    const vector = await parseSource("[1 2");
    expect(list.diagnostics.map((d) => d.code)).toContain(
      "PARSE_LIST_UNTERMINATED"
    );
    expect(vector.diagnostics.map((d) => d.code)).toContain(
      "PARSE_VECTOR_UNTERMINATED"
    );
    // set literals (reader dispatch) removed; no test here
  });

  test("reader macros require targets", async () => {
    const cases: Array<{ source: string; kind: ReaderMacroKind }> = [
      { source: "'", kind: NodeKind.Quote },
      { source: "`", kind: NodeKind.SyntaxQuote },
      { source: "~", kind: NodeKind.Unquote },
      { source: "~@", kind: NodeKind.UnquoteSplicing },
    ];

    for (const testCase of cases) {
      const result = await parseSource(testCase.source);
      expect(result.diagnostics.map((d) => d.code)).toContain(
        "PARSE_MACRO_MISSING_TARGET"
      );
      const node = result.program.body[0];
      expect(node?.kind).toBe(testCase.kind);
      if (
        node?.kind === NodeKind.Quote ||
        node?.kind === NodeKind.SyntaxQuote ||
        node?.kind === NodeKind.Unquote ||
        node?.kind === NodeKind.UnquoteSplicing
      ) {
        expect(node.target).toBeNull();
      }
    }
  });

  test("reader macros missing targets inside sequences keep delimiters", async () => {
    const result = await parseSource("(foo ')");

    expect(result.ok).toBeFalse();
    expect(result.diagnostics.map((d) => d.code)).toEqual([
      "PARSE_MACRO_MISSING_TARGET",
    ]);

    const listNode = result.program.body[0];
    expect(listNode?.kind).toBe(NodeKind.List);
    if (listNode?.kind !== NodeKind.List) {
      throw new Error("Expected list node");
    }
    expect(listNode.elements).toHaveLength(2);

    const orphanQuote = listNode.elements[1];
    expect(orphanQuote?.kind).toBe(NodeKind.Quote);
    if (orphanQuote?.kind !== NodeKind.Quote) {
      throw new Error("Expected quote node");
    }
    expect(orphanQuote.target).toBeNull();
  });

  // Dispatch reader syntax removed; related tests removed.

  test("bubble up lexer diagnostics", async () => {
    const result = await parseSource('(println "oops');

    expect(result.ok).toBeFalse();
    expect(result.diagnostics.map((diag) => diag.code)).toContain(
      "LEX_STRING_UNTERMINATED"
    );
  });

  test("annotates lexical scope identifiers", async () => {
    const result = await parseSource("(let [x 1] (fn [y] y) x)");

    expect(result.ok).toBeTrue();
    const program = result.program;
    expect(program.scopeId).toBeDefined();

    const rootScope = program.scopeId;
    const letNode = program.body[0];
    expect(letNode?.scopeId).toBeDefined();
    if (!letNode || letNode.kind !== NodeKind.List) {
      throw new Error("Expected let list");
    }

    const bindingsVector = letNode.elements[1];
    if (!bindingsVector || bindingsVector.kind !== NodeKind.Vector) {
      throw new Error("Expected let bindings vector");
    }

    const bindingTarget = bindingsVector.elements[0];
    const bindingValue = bindingsVector.elements[1];
    if (
      !bindingTarget ||
      !bindingValue ||
      bindingTarget.kind !== NodeKind.Symbol ||
      bindingValue.kind !== NodeKind.Number
    ) {
      throw new Error("Expected binding pair");
    }

    expect(bindingTarget.scopeId).toBeDefined();
    expect(bindingTarget.scopeId).toBe(bindingValue.scopeId);
    expect(bindingTarget.scopeId).not.toBe(rootScope);

    const fnNode = letNode.elements[2];
    if (!fnNode || fnNode.kind !== NodeKind.List) {
      throw new Error("Expected fn form");
    }

    const fnScope = fnNode.scopeId;
    expect(fnScope).toBe(bindingTarget.scopeId);

    const fnParams = fnNode.elements[1];
    if (!fnParams || fnParams.kind !== NodeKind.Vector) {
      throw new Error("Expected fn params vector");
    }
    expect(fnParams.scopeId).toBe(fnNode.scopeId);

    const parameterSymbol = fnParams.elements[0];
    if (!parameterSymbol || parameterSymbol.kind !== NodeKind.Symbol) {
      throw new Error("Expected parameter symbol");
    }
    expect(parameterSymbol.scopeId).not.toBe(letNode.scopeId);

    const bodySymbol = fnNode.elements[2];
    if (!bodySymbol || bodySymbol.kind !== NodeKind.Symbol) {
      throw new Error("Expected fn body symbol");
    }
    expect(bodySymbol.scopeId).toBe(parameterSymbol.scopeId);

    const trailingSymbol = letNode.elements[3];
    if (!trailingSymbol || trailingSymbol.kind !== NodeKind.Symbol) {
      throw new Error("Expected trailing symbol");
    }
    expect(trailingSymbol.scopeId).toBe(bindingTarget.scopeId);
  });

  test("parses namespace imports as dedicated nodes", async () => {
    const result = await parseSource(`
      (require math "./math.lang")
      (external fs "node:fs")
    `);

    expect(result.ok).toBeTrue();
    const [requireNode, externalNode] = result.program.body;

    expect(requireNode?.kind).toBe(NodeKind.NamespaceImport);
    if (!requireNode || requireNode.kind !== NodeKind.NamespaceImport) {
      throw new Error("Expected namespace import node");
    }
    expect(requireNode.importKind).toBe("require");
    expect(requireNode.alias?.kind).toBe(NodeKind.Symbol);
    expect(requireNode.source?.kind).toBe(NodeKind.String);
    expect(requireNode.elements).toHaveLength(3);

    expect(externalNode?.kind).toBe(NodeKind.NamespaceImport);
    if (!externalNode || externalNode.kind !== NodeKind.NamespaceImport) {
      throw new Error("Expected namespace import node");
    }
    expect(externalNode.importKind).toBe("external");
    expect(externalNode.alias?.kind).toBe(NodeKind.Symbol);
    expect(externalNode.source?.kind).toBe(NodeKind.String);
  });

  test("parses import forms without aliases", async () => {
    const result = await parseSource('(import "./prelude.lang")');

    expect(result.ok).toBeTrue();
    const [importNode] = result.program.body;
    expect(importNode?.kind).toBe(NodeKind.NamespaceImport);
    if (!importNode || importNode.kind !== NodeKind.NamespaceImport) {
      throw new Error("Expected namespace import node");
    }
    expect(importNode.importKind).toBe("import");
    expect(importNode.alias).toBeNull();
    expect(importNode.source?.kind).toBe(NodeKind.String);
    expect(importNode.elements).toHaveLength(2);
  });

  test("parses function definitions with Clojure-style names", async () => {
    const result = await parseSource(`
      (def is-valid? (fn [x] (> x 0)))
      (def set-value! (fn [v] v))
      (def splat* (fn [] 42))
      (def foo-bar? (fn [] true))
    `);

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.body).toHaveLength(4);

    const defs = result.program.body;
    if (!defs[0] || defs[0].kind !== NodeKind.List) {
      throw new Error("Expected list node");
    }

    const [defSym1, nameSym1] = defs[0].elements;
    expect(defSym1?.kind).toBe(NodeKind.Symbol);
    expect(nameSym1?.kind).toBe(NodeKind.Symbol);
    if (nameSym1?.kind === NodeKind.Symbol) {
      expect(nameSym1.value).toBe("is-valid?");
    }

    const [defSym2, nameSym2] = (defs[1] as any).elements;
    expect(nameSym2?.kind).toBe(NodeKind.Symbol);
    if (nameSym2?.kind === NodeKind.Symbol) {
      expect(nameSym2.value).toBe("set-value!");
    }

    const [defSym3, nameSym3] = (defs[2] as any).elements;
    expect(nameSym3?.kind).toBe(NodeKind.Symbol);
    if (nameSym3?.kind === NodeKind.Symbol) {
      expect(nameSym3.value).toBe("splat*");
    }

    const [defSym4, nameSym4] = (defs[3] as any).elements;
    expect(nameSym4?.kind).toBe(NodeKind.Symbol);
    if (nameSym4?.kind === NodeKind.Symbol) {
      expect(nameSym4.value).toBe("foo-bar?");
    }
  });

  test("parses gensym placeholders inside syntax quotes", async () => {
    const source = "`(let [foo# 1 bar# foo#] bar#)";
    const result = await parseSource(source);

    expect(result.ok).toBeTrue();
    expect(result.diagnostics).toHaveLength(0);
    const programBody = result.program.body[0];
    expect(programBody?.kind).toBe(NodeKind.SyntaxQuote);
    if (!programBody || programBody.kind !== NodeKind.SyntaxQuote) {
      throw new Error("Expected syntax quote node");
    }
    const target = programBody.target;
    if (!target || target.kind !== NodeKind.List) {
      throw new Error("Expected list expression inside syntax quote");
    }
    const vector = target.elements[1];
    if (!vector || vector.kind !== NodeKind.Vector) {
      throw new Error("Expected binding vector inside syntax quote");
    }
    const placeholderSymbols = target.elements
      .concat(vector.elements)
      .filter(
        (node): node is SymbolNode =>
          Boolean(node) && node.kind === NodeKind.Symbol
      )
      .map((node) => node.value);
    const placeholders = placeholderSymbols.filter((value) =>
      value.endsWith("#")
    );
    expect(placeholders).toEqual(["bar#", "foo#", "bar#", "foo#"]);
  });
});
