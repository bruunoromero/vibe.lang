
import { describe, expect, test } from "bun:test";
import { parse } from "@vibe/parser";
import { analyze } from "@vibe/semantics";
import { lower } from "../src/impl/lower";
import type { IRCase } from "../src/types";

describe("Qualified constructor tags", () => {
    test("tag for qualified constructor from dependency is correctly resolved", () => {
        // 1. Setup Dependency Module (ModuleA)
        const sourceA = `
module ModuleA exposing (Result(..))
type Result e a = Ok a | Err e
`;
        const astA = parse(sourceA);
        const semanticsA = analyze(astA);

        // 2. Setup Current Module (ModuleB)
        const sourceB = `
module ModuleB exposing (..)
import ModuleA

fromResult r =
    case r of
        ModuleA.Ok v -> 0
        ModuleA.Err _ -> 1
`;
        const astB = parse(sourceB);
        const dependencies = new Map([["ModuleA", semanticsA]]);
        const semanticsB = analyze(astB, { dependencies });

        // 3. Lower to IR
        const irB = lower(astB, semanticsB, { dependencies });

        // 4. Verify tags in IR
        const fromResult = irB.values["fromResult"]!;
        expect(fromResult.body.kind).toBe("IRCase");
        const caseExpr = fromResult.body as IRCase;

        // Ok branch should have tag 0
        expect(caseExpr.branches[0]!.pattern.kind).toBe("IRConstructorPattern");
        // @ts-ignore
        expect(caseExpr.branches[0]!.pattern.tag).toBe(0);

        // Err branch should have tag 1
        expect(caseExpr.branches[1]!.pattern.kind).toBe("IRConstructorPattern");
        // @ts-ignore
        expect(caseExpr.branches[1]!.pattern.tag).toBe(1);

    });

    test("tag for aliased qualified constructor is correctly resolved", () => {
        // 1. Setup Dependency Module (ModuleA)
        const sourceA = `
module ModuleA exposing (Result(..))
type Result e a = Ok a | Err e
`;
        const astA = parse(sourceA);
        const semanticsA = analyze(astA);

        // 2. Setup Current Module (ModuleB)
        const sourceB = `
module ModuleB exposing (..)
import ModuleA as MA

fromResult r =
    case r of
        MA.Ok v -> 0
        MA.Err _ -> 1
`;
        const astB = parse(sourceB);
        const dependencies = new Map([["ModuleA", semanticsA]]);
        const semanticsB = analyze(astB, { dependencies });

        // 3. Lower to IR
        const irB = lower(astB, semanticsB, { dependencies });

        // 4. Verify tags in IR
        const fromResult = irB.values["fromResult"]!;
        expect(fromResult.body.kind).toBe("IRCase");
        const caseExpr = fromResult.body as IRCase;

        // Err branch (MA.Err) should have tag 1
        // @ts-ignore
        expect(caseExpr.branches[1]!.pattern.tag).toBe(1);
    });
});
