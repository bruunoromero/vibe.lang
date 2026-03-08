// @ts-ignore — compiled Vibe module, no TypeScript declarations
import {
  parseToJson,
  parseWithInfixToJson,
  collectInfixToJson,
  parseWithRegistryToJson,
} from "@vibe/vibe-parser";
import type {
  Program,
  Span,
  OperatorRegistry,
  OperatorInfo,
  InfixDeclaration,
} from "@vibe/syntax";

export class ParseError extends Error {
  constructor(
    message: string,
    public readonly span: Span,
    public readonly filePath?: string,
  ) {
    super(message);
  }
}

type RegistryEntry = { op: string; precedence: number; associativity: string };

const nullReviver = (_key: string, value: unknown) =>
  value === null ? undefined : value;

function deserializeRegistry(entries: RegistryEntry[]): OperatorRegistry {
  const map: OperatorRegistry = new Map();
  for (const entry of entries) {
    map.set(entry.op, {
      precedence: entry.precedence,
      associativity: entry.associativity as OperatorInfo["associativity"],
    });
  }
  return map;
}

function serializeRegistry(registry: OperatorRegistry): string {
  const entries: RegistryEntry[] = [];
  for (const [op, info] of registry) {
    entries.push({
      op,
      precedence: info.precedence,
      associativity: info.associativity,
    });
  }
  return JSON.stringify(entries);
}

export function parse(
  source: string,
  operatorRegistry?: OperatorRegistry,
): Program {
  let json: string;
  if (operatorRegistry) {
    const registryJson = serializeRegistry(operatorRegistry);
    json = (parseWithRegistryToJson as (r: string) => (s: string) => string)(
      registryJson,
    )(source);
  } else {
    json = (parseToJson as (s: string) => string)(source);
  }
  const result = JSON.parse(json, nullReviver);
  if (!result.ok) {
    throw new ParseError(result.message, result.span);
  }
  const program = result.program as Program;
  // ModuleDeclaration.exposing is typed as `Exposing | null`, not optional.
  // The null reviver converts it to undefined; restore null when absent.
  if (program.module && program.module.exposing === undefined) {
    program.module.exposing = null;
  }
  return program;
}

export function parseWithInfix(source: string): {
  program: Program;
  operatorRegistry: OperatorRegistry;
  infixErrors: ParseError[];
} {
  const json = (parseWithInfixToJson as (s: string) => string)(source);
  const result = JSON.parse(json, nullReviver);
  if (!result.ok) {
    throw new ParseError(result.message, result.span);
  }
  const program = result.program as Program;
  if (program.module && program.module.exposing === undefined) {
    program.module.exposing = null;
  }
  return {
    program,
    operatorRegistry: deserializeRegistry(result.registry),
    infixErrors: [],
  };
}

export function collectInfixDeclarations(source: string): {
  registry: OperatorRegistry;
  declarations: InfixDeclaration[];
  errors: ParseError[];
} {
  const json = (collectInfixToJson as (s: string) => string)(source);
  const result = JSON.parse(json, nullReviver);
  return {
    registry: deserializeRegistry(result.registry),
    declarations: [],
    errors: [],
  };
}
