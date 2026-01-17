/**
 * IR Pretty Printer
 *
 * Provides human-readable string representations of IR structures.
 * Useful for debugging and understanding the lowered code.
 */

import type {
  IRProgram,
  IRValue,
  IRExpr,
  IRPattern,
  IRType,
  IRConstraint,
  SCC,
} from "./types";

// ============================================================================
// Pretty Printing Options
// ============================================================================

export interface PrettyPrintOptions {
  /** Indentation string (default: "  ") */
  indent?: string;
  /** Whether to include type annotations (default: true) */
  showTypes?: boolean;
  /** Whether to include constraint annotations (default: true) */
  showConstraints?: boolean;
  /** Whether to include spans (default: false) */
  showSpans?: boolean;
  /** Maximum line width before wrapping (default: 80) */
  maxWidth?: number;
}

const defaultOptions: Required<PrettyPrintOptions> = {
  indent: "  ",
  showTypes: true,
  showConstraints: true,
  showSpans: false,
  maxWidth: 80,
};

// ============================================================================
// Main Pretty Print Functions
// ============================================================================

/**
 * Pretty print an entire IR program.
 */
export function printProgram(
  program: IRProgram,
  options: PrettyPrintOptions = {},
): string {
  const opts = { ...defaultOptions, ...options };
  const lines: string[] = [];

  // Module header
  lines.push(`module ${program.moduleName}`);
  lines.push("");

  // External imports
  if (program.externalImports.size > 0) {
    lines.push("-- External Imports");
    for (const imp of program.externalImports) {
      lines.push(`import "${imp}"`);
    }
    lines.push("");
  }

  // ADTs
  if (Object.keys(program.adts).length > 0) {
    lines.push("-- Types");
    for (const [name, adt] of Object.entries(program.adts)) {
      const params = adt.params.length > 0 ? " " + adt.params.join(" ") : "";
      const ctors = adt.constructors
        .map((c) => {
          const info = program.constructors[c];
          const tag = info ? `[${info.tag}]` : "";
          return `${c}${tag}`;
        })
        .join(" | ");
      lines.push(`type ${name}${params} = ${ctors}`);
    }
    lines.push("");
  }

  // Protocols
  if (Object.keys(program.protocols).length > 0) {
    lines.push("-- Protocols");
    for (const [name, proto] of Object.entries(program.protocols)) {
      const params = proto.params.join(" ");
      lines.push(`protocol ${name} ${params} where`);
      for (const method of proto.methods) {
        const def = method.hasDefault ? " {default}" : "";
        if (opts.showTypes) {
          lines.push(
            `${opts.indent}${method.name} : ${printType(method.type)}${def}`,
          );
        } else {
          lines.push(`${opts.indent}${method.name}${def}`);
        }
      }
    }
    lines.push("");
  }

  // Instances
  if (program.instances.length > 0) {
    lines.push("-- Instances");
    for (const inst of program.instances) {
      const typeArgs = inst.typeArgs.map(printType).join(" ");
      const constraints =
        inst.constraints.length > 0
          ? `(${inst.constraints.map(printConstraint).join(", ")}) => `
          : "";
      lines.push(
        `implement ${constraints}${inst.protocolName} ${typeArgs} where`,
      );
      for (const [method, impl] of Object.entries(inst.methods)) {
        lines.push(`${opts.indent}${method} = ${impl}`);
      }
    }
    lines.push("");
  }

  // Values by dependency order
  lines.push("-- Values (in dependency order)");
  for (const scc of program.dependencyOrder) {
    if (scc.isMutuallyRecursive) {
      lines.push(`-- SCC (mutually recursive): ${scc.values.join(", ")}`);
    }
    for (const valueName of scc.values) {
      const value = program.values[valueName];
      if (value) {
        lines.push(printValue(value, opts));
        lines.push("");
      }
    }
  }

  // Lifted bindings
  if (program.liftedBindings.length > 0) {
    lines.push("-- Lifted Bindings");
    for (const binding of program.liftedBindings) {
      lines.push(printValue(binding, opts));
      lines.push("");
    }
  }

  return lines.join("\n");
}

/**
 * Pretty print a single IR value.
 */
export function printValue(
  value: IRValue,
  options: PrettyPrintOptions = {},
): string {
  const opts = { ...defaultOptions, ...options };
  const lines: string[] = [];

  // Constraints annotation
  if (opts.showConstraints && value.constraints.length > 0) {
    const constraints = value.constraints.map(printConstraint).join(", ");
    lines.push(`-- Constraints: ${constraints}`);
  }

  // Type annotation
  if (opts.showTypes) {
    lines.push(`${value.name} : ${printType(value.type)}`);
  }

  // External marker
  if (value.isExternal && value.externalTarget) {
    lines.push(
      `@external "${value.externalTarget.modulePath}" "${value.externalTarget.exportName}"`,
    );
  }

  // Value definition
  const params =
    value.params.length > 0
      ? " " + value.params.map((p) => printPattern(p)).join(" ")
      : "";

  if (value.isExternal) {
    lines.push(`${value.name}${params}`);
  } else {
    const body = printExpr(value.body, opts.indent, 0);
    lines.push(`${value.name}${params} =`);
    lines.push(`${opts.indent}${body}`);
  }

  return lines.join("\n");
}

/**
 * Pretty print an IR expression.
 */
export function printExpr(
  expr: IRExpr,
  indent: string = "  ",
  depth: number = 0,
): string {
  const ind = indent.repeat(depth);
  const ind1 = indent.repeat(depth + 1);

  switch (expr.kind) {
    case "IRVar":
      return expr.name;

    case "IRModuleAccess":
      return `${expr.importAlias}.${expr.externalName || expr.valueName}`;

    case "IRLiteral":
      if (expr.literalType === "string") {
        return `"${expr.value}"`;
      } else if (expr.literalType === "char") {
        return `'${expr.value}'`;
      } else if (expr.literalType === "bool") {
        return expr.value ? "True" : "False";
      }
      return String(expr.value);

    case "IRLambda": {
      const params = expr.params.map(printPattern).join(" ");
      const body = printExpr(expr.body, indent, 0);
      return `\\${params} -> ${body}`;
    }

    case "IRApply": {
      const callee = printExpr(expr.callee, indent, 0);
      const args = expr.args.map((a) => printExpr(a, indent, 0));

      // Wrap callee in parens if it's a lambda
      const calleeStr =
        expr.callee.kind === "IRLambda" ? `(${callee})` : callee;

      // Wrap args in parens if they're complex
      const argStrs = args.map((a, i) => {
        const arg = expr.args[i];
        if (
          arg &&
          (arg.kind === "IRApply" ||
            arg.kind === "IRLambda" ||
            arg.kind === "IRIf" ||
            arg.kind === "IRCase")
        ) {
          return `(${a})`;
        }
        return a;
      });

      return `${calleeStr} ${argStrs.join(" ")}`;
    }

    case "IRIf":
      return (
        `if ${printExpr(expr.condition, indent, 0)}\n` +
        `${ind1}then ${printExpr(expr.thenBranch, indent, depth + 1)}\n` +
        `${ind1}else ${printExpr(expr.elseBranch, indent, depth + 1)}`
      );

    case "IRCase": {
      const disc = printExpr(expr.discriminant, indent, 0);
      const branches = expr.branches
        .map((b) => {
          const pat = printPattern(b.pattern);
          const body = printExpr(b.body, indent, depth + 2);
          return `${ind1}${pat} -> ${body}`;
        })
        .join("\n");
      return `case ${disc} of\n${branches}`;
    }

    case "IRTuple": {
      const elems = expr.elements.map((e) => printExpr(e, indent, 0));
      return `(${elems.join(", ")})`;
    }

    case "IRUnit":
      return "()";

    case "IRList": {
      const elems = expr.elements.map((e) => printExpr(e, indent, 0));
      return `[${elems.join(", ")}]`;
    }

    case "IRRecord": {
      const fields = expr.fields.map(
        (f) => `${f.name} = ${printExpr(f.value, indent, 0)}`,
      );
      return `{ ${fields.join(", ")} }`;
    }

    case "IRRecordUpdate": {
      const base = printExpr(expr.base, indent, 0);
      const updates = expr.updates.map(
        (f) => `${f.name} = ${printExpr(f.value, indent, 0)}`,
      );
      return `{ ${base} | ${updates.join(", ")} }`;
    }

    case "IRFieldAccess":
      return `${printExpr(expr.target, indent, 0)}.${expr.field}`;

    case "IRConstructor": {
      if (expr.args.length === 0) {
        return expr.name;
      }
      const args = expr.args.map((a) => {
        const s = printExpr(a, indent, 0);
        return a.kind === "IRApply" || a.kind === "IRConstructor"
          ? `(${s})`
          : s;
      });
      return `${expr.name} ${args.join(" ")}`;
    }

    case "IRUnary":
      return `-${printExpr(expr.operand, indent, 0)}`;

    default:
      return `<unknown: ${(expr as any).kind}>`;
  }
}

/**
 * Pretty print an IR pattern.
 */
export function printPattern(pattern: IRPattern): string {
  switch (pattern.kind) {
    case "IRVarPattern":
      return pattern.name;

    case "IRWildcardPattern":
      return "_";

    case "IRConstructorPattern": {
      if (pattern.args.length === 0) {
        return pattern.name;
      }
      const args = pattern.args.map(printPattern);
      return `(${pattern.name} ${args.join(" ")})`;
    }

    case "IRTuplePattern": {
      const elems = pattern.elements.map(printPattern);
      return `(${elems.join(", ")})`;
    }

    case "IRLiteralPattern":
      if (pattern.literalType === "string") {
        return `"${pattern.value}"`;
      } else if (pattern.literalType === "char") {
        return `'${pattern.value}'`;
      }
      return String(pattern.value);

    case "IRListPattern": {
      const elems = pattern.elements.map(printPattern);
      return `[${elems.join(", ")}]`;
    }

    case "IRConsPattern":
      return `(${printPattern(pattern.head)} :: ${printPattern(pattern.tail)})`;

    default:
      return `<unknown: ${(pattern as any).kind}>`;
  }
}

/**
 * Pretty print an IR type.
 */
export function printType(type: IRType): string {
  switch (type.kind) {
    case "var":
      // Use letters for type variables
      if (type.id >= 0 && type.id < 26) {
        return String.fromCharCode(97 + type.id); // a, b, c, ...
      }
      return `t${type.id}`;

    case "con":
      if (type.args.length === 0) {
        return type.name;
      }
      const args = type.args.map((a) => {
        const s = printType(a);
        // Wrap function types in parens
        return a.kind === "fun" ? `(${s})` : s;
      });
      return `${type.name} ${args.join(" ")}`;

    case "fun": {
      const from = printType(type.from);
      const to = printType(type.to);
      // Wrap from in parens if it's a function type
      const fromStr = type.from.kind === "fun" ? `(${from})` : from;
      return `${fromStr} -> ${to}`;
    }

    case "tuple": {
      const elems = type.elements.map(printType);
      return `(${elems.join(", ")})`;
    }

    case "record": {
      const fields = Object.entries(type.fields)
        .map(([k, v]) => `${k} : ${printType(v)}`)
        .join(", ");
      return `{ ${fields} }`;
    }

    case "list":
      return `List ${printType(type.element)}`;

    default:
      return `<unknown>`;
  }
}

/**
 * Pretty print a constraint.
 */
export function printConstraint(constraint: IRConstraint): string {
  const args = constraint.typeArgs.map(printType).join(" ");
  return `${constraint.protocolName} ${args}`;
}

/**
 * Pretty print an SCC (dependency info).
 */
export function printSCC(scc: SCC): string {
  const kind = scc.isMutuallyRecursive ? "recursive" : "single";
  return `[${kind}] ${scc.values.join(", ")}`;
}

/**
 * Print dependency order as a readable list.
 */
export function printDependencyOrder(sccs: SCC[]): string {
  return sccs.map((scc, i) => `${i + 1}. ${printSCC(scc)}`).join("\n");
}

// ============================================================================
// Compact Printing (for debugging)
// ============================================================================

/**
 * Print a compact one-line representation of an expression.
 */
export function printExprCompact(expr: IRExpr): string {
  switch (expr.kind) {
    case "IRVar":
      return expr.name;
    case "IRModuleAccess":
      return `${expr.importAlias}.${expr.externalName || expr.valueName}`;
    case "IRLiteral":
      return String(expr.value);
    case "IRLambda":
      return `(\\... -> ...)`;
    case "IRApply":
      return `(${printExprCompact(expr.callee)} ...)`;
    case "IRIf":
      return `(if ... then ... else ...)`;
    case "IRCase":
      return `(case ... of ...)`;
    case "IRTuple":
      return `(${expr.elements.length} elems)`;
    case "IRUnit":
      return "()";
    case "IRList":
      return `[${expr.elements.length} elems]`;
    case "IRRecord":
      return `{${expr.fields.length} fields}`;
    case "IRFieldAccess":
      return `_.${expr.field}`;
    case "IRRecordUpdate":
      return `{ ... | ${expr.updates.length} fields }`;
    case "IRConstructor":
      return expr.args.length > 0 ? `${expr.name} ...` : expr.name;
    case "IRUnary":
      return `-${printExprCompact(expr.operand)}`;
    default:
      return "?";
  }
}
