import type { Span } from "@vibe/syntax";

/**
 * Base error class for semantic analysis errors.
 * Includes source span information for error reporting.
 */
export class SemanticError extends Error {
  constructor(
    message: string,
    public readonly span: Span,
    public readonly filePath: string,
  ) {
    super(message);
  }
}

/**
 * Error thrown when a type uses 'implementing' with a protocol that has methods without defaults.
 * Provides a clear message listing missing defaults and actionable hints.
 */
export class ImplementingProtocolError extends SemanticError {
  constructor(
    public readonly typeName: string,
    public readonly protocolName: string,
    public readonly methodsWithoutDefaults: string[],
    public readonly methodsWithDefaults: string[],
    span: Span,
    public override readonly filePath: string,
  ) {
    const missing = methodsWithoutDefaults.join(", ");
    const hasDefaults =
      methodsWithDefaults.length > 0
        ? `\n  Methods with defaults: ${methodsWithDefaults.join(", ")}`
        : "";

    const message =
      `Cannot use 'implementing' with protocol '${protocolName}' for type '${typeName}'\n` +
      `  Protocol '${protocolName}' has methods without default implementations\n` +
      `  Missing defaults for: ${missing}${hasDefaults}\n\n` +
      `Hint: Either add default implementations for these methods in the protocol,\n` +
      `      or write a manual 'implement' block for this type.`;

    super(message, span, filePath);
    this.name = "ImplementingProtocolError";
  }
}

/**
 * Error container for multiple accumulated semantic errors.
 * Thrown at the end of analysis when errors have been collected.
 * Follows Elm's approach: per-definition isolation allows collecting
 * errors across definitions while preserving error quality.
 */
export class MultipleSemanticErrors extends Error {
  constructor(public readonly errors: SemanticError[]) {
    const count = errors.length;
    const summary = count === 1
      ? errors[0]!.message
      : `${count} semantic error(s) found`;
    super(summary);
    this.name = "MultipleSemanticErrors";
  }
}
