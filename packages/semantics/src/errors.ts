import type { Span } from "@vibe/syntax";

/**
 * Base error class for semantic analysis errors.
 * Includes source span information for error reporting.
 */
export class SemanticError extends Error {
  constructor(message: string, public readonly span: Span) {
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
    span: Span
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

    super(message, span);
    this.name = "ImplementingProtocolError";
  }
}
