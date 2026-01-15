/**
 * Vibe Language Server
 *
 * Main entry point - exports server and types.
 */

// Re-export types
export * from "./types";

// Re-export document manager
export { DocumentManager } from "./document-manager";

// Re-export semantic tokens
export {
  SEMANTIC_TOKENS_LEGEND,
  TOKEN_TYPES,
  TOKEN_MODIFIERS,
  provideSemanticTokens,
} from "./semantic-tokens";

// Server is started via server.ts directly (stdio)
