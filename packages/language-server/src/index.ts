/**
 * Vibe Language Server
 *
 * Main entry point - exports server and types.
 */

// Re-export types
export * from "./types.js";

// Re-export document manager
export { DocumentManager } from "./document-manager.js";

// Re-export semantic tokens
export {
  SEMANTIC_TOKENS_LEGEND,
  TOKEN_TYPES,
  TOKEN_MODIFIERS,
  provideSemanticTokens,
} from "./semantic-tokens.js";

// Server is started via server.ts directly (stdio)
