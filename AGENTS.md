# Project Guide

> This brief is for a single assistant. Use it to understand the scope, constraints, and expectations for contributing to the `vibe` compiler.

## Overview

- Build a Lisp-like programming language implemented entirely in TypeScript, executed with Bun, and transpiled to JavaScript for final output.
- Maintain a Bun-powered monorepo (`workspaces: packages/*`) where each compiler subsystem lives in its own package.
- Prioritize composability between packages while keeping their boundaries clean and deterministic.

## Current packages

- `@vibe/cli`: entry point that parses CLI args, orchestrates compiler stages, and surfaces diagnostics/flags.
- `@vibe/syntax`: shared syntax utilities (tokens, AST nodes, printers, grammar helpers).

## Core constraints

1. Target runtime is Bun; rely on Bun APIs where possible and add `@types/bun` when typing is required.
2. All source code ships as TypeScript that transpiles to JavaScript artifacts.
3. Keep packages isolated yet composable—no circular dependencies; re-export shared primitives from dedicated utility packages when needed.
4. Maintain deterministic builds; avoid global singletons that jump across packages.
5. Ensure every package exposes `bun test` and `bun run build` scripts (tests must succeed via `bun`).
6. Any syntax/token/AST change must update `docs/syntax-spec.md`, the relevant ADR, and the affected package `CHANGELOG.md`.

## Responsibilities

- Extend lexer, parser, semantic analysis, and codegen stages while preserving the immutability guarantees from `@vibe/syntax`.
- Thread diagnostics (with spans) through every pipeline stage and surface them in the CLI.
- Keep ADRs, docs, and changelogs synchronized with implementation changes.

## Workflow expectations

1. Open or update ADRs (`docs/ADR-###.md`) before making cross-cutting changes.
2. Keep package READMEs current with exported APIs and sample usage.
3. Prefer small, package-scoped pull requests; when multiple packages change, describe the coordination plan explicitly.
4. When adding a new package, update the root `package.json`, `tsconfig.base.json`, and supply a minimal integration test wired through the CLI.

## Future expansions

- `@vibe/lexer`: streaming lexer optimized for Bun file IO.
- `@vibe/parser`: Pratt or recursive-descent parser tailored for the Lisp-like surface syntax.
- `@vibe/semantics`: symbol resolution, macro hygiene, and IR production.
- `@vibe/codegen`: JS module emitter with sourcemap support and feature flags.
- `@vibe/runtime`: runtime helpers and stdlib utilities destined for generated code.

## Reference docs

- Syntax specification: `docs/syntax-spec.md` (must stay updated with syntax work).
- Architectural decisions: `docs/ADR-001-parser.md`, `docs/ADR-002-semantics.md`, `docs/ADR-003-codegen.md`.
- Long-term roadmap: `docs/compiler-plan.md`.
