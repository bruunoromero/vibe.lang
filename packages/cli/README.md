# @vibe/cli

Command-line interface for the Lang compiler toolchain.

## REPL (lang repl) ✅

Start an interactive REPL that parses, analyzes, compiles, and executes Lang forms:

- Start REPL with Bun:

  bun packages/cli/index.ts repl

- Options:

  - `--pretty, -p <n>` — Pretty-print JSON results (default: 2)
  - `--debug-macros` — Dump macro-expansion metadata to stderr

- Usage:

  - Primary prompt: `lang> `
  - Continuation prompt: `... ` (used for multi-line forms)
  - Exit the REPL with `exit`, `.exit`, or Ctrl-D

- Example session:

  lang> (+ 1 2)
  {"result":3}

  lang> (def a 10)
  {"result":10}

  lang> a
  {"result":10}

- Notes:
  - The REPL preserves definitions for the session (each input is evaluated in the cumulative session context).
  - Diagnostics (parse/semantic/codegen) are printed to stderr in human-readable form.
