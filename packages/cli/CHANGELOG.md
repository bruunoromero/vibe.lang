# @vibe/cli Changelog

## 2025-12-21

- Added `vibe compile-all <dir>` to recursively compile every `.lang` file in a directory tree into a mirror `--out-dir`, so example apps no longer have to invoke `vibe compile` for each entrypoint manually.
- `lang compile-all` now shares a workspace-aware module resolver with the analyzer, so missing `(require ...)` targets surface `SEM_REQUIRE_RESOLVE_FAILED` while the remaining files continue compiling.

## 2025-12-20

- Added the `lang parse` command for printing the AST of inline or file-based source.
- Added the `lang analyze` command to run the semantic analyzer and print scope metadata.
- Added the `lang run` command that wires parser → analyzer → codegen and executes the stub module.
- `vibe run` now executes lowered JavaScript (supporting `def`, `let`, arithmetic) and emits inline source maps for better stack traces.
- Added the `lang compile` command to emit the generated JavaScript to stdout or a file.
- Added a shared parser → analyzer pipeline plus `--debug-macros`/`--pretty` flags so `analyze`, `run`, and `compile` surface macro-aware diagnostics consistently and can dump semantic graphs to stderr without breaking primary output.
- `lang run` and `lang compile` now accept `--show-ast`/`--show-ir` flags that print the parsed AST and generated IR JSON to stderr, making it easier to inspect snapshots without interfering with primary output streams.
