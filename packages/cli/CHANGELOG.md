# @vibe/cli Changelog

## 2025-12-24

- Dropped `package.json#vibe.modules` in favor of a single `vibe.sources` folder plus `vibe.entry`. Package imports now resolve by crawling each `.lang` file under the declared sources, seed module-export metadata automatically, and fall back to `vibe.entry` for bare specifiers like `(require "@vibe/prelude")` without emitting `SEM_IMPORT_MISSING_EXPORTS`.
- The REPL now embeds the auto-loaded prelude into the analyzer history and filters `+`, `-`, `*`, and `/` out of the builtin seeding so those operators behave like ordinary vars supplied by `@vibe/prelude` instead of analyzer-defined builtins.

## 2025-12-23

- Added `vibe build [package|path]`, which walks `package.json#dependencies`, topo-sorts packages that declare `vibe.sources`, and compiles every `.lang` file into the configured `vibe.outDir` so dependents import the emitted JavaScript directly.
- `package.json#vibe` now supports `sources` (one or more Lang roots) and `outDir` (where compiled JS should be written). The CLI normalizes these paths and surfaces helpful errors when packages omit them.
- `vibe run`, `vibe compile`, and `vibe compile-all` now read each dependency's `package.json#vibe.sources` (plus optional `vibe.entry`) metadata so `(require foo "@scope/pkg")` resolves to the right `.lang` file during analysis while the emitted JavaScript keeps importing the package's published JS entry.
- The compile-all resolver learned how to combine workspace metadata with filesystem discovery, allowing package-based requires to be validated alongside relative imports.
- The REPL now feeds analyzer-builtins derived from the live interpreter environment, so prelude-loaded globals (e.g., `println`, `map`, `filter`) no longer rely on an oversized default builtin list.
- Module resolution now treats `(import "./module.lang")` the same as `(require ...)`, providing the analyzer/codegen pipeline with consistent module IDs even though the source form omits an alias.

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
