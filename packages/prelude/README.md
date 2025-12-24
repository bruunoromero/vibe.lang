# @vibe/prelude

Standard library for the vibe language, written entirely in vibe itself.

## Overview

The prelude provides all standard library functions by importing primitives from `@vibe/runtime` and composing them into higher-level functions. This makes the stdlib:

- **Self-hosted**: Written in vibe, not JavaScript
- **Extensible**: Easy to add new functions
- **Transparent**: Users can read and understand the implementation

## Usage

The prelude is **automatically loaded by the REPL only** for convenience.

When writing vibe source files, you can optionally import prelude:

```clojure
(require prelude "./path/to/prelude.lang")

;; Or use primitives directly (they're available as built-ins in the interpreter)
(def doubled (map (fn [x] (* x 2)) [1 2 3]))
```

For compiled code that needs stdlib functions, you would typically include a prelude require statement or use the primitives directly.

## Architecture

```clojure
(external runtime "@vibe/runtime")

;; Define + using the runtime primitive add*
(def + (fn [x y] (runtime/add* x y)))

;; Higher-level functions compose primitives
(def map (fn [f coll]
  (if (runtime/eq* (runtime/count coll) 0)
    []
    (runtime/cons (f (runtime/first coll))
                  (map f (runtime/rest coll))))))
```

## Provided Functions

See [prelude.lang](./src/prelude.lang) for the complete list.
