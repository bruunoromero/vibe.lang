# Import Feature Documentation

## Overview

The `import` statement provides a way to load definitions from another vibe module and flatten them directly into the current namespace. This differs from `require`, which creates a namespaced alias that requires qualified access (e.g., `alias/function`).

## Syntax

```clojure
(import "path/to/module.lang")
```

Unlike `require`, `import` does **not** take an alias parameter. All exported definitions are imported directly into the current scope.

## Example

Given a module `math.lang`:

```clojure
(def add (fn [a b] (+ a b)))
(def multiply (fn [a b] (* a b)))
(def pi 3.14159)
```

### Using `require` (namespaced access):

```clojure
(require math "./math.lang")
(math/add 2 3)        ; => 5
(math/multiply 4 5)   ; => 20
math/pi               ; => 3.14159
```

### Using `import` (flat namespace):

```clojure
(import "./math.lang")
(add 2 3)             ; => 5
(multiply 4 5)        ; => 20
pi                    ; => 3.14159
```

## Comparison with require

| Feature        | `require`                 | `import`                        |
| -------------- | ------------------------- | ------------------------------- |
| Syntax         | `(require alias "path")`  | `(import "path")`               |
| Namespace      | Creates aliased namespace | Flattens into current scope     |
| Access         | Qualified: `alias/name`   | Direct: `name`                  |
| Name conflicts | Isolated by alias         | Can overwrite existing bindings |

## Use Cases

### When to use `import`:

- Working with a prelude or standard library that you want always available
- Module has no naming conflicts with current scope
- Prefer concise code without namespace prefixes
- Example: `(import "@vibe/prelude")`

### When to use `require`:

- Want to avoid name conflicts
- Make dependencies explicit
- Multiple modules with overlapping names
- Example: `(require json "./json.lang")` and `(require xml "./xml.lang")`

## Implementation Notes

- `import` is implemented in the semantic analyzer which handles proper scoping and conflict detection at compile time
- At runtime (interpreter), import uses `evaluateAsync()` to handle asynchronous module loading
- The codegen phase properly emits ES module imports with destructuring for flat imports
- Package-level imports rely on each dependency's `package.json#vibe.sources` directory for resolution, and bare specifiers (e.g., `(import "@vibe/prelude")`) load the manifest's `vibe.entry` file by default while nested specifiers map to `.lang` files under that sources folder.

## Error Handling

### Name Conflicts

If an imported name would overwrite an existing binding, the semantic analyzer reports an error:

```clojure
(def add (fn [x] x))
(import "./math.lang")  ; Error: import would overwrite existing binding 'add'
```

### Missing Exports

If a module has no exports or cannot be loaded:

```clojure
(import "./empty.lang")  ; Error: Unable to import because no exports were found
```

## Technical Details

- Defined in `@vibe/syntax` as `NamespaceImportKind` with value `"import"`
- Handled in `@vibe/semantics` analyzer via `handleImport()` method
- Runtime evaluation requires `evaluateAsync()` from `@vibe/interpreter`
- Codegen creates ES6 destructured imports
