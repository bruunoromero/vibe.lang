import type { Pattern } from "@vibe/syntax";
import type { ADTInfo, ConstructorInfo, Type } from "./types";

// A Matrix row is a list of patterns (a vector) representing the remaining patterns to match for a branch
type PatternVector = Pattern[];
type PatternMatrix = PatternVector[];

/**
 * Result of an exhaustiveness check.
 * - exhaustive: true if the patterns cover all possible values.
 * - missing: a string representation of a missing pattern (counter-example) if not exhaustive.
 */
export type ExhaustivenessResult =
  | { exhaustive: true }
  | { exhaustive: false; missing: string };

/**
 * Main entry point for checking exhaustiveness of a case expression.
 *
 * @param branches - The list of patterns from the case expression branches.
 * @param scrutineeType - The type of the value being matched (needed to determine ADT/Enum completeness).
 * @param adts - Registry of ADT definitions.
 * @param constructors - Registry of constructor definitions.
 */
export function checkExhaustiveness(
  branches: Pattern[],
  scrutineeType: Type,
  adts: Record<string, ADTInfo>,
  constructors: Record<string, ConstructorInfo>,
): ExhaustivenessResult {
  // Convert branches to a matrix of width 1 (since 'case' matches a single expression)
  // Each row corresponds to one case branch.
  const matrix: PatternMatrix = branches.map((p) => [p]);

  // We test if a "Wildcard" pattern is useful given the matrix.
  // If the wildcard is useful, it means there is at least one value not matched by the matrix,
  // so the match is NOT exhaustive. The 'isUseful' function returns a witness pattern if useful.
  const witness = isUseful(matrix, [{ kind: "WildcardPattern", span: createDummySpan() }], adts, constructors);

  if (witness) {
    return { exhaustive: false, missing: patternToString(witness[0]!) };
  }

  return { exhaustive: true };
}

/**
 * Checks if a pattern vector `q` is "useful" with respect to a matrix `P`.
 * A pattern `q` is useful if there exists at least one value matched by `q`
 * that is NOT matched by any row in `P`.
 *
 * Returns a "witness" pattern (counter-example) if useful, or null if not useful (not exhaustive).
 */
function isUseful(
  matrix: PatternMatrix,
  vector: PatternVector,
  adts: Record<string, ADTInfo>,
  constructors: Record<string, ConstructorInfo>,
): Pattern[] | null {
  // Base Case 1: If the matrix has no rows, then nothing matches.
  // So 'vector' is definitely useful (it matches something the matrix doesn't).
  // Return the vector itself as the witness.
  if (matrix.length === 0) {
    return vector;
  }

  // Base Case 2: If the vector is empty (0 columns), and the matrix is NOT empty (checked above),
  // then the matrix has rows of length 0. A row of length 0 matches "unit" (or successfully completed match).
  // Since the matrix has at least one matching row, the input is already covered.
  // So 'vector' is NOT useful.
  if (vector.length === 0) {
    return null;
  }

  const p = vector[0]!;
  const restVector = vector.slice(1);

  // Case 3: The first pattern in the vector is a Constructor (e.g., `Just x`, `(x, y)`, `x :: xs`)
  if (isConstructorLike(p)) {
    const { name, args } = getConstructorDecomposition(p);
    
    // Specialize the matrix for this constructor
    const specializedMatrix = specialize(matrix, name, args.length, adts, constructors);
    
    // Check usefulness recursively with the expanded arguments
    const witness = isUseful(specializedMatrix, [...args, ...restVector], adts, constructors);
    
    if (witness) {
      // Reconstruct the witness by wrapping the result back in the constructor
      const witnessArgs = witness.slice(0, args.length);
      const witnessRest = witness.slice(args.length);
      return [reconstructConstructor(p, witnessArgs), ...witnessRest];
    }
    return null;
  }

  // Case 4: The first pattern in the vector is a Wildcard or Variable
  // We need to check if the matrix covers all possible constructors for the type of this column.
  if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
    // Collect the set of constructors present in the first column of the matrix
    const usedConstructors = getUsedConstructors(matrix);
    
    // Determine the set of ALL possible constructors for this type
    // Note: We infer this from the used constructors or type info if available.
    // For now, we try to deduce completeness from the matrix.
    const typeInfo = inferTypeInfo(usedConstructors, adts, constructors);

    if (typeInfo.isComplete) {
      // If the matrix covers all constructors, we must check if `vector` is useful 
      // for *each* constructor. If it's useful for ANY constructor, then it's useful overall.
      // Actually, if the matrix lists ALL constructors explicitly in the first column,
      // then a Wildcard pattern is only useful if one of the sub-patterns is useful 
      // for the corresponding specialized matrix.
      
      // We iterate over all possible constructors `c`
      for (const ctor of typeInfo.allConstructors) {
        const arity = getConstructorArity(ctor, constructors, adts);
        const specializedMatrix = specialize(matrix, ctor, arity, adts, constructors);
        // Specialize the vector (Wildcard becomes c(_, ..., _))
        // We simulate `c(_, ...)` by passing `arity` wildcards
        const dummyArgs = Array(arity).fill({ kind: "WildcardPattern", span: createDummySpan() });
        
        const witness = isUseful(specializedMatrix, [...dummyArgs, ...restVector], adts, constructors);
        if (witness) {
           // We found a gap in this constructor branch
           const witnessArgs = witness.slice(0, arity);
           const witnessRest = witness.slice(arity);
           // Reconstruct: C(args)
           const ctorPattern: Pattern = createConstructorPattern(ctor, witnessArgs);
           return [ctorPattern, ...witnessRest];
        }
      }
      // If we checked all constructors and found no gaps, then Wildcard is not unique.
      return null;

    } else {
      // The matrix does NOT cover all constructors.
      // The Wildcard represents the missing constructors.
      // So checking `isUseful` for the default matrix (rows that are wildcard-like)
      // is sufficient?
      // No, if there is a missing constructor `M`, then `Wildcard` matches `M`. 
      // Since `M` is not in the matrix (by definition of missing), the matrix patterns
      // for `M` are only those that were originally Wildcards.
      
      const defaultMatrix = specializeDefault(matrix);
      const witness = isUseful(defaultMatrix, restVector, adts, constructors);
      
      if (witness) {
        // We found a witness in the default case.
        // We need to pick a concrete missing constructor to report.
        const missingCtor = pickMissingConstructor(typeInfo.allConstructors, usedConstructors);
        if (missingCtor) {
           // If we have a concrete missing constructor (e.g., "Nothing"), use it.
           const arity = getConstructorArity(missingCtor, constructors, adts);
           const args = Array(arity).fill({ kind: "WildcardPattern", span: createDummySpan() });
           return [createConstructorPattern(missingCtor, args), ...witness];
        } else {
           // Infinite type (Int, String) or unknown. Keep as wildcard or generic placeholder.
           return [p, ...witness];
        }
      }
      return null;
    }
  }

  return null;
}

// --- Helpers ------------------------------------------

function isConstructorLike(p: Pattern): boolean {
  return (
    p.kind === "ConstructorPattern" ||
    p.kind === "TuplePattern" ||
    p.kind === "ListPattern" ||
    p.kind === "ConsPattern"
  );
}

function getConstructorDecomposition(p: Pattern): { name: string; args: Pattern[] } {
  switch (p.kind) {
    case "ConstructorPattern":
      return { name: p.name, args: p.args };
    case "TuplePattern":
      return { name: `Tuple${p.elements.length}`, args: p.elements };
    case "ConsPattern":
      return { name: "::", args: [p.head, p.tail] };
    case "ListPattern":
       // Convert [a, b] -> a :: b :: []
       if (p.elements.length === 0) return { name: "[]", args: [] };
       // This is a simplification. For exhaustiveness, [x] is x :: [].
       // Ideally we Desugar lists before checking, or handle "ListPattern" specially.
       // Here we convert to Cons structure on the fly?
       // Actually, strictly speaking, [a] is Cons(a, Nil).
       // Let's treat it as such.
       return convertListToCons(p.elements);
    default:
      throw new Error(`Unexpected pattern kind in decomposition: ${p.kind}`);
  }
}

function convertListToCons(elements: Pattern[]): { name: string; args: Pattern[] } {
  if (elements.length === 0) {
    return { name: "[]", args: [] };
  }
  const [head, ...tail] = elements;
  // Make a list pattern for the tail to recurse
  const tailPattern: Pattern = { kind: "ListPattern", elements: tail, span: createDummySpan() };
  return { name: "::", args: [head!, tailPattern] };
}

function reconstructConstructor(original: Pattern, args: Pattern[]): Pattern {
  if (original.kind === "ConstructorPattern") {
    return { ...original, args };
  }
  if (original.kind === "TuplePattern") {
    return { ...original, elements: args };
  }
  if (original.kind === "ConsPattern") {
    return { ...original, head: args[0]!, tail: args[1]! };
  }
  // For ListPattern, reconstruction is tricky if we desugared it.
  // But purely for reporting, we can try to rebuild a list or use Cons.
  if (original.kind === "ListPattern") {
     // If we desugared [x] to x :: [], and got back witness args [w_head, w_tail]
     // We can try to rebuild [w_head, ...w_tail] if w_tail is a list.
     // For simplicity, return Cons syntax or rebuild list if possible.
     return { kind: "ConsPattern", head: args[0]!, tail: args[1]!, span: createDummySpan() };
  }
  // Literals don't have args
  return original;
}

function createConstructorPattern(name: string, args: Pattern[]): Pattern {
  if (name.startsWith("Tuple")) {
    return { kind: "TuplePattern", elements: args, span: createDummySpan() };
  }
  if (name === "::") {
    return { kind: "ConsPattern", head: args[0]!, tail: args[1]!, span: createDummySpan() };
  }
  if (name === "[]") {
    return { kind: "ListPattern", elements: [], span: createDummySpan() };
  }
  return { kind: "ConstructorPattern", name, args, span: createDummySpan() };
}

/**
 * Filter and transform the matrix for a specific constructor `c`.
 * Keeps rows that start with `c` (extracting args) or Wildcard (expanding to wildcards).
 */
function specialize(
  matrix: PatternMatrix,
  ctorName: string,
  arity: number,
  adts: Record<string, ADTInfo>,
  constructors: Record<string, ConstructorInfo>
): PatternMatrix {
  const newMatrix: PatternMatrix = [];
  
  for (const row of matrix) {
    if (row.length === 0) continue;
    const p = row[0]!;
    const rest = row.slice(1);

    if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
      // Expand wildcard to matches arguments: _, _, ..., _
      const newArgs = Array(arity).fill({ kind: "WildcardPattern", span: p.span });
      newMatrix.push([...newArgs, ...rest]);
    } else if (isConstructorLike(p)) {
      const { name, args } = getConstructorDecomposition(p);
      if (name === ctorName) {
        // We handle ListPattern specially to ensure arity matches the "::" or "[]" view
        // If we specialized on "::", getConstructorDecomposition returns 2 args.
        // If "[]", 0 args.
        newMatrix.push([...args, ...rest]);
      }
    }
  }
  
  return newMatrix;
}

/**
 * Filter the matrix for the "default" case (when the constructor is NOT one of the explicitly present ones).
 * Keeps rows that start with Wildcard/Var.
 */
function specializeDefault(matrix: PatternMatrix): PatternMatrix {
  const newMatrix: PatternMatrix = [];
  for (const row of matrix) {
    if (row.length === 0) continue;
    const p = row[0]!;
    const rest = row.slice(1);
    if (p.kind === "WildcardPattern" || p.kind === "VarPattern") {
      newMatrix.push(rest);
    }
  }
  return newMatrix;
}

function getUsedConstructors(matrix: PatternMatrix): Set<string> {
  const used = new Set<string>();
  for (const row of matrix) {
    if (row.length > 0) {
      const p = row[0]!;
      if (isConstructorLike(p)) {
        const { name } = getConstructorDecomposition(p);
        used.add(name);
      }
    }
  }
  return used;
}

function inferTypeInfo(
  usedConstructors: Set<string>,
  adts: Record<string, ADTInfo>,
  constructors: Record<string, ConstructorInfo>
): { isComplete: boolean; allConstructors: string[] } {
  if (usedConstructors.size === 0) {
    return { isComplete: false, allConstructors: [] };
  }
  
  const firstCtor = usedConstructors.values().next().value!;

  // Handle builtin Tuple types (Tuple2, Tuple3, ...)
  if (firstCtor.startsWith("Tuple")) {
    return { isComplete: true, allConstructors: [firstCtor] };
  }

  // Handle Lists
  if (firstCtor === "::" || firstCtor === "[]") {
    return { isComplete: true, allConstructors: ["[]", "::"] };
  }

  // Handle ADTs
  // Look up parent type of the constructor
  const info = constructors[firstCtor];
  if (info) {
    const adt = adts[info.parentType];
    if (adt) {
      // Check if we have all constructors
      const all = adt.constructors;
      // It is complete if the used set covers all? 
      // No, `isComplete` here means "Do we know the closed set of all constructors?"
      // Yes, for ADTs we do.
      return { isComplete: true, allConstructors: all };
    }
  }

  // Fallback
  return { isComplete: false, allConstructors: [] };
}

function getConstructorArity(
  name: string,
  constructors: Record<string, ConstructorInfo>,
  adts: Record<string, ADTInfo>
): number {
  if (name === "::") return 2;
  if (name === "[]") return 0;
  if (name.startsWith("Tuple")) {
     // extract N from TupleN? Or just trust usage?
     // We can just parse the number typically, but here we might not have it.
     // However, we only call this for known constructors.
     // In `inferTypeInfo` we returned `[firstCtor]` for tuples.
     // So we can assume the arity matches the usage found in specialization?
     // Actually, if we are in `inferTypeInfo`'s "isComplete" branch, we are iterating `allConstructors`.
     // For Tuples, `allConstructors` has 1 element: the tuple constructor itself.
     // We need to know its arity.
     // We can pass `usedConstructors` into a better structure to retrieve it.
     // Hack: extract from name? "Tuple2" -> 2?
     // Vibe "Tuple" Ast doesn't have explicit names like Tuple2. 
     // `getConstructorDecomposition` generated `Tuple${len}`.
     const match = name.match(/^Tuple(\d+)$/);
     if (match) return parseInt(match[1]!, 10);
     return 0; // Should not happen
  }
  
  const info = constructors[name];
  if (info) return info.arity;
  
  return 0; // Unknown or literal
}

function pickMissingConstructor(all: string[], used: Set<string>): string | undefined {
  for (const c of all) {
    if (!used.has(c)) return c;
  }
  return undefined;
}

function createDummySpan() {
  return { start: { offset: 0, line: 0, column: 0 }, end: { offset: 0, line: 0, column: 0 } };
}

function patternToString(p: Pattern): string {
    switch (p.kind) {
        case "WildcardPattern": return "_";
        case "VarPattern": return p.name;
        case "ConstructorPattern":
            if (p.args.length === 0) return p.name;
            return `(${p.name} ${p.args.map(patternToString).join(" ")})`;
        case "TuplePattern":
            return `(${p.elements.map(patternToString).join(", ")})`;
        case "ListPattern":
            return `[${p.elements.map(patternToString).join(", ")}]`;
        case "ConsPattern":
            return `(${patternToString(p.head)} :: ${patternToString(p.tail)})`;
        case "RecordPattern":
            const fields = p.fields.map(f => `${f.name}${f.pattern ? " = " + patternToString(f.pattern) : ""}`);
            return `{ ${fields.join(", ")} }`; 
        // case "Number": return p.value;
        // case "String": return p.value;
        // case "Char": return p.value;
    }
}
