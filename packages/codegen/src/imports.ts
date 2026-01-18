/**
 * @vibe/codegen - Import Path Calculation
 *
 * Utilities for calculating relative import paths between modules.
 */

import type { IRProgram } from "@vibe/ir";
import { sanitizeOperator } from "@vibe/syntax";
import { sanitizeIdentifier } from "./sanitize.js";

/**
 * External bindings map type: module path -> (Vibe name -> runtime name)
 */
export type ExternalBindingsMap = Map<string, Map<string, string>>;

/**
 * Generate import statements for external (FFI) modules.
 */
export function generateExternalImports(
  externalBindings: ExternalBindingsMap,
): string[] {
  const lines: string[] = [];

  for (const [modulePath, bindings] of externalBindings) {
    if (bindings.size > 0) {
      // Generate import specifiers with aliasing where needed
      // e.g., "intAdd as add" when Vibe name differs from runtime name
      const importSpecifiers: string[] = [];
      const sortedEntries = Array.from(bindings.entries()).sort(([a], [b]) =>
        a.localeCompare(b),
      );

      for (const [vibeName, runtimeName] of sortedEntries) {
        const safeVibeName = sanitizeIdentifier(vibeName);
        if (runtimeName === vibeName || runtimeName === safeVibeName) {
          // No aliasing needed
          importSpecifiers.push(runtimeName);
        } else {
          // Alias runtime name to Vibe name
          importSpecifiers.push(`${runtimeName} as ${safeVibeName}`);
        }
      }

      lines.push(
        `import { ${importSpecifiers.join(", ")} } from "${modulePath}";`,
      );
    }
  }

  return lines;
}

/**
 * Generate import statements for dependency modules.
 *
 * The import path is calculated based on:
 * - Current module's package and path depth
 * - Imported module's package and path
 *
 * Examples:
 * - ExampleApp/SimpleTest.js importing Vibe/Vibe.js -> ../Vibe/Vibe.js
 * - ExampleApp/Sub/Module.js importing Vibe/Vibe.js -> ../../Vibe/Vibe.js
 * - ExampleApp/SimpleTest.js importing ExampleApp/ExampleApp.js -> ./ExampleApp.js
 */
export function generateDependencyImports(
  program: IRProgram,
  modulePackages: Map<string, string>,
): string[] {
  const lines: string[] = [];

  // Get imports from source program
  const imports = program.sourceProgram.imports || [];
  const currentModule = program.moduleName;
  const currentPackage = program.packageName;
  const importedModules = new Set<string>();

  // Calculate the depth of the current module within its package
  // e.g., "SimpleTest" -> 0, "Sub.Module" -> 1
  const currentDepth = currentModule.split(".").length - 1;

  for (const imp of imports) {
    const moduleName = imp.moduleName;
    importedModules.add(moduleName);

    // Get the imported module's package
    const importedPackage = modulePackages.get(moduleName) || moduleName;

    // Calculate relative import path
    const importPath = calculateImportPath(
      currentPackage,
      currentDepth,
      importedPackage,
      moduleName,
    );

    // Determine alias name: explicit alias or default to last segment
    const alias = imp.alias || moduleName.split(".").pop() || moduleName;

    // If there's an alias, always generate namespace import
    // This is needed for qualified access like `Bool.eq`
    if (imp.alias) {
      lines.push(`import * as ${alias} from "${importPath}";`);
    }

    if (imp.exposing?.kind === "All") {
      // import * as ModuleName from "..."
      // Only generate if we didn't already generate it above for the alias
      if (!imp.alias) {
        lines.push(`import * as ${alias} from "${importPath}";`);
      }
    } else if (imp.exposing?.kind === "Explicit") {
      // Extract names from the export specs
      const names: string[] = [];

      for (const spec of imp.exposing.exports) {
        switch (spec.kind) {
          case "ExportValue":
            names.push(sanitizeIdentifier(spec.name));
            break;
          case "ExportOperator":
            names.push(sanitizeOperator(spec.operator));
            break;
          case "ExportTypeAll": {
            // For ADTs, import the constructors, not the type name
            // Types don't exist at runtime, only constructors do
            const adtInfo = program.adts[spec.name];
            if (adtInfo && adtInfo.constructors.length > 0) {
              for (const ctorName of adtInfo.constructors) {
                names.push(sanitizeIdentifier(ctorName));
              }
            }
            // Note: For protocols, ExportTypeAll means import protocol methods,
            // but those are typically accessed via dictionaries, not direct imports
            break;
          }
          case "ExportTypeSome": {
            // For ExportTypeSome, import only the specified constructors
            if (spec.members) {
              for (const ctorName of spec.members) {
                names.push(sanitizeIdentifier(ctorName));
              }
            }
            break;
          }
          default:
            break;
        }
      }

      if (names.length > 0) {
        // import { specific, names } from "..."
        lines.push(`import { ${names.join(", ")} } from "${importPath}";`);
      } else if (!imp.alias) {
        // No explicit imports and no alias, import everything as namespace
        lines.push(`import * as ${alias} from "${importPath}";`);
      }
      // If we have alias but no names, namespace import was already generated above
    } else if (!imp.alias) {
      // Default: import everything (exposing all) - only if no alias
      lines.push(`import * as ${alias} from "${importPath}";`);
    }
  }

  return lines;
}

/**
 * Calculate the relative import path between two modules.
 *
 * @param currentPackage - Package of the importing module
 * @param currentDepth - Depth of importing module within its package (0 for top-level)
 * @param importedPackage - Package of the imported module
 * @param importedModule - Full module name of the imported module
 */
export function calculateImportPath(
  currentPackage: string,
  currentDepth: number,
  importedPackage: string,
  importedModule: string,
): string {
  // Convert module name to path segments
  const moduleSegments = importedModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath =
    moduleSegments.length > 1
      ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js`
      : `${fileName}.js`;

  if (currentPackage === importedPackage) {
    // Same package - use relative path within package
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    // Different package - go up to dist, then into imported package
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${importedPackage}/${modulePath}`;
  }
}

/**
 * Calculate the import path for re-exporting from another module.
 * This mirrors the logic in generateDependencyImports.
 */
export function calculateReExportPath(
  program: IRProgram,
  targetModule: string,
  modulePackages: Map<string, string>,
): string {
  const currentModule = program.moduleName;
  const currentPackage = program.packageName;

  // Calculate the depth of the current module within its package
  const currentDepth = currentModule.split(".").length - 1;

  // Look up the target package from modulePackages map
  // Default to the first segment of the module name (e.g., "Vibe" from "Vibe.Maybe")
  const targetPackage =
    modulePackages.get(targetModule) || targetModule.split(".")[0];

  // Convert module name to path segments
  const moduleSegments = targetModule.split(".");
  const fileName = moduleSegments[moduleSegments.length - 1];
  const modulePath =
    moduleSegments.length > 1
      ? moduleSegments.slice(0, -1).join("/") + `/${fileName}.js`
      : `${fileName}.js`;

  if (currentPackage === targetPackage) {
    // Same package - use relative path within package
    if (currentDepth === 0) {
      return `./${modulePath}`;
    } else {
      const ups = "../".repeat(currentDepth);
      return `${ups}${modulePath}`;
    }
  } else {
    // Different package - go up to dist, then into target package
    const ups = "../".repeat(currentDepth + 1);
    return `${ups}${targetPackage}/${modulePath}`;
  }
}
