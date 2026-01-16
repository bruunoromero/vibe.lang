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
  externalBindings: ExternalBindingsMap
): string[] {
  const lines: string[] = [];

  for (const [modulePath, bindings] of externalBindings) {
    if (bindings.size > 0) {
      // Generate import specifiers with aliasing where needed
      // e.g., "intAdd as add" when Vibe name differs from runtime name
      const importSpecifiers: string[] = [];
      const sortedEntries = Array.from(bindings.entries()).sort(([a], [b]) =>
        a.localeCompare(b)
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
        `import { ${importSpecifiers.join(", ")} } from "${modulePath}";`
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
  modulePackages: Map<string, string>
): string[] {
  const lines: string[] = [];

  // Get imports from source program
  const imports = program.sourceProgram.imports || [];
  const currentModule = program.moduleName || "Main";
  const currentPackage = program.packageName || currentModule;
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
      moduleName
    );

    if (imp.exposing?.kind === "All") {
      // import * as ModuleName from "..."
      // Use just the base module name (last segment) as the alias
      const alias = moduleName.split(".").pop() || moduleName;
      lines.push(`import * as ${alias} from "${importPath}";`);
    } else if (imp.exposing?.kind === "Explicit") {
      // Extract names from the export specs
      const names = imp.exposing.exports
        .map((spec) => {
          switch (spec.kind) {
            case "ExportValue":
            case "ExportTypeAll":
              return sanitizeIdentifier(spec.name);
            case "ExportOperator":
              return sanitizeOperator(spec.operator);
            case "ExportTypeSome":
              // For ExportTypeSome, we import the type constructor and the specific members
              return sanitizeIdentifier(spec.name);
            default:
              return null;
          }
        })
        .filter((name): name is string => name !== null);

      if (names.length > 0) {
        // import { specific, names } from "..."
        lines.push(`import { ${names.join(", ")} } from "${importPath}";`);
      } else {
        // No explicit imports, import everything (exposing all)
        const alias = moduleName.split(".").pop() || moduleName;
        lines.push(`import * as ${alias} from "${importPath}";`);
      }
    } else {
      // Default: import everything (exposing all)
      const alias = moduleName.split(".").pop() || moduleName;
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
  importedModule: string
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
  targetModule: string
): string {
  const currentModule = program.moduleName || "Main";
  const currentPackage = program.packageName || currentModule;

  // Calculate the depth of the current module within its package
  const currentDepth = currentModule.split(".").length - 1;

  // Find the import for this module to get its package info
  const imports = program.sourceProgram.imports || [];
  let targetPackage = targetModule; // Default: assume module name is package name

  for (const imp of imports) {
    if (imp.moduleName === targetModule) {
      // Could extract package info here if available
      break;
    }
  }

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
