/**
 * @vibe/codegen - Import Path Calculation
 *
 * Utilities for calculating relative import paths between modules.
 */

import type { IRProgram } from "@vibe/ir";
import { sanitizeIdentifier } from "./sanitize.js";

/**
 * External bindings map type: module path -> (Vibe name -> runtime name)
 */
export type ExternalBindingsMap = Map<string, Map<string, string>>;

/**
 * Generate default import statements from @import type declarations.
 * These are hoisted to the top of the generated file.
 *
 * Example: @import "node:fs/promises" type FileSystem
 * Emits:   import FileSystem from "node:fs/promises";
 */
export function generateDefaultImports(program: IRProgram): string[] {
  const lines: string[] = [];
  for (const { name, modulePath } of program.defaultImports) {
    lines.push(`import ${name} from "${modulePath}";`);
  }
  return lines;
}

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

  const currentPackage = program.packageName;
  const currentDepth = program.moduleName.split(".").length - 1;

  for (const resolved of program.resolvedImports) {
    const importedPackage =
      modulePackages.get(resolved.moduleName) || resolved.moduleName;
    const importPath = calculateImportPath(
      currentPackage,
      currentDepth,
      importedPackage,
      resolved.moduleName,
    );

    if (resolved.namespaceImport) {
      lines.push(
        `import * as ${resolved.namespaceImport} from "${importPath}";`,
      );
    }

    if (resolved.namedImports.length > 0) {
      const sanitized = resolved.namedImports.map((n) => sanitizeIdentifier(n));
      lines.push(`import { ${sanitized.join(", ")} } from "${importPath}";`);
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
