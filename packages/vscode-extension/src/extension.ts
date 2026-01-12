/**
 * Vibe VS Code Extension
 *
 * Activates the Vibe language server and provides language support.
 */
import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";
import type {
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

/**
 * Extension activation.
 */
export function activate(context: vscode.ExtensionContext): void {
  const config = vscode.workspace.getConfiguration("vibe");
  const languageServerEnabled = config.get<boolean>(
    "languageServer.enabled",
    true
  );

  if (!languageServerEnabled) {
    vscode.window.showInformationMessage(
      "Vibe language server is disabled. Enable it in settings to get full language support."
    );
    return;
  }

  // Server module path - look for language-server in node_modules
  const serverModule = resolveServerModule(context);

  if (!serverModule) {
    vscode.window.showWarningMessage(
      "Could not find Vibe language server. Some features may not be available."
    );
    return;
  }

  // Server options - run via Node/Bun
  const serverOptions: ServerOptions = {
    run: {
      module: serverModule,
      transport: TransportKind.ipc,
    },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: {
        execArgv: ["--nolazy", "--inspect=6009"],
      },
    },
  };

  // Client options
  const clientOptions: LanguageClientOptions = {
    // Register for Vibe documents
    documentSelector: [{ scheme: "file", language: "vibe" }],
    synchronize: {
      // Watch for changes to vibe.json config files
      fileEvents: vscode.workspace.createFileSystemWatcher("**/vibe.json"),
    },
    // Pass configuration
    initializationOptions: {
      semanticTokens: config.get<boolean>("semanticTokens.enabled", true),
      diagnostics: config.get<boolean>("diagnostics.enabled", true),
      maxDiagnostics: config.get<number>("diagnostics.maxProblems", 100),
    },
  };

  // Create and start the language client
  client = new LanguageClient(
    "vibeLanguageServer",
    "Vibe Language Server",
    serverOptions,
    clientOptions
  );

  // Start the client (also starts the server)
  client.start();

  // Register commands
  registerCommands(context);

  console.log("Vibe extension activated");
}

/**
 * Extension deactivation.
 */
export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
  }
}

/**
 * Resolve the server module path.
 */
function resolveServerModule(
  context: vscode.ExtensionContext
): string | undefined {
  // Try several possible locations

  // 1. Bundled with extension (production)
  const bundledPath = path.join(
    context.extensionPath,
    "node_modules",
    "@vibe",
    "language-server",
    "dist",
    "server.js"
  );

  // 2. Workspace development mode
  const workspacePath = findWorkspaceServerPath();

  // 3. Global installation (via npm/bun)
  const globalPath = findGlobalServerPath();

  // Check each path
  for (const serverPath of [bundledPath, workspacePath, globalPath]) {
    if (serverPath && pathExists(serverPath)) {
      return serverPath;
    }
  }

  return undefined;
}

/**
 * Find server in workspace (development mode).
 */
function findWorkspaceServerPath(): string | undefined {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (!workspaceFolders) return undefined;

  for (const folder of workspaceFolders) {
    // Check if this is the vibe monorepo
    const serverPath = path.join(
      folder.uri.fsPath,
      "packages",
      "language-server",
      "dist",
      "server.js"
    );

    if (pathExists(serverPath)) {
      return serverPath;
    }

    // Also check for src version (for running with bun directly)
    const srcPath = path.join(
      folder.uri.fsPath,
      "packages",
      "language-server",
      "src",
      "server.ts"
    );

    if (pathExists(srcPath)) {
      return srcPath;
    }
  }

  return undefined;
}

/**
 * Find globally installed server.
 */
function findGlobalServerPath(): string | undefined {
  // This would require checking npm/bun global paths
  // For now, return undefined
  return undefined;
}

/**
 * Check if a path exists.
 */
function pathExists(p: string): boolean {
  try {
    require("fs").accessSync(p);
    return true;
  } catch {
    return false;
  }
}

/**
 * Register extension commands.
 */
function registerCommands(context: vscode.ExtensionContext): void {
  // Restart language server command
  const restartCommand = vscode.commands.registerCommand(
    "vibe.restartLanguageServer",
    async () => {
      if (client) {
        await client.stop();
        await client.start();
        vscode.window.showInformationMessage("Vibe language server restarted");
      }
    }
  );

  context.subscriptions.push(restartCommand);

  // Show output channel command
  const showOutputCommand = vscode.commands.registerCommand(
    "vibe.showOutput",
    () => {
      if (client) {
        client.outputChannel.show();
      }
    }
  );

  context.subscriptions.push(showOutputCommand);
}
