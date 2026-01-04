import path from "node:path";
import { existsSync } from "node:fs";
import * as vscode from "vscode";
import { formatSource, loadFormatterFormConfig } from "@vibe/formatter";
import type { FormFormattingConfig } from "@vibe/formatter";
import { LanguageClient } from "vscode-languageclient/node";
import type {
  Executable,
  LanguageClientOptions,
  ServerOptions,
  DocumentSelector,
} from "vscode-languageclient/node";

const LANGUAGE_ID = "vibe";
const CONFIG_FILE_PATTERN = "vibe.config.{js,cjs,mjs,ts,cts,mts}";
const CONFIG_GLOB = `**/${CONFIG_FILE_PATTERN}`;

let clientManager: VibeClientManager | undefined;

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  clientManager = new VibeClientManager(context);
  context.subscriptions.push(clientManager);
  registerFormatDocumentCommand(context);
  await clientManager.initialize();
}

export async function deactivate(): Promise<void> {
  await clientManager?.shutdown();
}

type ClientEntry = {
  readonly client: LanguageClient;
  readonly configUri: vscode.Uri;
  readonly workspaceFolder?: vscode.WorkspaceFolder;
};

class VibeClientManager implements vscode.Disposable {
  private readonly clients = new Map<string, ClientEntry>();
  private readonly workspaceWatchers = new Map<
    string,
    vscode.FileSystemWatcher
  >();
  private readonly disposables: vscode.Disposable[] = [];
  private serverModulePath: string | undefined;

  constructor(private readonly context: vscode.ExtensionContext) {}

  async initialize(): Promise<void> {
    if (this.serverModulePath === undefined) {
      this.serverModulePath = await this.resolveServerModule();
      if (!this.serverModulePath) {
        return;
      }
    }

    const folders = vscode.workspace.workspaceFolders ?? [];
    await Promise.all(folders.map((folder) => this.trackWorkspace(folder)));

    this.disposables.push(
      vscode.workspace.onDidChangeWorkspaceFolders(async (event) => {
        await Promise.all(
          event.added.map((folder) => this.trackWorkspace(folder))
        );
        event.removed.forEach((folder) => {
          this.disposeWorkspace(folder);
        });
      })
    );

    this.disposables.push(
      vscode.workspace.onDidChangeConfiguration((event) => {
        if (event.affectsConfiguration("vibe.languageServer")) {
          void this.restartAllClients();
        }
      })
    );
  }

  dispose(): void {
    for (const disposable of this.disposables) {
      disposable.dispose();
    }
    for (const watcher of this.workspaceWatchers.values()) {
      watcher.dispose();
    }
    this.workspaceWatchers.clear();
    void this.shutdown();
  }

  async shutdown(): Promise<void> {
    const stops = Array.from(this.clients.keys()).map((root) =>
      this.stopClient(root)
    );
    await Promise.all(stops);
  }

  private async trackWorkspace(folder: vscode.WorkspaceFolder): Promise<void> {
    const folderKey = folder.uri.toString();
    if (this.workspaceWatchers.has(folderKey)) {
      return;
    }

    const pattern = new vscode.RelativePattern(folder, CONFIG_GLOB);
    const configs = await vscode.workspace.findFiles(pattern);
    await Promise.all(configs.map((uri) => this.ensureClient(uri)));

    const watcher = vscode.workspace.createFileSystemWatcher(pattern);
    watcher.onDidCreate((uri) => this.ensureClient(uri));
    watcher.onDidDelete((uri) => {
      void this.handleConfigRemoval(uri);
    });
    this.workspaceWatchers.set(folderKey, watcher);
    this.context.subscriptions.push(watcher);
  }

  private disposeWorkspace(folder: vscode.WorkspaceFolder): void {
    const folderKey = folder.uri.toString();
    const watcher = this.workspaceWatchers.get(folderKey);
    if (watcher) {
      watcher.dispose();
      this.workspaceWatchers.delete(folderKey);
    }
    for (const [root] of this.clients) {
      if (this.isInsideWorkspace(root, folder)) {
        void this.stopClient(root);
      }
    }
  }

  private async ensureClient(configUri: vscode.Uri): Promise<void> {
    if (!this.serverModulePath) {
      return;
    }
    const rootDir = this.normalizePath(path.dirname(configUri.fsPath));
    if (this.clients.has(rootDir)) {
      return;
    }
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(configUri);
    const serverOptions = this.createServerOptions(rootDir);
    const langWatcher = vscode.workspace.createFileSystemWatcher(
      workspaceFolder
        ? new vscode.RelativePattern(workspaceFolder, "**/*.lang")
        : new vscode.RelativePattern(vscode.Uri.file(rootDir), "**/*.lang")
    );
    this.context.subscriptions.push(langWatcher);
    const clientOptions: LanguageClientOptions = {
      documentSelector: this.buildDocumentSelector(rootDir),
      workspaceFolder,
      initializationOptions: {
        rootDir,
      },
      synchronize: {
        fileEvents: langWatcher,
      },
      diagnosticCollectionName: "vibe",
      outputChannelName: `Vibe (${path.basename(rootDir) || rootDir})`,
    };
    const clientId = this.buildClientId(rootDir);
    const client = new LanguageClient(
      clientId,
      `Vibe (${path.basename(rootDir) || rootDir})`,
      serverOptions,
      clientOptions
    );
    void client.start();
    this.context.subscriptions.push(client);
    this.clients.set(rootDir, { client, configUri, workspaceFolder });
  }

  private createServerOptions(rootDir: string): ServerOptions {
    const extraArgs = this.getExtraArgs();
    const executable: Executable = {
      command: process.execPath,
      args: [this.serverModulePath!, "--stdio", ...extraArgs],
      options: {
        cwd: rootDir,
        env: {
          ...process.env,
          VIBE_WORKSPACE_ROOT: rootDir,
        },
      },
    };
    const options: ServerOptions = {
      run: executable,
      debug: executable,
    };
    return options;
  }

  private getExtraArgs(): readonly string[] {
    const config = vscode.workspace.getConfiguration("vibe.languageServer");
    return config.get<string[]>("extraArgs", []) ?? [];
  }

  private async handleConfigRemoval(configUri: vscode.Uri): Promise<void> {
    const rootDir = this.normalizePath(path.dirname(configUri.fsPath));
    const stillExists = await this.hasConfigInDirectory(configUri);
    if (!stillExists) {
      await this.stopClient(rootDir);
    }
  }

  private async hasConfigInDirectory(configUri: vscode.Uri): Promise<boolean> {
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(configUri);
    if (!workspaceFolder) {
      return false;
    }
    const dir = path.dirname(configUri.fsPath);
    const relative = path.relative(workspaceFolder.uri.fsPath, dir);
    if (relative.startsWith("..")) {
      return false;
    }
    const normalized = relative ? `${relative.split(path.sep).join("/")}/` : "";
    const pattern = new vscode.RelativePattern(
      workspaceFolder,
      `${normalized}${CONFIG_FILE_PATTERN}`
    );
    const matches = await vscode.workspace.findFiles(pattern, undefined, 1);
    return matches.length > 0;
  }

  private async stopClient(rootDir: string): Promise<void> {
    const entry = this.clients.get(rootDir);
    if (!entry) {
      return;
    }
    this.clients.delete(rootDir);
    await entry.client.stop();
  }

  private async restartAllClients(): Promise<void> {
    const entries = Array.from(this.clients.entries());
    await Promise.all(entries.map(([root]) => this.stopClient(root)));
    await Promise.all(
      entries.map(([, entry]) => this.ensureClient(entry.configUri))
    );
  }

  private isInsideWorkspace(
    rootDir: string,
    folder: vscode.WorkspaceFolder
  ): boolean {
    const folderPath = this.normalizePath(folder.uri.fsPath);
    return (
      rootDir === folderPath || rootDir.startsWith(`${folderPath}${path.sep}`)
    );
  }

  private normalizePath(value: string): string {
    return path.resolve(value);
  }

  private buildClientId(rootDir: string): string {
    const sanitized = rootDir.replace(/[^A-Za-z0-9_-]/g, "_");
    return `vibe-${sanitized}`;
  }

  private buildDocumentSelector(rootDir: string): DocumentSelector {
    const normalized = rootDir.split(path.sep).join("/");
    return [
      {
        scheme: "file",
        language: LANGUAGE_ID,
        pattern: `${normalized}/**/*.lang`,
      },
    ];
  }

  private async resolveServerModule(): Promise<string | undefined> {
    const candidate = this.context.asAbsolutePath(
      path.join("node_modules", "@vibe", "lsp", "dist", "server.js")
    );
    if (existsSync(candidate)) {
      return candidate;
    }
    await vscode.window.showErrorMessage(
      "Unable to locate the compiled Vibe language server. Run 'bun run build' inside packages/lsp (or rerun bun install) before starting VS Code."
    );
    return undefined;
  }
}

const registerFormatDocumentCommand = (
  context: vscode.ExtensionContext
): void => {
  const disposable = vscode.commands.registerCommand(
    "vibe.formatDocument",
    async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        void vscode.window.showInformationMessage(
          "Open a Vibe file to run the format command."
        );
        return;
      }
      if (editor.document.languageId !== LANGUAGE_ID) {
        void vscode.window.showInformationMessage(
          "Vibe: Format Document only applies to .lang files."
        );
        return;
      }
      await formatDocumentWithFormatter(editor);
    }
  );
  context.subscriptions.push(disposable);
};

const formatDocumentWithFormatter = async (
  editor: vscode.TextEditor
): Promise<void> => {
  const document = editor.document;
  const source = document.getText();
  if (!source) {
    return;
  }
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  const documentPath =
    document.uri.scheme === "file" ? document.uri.fsPath : undefined;
  const fallbackRoot =
    vscode.workspace.workspaceFolders?.[0]?.uri.fsPath ?? process.cwd();
  const sourceRoot =
    workspaceFolder?.uri.fsPath ??
    (documentPath ? path.dirname(documentPath) : fallbackRoot);
  let formConfig: FormFormattingConfig | undefined;
  try {
    formConfig = loadFormatterFormConfig(sourceRoot);
  } catch (error) {
    console.warn(
      `[vibe][fmt] Failed to load formatter config from ${sourceRoot}:`,
      error
    );
  }
  const indent = buildIndentFromEditor(editor);
  const newline = document.eol === vscode.EndOfLine.LF ? "\n" : "\r\n";
  const result = await formatSource(source, {
    indent,
    newline,
    formConfig,
  });
  if (!result.ok) {
    const diagnostic = result.diagnostics[0];
    const message = diagnostic
      ? diagnostic.message
      : "Unable to format document due to syntax errors.";
    void vscode.window.showErrorMessage(`[vibe][fmt] ${message}`);
    return;
  }
  if (result.formatted === source) {
    return;
  }
  const endPosition = document.positionAt(source.length);
  const fullRange = new vscode.Range(new vscode.Position(0, 0), endPosition);
  await editor.edit((editBuilder) => {
    editBuilder.replace(fullRange, result.formatted);
  });
};

const buildIndentFromEditor = (editor: vscode.TextEditor): string => {
  const { insertSpaces, tabSize } = editor.options;
  if (insertSpaces === false) {
    return "\t";
  }
  const resolvedSize =
    typeof tabSize === "number"
      ? tabSize
      : typeof tabSize === "string"
      ? Number(tabSize)
      : undefined;
  const size =
    resolvedSize && Number.isFinite(resolvedSize)
      ? Math.max(1, resolvedSize)
      : 2;
  return " ".repeat(size);
};
