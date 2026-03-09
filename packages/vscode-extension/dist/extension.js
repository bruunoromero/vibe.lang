var __create = Object.create;
var __getProtoOf = Object.getPrototypeOf;
var __defProp = Object.defineProperty;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __hasOwnProp = Object.prototype.hasOwnProperty;
function __accessProp(key) {
  return this[key];
}
var __toESMCache_node;
var __toESMCache_esm;
var __toESM = (mod, isNodeMode, target) => {
  var canCache = mod != null && typeof mod === "object";
  if (canCache) {
    var cache = isNodeMode ? __toESMCache_node ??= new WeakMap : __toESMCache_esm ??= new WeakMap;
    var cached = cache.get(mod);
    if (cached)
      return cached;
  }
  target = mod != null ? __create(__getProtoOf(mod)) : {};
  const to = isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target;
  for (let key of __getOwnPropNames(mod))
    if (!__hasOwnProp.call(to, key))
      __defProp(to, key, {
        get: __accessProp.bind(mod, key),
        enumerable: true
      });
  if (canCache)
    cache.set(mod, to);
  return to;
};
var __toCommonJS = (from) => {
  var entry = (__moduleCache ??= new WeakMap).get(from), desc;
  if (entry)
    return entry;
  entry = __defProp({}, "__esModule", { value: true });
  if (from && typeof from === "object" || typeof from === "function") {
    for (var key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(entry, key))
        __defProp(entry, key, {
          get: __accessProp.bind(from, key),
          enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable
        });
  }
  __moduleCache.set(from, entry);
  return entry;
};
var __moduleCache;
var __returnValue = (v) => v;
function __exportSetter(name, newValue) {
  this[name] = __returnValue.bind(null, newValue);
}
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, {
      get: all[name],
      enumerable: true,
      configurable: true,
      set: __exportSetter.bind(all, name)
    });
};

// src/extension.ts
var exports_extension = {};
__export(exports_extension, {
  deactivate: () => deactivate,
  activate: () => activate
});
module.exports = __toCommonJS(exports_extension);
var path = __toESM(require("path"));
var vscode = __toESM(require("vscode"));
var import_node = require("vscode-languageclient/node");
var client;
function activate(context) {
  const config = vscode.workspace.getConfiguration("vibe");
  const languageServerEnabled = config.get("languageServer.enabled", true);
  if (!languageServerEnabled) {
    vscode.window.showInformationMessage("Vibe language server is disabled. Enable it in settings to get full language support.");
    return;
  }
  const serverModule = resolveServerModule(context);
  if (!serverModule) {
    vscode.window.showWarningMessage("Could not find Vibe language server. Some features may not be available.");
    return;
  }
  const serverOptions = {
    run: {
      module: serverModule,
      transport: import_node.TransportKind.ipc
    },
    debug: {
      module: serverModule,
      transport: import_node.TransportKind.ipc,
      options: {
        execArgv: ["--nolazy", "--inspect=6009"]
      }
    }
  };
  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "vibe" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/vibe.json")
    },
    initializationOptions: {
      semanticTokens: config.get("semanticTokens.enabled", true),
      diagnostics: config.get("diagnostics.enabled", true),
      maxDiagnostics: config.get("diagnostics.maxProblems", 100)
    }
  };
  client = new import_node.LanguageClient("vibeLanguageServer", "Vibe Language Server", serverOptions, clientOptions);
  client.start();
  registerCommands(context);
  console.log("Vibe extension activated");
}
async function deactivate() {
  if (client) {
    await client.stop();
  }
}
function resolveServerModule(context) {
  const bundledPath = path.join(context.extensionPath, "node_modules", "@vibe", "language-server", "dist", "server.js");
  const workspacePath = findWorkspaceServerPath();
  const globalPath = findGlobalServerPath();
  for (const serverPath of [bundledPath, workspacePath, globalPath]) {
    if (serverPath && pathExists(serverPath)) {
      return serverPath;
    }
  }
  return;
}
function findWorkspaceServerPath() {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (!workspaceFolders)
    return;
  for (const folder of workspaceFolders) {
    const serverPath = path.join(folder.uri.fsPath, "packages", "language-server", "dist", "server.js");
    if (pathExists(serverPath)) {
      return serverPath;
    }
    const srcPath = path.join(folder.uri.fsPath, "packages", "language-server", "src", "server.ts");
    if (pathExists(srcPath)) {
      return srcPath;
    }
  }
  return;
}
function findGlobalServerPath() {
  return;
}
function pathExists(p) {
  try {
    require("fs").accessSync(p);
    return true;
  } catch {
    return false;
  }
}
function registerCommands(context) {
  const restartCommand = vscode.commands.registerCommand("vibe.restartLanguageServer", async () => {
    if (client) {
      await client.stop();
      await client.start();
      vscode.window.showInformationMessage("Vibe language server restarted");
    }
  });
  context.subscriptions.push(restartCommand);
  const showOutputCommand = vscode.commands.registerCommand("vibe.showOutput", () => {
    if (client) {
      client.outputChannel.show();
    }
  });
  context.subscriptions.push(showOutputCommand);
}
