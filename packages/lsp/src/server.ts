import net from "node:net";
import {
  ProposedFeatures,
  StreamMessageReader,
  StreamMessageWriter,
  createConnection,
} from "vscode-languageserver/node";
import { registerLanguageServer, type TransportKind } from "./language-server";

type CliOptions = {
  readonly transport: TransportKind;
  readonly host: string;
  readonly port: number;
};

const options = parseArgs(process.argv.slice(2));

if (options.transport === "stdio") {
  const connection = createConnection(ProposedFeatures.all);
  registerLanguageServer(connection, { transport: "stdio" });
  connection.listen();
} else {
  startSocketServer(options);
}

function parseArgs(argv: readonly string[]): CliOptions {
  let transport: TransportKind = "stdio";
  let host = "127.0.0.1";
  let port = 7413;

  const consumeValue = (index: number): string | undefined => {
    const value = argv[index + 1];
    if (value === undefined) {
      return undefined;
    }
    return value;
  };

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (!arg) {
      continue;
    }
    if (arg === "--stdio") {
      transport = "stdio";
      continue;
    }
    if (arg === "--socket") {
      transport = "socket";
      continue;
    }
    if (arg.startsWith("--transport")) {
      const value = arg.includes("=")
        ? arg.split("=", 2)[1]
        : consumeValue(index) ?? "";
      if (value === "stdio" || value === "socket") {
        transport = value;
      } else {
        throw new Error(`Unknown transport: ${value}`);
      }
      if (!arg.includes("=")) {
        index += 1;
      }
      continue;
    }
    if (arg.startsWith("--host")) {
      const value = arg.includes("=")
        ? arg.split("=", 2)[1]
        : consumeValue(index);
      if (!value) {
        throw new Error("--host requires a value");
      }
      host = value;
      if (!arg.includes("=")) {
        index += 1;
      }
      continue;
    }
    if (arg.startsWith("--port")) {
      const value = arg.includes("=")
        ? arg.split("=", 2)[1]
        : consumeValue(index);
      if (!value) {
        throw new Error("--port requires a value");
      }
      const parsed = Number.parseInt(value, 10);
      if (!Number.isFinite(parsed) || parsed <= 0) {
        throw new Error(`Invalid port: ${value}`);
      }
      port = parsed;
      if (!arg.includes("=")) {
        index += 1;
      }
      continue;
    }
  }

  return { transport, host, port } satisfies CliOptions;
}

function startSocketServer(options: CliOptions): void {
  const server = net.createServer((socket) => {
    const reader = new StreamMessageReader(socket);
    const writer = new StreamMessageWriter(socket);
    const connection = createConnection(
      ProposedFeatures.all,
      reader,
      writer
    );
    registerLanguageServer(connection, { transport: "socket" });
    connection.listen();
    socket.on("error", (error) => {
      console.error(`[vibe][lsp] socket error: ${String(error)}`);
    });
  });

  server.on("listening", () => {
    const address = server.address();
    if (typeof address === "object" && address) {
      console.error(
        `[vibe][lsp] Listening on ${address.address}:${address.port}`
      );
    } else {
      console.error(`[vibe][lsp] Listening for TCP clients`);
    }
  });

  server.on("error", (error) => {
    console.error(`[vibe][lsp] server error: ${String(error)}`);
  });

  server.listen({ host: options.host, port: options.port });
}
