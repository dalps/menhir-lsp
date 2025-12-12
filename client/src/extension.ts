import * as path from "path";
import * as vscode from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  const serverOptions: ServerOptions = {
    command: "menhir-lsp",
    transport: TransportKind.stdio,
  };

  let outputChannel = vscode.window.createOutputChannel(
    "Menhir Language Server"
  );

  const clientOptions: LanguageClientOptions = {
    outputChannel,
    documentSelector: [{ scheme: "file", language: "ocaml.menhir" }],
  };

  client = new LanguageClient(
    "menhir-lsp-client",
    "Menhir VS Code Client",
    serverOptions,
    clientOptions
  );

  vscode.commands.registerCommand("menhir-lsp.showOutput", outputChannel.show);

  vscode.window.showInformationMessage("Starting Menhir Client...");

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
