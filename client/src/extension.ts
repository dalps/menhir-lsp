import { exec } from "child_process";
import * as vscode from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

const serverName = "menhir-lsp";

export function activate(context: vscode.ExtensionContext) {
  const extId = context.extension.packageJSON.name;

  const serverOptions: ServerOptions = {
    command: serverName,
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

  let command = `which ${serverName}`;

  exec(command, async (error, output, stderr) => {
    // server is there and we can start the client
    if (!error) {
      client.start();
      return;
    }

    let install = await vscode.window.showErrorMessage(
      `[Menhir] The package ${serverName} is required but not installed. Would you like to install automatically it with opam?`,
      "Install automatically",
      "Cancel"
    );

    if (install === undefined || install === "Cancel") return;

    vscode.window.showInformationMessage(
      `[Menhir] Installing ${serverName}. Make sure to reload the window once the installation is over to activate client.`
    );

    let opamInstallCmd = `opam install ${serverName}`;
    let terminal = vscode.window.createTerminal(serverName);

    terminal.show();
    terminal.sendText(opamInstallCmd);
  });

  vscode.commands.registerCommand(
    "menhir-lsp-client.showOutput",
    outputChannel.show
  );

  vscode.window.showInformationMessage("Starting Menhir Client...");
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
