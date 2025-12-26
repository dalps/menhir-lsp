import { exec } from "child_process";
import * as vscode from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  ClientCapabilities,
  RenameRequest,
  WorkspaceEdit,
  Range,
  DocumentUri,
} from "vscode-languageclient/node";

let client: LanguageClient;

const serverName = "menhir-lsp";

export function activate(context: vscode.ExtensionContext) {
  const _extId = context.extension.packageJSON.name;

  const serverOptions: ServerOptions = {
    command: serverName,
    transport: TransportKind.stdio,
  };

  let outputChannel = vscode.window.createOutputChannel(
    "Menhir Language Server"
  );

  const clientOptions: LanguageClientOptions = {
    outputChannel,
    documentSelector: [
      { scheme: "file", language: "ocaml.menhir" },
      { scheme: "file", language: "ocaml.ocamllex" },
    ],
  };

  client = new LanguageClient(
    "menhir-lsp-client",
    "Menhir VS Code Client",
    serverOptions,
    clientOptions
  );

  let command = `which ${serverName}`;

  exec(command, async (error: any, _output: any, _stderr: any) => {
    // server is there and we can start the client
    if (!error) {
      client.start();
      return;
    }

    let install = await vscode.window.showErrorMessage(
      `[Menhir] The package ${serverName} is required but not installed. Would you like to install automatically it with opam?`,
      `Install ${serverName}`,
      "Cancel"
    );

    if (install === undefined || install === "Cancel") return;

    vscode.window.showInformationMessage(
      `[Menhir] Installing ${serverName}. Please reload the window once the installation completes to activate client.`
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

  // vscode.commands.registerCommand("menhir-lsp-client.restartClient", () => {
  //   if (client.isRunning()) client.stop();

  //   client.start();
  // });

  vscode.commands.registerCommand(
    "menhir-lsp-client.promptAlias",
    async (
      term: string,
      range: Range,
      rawUri: DocumentUri,
      occurrences: Range[]
    ) => {
      let input = await vscode.window.showInputBox({
        prompt: "Enter the unquoted alias",
        title: "Replace all terminal occurrences with alias",
        placeHolder: "e.g. +",
      });

      if (!input) return;

      let uri = vscode.Uri.parse(rawUri);
      let w = new vscode.WorkspaceEdit();

      w.replace(uri, liftRange(range), `${term} "${input}"`);
      occurrences.forEach((r) => w.replace(uri, liftRange(r), `"${input}"`));

      vscode.workspace.applyEdit(w);
    }
  );

  // vscode.window.showInformationMessage("Starting Menhir Client...");
}

const liftRange = (r: Range): vscode.Range => {
  let { start, end } = r;

  return new vscode.Range(
    new vscode.Position(start.line, start.character),
    new vscode.Position(end.line, end.character)
  );
};

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
