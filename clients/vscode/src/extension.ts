import { exec } from "child_process";
import * as vscode from "vscode";

import {
  CancellationToken,
  DocumentUri,
  ExecuteCommandParams,
  LanguageClient,
  LanguageClientOptions,
  Range,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import { ASTPanel, getWebviewOptions } from "./astPanel";
import { activateStatusBar } from "./status";

let client: LanguageClient;

const serverName = "menhir-lsp";
const clientName = "menhir-lsp-client";

////////////////////////////////////////////////////////////////////////////////
// Helpers for defining commands
//

const commandName = (name: string) => `${clientName}.${name}`;

const registerCmd = (name: string, callback: (...args: any[]) => any) =>
  vscode.commands.registerCommand(commandName(name), callback);

export const execServerCmd = async <T>(command: string, ...args: any[]) =>
  await client.sendRequest<T>(
    "workspace/executeCommand",
    { command, arguments: args } as ExecuteCommandParams,
    CancellationToken.None,
  );

/** Register a new command with zero arguments that runs in the server.
 * The server is passed the uri and the current cursor position of the active editor. */
const serverCmdWithActiveEditor = (command: string) =>
  registerCmd(command, () => {
    const editor = vscode.window.activeTextEditor;

    if (!editor) return;

    execServerCmd(
      command,
      editor.document.uri.toString(),
      editor.selection.active,
    );
  });

////////////////////////////////////////////////////////////////////////////////

export function activate(context: vscode.ExtensionContext) {
  const _extId: string = context.extension.packageJSON.name;

  const serverOptions: ServerOptions = {
    command: serverName,
    transport: TransportKind.stdio,
  };

  let outputChannel = vscode.window.createOutputChannel(
    "Menhir Language Server",
  );

  const clientOptions: LanguageClientOptions = {
    outputChannel,
    documentSelector: [
      { scheme: "file", language: "ocaml.menhir" },
      { scheme: "file", language: "ocaml.menhir.messages" },
      { scheme: "file", language: "ocaml.ocamllex" },
    ],
    synchronize: {
      configurationSection: ["menhir.format"],
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.conflicts"),
    },
  };

  client = new LanguageClient(
    "menhir-lsp-client",
    "Menhir VS Code Client",
    serverOptions,
    clientOptions,
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
      "Cancel",
    );

    if (install === undefined || install === "Cancel") return;

    vscode.window.showInformationMessage(
      `[Menhir] Installing ${serverName}. Please reload the window once the installation completes to activate client.`,
    );

    let opamInstallCmd = `opam install ${serverName}`;
    let terminal = vscode.window.createTerminal(serverName);

    terminal.show();
    terminal.sendText(opamInstallCmd);
  });

  vscode.commands.registerCommand(
    "menhir-lsp-client.showOutput",
    outputChannel.show,
  );

  vscode.commands.registerCommand(
    "menhir-lsp-client.restartServer",
    async () => {
      if (client.isRunning()) await client.stop();

      client.start();
    },
  );

  vscode.commands.registerCommand(
    "menhir-lsp-client.promptAlias",
    async (
      term: string,
      range: Range,
      rawUri: DocumentUri,
      occurrences: Range[],
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
    },
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(commandName("gotoImplementation"), () => {
      const editor = vscode.window.activeTextEditor;

      if (!editor) return;

      gotoImplementation(editor.document.uri, editor.selection.active);
    }),

    vscode.commands.registerCommand(commandName("astView"), () => {
      ASTPanel.createOrShow(context.extensionUri);
    }),

    vscode.commands.registerCommand(commandName("revealAstNode"), () => {
      ASTPanel.revealNodeUnderCursor(context.extensionUri);
    }),
  );

  vscode.window.registerWebviewPanelSerializer(ASTPanel.viewType, {
    async deserializeWebviewPanel(panel: vscode.WebviewPanel, state: unknown) {
      const editor = vscode.window.activeTextEditor;

      if (!editor) return;

      panel.webview.options = getWebviewOptions(context.extensionUri);

      ASTPanel.revive(panel, editor, context.extensionUri);
    },
  });

  // vscode.window.showInformationMessage("Starting Menhir Client...");

  //////////////////////////////////////////////////////////////////////////////
  // .messages file features
  //

  activateStatusBar(context);

  context.subscriptions.push(
    serverCmdWithActiveEditor("nextMessage"),
    serverCmdWithActiveEditor("nextDummyMessage"),
    serverCmdWithActiveEditor("previousMessage"),
    serverCmdWithActiveEditor("previousDummyMessage"),
    registerCmd("startTokenizer", async () => {
      const editor = vscode.window.activeTextEditor;

      if (!editor) return;

      const lexers: any[] = await execServerCmd(
        "startTokenizer", // "listLexers",
        editor.document.uri.toString(),
        editor.selection.active,
      );

      if (lexers.length <= 0) {
        vscode.window.showErrorMessage(
          "You need to open at least one .mll file",
        );
        return;
      }

      // const selection = await vscode.window.showQuickPick(lexers, {
      //   title: "Select the lexer to use",
      // });

      async function setupQuickPick(
        title: string,
        items: vscode.QuickPickItem[],
      ) {
        const qp = vscode.window.createQuickPick();
        qp.title = title;
        // qp.prompt =
        //   "Select the entry point to use among the lexers opened so far.";
        // qp.items = lexers;
        qp.items = items;

        qp.show();

        let selection: vscode.QuickPickItem | undefined;

        qp.onDidChangeActive((item) => (selection = item.at(0)));

        try {
          await new Promise(
            (resolve, reject) => (
              qp.onDidAccept(resolve),
              qp.onDidHide(() => reject("Cancelled selection."))
            ),
          );
        } catch (error) {
          console.log(error);
          selection = undefined;
        }

        selection && console.log("Picked item: ", selection);
        return selection;
      }

      // Ask which lexer rule shall be  run
      const lexerRule = await setupQuickPick(
        "Select Lexer Entry Point",
        lexers.map(({ label, detail }) => ({
          label: `\$(symbol-function) ${label} · \$(symbol-module) ${detail.split("/").at(-1)! as string}`,
          description: detail,
          _name: label,
          _uri: detail,
        })),
      );

      if (!lexerRule) return;

      // Ask where to source input from (you will reuse this function  for parser debugger)
      const inputSource = await setupQuickPick("Select the text source", [
        { label: "$(target) Use Active Editor" },
        { label: "$(file-text) Enter Path To Text File" },
        { label: "$(pencil) Enter Text" },
      ]);

      if (!inputSource) return;

      // Start the webview / debugger
    }),
  );

  //////////////////////////////////////////////////////////////////////////////
}

export const liftRange = (r: Range): vscode.Range => {
  let { start, end } = r;

  return new vscode.Range(
    new vscode.Position(start.line, start.character),
    new vscode.Position(end.line, end.character),
  );
};

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

export async function getAst(uri: vscode.Uri) {
  console.log(`Requesting AST of document: ${uri}`);

  return await client.sendRequest(
    "workspace/executeCommand",
    { command: "getAst", arguments: [uri.toString()] } as ExecuteCommandParams,
    CancellationToken.None,
  );
}

export async function gotoImplementation(
  uri: vscode.Uri,
  pos?: vscode.Position,
) {
  console.log(
    `Requesting implementation of document: ${uri} at position ${pos}`,
  );

  return await client.sendRequest(
    "workspace/executeCommand",
    {
      command: "gotoImplementation",
      arguments: [uri.toString(), pos], // Position is serialized automatically
    } as ExecuteCommandParams,
    CancellationToken.None,
  );
}
