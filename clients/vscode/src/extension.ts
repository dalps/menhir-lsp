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
import { liftRange, rand, setupQuickPick } from "./utils";
import { readFileSync } from "fs";
import path = require("path");
import { RawRange } from "./webviews/ast/types";

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
    registerCmd("startTokenizer", startTokenizerView),
    registerCmd("startTokenizerWithEditor", () =>
      startTokenizerView(undefined, { value: InputSourceKind.ActiveEditor }),
    ),
    registerCmd("startTokenizerWithRule", (arg) =>
      startTokenizerView(arg, undefined),
    ),
  );

  //////////////////////////////////////////////////////////////////////////////
}

type LexerRule = { name: string; moduleUri: string };

enum InputSourceKind {
  ActiveEditor,
  File,
  TextBox,
}

type InputSource = { value: InputSourceKind };

interface Token {
  range: Range;
  rawRange: RawRange;
  text: string;
}

async function startTokenizerView(rule?: LexerRule, source?: InputSource) {
  let editor = vscode.window.activeTextEditor;

  if (!rule) {
    const lexers = await execServerCmd<LexerRule[]>("listLexerRules");

    if (lexers.length <= 0) {
      vscode.window.showErrorMessage("You need to open at least one .mll file");
      return;
    }

    console.log(lexers);

    // Ask which lexer rule to run
    rule = await setupQuickPick<LexerRule>(
      "Select Lexer Entry Point",
      lexers.map((rule) => {
        return {
          ...rule,
          label: `\$(symbol-function) ${rule.name} · \$(symbol-module) ${rule.moduleUri.split("/").at(-1)}`,
          description: rule.moduleUri,
        };
      }),
    );
  }

  if (!source) {
    source = await setupQuickPick("Select the input source", [
      {
        label: "$(target) Active Editor",
        value: InputSourceKind.ActiveEditor,
      },
      {
        label: "$(file-text) Read input from a file",
        value: InputSourceKind.File,
      },
      { label: "$(pencil) Enter some text", value: InputSourceKind.TextBox },
    ]);

    if (!source) return;
  }

  // vscode.workspace.findFiles
  let cmd = "startTokenizer";

  switch (source.value) {
    case InputSourceKind.ActiveEditor:
      {
        if (!editor) {
          vscode.window.showErrorMessage("No active editor found.");
          return;
        }
        const tokens: Token[] = await execServerCmd(
          cmd,
          editor.document.uri.toString(),
          null,
          rule,
        );

        highlightTokens(editor, tokens);
      }
      break;
    case InputSourceKind.File:
      {
        const uri = (await vscode.window.showOpenDialog())?.at(0);

        if (!uri) return;
        // const content = readFileSync(uri.fsPath, { encoding: "utf8" });
        const tokens = await execServerCmd(cmd, uri.toString(), null, rule);

        console.log(tokens);
      }
      break;

    case InputSourceKind.TextBox:
      {
        const content = await vscode.window.showInputBox({
          placeHolder: "Enter or paste some text here",
        });
        execServerCmd(cmd, null, content, rule);
      }
      break;

    default:
      break;
  }
}

let decos: vscode.TextEditorDecorationType[] = [];

function highlightTokens(editor: vscode.TextEditor, tokens: Token[]) {
  // Clear the old decorations
  decos.forEach((d) => editor.setDecorations(d, []));
  decos = [];

  // Bad: vscode merges distinct ranges of the same decoration type
  // editor.setDecorations(
  //   deco,
  //   tokens.map((t) => liftRange(t.range)),
  // );

  tokens.forEach((t) => {
    const color = `rgb(${rand(127, 255)},${rand(127, 255)},${rand(127, 255)})`;

    const deco = vscode.window.createTextEditorDecorationType({
      backgroundColor: color,
      outlineColor: "blue",
      outlineWidth: "2px",
      borderSpacing: "2px",
      borderRadius: "5px",
      borderColor: "black",
      borderWidth: "1px",
      border: "solid",
    });

    decos.push(deco);

    editor.setDecorations(deco, [liftRange(t.range)]);
  });
}

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
