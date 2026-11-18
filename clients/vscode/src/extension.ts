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
import {
  ActionMapProvider,
  DerivationTreeProvider,
  EntrypointProvider,
  StackProvider,
} from "./derivationTree";
import { activateStatusBar } from "./status";
import {
  ActionMapping,
  Checkpoint,
  DerivationTree,
  Entry,
  InputSourceKind,
  LexerAction,
  LexerActionKind,
  ModuleEntries,
  Result,
  StackItem,
  Terminal,
  Token,
} from "./types";
import {
  choose,
  liftRange,
  rand,
  rgba,
  sep,
  setupQuickPick,
  uriEqual,
} from "./utils";
import path = require("path");

let client: LanguageClient;
let derivationTreeView: vscode.TreeView<DerivationTree>;
let stackView: vscode.TreeView<StackItem>;
let actionBindingView: vscode.TreeView<ActionMapping | Terminal>;
let entrypointExplorerView: vscode.TreeView<Entry | ModuleEntries>;
let workspaceParsersView: vscode.TreeView<Entry | ModuleEntries>;

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

const setContext = (key: string, value: any) =>
  vscode.commands.executeCommand("setContext", `${clientName}.${key}`, value);

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

  const derivationTreeProvider = new DerivationTreeProvider(context);
  const stackProvider = new StackProvider(context);

  const actionProvider = new ActionMapProvider(context);

  const entrypointProvider = new EntrypointProvider(context);

  // derivationTreeProvider.onDidChangeTreeData((d) =>
  //   console.log("tree data has changed.", d),
  // );

  // vscode.window.registerTreeDataProvider(
  //   "menhir-lsp-derivation-view",
  //   derivationTreeProvider,
  // );

  //#region Views
  derivationTreeView = vscode.window.createTreeView(
    "menhir-lsp-derivation-view",
    {
      treeDataProvider: derivationTreeProvider,
    },
  );

  stackView = vscode.window.createTreeView("menhir-lsp-stack-view", {
    treeDataProvider: stackProvider,
  });

  actionBindingView = vscode.window.createTreeView("menhir-lsp-action-map", {
    treeDataProvider: actionProvider,
  });

  entrypointExplorerView = vscode.window.createTreeView(
    "menhir-lsp-entrypoint-explorer",
    {
      treeDataProvider: entrypointProvider,
    },
  );

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
      startTokenizerView(undefined, InputSourceKind.ActiveEditor),
    ),
    registerCmd("startTokenizerWithRule", (arg) =>
      startTokenizerView(arg, undefined),
    ),
    registerCmd("startParser", (start) =>
      startParserView(derivationTreeProvider, start),
    ),

    registerCmd(
      "startParserWithCheckpoints",
      (parserEntrypoint, lexerEntrypoint, source) =>
        startParserWithCheckpointsView(
          context,
          actionProvider,
          derivationTreeProvider,
          stackProvider,
          parserEntrypoint,
          lexerEntrypoint,
          source,
        ),
    ),

    registerCmd("refreshEntrypoints", () => {
      entrypointProvider.refresh([]);
    }),
  );

  const aux = ({ languageId }: vscode.TextDocument) => {
    (languageId === "ocaml.ocamllex" || languageId === "ocaml.menhir") &&
      entrypointProvider.refresh([]);
  };

  vscode.workspace.onDidOpenTextDocument(aux);
  vscode.workspace.onDidChangeTextDocument((d) => aux(d.document));
  vscode.workspace.textDocuments.forEach(aux);

  //////////////////////////////////////////////////////////////////////////////
}

let currentParserEntrypoint: Entry | undefined = undefined;

async function selectEntrypointQuickPick(
  msg: string,
  modules: ModuleEntries[],
) {
  return setupQuickPick<Entry>(
    msg,
    modules.flatMap(({ moduleUri, entrypoints }) => {
      let uri = vscode.Uri.parse(moduleUri);
      return [
        sep(uri.fsPath),
        ...entrypoints.map((rule) => ({
          label: `\$(symbol-function) ${rule.name}`,
          description: moduleUri.split("/").at(-1),
          data: rule,
        })),
      ];
    }),
  );
}

async function selectParser() {
  const modules = await execServerCmd<ModuleEntries[]>("listParsers");

  if (modules.length <= 0) {
    vscode.window.showErrorMessage(
      "You need to open at least one .mly file that has been compiled to the CMLY format.\n\nAdd the --cmly flag inside the menhir stanza of the dune file that declares the parser you'd like to use.",
    );
    return;
  }

  return await selectEntrypointQuickPick("Select Parser Entry Point", modules);
}

async function selectLexer() {
  const lexers = await execServerCmd<ModuleEntries[]>("listLexers");

  if (lexers.length <= 0) {
    vscode.window.showErrorMessage("You need to open at least one .mll file");
    return;
  }

  return await selectEntrypointQuickPick("Select Lexer Entry Point", lexers);
}

async function startParserView(
  treeProvider: DerivationTreeProvider,
  startSymbol?: Entry,
) {
  startSymbol = startSymbol || (await selectParser());
  if (!startSymbol) return;

  const cmd = "startParser";

  const input = await vscode.window.showInputBox({
    placeHolder: "Enter a space-separated list of tokens here",
  });

  if (!input) return;

  const tokens = input.split(/\s+/);

  const derivations = await execServerCmd<DerivationTree[]>(
    cmd,
    startSymbol,
    tokens,
  );

  const d0 = derivations.at(0);
  if (!d0) {
    console.log("No derivations");
    return;
  }

  treeProvider.refresh([d0]);
}

/** Invoked from the "regexp-token mapping" side view. */
async function startParserWithCheckpointsView(
  context: vscode.ExtensionContext,
  actionProvider: ActionMapProvider,
  treeProvider: DerivationTreeProvider,
  stackProvider: StackProvider,
  startSymbol?: Entry,
  lexerRule?: Entry,
  inputSource = InputSourceKind.ActiveEditor,
) {
  const cmd = "startParserWithCheckpoints";

  startSymbol = startSymbol || (await selectParser());
  if (!startSymbol) return;

  lexerRule = lexerRule || (await selectLexer());

  let editor = vscode.window.activeTextEditor;

  if (!editor) {
    vscode.window.showErrorMessage("No active editor available.");
    return;
  }

  let sourceUri = editor.document.uri;

  if (sourceUri.scheme !== "file") {
    vscode.window.showErrorMessage("Input source must be a file.");
    return;
  }

  let grammarUri = vscode.Uri.parse(startSymbol.moduleUri);

  const res = await execServerCmd<{
    lexerBindings: ActionMapping[];
    start: Checkpoint;
  }>(cmd, sourceUri.toString(), lexerRule, startSymbol);

  setContext("interpreterRunning", true);

  actionProvider.refresh(res.lexerBindings);
  updateViews(res.start);

  const cursorLight = context.asAbsolutePath("icons/cursor_light.svg");
  const cursorDark = context.asAbsolutePath("icons/cursor_dark.svg");
  const highlightColor = rgba(0, 255, 0, 0.25);
  let progressHighlight: vscode.TextEditorDecorationType =
    vscode.window.createTextEditorDecorationType({
      backgroundColor: highlightColor,
    });

  // editor.document.isDirty
  // vscode.workspace.onDidChangeTextDocument(e => e.contentChanges.map(ch => ch))

  let cursorDeco: vscode.TextEditorDecorationType =
    vscode.window.createTextEditorDecorationType({
      dark: {
        before: {
          contentIconPath: cursorDark,
        },
        after: {
          contentIconPath: cursorDark,
        },
      },
      light: {
        before: {
          contentIconPath: cursorLight,
        },
        after: {
          contentIconPath: cursorLight,
        },
      },
    });

  // Refresh the derivation and stack views and their data providers
  function updateViews(c: Checkpoint) {
    let { env } = c.value;

    derivationTreeView.description = `(${c.kind})`;
    derivationTreeView.message = `Checkpoint #${env.time} · ${c.kind} · State ${env.state}`;

    env.lookahead &&
      (derivationTreeView.message += `, looking at ${env.lookahead.terminal}`);

    if (editor && env.lookahead) {
      let range = liftRange(env.lookahead.token.range);
      editor.setDecorations(progressHighlight, [
        {
          range: new vscode.Range(new vscode.Position(0, 0), range.end),
        },
      ]);
      editor.setDecorations(cursorDeco, [{ range }]);
    }

    stackView.description = `${env.stack.length} items`;
    stackProvider.refresh(env.stack);
    treeProvider.refresh(env.derivation);

    // Update the derivation view message
    switch (c.kind) {
      case "inputNeeded":
        break;
      case "shifting":
        derivationTreeView.message += ` · Shifting token ${c.value.terminal}`;
        break;
      case "reducing":
        let { lhs, rhs } = c.value.production.value;
        derivationTreeView.message += ` · Reducing production ${lhs} -> ${rhs.join(" ")}`;
      case "accepted":
        break;
      case "rejected":
        break;
    }
  }

  const focusGrammar = (selection?: Range) =>
    vscode.window.showTextDocument(grammarUri, {
      preserveFocus: true,
      selection: selection && liftRange(selection),
      viewColumn: vscode.ViewColumn.Beside,
    });

  focusGrammar();

  const disposables: vscode.Disposable[] = [];

  const aux = (cmd: string) =>
    registerCmd(cmd, async () => {
      const c = await execServerCmd<Checkpoint>(cmd, startSymbol.moduleUri);

      switch (c.kind) {
        case "reducing":
          focusGrammar(c.value.production.range);
          break;
      }

      updateViews(c);
    });

  disposables.push(
    aux("stepForward"),
    aux("stepBackward"),
    aux("playForward"),
    aux("playBackward"),
    aux("playToStart"),
    aux("playToEnd"),
    registerCmd("stopInterpreter", async () => {
      setContext("interpreterRunning", false);
      await execServerCmd("disconnect", startSymbol.moduleUri);

      derivationTreeView.message =
        derivationTreeView.description =
        stackView.message =
        stackView.description =
          undefined;

      actionProvider.refresh([]);
      treeProvider.refresh([]);
      stackProvider.refresh([]);

      editor.setDecorations(progressHighlight, []);

      disposables.forEach((d) => d.dispose());
    }),
    // Note: commands invoked from tree items buttons receive the item's data as the first argument
    registerCmd("editRegexpTokenBinding", (binding) => {
      editRegexpBinding(actionProvider, binding, startSymbol);
    }),
  );
}

async function startTokenizerView(rule?: Entry, source?: InputSourceKind) {
  let editor = vscode.window.activeTextEditor;

  rule = rule || (await selectLexer());
  if (!rule) return;

  source = source || (await askForSourceKind());
  if (!source) return;

  // vscode.workspace.findFiles
  let cmd = "startTokenizer";

  switch (source) {
    case InputSourceKind.ActiveEditor: // what?!?
      {
        if (!editor) {
          vscode.window.showErrorMessage("No active editor available.");
          return;
        }
        const updateUI = async () => {
          const tokens: Token[] = await execServerCmd(
            cmd,
            editor.document.uri.toString(),
            null,
            rule,
          );

          highlightTokens(editor, tokens);
        };

        vscode.workspace.onDidChangeTextDocument(async (e) => {
          console.log(e.reason);
          if (!uriEqual(e.document.uri, editor.document.uri)) return;

          updateUI();
        });

        updateUI();
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

  // Classify the tokens by the action that produced them and associate
  const classes: Map<number, Token[]> = new Map();

  tokens.forEach((t) =>
    classes.has(t.action)
      ? classes.get(t.action)!.push(t)
      : classes.set(t.action, [t]),
  );

  classes.forEach((ts) => {
    const color = rgba(rand(127, 255), rand(127, 255), rand(127, 255), 0.5);

    const deco = vscode.window.createTextEditorDecorationType({
      backgroundColor: color,
      // outlineColor: color,
      // outlineStyle: "solid",
      // outlineWidth: "2px",
      borderSpacing: "10px",
      borderRadius: "5px",
      borderColor: color,
      borderWidth: "2px",
      border: "solid",

      rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed,
    });

    decos.push(deco);

    editor.setDecorations(
      deco,
      ts.map((t) => ({
        range: liftRange(t.range),
        renderOptions: {
          dark: {
            after: { color: "#888" },
          },
          after:
            t.text === "eof"
              ? { contentText: t.text, color: "#ccc", fontStyle: "italic" }
              : t.action === -1
                ? {
                    contentText: "error",
                    color: "red",
                    fontStyle: "bold",
                  }
                : {},
        },
        hoverMessage: new vscode.MarkdownString(
          t.action === -1
            ? `Unhandled character \`"${t.text}"\``
            : `\`\`\`
"${t.text}"
\`\`\`

---

$(target) Go to action ${t.action}`,
          true,
        ),
      })),
    );
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

async function editRegexpBinding(
  actionProvider: ActionMapProvider,
  oldBinding: ActionMapping,
  entry: Entry,
) {
  const tokens = (await execServerCmd(
    "listTokens",
    entry.moduleUri,
  )) as Terminal[];

  const newBinding = await setupQuickPick<LexerAction>(
    `Select the action to perform for the pattern ${oldBinding.regexp.value}`,
    [
      choose("$(skip) Skip", { kind: LexerActionKind.Skip }),
      choose("$(error) Fail", { kind: LexerActionKind.Fail }),
      ...tokens.map((token) =>
        choose(`$(symbol-constant) Produce ${token.name}`, {
          kind: LexerActionKind.Produce,
          value: token,
        }),
      ),
    ],
  );

  if (!newBinding) return;

  let bindings = await execServerCmd<ActionMapping[]>(
    "updateLexerBinding",
    entry.moduleUri, // mly_uri
    oldBinding.moduleUri, // mll_uri
    oldBinding.id,
    newBinding,
  );

  vscode.window.showInformationMessage("Binding updated.");
  actionProvider.refresh(bindings);
}

const askForSourceKind = () =>
  setupQuickPick<InputSourceKind>("Select the input source", [
    choose("$(target) Active Editor", InputSourceKind.ActiveEditor),
    choose("$(file-text) Read input from a file", InputSourceKind.File),
    choose("$(pencil) Enter some text", InputSourceKind.TextBox),
  ]);
