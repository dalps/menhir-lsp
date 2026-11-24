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
  Lr0ItemProvider,
  StackProvider,
} from "./derivationTree";
import { activateStatusBar } from "./status";
import { TreeProvider } from "./TreeProvider";
import {
  ActionMapping,
  Checkpoint,
  DerivationTree,
  Entry,
  InputSourceKind,
  InterpreterResult,
  Item,
  LexerAction,
  LexerActionKind,
  ModuleEntries,
  Result,
  StackItem,
  Terminal,
  Token,
} from "./types";
import {
  askForInputSource,
  choose,
  closeEditor,
  decorationType,
  focusView,
  GREEN,
  liftRange,
  md,
  rand,
  RED,
  rgba,
  sep,
  setupQuickPick,
  sinceStartRange,
  uriEqual,
  YELLOW,
} from "./utils";

let client: LanguageClient;
let derivationTreeView: vscode.TreeView<DerivationTree>;
let stackView: vscode.TreeView<StackItem>;
let lr0ItemsView: vscode.TreeView<Item>;
let actionBindingView: vscode.TreeView<ActionMapping | Terminal>;
let entrypointExplorerView: vscode.TreeView<Entry | ModuleEntries>;
let lr0ItemsProvider: TreeProvider<Item>;
let derivationTreeProvider: DerivationTreeProvider;
let stackProvider: StackProvider;
let actionProvider: ActionMapProvider;
let entrypointProvider: EntrypointProvider;

const serverName = "menhir-lsp";
const clientName = "menhir-lsp-client";

const showError = (msg: string, ...items: string[]) =>
  vscode.window.showErrorMessage(`[${serverName}] ${msg}`, ...items);

////////////////////////////////////////////////////////////////////////////////
// Helpers for defining commands
//

const commandName = (name: string) => `${clientName}.${name}`;

const registerCmd = (name: string, callback: (...args: any[]) => any) =>
  vscode.commands.registerCommand(commandName(name), callback);

export const execServerCmd = async <T>(command: string, ...args: any[]) => {
  const res = await client.sendRequest<{ status: number; value: T }>(
    "workspace/executeCommand",
    { command, arguments: args } as ExecuteCommandParams,
    CancellationToken.None,
  );

  if (res.status < 0) {
    showError(
      `Command not found. Make sure you have the latest version of ${serverName} installed. If the error persists, contact the extension's developer.`,
      `Update ${serverName}`,
    ).then((install) => {
      if (!install) return;

      let opamInstallCmd = `opam update && opam upgrade ${serverName}`;
      let terminal = vscode.window.createTerminal(serverName);

      terminal.show();
      terminal.sendText(opamInstallCmd);
    });
  }

  return res.value;
};

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

    let install = await showError(
      `The ${serverName} executable was not found on the host. Would you like to install it automatically with opam?`,
      `Install ${serverName}`,
    );

    if (install === undefined) return;

    vscode.window.showInformationMessage(
      `Installing ${serverName}. Please reload the window once the installation completes to activate client.`,
    );

    let opamInstallCmd = `opam install ${serverName}`;
    let terminal = vscode.window.createTerminal(serverName);

    terminal.show();
    terminal.sendText(opamInstallCmd);
  });

  derivationTreeProvider = new DerivationTreeProvider(context);
  stackProvider = new StackProvider(context);

  actionProvider = new ActionMapProvider(context);

  entrypointProvider = new EntrypointProvider(context);
  lr0ItemsProvider = new Lr0ItemProvider(context);

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

  lr0ItemsView = vscode.window.createTreeView("menhir-lr0-items-view", {
    treeDataProvider: lr0ItemsProvider,
  });

  entrypointExplorerView = vscode.window.createTreeView(
    "menhir-lsp-entrypoint-explorer",
    {
      treeDataProvider: entrypointProvider,
    },
  );

  context.subscriptions.push(
    registerCmd("showOutput", outputChannel.show),

    registerCmd("restartServer", async () => {
      if (client.isRunning()) await client.stop();

      client.start();
    }),

    registerCmd(
      "promptAlias",
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
    ),

    registerCmd("gotoImplementation", () => {
      const editor = vscode.window.activeTextEditor;

      if (!editor) return;

      gotoImplementation(editor.document.uri, editor.selection.active);
    }),

    registerCmd("astView", () => {
      ASTPanel.createOrShow(context.extensionUri);
    }),

    registerCmd("revealAstNode", () => {
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

  //////////////////////////////////////////////////////////////////////////////
  // More commands
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
    registerCmd("startParser", startParserView),
    registerCmd(
      "startParserWithCheckpoints",
      (parserEntrypoint, lexerEntrypoint, source) =>
        startParserWithCheckpointsView(
          context,
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
  vscode.workspace.onDidCloseTextDocument(aux);
  vscode.workspace.onDidChangeTextDocument((d) => aux(d.document));
  entrypointProvider.refresh([]);

  //////////////////////////////////////////////////////////////////////////////
}

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
    showError(
      "You need to open at least one .mly file that has been compiled to the CMLY format.\n\nAdd the --cmly flag inside the menhir stanza of the dune file that declares the parser you'd like to use.",
    );
    return;
  }

  return await selectEntrypointQuickPick(
    "Select a start symbol of the grammar",
    modules,
  );
}

async function selectLexer() {
  const lexers = await execServerCmd<ModuleEntries[]>("listLexers");

  if (lexers.length <= 0) {
    showError("You need to open at least one .mll file");
    return;
  }

  return await selectEntrypointQuickPick(
    "Select a lexer rule to scan the input",
    lexers,
  );
}

async function startParserView(startSymbol?: Entry) {
  startSymbol = startSymbol || (await selectParser());
  if (!startSymbol) return;

  const tokensNames = (
    await execServerCmd<Terminal[]>("listTokens", startSymbol.moduleUri)
  ).map((t) => t.name);

  const cmd = "startParser";

  const input = await vscode.window.showInputBox({
    placeHolder: "Enter a space-separated list of tokens (case insensitive)",
    validateInput(value) {
      const inputTokens = value.toUpperCase().split(/\s+/);

      let bad = inputTokens.find((v) => !tokensNames.includes(v));

      return (
        bad && {
          message: `${bad}: unkown token.`,
          severity: vscode.InputBoxValidationSeverity.Error,
        }
      );
    },
  });
  if (!input) return;

  const tokens = input.toUpperCase().split(/\s+/);

  const res = await execServerCmd<
    Result<DerivationTree, { message: string; derivations: DerivationTree[] }>
  >(cmd, startSymbol, tokens);

  if (res.kind === "error") {
    derivationTreeView.message = `⚠ Error: ${res.value.message}. Showing a forest of partial derivations.`;
    derivationTreeProvider.refresh(res.value.derivations);
  } else {
    // Start symbol reduced successfully
    derivationTreeProvider.refresh([res.value]);
  }

  focusView("menhir-lsp-derivation-view");
}

/** Invoked from the "regexp-token mapping" side view. */
async function startParserWithCheckpointsView(
  context: vscode.ExtensionContext,
  startSymbol?: Entry,
  lexerRule?: Entry,
  sourceKind?: InputSourceKind,
) {
  const cmd = "startParserWithCheckpoints";

  startSymbol = startSymbol || (await selectParser());
  if (!startSymbol) return;

  lexerRule = lexerRule || (await selectLexer());
  if (!lexerRule) return;

  let source = await askForInputSource(sourceKind, "Enter some text to parse");
  if (!source) return;

  let { editor } = source;

  let sourceUri = editor.document.uri;
  let grammarUri = vscode.Uri.parse(startSymbol.moduleUri);

  const res = await execServerCmd<{
    lexerBindings: ActionMapping[];
    start: Checkpoint;
  }>(
    cmd,
    editor.document.getText(),
    sourceUri.toString(),
    lexerRule,
    startSymbol,
  );

  if (!res) {
    showError("Server failed to launch the interpreter.");
    return;
  }

  setContext("interpreterRunning", true);

  actionProvider.refresh(res.lexerBindings);
  updateViews(res.start);

  let grammarHighlight = decorationType({
    backgroundColor: YELLOW,
    isWholeLine: true,
  });
  let progressHighlight = decorationType({
    backgroundColor: GREEN,
  });
  let discardHighlight = decorationType({
    backgroundColor: RED,
    textDecoration: "red dotted underline",
  });
  let inputHighlight = decorationType({
    borderColor: "var(--vscode-focusBorder)",
    borderWidth: "2px",
    borderRadius: "4px",
    borderStyle: "dashed",
  });

  const statusBar = vscode.window.createStatusBarItem(
    "menhir-lsp-client.interpreterStatusBar",
    vscode.StatusBarAlignment.Left,
  );

  statusBar.name = "Menhir LSP Interpreter";
  statusBar.text = `$(stop-circle) Interpreter (${startSymbol.name})`;
  statusBar.tooltip = "Click to disconnect";
  statusBar.command = "menhir-lsp-client.stopInterpreter";
  statusBar.backgroundColor = new vscode.ThemeColor(
    "statusBarItem.errorBackground",
  );

  statusBar.show();

  // Refresh the derivation and stack views and their data providers
  function updateViews(c: Checkpoint) {
    let { env } = c.value;

    if (editor && env.lookahead) {
      let range = liftRange(env.lookahead.token.range);
      editor.setDecorations(progressHighlight, [
        {
          range: sinceStartRange(range.end),
          renderOptions:
            c.kind === "rejected" || c.kind === "lexerError"
              ? rejectedDecoration
              : c.kind === "accepted"
                ? acceptedDecoration
                : undefined,
        },
      ]);
      editor.setDecorations(inputHighlight, [
        { range, hoverMessage: md`Token ${{ code: env.lookahead.terminal }}` },
      ]);
    }

    if (env.incoming) {
      lr0ItemsView.message = `Showing LR(0) items of state: ${env.state} · Incoming symbol: ${env.incoming.name}`;
    }

    stackView.description = `${env.stack.length} items`;
    stackProvider.refresh(env.stack);
    derivationTreeProvider.refresh(env.derivation);

    lr0ItemsProvider.refresh(env.items);

    // Update the derivation view's description
    switch (c.kind) {
      case "inputNeeded":
        derivationTreeView.description = `Input needed`;
        break;
      case "lexerError":
        derivationTreeView.description = `Lexer error`;
        break;
      case "shifting":
        derivationTreeView.description = `Shifting`;
        break;
      case "reducing":
        derivationTreeView.description = `Reducing`;
        break;
      case "accepted":
        derivationTreeView.description = `Accepted`;
        break;
      case "rejected":
        derivationTreeView.description = `Rejected`;
        break;
    }

    // Update the derivation view's message
    derivationTreeView.message = `Checkpoint #${env.time} · State ${env.state}`;

    env.lookahead &&
      (derivationTreeView.message += `, looking at ${env.lookahead.terminal}`);

    switch (c.kind) {
      case "inputNeeded":
        derivationTreeView.message += ` · Input needed`;
        break;
      case "lexerError":
        derivationTreeView.message += ` · Lexer error: ${c.value.message}`;
        break;
      case "shifting":
        derivationTreeView.message += ` · Shifting token ${c.value.terminal}`;
        break;
      case "reducing":
        let { lhs, rhs } = c.value.production.value;
        derivationTreeView.message += ` · Reducing production ${lhs} → ${rhs.map((x) => x.name).join(" ")}`;
        break;
      case "accepted":
        derivationTreeView.message += ` · Accepted (reduced start production)`;
        break;
      case "rejected":
        derivationTreeView.message += ` · Rejected`;
        break;
    }
  }

  const focusGrammar = async (selection?: Range) => {
    const editor = await vscode.window.showTextDocument(grammarUri, {
      preview: false,
      preserveFocus: true,
      viewColumn: vscode.ViewColumn.Beside,
    });

    if (selection) {
      let range = liftRange(selection);
      editor.revealRange(range);
      editor.setDecorations(grammarHighlight, [range]);
    }

    return editor;
  };

  const disposables: vscode.Disposable[] = [];

  const grammarEditor = await focusGrammar();

  focusView("menhir-lsp-derivation-view");
  focusView("menhir-lsp-stack-view");
  focusView("menhir-lr0-items-view");

  const stepCmd = (cmd: string) =>
    registerCmd(cmd, async () => {
      const c = await execServerCmd<InterpreterResult<Checkpoint>>(
        cmd,
        startSymbol.moduleUri,
      );

      if (c.kind === "error") return;

      const checkpoint = c.value;

      editor.setDecorations(inputHighlight, []);
      editor.setDecorations(discardHighlight, []);

      switch (checkpoint.kind) {
        case "reducing":
          focusGrammar(checkpoint.value.production.range);
          break;
      }

      updateViews(checkpoint);
    });

  disposables.push(
    stepCmd("stepForward"),
    stepCmd("stepBackward"),
    stepCmd("playForward"),
    stepCmd("playBackward"),
    stepCmd("playToStart"),
    stepCmd("playToEnd"),
    vscode.workspace.onDidChangeTextDocument(async (e) => {
      if (!uriEqual(e.document.uri, editor.document.uri)) return;

      let firstChangeStart = e.contentChanges.at(0)?.range.start;
      if (!firstChangeStart) return;

      const res = await execServerCmd<
        InterpreterResult<{
          discarded: number;
          range: Range;
        }>
      >(
        "syncDocument",
        startSymbol.moduleUri,
        firstChangeStart,
        e.document.getText(),
      );

      if (res.kind === "error") return;

      const { range: r, discarded } = res.value;
      const range = liftRange(r);
      editor.setDecorations(progressHighlight, [sinceStartRange(range.start)]);
      editor.setDecorations(discardHighlight, [
        { range, hoverMessage: "Discarded" },
      ]);

      if (discarded > 0) {
        derivationTreeView.message = `⚠ Parsed input has changed! The interpreter travelled to the last valid checkpoint, please issue another step to resume.`;
        derivationTreeProvider.flush();
        stackProvider.flush();
        lr0ItemsProvider.flush();
      }
    }),
    registerCmd("highlightText", (range: vscode.Range) => {
      editor.setDecorations(inputHighlight, [range]);
    }),
    registerCmd("stopInterpreter", async () => {
      setContext("interpreterRunning", false);
      await execServerCmd("disconnect", startSymbol.moduleUri);

      derivationTreeView.message =
        derivationTreeView.description =
        stackView.message =
        stackView.description =
        lr0ItemsView.message =
          undefined;

      actionProvider.flush();
      derivationTreeProvider.flush();
      stackProvider.flush();
      lr0ItemsProvider.flush();

      editor.setDecorations(progressHighlight, []);
      editor.setDecorations(inputHighlight, []);
      editor.setDecorations(discardHighlight, []);

      closeEditor(grammarEditor);
      statusBar.hide();

      disposables.forEach((d) => d.dispose());
    }),
    // Note: commands invoked from tree items buttons receive the item's data as the first argument
    registerCmd("editRegexpTokenBinding", (binding) => {
      editRegexpBinding(actionProvider, binding, startSymbol);
    }),
  );
}

async function startTokenizerView(rule?: Entry, sourceKind?: InputSourceKind) {
  rule = rule || (await selectLexer());
  if (!rule) return;

  let cmd = "startTokenizer";

  let source = await askForInputSource(
    sourceKind,
    "Enter some text to be scanned",
  );
  if (!source) return;

  let { editor } = source;

  const actions = await execServerCmd<Range[]>("listActions", rule);

  const regularTokenDecorations = actions.map(() => {
    const color = rgba(rand(105, 225), rand(105, 225), rand(105, 225), 0.5);
    return decorationType({
      backgroundColor: color,
      borderSpacing: "10px",
      borderRadius: "2px",
      borderColor: color,
      borderWidth: "2px",
      border: "solid",
      rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed,
    });
  });

  const decorations = [
    decorationType(rejectedDecoration),
    ...regularTokenDecorations,
  ];

  const clearUI = () => {
    regularTokenDecorations.forEach((d) => d.dispose());
  };

  const updateUI = async () => {
    const input = editor.document.getText();
    const tokens = await execServerCmd<Token[]>(cmd, rule, input);

    for (let i = -1; i < actions.length; i++) {
      editor.setDecorations(
        decorations[i + 1],
        tokens
          .filter((t) => t.action === i)
          .map((t): vscode.DecorationOptions => {
            t.text = t.text.replace(/\n/, "\\n");
            const text = /\s+/.test(t.text) ? "<whitespace>" : t.text;
            return {
              range: liftRange(t.range),
              hoverMessage:
                t.action === -1
                  ? md`
$(warning) Empty token
                    `
                  : md`${{ block: text }}\n\n---\n\n$(milestone) Go to [action ${t.action}](${rule.moduleUri}#${actions[i].start.line})`,
            };
          }),
      );
    }
  };

  const changeSubscription = vscode.workspace.onDidChangeTextDocument(
    (e) => uriEqual(e.document.uri, editor.document.uri) && updateUI(),
  );

  vscode.window.onDidChangeActiveTextEditor(() => {
    changeSubscription.dispose();
    clearUI();
  });

  updateUI();
}

const rejectedDecoration: vscode.DecorationRenderOptions = {
  after: {
    contentText: "⮿",
    color: "var(--vscode-errorForeground)",
    fontStyle: "bold",
  },
};
const acceptedDecoration: vscode.DecorationRenderOptions = {
  after: {
    contentText: "✔",
    color: "var(--vscode-terminal-ansiBrightGreen)",
    fontStyle: "bold",
  },
};
const eofActionDecoration: vscode.DecorationRenderOptions = {
  after: {
    contentText: "eof",
    color: "var(--vscode-disabledForeground)",
    fontStyle: "italic",
  },
};
const newlineActionDecoration: vscode.DecorationRenderOptions = {
  after: {
    contentText: "⮠",
    ...eofActionDecoration.after,
  },
};

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

export async function getAst(uri: vscode.Uri) {
  console.log(`Requesting AST of document: ${uri}`);

  return await execServerCmd("getAst", uri.toString());
}

export async function gotoImplementation(
  uri: vscode.Uri,
  pos?: vscode.Position,
) {
  console.log(
    `Requesting implementation of document: ${uri} at position ${pos}`,
  );

  return await execServerCmd("gotoImplementation", uri.toString(), pos);
}

async function editRegexpBinding(
  actionProvider: ActionMapProvider,
  oldBinding: ActionMapping,
  entry: Entry,
) {
  const tokens = await execServerCmd<Terminal[]>("listTokens", entry.moduleUri);

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

  // vscode.window.showInformationMessage("Binding updated.");
  actionProvider.refresh(bindings);
}
