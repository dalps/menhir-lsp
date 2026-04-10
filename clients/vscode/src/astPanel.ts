import * as vscode from "vscode";
import { Uri } from "vscode";
import { getAst, liftRange } from "./extension";
import { Range } from "vscode-languageclient";
import fs from "fs";

const highlightDecorationType = vscode.window.createTextEditorDecorationType({
  backgroundColor: { id: "menhirlsp.astExplorer.highlightColor" },
  borderRadius: "2px",
});

export function getWebviewOptions(extensionUri: Uri): vscode.WebviewOptions {
  return {
    enableScripts: true,
    localResourceRoots: [Uri.joinPath(extensionUri, "out", "webviews")],
  };
}

/** Singleton manager for the AST webview. */
export class ASTPanel implements vscode.Disposable {
  public static currentPanel: ASTPanel | undefined;

  public static readonly viewType = "astView";
  private readonly _title = "AST View";
  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: Uri;

  private _editor: vscode.TextEditor | undefined;
  private _disposables: vscode.Disposable[] = [];

  set editor(textEditor: vscode.TextEditor) {
    let { languageId } = textEditor.document;
    if (languageId !== "ocaml.menhir" && languageId !== "ocaml.ocamllex")
      return;
    this._editor = textEditor;
  }

  public static async createOrShow(extensionUri: Uri) {
    const editor = vscode.window.activeTextEditor;

    // There must be an open document
    if (!editor) {
      console.log("Couldn't start AST explorer: no open editor");
      return;
    }

    const column = editor.viewColumn;

    if (ASTPanel.currentPanel) {
      ASTPanel.currentPanel._panel.reveal();
      return;
    }

    const panel = vscode.window.createWebviewPanel(
      ASTPanel.viewType,
      "AST Browser",
      column ? column + 1 : vscode.ViewColumn.One,
      getWebviewOptions(extensionUri),
    );

    ASTPanel.currentPanel = new ASTPanel(panel, editor, extensionUri);
  }

  // public static revive(panel: vscode.WebviewPanel, extensionUri: Uri) {
  //   ASTPanel.currentPanel = new ASTPanel(panel, extensionUri);
  // }

  /** Only the manager can instantiate a panel. */
  private constructor(
    panel: vscode.WebviewPanel,
    editor: vscode.TextEditor,
    extensionUri: Uri,
  ) {
    this._panel = panel;
    this.editor = editor;
    this._extensionUri = extensionUri;

    const { webview } = this._panel;
    webview.html = this._getHtmlForWebview(this._extensionUri, webview);

    // Set the webview's initial html content
    this._update();

    // Dispose of this manager when the panel is closed
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    vscode.window.onDidChangeActiveTextEditor(
      (newEditor) => {
        if (!newEditor) return;

        console.log("active editor changed to:", newEditor?.document.fileName);
        this.editor = newEditor;
        this._update();
      },
      null,
      this._disposables,
    );

    // Update the content based on view changes (e.g. the user clicks outside the webview)
    this._panel.onDidChangeViewState(
      () => {
        if (this._panel.visible) this._update();
      },
      null,
      this._disposables,
    );

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      (message) => {
        switch (message.command) {
          case "highlight":
            this.focusAstNodeInEditor(message.data);
            break;
        }
      },
      null,
      this._disposables,
    );
  }

  private async _update() {
    console.log("Updating webview");

    this._panel.title = this._title;

    const { webview } = this._panel;
    const editor = this._editor;

    // There must be an open document
    if (!editor) return;

    let ast = await getAst(editor.document.uri);
    webview.postMessage({ type: "publishAst", data: ast });
  }

  public dispose() {
    ASTPanel.currentPanel = undefined;

    this._panel.dispose();

    while (this._disposables.length) {
      const d = this._disposables.pop();
      d?.dispose();
    }
  }

  private _getHtmlForWebview(extensionUri: Uri, webview: vscode.Webview) {
    const webviewPath = Uri.joinPath(
      this._extensionUri,
      "out",
      "webviews",
      "ast",
    );

    const scriptUri = webview.asWebviewUri(
      Uri.joinPath(webviewPath, "index.js"),
    );

    const styleUri = webview.asWebviewUri(
      Uri.joinPath(webviewPath, "index.css"),
    );

    const template = fs
      .readFileSync(Uri.joinPath(webviewPath, "index.html").fsPath)
      .toString("utf-8");

    return template
      .replace(/\${webview.cspSource}/g, webview.cspSource)
      .replace(/\${styleUri}/g, styleUri.toString())
      .replace(/\${scriptUri}/g, scriptUri.toString())
      .replace(/\${nonce}/g, getNonce());
  }

  //#region commands

  public publishAst(data: any) {
    ASTPanel.currentPanel?._panel.webview.postMessage({
      command: "publishAst",
      data,
    });
  }

  public focusAstNodeInEditor(data: { range: Range }) {
    const activeEditor = this._editor;

    if (!activeEditor) {
      console.log("No active edtior, bye");
      return;
    }

    let r = liftRange(data.range);

    activeEditor.revealRange(r);
    activeEditor.setDecorations(highlightDecorationType, [r]);
  }

  public static async revealNodeUnderCursor(extensionUri: Uri) {
    await ASTPanel.createOrShow(extensionUri);

    const activeEditor = ASTPanel.currentPanel?._editor;

    if (!activeEditor) {
      console.log("No active edtior, bye");
      return;
    }

    const cursor = activeEditor.selection.start;
    console.log("cursor is at:", cursor);
    const offset = activeEditor.document.offsetAt(cursor);

    this.currentPanel?._panel.webview.postMessage({ type: "focus", offset });
  }

  //#endregion commands
}

function getNonce() {
  let text = "";
  const possible =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < 32; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  return text;
}
