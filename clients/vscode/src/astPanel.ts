import * as vscode from "vscode";
import { Uri } from "vscode";
import { getAst, liftRange } from "./extension";
import { Range } from "vscode-languageclient";
import fs from "fs";

const highlightDecorationType = vscode.window.createTextEditorDecorationType({
  backgroundColor: "#0000ff33", // #0000ff33
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

  private static _editor: vscode.TextEditor | undefined;
  private readonly _title = "AST View";
  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: Uri;
  private _disposables: vscode.Disposable[] = [];

  public static async createOrShow(extensionUri: Uri) {
    const editor = vscode.window.activeTextEditor;

    this._editor = editor;

    // There must be an open document
    if (!editor) return;

    let { uri, languageId } = editor.document;
    if (languageId !== "ocaml.menhir" && languageId !== "ocaml.ocamllex")
      return;

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

    let ast = await getAst(uri);
    panel.webview.postMessage({ type: "publishAst", data: ast });

    console.log("[ASTpanel] This is the AST I got:", ast);

    ASTPanel.currentPanel = new ASTPanel(panel, extensionUri);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: Uri) {
    ASTPanel.currentPanel = new ASTPanel(panel, extensionUri);
  }

  /** Only the manager can instantiate a panel. */
  private constructor(panel: vscode.WebviewPanel, extensionUri: Uri) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    // Set the webview's initial html content
    this._update();

    // Dispose of this manager when the panel is closed
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Update the content based on view changes
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
            console.log("message data", message.data);
            this.focusAstNodeInEditor(message.data);
            break;
        }
      },
      null,
      this._disposables,
    );
  }

  private _update() {
    const { webview } = this._panel;

    this._panel.title = this._title;
    webview.html = this._getHtmlForWebview(this._extensionUri, webview);
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
    const activeEditor = ASTPanel._editor;

    if (!activeEditor) {
      console.log("No active edtior, bye");
      return;
    }

    let r = liftRange(data.range);

    activeEditor.revealRange(r);
    activeEditor.setDecorations(highlightDecorationType, [r]);
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
