import * as vscode from "vscode";
import { Uri } from "vscode";

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
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: Uri) {
    const column = vscode.window.activeTextEditor?.viewColumn;

    if (ASTPanel.currentPanel) {
      ASTPanel.currentPanel._panel.reveal();
      return;
    }

    const panel = vscode.window.createWebviewPanel(
      ASTPanel.viewType,
      "AST Viewer",
      column || vscode.ViewColumn.One,
      getWebviewOptions(extensionUri),
    );

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
        // todo
      },
      null,
      this._disposables,
    );
  }

  private _update() {
    const { webview } = this._panel;

    this._panel.title = this._title;
    webview.html = this._getHtmlForWebview(webview);
  }

  public dispose() {
    this._panel.dispose();

    while (this._disposables.length) {
      const d = this._disposables.pop();
      d?.dispose();
    }
  }

  private _getHtmlForWebview(webview: vscode.Webview) {
    const nonce = getNonce();

    const scriptUri = webview.asWebviewUri(
      Uri.joinPath(this._extensionUri, "out", "webviews", "ast", "index.js"),
    );

    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Document</title>
</head>
<body>
  <div id="app"></div>
  <script nonce="${nonce}" src="${scriptUri}"></script>
</body>
</html>`;
  }
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
