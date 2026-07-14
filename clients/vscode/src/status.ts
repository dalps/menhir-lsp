// Inspired by https://github.com/MrOrz/vscode-gettext

import * as vscode from "vscode";
import { execServerCmd } from "./extension";

export function activateStatusBar(context: vscode.ExtensionContext) {
  let bar = vscode.window.createStatusBarItem(
    "menhir-lsp-client.messagesStatus",
    vscode.StatusBarAlignment.Left,
    0,
  );

  context.subscriptions.push(bar);

  toggle(vscode.window.activeTextEditor);

  vscode.window.onDidChangeActiveTextEditor(toggle);

  async function toggle(editor?: vscode.TextEditor) {
    if (editor && editor.document.languageId === "ocaml.menhir.messages") {
      bar.show();

      bar.text = await execServerCmd<string>(
        "echoErrors",
        editor.document.uri.toString(),
      );
      bar.command = ""
    } else {
      bar.hide();
    }
  }
}
