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
      // Expand the "Outline" panel on click
      bar.command = "outline.focus";

      bar.text = await execServerCmd<string>(
        "echoErrors",
        editor.document.uri.toString(),
      );

      bar.show();
    } else {
      bar.hide();
    }
  }
}
