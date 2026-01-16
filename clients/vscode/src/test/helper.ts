import * as vscode from "vscode";
import { Position, Range, Uri } from "vscode";
import * as path from "path";

export let doc: vscode.TextDocument;
export let editor: vscode.TextEditor;
export let documentEol: string;
export let platformEol: string;

const extName = "dalps.menhir-lsp-client";

export async function activate(docUri: Uri) {
  const ext = vscode.extensions.getExtension(extName);

  if (!ext) return console.error(`Couldn't activate ${extName}.`);

  await ext.activate();

  try {
    doc = await vscode.workspace.openTextDocument(docUri);
    editor = await vscode.window.showTextDocument(doc);
    await sleep(2000);
  } catch (e) {
    console.error(e);
  }
}

async function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

export const getDocPath = (p: string) =>
  path.resolve(__dirname, "../../samples", p);

export const getDocUri = (p: string) => Uri.file(getDocPath(p));

export async function setTestContent(content: string): Promise<boolean> {
  const all = new Range(
    doc.positionAt(0),
    doc.positionAt(doc.getText().length)
  );

  return editor.edit((eb) => eb.replace(all, content));
}
