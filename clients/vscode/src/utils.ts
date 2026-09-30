import * as vscode from "vscode";
import { Range } from "vscode-languageclient";

export const uriEqual = (u1: vscode.Uri, u2: vscode.Uri) =>
  u1.scheme === u2.scheme && u1.path === u2.path;

export const liftRange = (r: Range): vscode.Range => {
  let { start, end } = r;

  return new vscode.Range(
    new vscode.Position(start.line, start.character),
    new vscode.Position(end.line, end.character),
  );
};

export async function setupQuickPick<T = unknown>(
  title: string,
  items: (vscode.QuickPickItem & T)[],
): Promise<(vscode.QuickPickItem & T) | undefined> {
  const qp = vscode.window.createQuickPick();
  qp.title = title;
  // qp.prompt =
  //   "Select the entry point to use among the lexers opened so far.";
  // qp.items = lexers;
  qp.items = items;

  qp.show();

  let selection: (vscode.QuickPickItem & T) | undefined;

  qp.onDidChangeActive(
    (item) =>
      (selection = item.at(0) as (vscode.QuickPickItem & T) | undefined),
  );

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
  return selection satisfies vscode.QuickPickItem | undefined;
}

export const rand = (min: number, max: number) => lerp(min, max, Math.random());

export function pickRandom(...options: any[]): any {
  return options[Math.floor(Math.random() * options.length)];
}

export function clamp(min: number, max: number, n: number) {
  return Math.max(min, Math.min(n, max));
}

export function lerp(min: number, max: number, t: number) {
  return min * (1 - t) + max * t;
}
