import * as vscode from "vscode";
import * as vscode from "vscode";
import { Range } from "vscode-languageclient";

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
