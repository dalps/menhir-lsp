import * as vscode from "vscode";
import { Position, Range } from "vscode-languageclient";
import { CodiconId } from "./codicons";
import {
  InputSourceKind,
  Item,
  Production,
  QuickPickItemWithData,
  ViewId,
  WithRange,
} from "./types";

export const serverName = "menhir-lsp";
export const clientName = "menhir-lsp-client";

export const showError = (msg: string, ...items: string[]) =>
  vscode.window.showErrorMessage(`[${serverName}] ${msg}`, ...items);

export const showWarning = (msg: string, ...items: string[]) =>
  vscode.window.showWarningMessage(`[${serverName}] ${msg}`, ...items);

export const CURSOR_ICON = "🠷";

export const uriEqual = (u1: vscode.Uri, u2: vscode.Uri) =>
  u1.scheme === u2.scheme && u1.path === u2.path;

export const liftRange = (r: Range): vscode.Range => {
  let { start, end } = r;

  return new vscode.Range(
    new vscode.Position(start.line, start.character),
    new vscode.Position(end.line, end.character),
  );
};

export const sinceStartRange = (p: vscode.Position) =>
  new vscode.Range(new vscode.Position(0, 0), p);

export async function setupQuickPick<T>(
  title: string,
  items: QuickPickItemWithData<T>[],
): Promise<T | undefined> {
  const qp = vscode.window.createQuickPick();
  let selection: QuickPickItemWithData<T> | undefined;

  qp.title = title;

  qp.items = items;

  qp.show();

  qp.onDidChangeActive((item) => (selection = item.at(0)));

  // Wait for a selection or dismissal
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

  qp.dispose();

  return selection?.data;
}

export const sep = <T>(label: string): QuickPickItemWithData<T> => ({
  label,
  kind: vscode.QuickPickItemKind.Separator,
});

export const choose = <T>(
  label: string,
  data?: T,
): QuickPickItemWithData<T> => ({
  label,
  data,
});

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

/** Set this as the command field of a TreeItem for focusing a region of a file by clicking on the item. */
export function openCommand(
  uri: vscode.Uri,
  element?: WithRange,
): vscode.Command {
  return {
    command: "vscode.open",
    title: "Focus in editor",
    arguments: element
      ? [
          uri,
          {
            selection: liftRange(element?.range),
          } as vscode.TextDocumentShowOptions,
        ]
      : [uri],
  };
}

/** Set this as the command field of a TreeItem for focusing a region of the current text by clicking on the item. */
export function highlightText(range: Range): vscode.Command {
  return {
    command: "menhir-lsp-client.highlightText",
    title: "Focus in editor",
    arguments: [liftRange(range)],
  };
}

/** Credit to widlow: https://stackoverflow.com/a/75192905/12274930 */
export const closeEditor = async (editor: vscode.TextEditor) => {
  const uri = editor.document.uri;

  const tabs: vscode.Tab[] = vscode.window.tabGroups.all.flatMap((g) => g.tabs);

  const t = tabs.find(
    (t) =>
      t.input instanceof vscode.TabInputText &&
      t.group.viewColumn === editor.viewColumn &&
      uriEqual(t.input.uri, uri),
  );

  if (!t) {
    console.log("Tab not found!");
    return false;
  }

  return vscode.window.tabGroups.close(t);
};

export const themeicon = (id: CodiconId, colorId?: string | undefined) =>
  new vscode.ThemeIcon(
    id,
    colorId ? new vscode.ThemeColor(colorId) : undefined,
  );

/** Shorthand to build markdown strings from template literals tagged `md`.
 * Some MD features can be accessed by wrapping the placeholder expressions in
 * particular objects. For example, to render the value of `e` in an inline code
 * blocks, write `{ $: e }` inside the placeholder `${..}` syntax. */
export const md = (strings: TemplateStringsArray, ...values: any[]) =>
  new vscode.MarkdownString(
    String.raw(
      { raw: strings },
      ...values.map((v) =>
        v.code
          ? `\`${v.code}\``
          : v.block
            ? `\n\`\`\`\n${v.block}\n\`\`\`\n`
            : v,
      ),
    ),
    true,
  );

export const stringOfPosition = (p: vscode.Position | Position) =>
  `${p.line + 1}:${p.character + 1}`;

export const stringOfRange = (r: vscode.Range | Range) =>
  `[${stringOfPosition(r.start)}-${stringOfPosition(r.end)}]`;

export const rgba = (r: number, g: number, b: number, a: number) =>
  `rgba(${r},${g},${b}, ${a})`;

export const YELLOW = rgba(255, 255, 0, 0.25);
export const GREEN = rgba(0, 255, 0, 0.25);
export const RED = rgba(255, 0, 0, 0.5);

export const stringOfProduction = ({ lhs, rhs }: Production) =>
  `${lhs} → ${rhs.map((x) => x.name).join(" ")}`;

export const stringOfItem = ({ production: { lhs, rhs }, index: i }: Item) =>
  `${lhs} → ${rhs
    .map((x) => x.name)
    .slice(0, i)
    .join(" ")} ${CURSOR_ICON} ${rhs
    .map((x) => x.name)
    .slice(i)
    .join(" ")}`;

export const focusView = (id: ViewId) =>
  vscode.commands.executeCommand(`${id}.focus`);

export const inputSourcePicks = [
  choose("$(target) Use active editor", InputSourceKind.ActiveEditor),
  choose("$(file-text) Read input from a file", InputSourceKind.File),
  choose("$(pencil) Enter some text", InputSourceKind.TextBox),
];

export const decorationType = vscode.window.createTextEditorDecorationType;

export async function askForInputSource(
  sourceKind?: InputSourceKind,
  textBoxPlaceholder = "Enter some text",
): Promise<
  { sourceKind: InputSourceKind; editor: vscode.TextEditor } | undefined
> {
  let editor: vscode.TextEditor | undefined;

  sourceKind =
    sourceKind ||
    (await setupQuickPick<InputSourceKind>(
      "Select the input source",
      inputSourcePicks,
    ));
  if (!sourceKind) return;

  switch (sourceKind) {
    case InputSourceKind.ActiveEditor:
      editor = vscode.window.activeTextEditor;
      break;
    case InputSourceKind.File:
      const uri = (await vscode.window.showOpenDialog())?.at(0);
      if (!uri) return;

      editor = await vscode.window.showTextDocument(uri, { preview: false });
      break;

    case InputSourceKind.TextBox:
      const content = await vscode.window.showInputBox({
        placeHolder: textBoxPlaceholder,
      });
      if (!content || content === "") return;

      const document = await vscode.workspace.openTextDocument({
        language: "plaintext",
        content,
      });

      editor = await vscode.window.showTextDocument(document);
      break;
  }
  if (!editor) return;

  return { sourceKind, editor };
}

export const proposeUpdate = (cmdId: string) => {
  showWarning(
    `The command '${cmdId}' is not defined. Make sure you have the latest version of ${serverName} installed. If the error persists, contact the extension's developer.`,
    `Update ${serverName}`,
  ).then((install) => {
    if (!install) return;

    let opamInstallCmd = `opam update && opam upgrade ${serverName}`;
    let terminal = vscode.window.createTerminal(serverName);

    terminal.show();
    terminal.sendText(opamInstallCmd);
  });

  return undefined;
};
