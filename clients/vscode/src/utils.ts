import * as vscode from "vscode";
import { Position, Range } from "vscode-languageclient";
import { Item, Production, QuickPickItemWithData, WithRange } from "./types";
import { CodiconId } from "./codicons";

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
        v.$ ? `\`${v.$}\`` : v.$$ ? `\n\`\`\`\n${v.$$}\n\`\`\`\n` : v,
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
