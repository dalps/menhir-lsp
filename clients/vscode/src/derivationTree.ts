import * as vscode from "vscode";
import { TreeProvider } from "./TreeProvider";
import { Entry, ModuleEntries } from "./types";
import { execServerCmd } from "./extension";
import { liftRange } from "./utils";

export type DerivationTree =
  | string
  | { nt: string; children: DerivationTree[] };

const isTerminal = (tree: DerivationTree) => typeof tree === "string";

export class DerivationTreeProvider extends TreeProvider<DerivationTree> {
  getTreeItem(
    element: DerivationTree,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const isT = isTerminal(element);

    const item = new vscode.TreeItem(isT ? element : element.nt);
    item.collapsibleState = isT
      ? vscode.TreeItemCollapsibleState.None
      : vscode.TreeItemCollapsibleState.Expanded;

    return item;
  }

  override getChildren(
    element?: DerivationTree | undefined,
  ): vscode.ProviderResult<DerivationTree[]> {
    if (!element) return this.trees;

    const tree = element;

    return isTerminal(tree) ? [] : tree.children;
  }
}

export type ActionMapping = { action: number; token: string };

export class ActionMapProvider extends TreeProvider<ActionMapping> {
  getTreeItem(
    element: ActionMapping,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const item = new vscode.TreeItem(`${element.action} -> ${element.token}`);
    item.collapsibleState = vscode.TreeItemCollapsibleState.None;

    return item;
  }
}

export class FileProvider extends TreeProvider<vscode.Uri> {
  constructor(
    context: vscode.ExtensionContext,
    private pattern: string,
  ) {
    super(context);
  }

  getTreeItem(
    element: vscode.Uri,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const item = new vscode.TreeItem(element);

    return item;
  }

  getChildren(
    element?: vscode.Uri | undefined,
  ): vscode.ProviderResult<vscode.Uri[]> {
    return vscode.workspace.findFiles(this.pattern);
  }
}

export class EntrypointProvider extends TreeProvider<Entry | ModuleEntries> {
  constructor(
    context: vscode.ExtensionContext,
    public command: string,
  ) {
    super(context);
  }
  getTreeItem(
    element: Entry | ModuleEntries,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const uri = vscode.Uri.parse(element.moduleUri);

    if ("name" in element) {
      const item = new vscode.TreeItem(element.name);
      item.contextValue = "entrypoint";
      item.iconPath = new vscode.ThemeIcon("symbol-function");
      const selection = liftRange(element.range);
      item.command = {
        command: "vscode.open",
        title: "Focus in editor",
        arguments: [uri, { selection } as vscode.TextDocumentShowOptions],
      };
      return item;
    }

    const item = new vscode.TreeItem(
      uri,
      vscode.TreeItemCollapsibleState.Expanded,
    );
    item.iconPath = vscode.ThemeIcon.File;
    item.command = { command: "vscode.open", title: "Open", arguments: [uri] };
    return item;
  }

  getChildren(
    element?: Entry | ModuleEntries | undefined,
  ): vscode.ProviderResult<(Entry | ModuleEntries)[]> {
    if (!element) {
      return execServerCmd(this.command) as Promise<ModuleEntries[]>;
    }

    return "name" in element ? [] : element.entrypoints;
  }
}
