import * as vscode from "vscode";
import { TreeProvider } from "./TreeProvider";

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
