import * as vscode from "vscode";
import { execServerCmd } from "./extension";
import { TreeProvider } from "./TreeProvider";
import {
  ActionMapping,
  DerivationTree,
  Entry,
  LexerActionKind,
  ModuleEntries,
} from "./types";
import { openCommand as open, themeicon } from "./utils";

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

export class ActionMapProvider extends TreeProvider<ActionMapping> {
  getTreeItem(
    element: ActionMapping,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const item = new vscode.TreeItem(`${element.regexp.value}`);
    item.collapsibleState = vscode.TreeItemCollapsibleState.None;
    const uri = vscode.Uri.parse(element.moduleUri);
    item.command = open(uri, element.regexp);

    if (!element.value) {
      item.iconPath = themeicon("warning", "list.warningForeground");
      item.description = "Uninterpreted.";
      return item;
    }

    switch (element.value.kind) {
      case LexerActionKind.Fail:
        item.iconPath = themeicon("error");
        item.tooltip = "Halt on this pattern.";
        break;
      case LexerActionKind.Skip:
        item.iconPath = themeicon("skip");
        item.tooltip = "Skip this pattern.";
        break;
      case LexerActionKind.Produce:
        item.iconPath = themeicon("milestone");
        item.tooltip = `Emit the token ${element.value.token!.name} on this pattern.`;
        break;
    }

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
      item.command = open(uri, element);
      item.iconPath = themeicon("symbol-function");
      return item;
    }

    const item = new vscode.TreeItem(
      uri,
      vscode.TreeItemCollapsibleState.Expanded,
    );
    item.contextValue = "module";
    item.iconPath = vscode.ThemeIcon.File;
    item.command = open(uri);
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
