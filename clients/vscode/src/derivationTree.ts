import * as vscode from "vscode";
import { execServerCmd } from "./extension";
import { TreeProvider } from "./TreeProvider";
import {
  ActionMapping,
  DerivationTree,
  Entry,
  LexerActionKind,
  ModuleEntries,
  Terminal,
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

export class ActionMapProvider extends TreeProvider<ActionMapping | Terminal> {
  getTreeItem(
    element: ActionMapping | Terminal,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    if ("name" in element) {
      const item = new vscode.TreeItem(element.name);
      const uri = vscode.Uri.parse(element.moduleUri);

      item.resourceUri = uri;
      item.collapsibleState = vscode.TreeItemCollapsibleState.None;
      item.command = open(uri, element);
      return item;
    }

    const item = new vscode.TreeItem(
      element.regexp.value.replaceAll(/\n/g, " "),
    );
    const uri = vscode.Uri.parse(element.moduleUri);

    item.collapsibleState = vscode.TreeItemCollapsibleState.None;
    item.command = open(uri, element.regexp);
    item.contextValue = "regexpKey";

    if (!element.value) {
      item.iconPath = themeicon("warning", "list.warningForeground");
      item.tooltip = "Please choose an action for this pattern.";
      item.description = "Uninterpreted.";
      return item;
    }

    switch (element.value.kind) {
      case LexerActionKind.Fail:
        item.iconPath = themeicon("error");
        item.tooltip = "Will halt on this pattern.";
        break;
      case LexerActionKind.Skip:
        item.iconPath = themeicon("skip");
        item.tooltip = "Will skip this pattern.";
        break;
      case LexerActionKind.Produce:
        item.iconPath = themeicon("milestone");
        item.tooltip = `Will emit the token ${element.value.token!.name} on this pattern.`;
        // show the mapped token as the only child
        item.collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
        break;
    }

    return item;
  }

  getChildren(
    element?: ActionMapping | Terminal | undefined,
  ): vscode.ProviderResult<(ActionMapping | Terminal)[]> {
    if (!element) return this.trees;

    if ("value" in element && element.value?.kind === LexerActionKind.Produce) {
      return [element.value.token!];
    }
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
      item.iconPath = themeicon("symbol-method");
      return item;
    }

    const item = new vscode.TreeItem(uri);

    item.collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
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
