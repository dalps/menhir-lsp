import * as vscode from "vscode";
import { TreeItem, TreeItemCollapsibleState, Uri } from "vscode";
import { execServerCmd } from "./extension";
import { TreeProvider } from "./TreeProvider";
import {
  ActionMapping,
  DerivationTree,
  Entry,
  LexerActionKind,
  ModuleEntries,
  Terminal,
  WithRange,
} from "./types";
import { md, openCommand as open, stringOfRange, themeicon } from "./utils";

const isNonterminal = (tree: DerivationTree) => "nt" in tree;

export class DerivationTreeProvider extends TreeProvider<DerivationTree> {
  getTreeItem(element: DerivationTree): TreeItem | Thenable<TreeItem> {
    const isT = !isNonterminal(element);

    const data = isT ? element : element.nt;
    const { moduleUri, range } = data;
    const item = new TreeItem(isT ? element.value.terminal : element.nt.value);

    item.iconPath = themeicon(isT ? "symbol-constant" : "symbol-method");
    let hint = "\n\n---\n\n*Click to highlight in active editor*";
    item.tooltip = isT
      ? md`Terminal ${element.value.terminal} · Text ${{ $: element.value.token.text }}${hint}`
      : md`Nonterminal ${element.nt.value}${hint}`;

    item.collapsibleState = isT
      ? TreeItemCollapsibleState.None
      : TreeItemCollapsibleState.Expanded;

    if (moduleUri && range) {
      item.description = stringOfRange(range);
      item.command = open(Uri.parse(moduleUri), data as WithRange);
    }

    return item;
  }

  override getChildren(
    element?: DerivationTree | undefined,
  ): vscode.ProviderResult<DerivationTree[]> {
    if (!element) return this.trees;

    const tree = element;

    return isNonterminal(tree) ? tree.children : [];
  }
}

export class ActionMapProvider extends TreeProvider<ActionMapping | Terminal> {
  public parserEntrypoint: Entry | undefined = undefined;
  public lexerEntrypoint: Entry | undefined = undefined;

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

  // refresh(newData: (ActionMapping | Terminal)[], p: Entry, l: Entry): void {
  //   super.refresh(newData);

  //   this.parserEntrypoint = p;
  //   this.lexerEntrypoint = l;
  // }
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
