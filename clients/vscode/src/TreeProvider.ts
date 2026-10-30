import * as vscode from "vscode";

export abstract class TreeProvider<T> implements vscode.TreeDataProvider<T> {
  constructor(protected readonly context: vscode.ExtensionContext) {}

  protected trees: T[] | undefined = undefined;

  private _onDidChangeTreeData = new vscode.EventEmitter<
    T | undefined | void
  >();

  readonly onDidChangeTreeData: vscode.Event<
    void | T | T[] | null | undefined
  > = this._onDidChangeTreeData.event;

  /**
   * IMPORTANT: if the collapsible state is the default value `None`,
   * the node and therefore its children will not be rendered in the view
   * (`getChildren` not be called on them.)
   */
  abstract getTreeItem(element: T): vscode.TreeItem | Thenable<vscode.TreeItem>;

  // abstract isTerminal(element: T): boolean;
  /**
   * The first call is always with `undefined` to get the root.
   * Also note that subtrees are computed on-demand (as debugging shows).
   */
  getChildren(element?: T | undefined): vscode.ProviderResult<T[]> {
    return this.trees;
  }

  refresh(newData: T[]) {
    this.trees = newData;
    this._onDidChangeTreeData.fire();
  }
}
