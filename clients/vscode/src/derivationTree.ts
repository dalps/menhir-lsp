import * as vscode from "vscode";

export type DerivationTree =
  | string
  | { nt: string; children: DerivationTree[] };

const isTerminal = (tree: DerivationTree) => typeof tree === "string";

export class DerivationTreeProvider implements vscode.TreeDataProvider<DerivationTree> {
  constructor(private readonly context: vscode.ExtensionContext) {}

  private tree: DerivationTree | undefined = undefined;

  private _onDidChangeTreeData = new vscode.EventEmitter<
    DerivationTree | undefined | void
  >();

  readonly onDidChangeTreeData: vscode.Event<
    void | DerivationTree | DerivationTree[] | null | undefined
  > = this._onDidChangeTreeData.event;

  /**
   * IMPORTANT: if the collapsible state is the default value `None`,
   * the node and therefore its children will not be rendered in the view
   * (`getChildren` not be called on them.)
   */
  getTreeItem(
    element: DerivationTree,
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const isT = isTerminal(element);

    const item = new vscode.TreeItem(isT ? element : element.nt);
    item.collapsibleState = isT
      ? vscode.TreeItemCollapsibleState.None
      : vscode.TreeItemCollapsibleState.Expanded;

    // item.iconPath = isT ? "symbol-constant" : "symbol-function";
    return item;
  }

  /**
   * The first call is always with `undefined` to get the root.
   * Also note that subtrees are computed on-demand (as debugging shows).
   */
  getChildren(
    element?: DerivationTree | undefined,
  ): vscode.ProviderResult<DerivationTree[]> {
    const tree = element || this.tree;

    if (!tree || isTerminal(tree)) return [];

    return tree.children;
  }

  refresh(derivation: DerivationTree) {
    this.tree = derivation;
    this._onDidChangeTreeData.fire();
  }

  getParent?(element: DerivationTree): vscode.ProviderResult<DerivationTree> {
    throw new Error("Method not implemented.");
  }

  resolveTreeItem?(
    item: vscode.TreeItem,
    element: DerivationTree,
    token: vscode.CancellationToken,
  ): vscode.ProviderResult<vscode.TreeItem> {
    throw new Error("Method not implemented.");
  }
}
