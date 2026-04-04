import type { WebviewApi } from "vscode-webview";
import type { Range, Position } from "vscode-languageclient";

export type State = {};

export type ASTNode = {
  expanded?: boolean;
  range: Range;
  value: ASTNode[] | string;
};

export type VsCode = WebviewApi<State>;
