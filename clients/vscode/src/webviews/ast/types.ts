import type { WebviewApi } from "vscode-webview";
import type { Range, Position } from "vscode-languageclient";

export type State = {};

export type Offset = number;

export type RawRange = [Offset, Offset];

export function isInRange(pos: Offset, [start, end]: RawRange): boolean {
  return start <= pos && pos <= end;
}

export interface ASTNode {
  range: Range;
  rawRange: RawRange;
  value: ASTNode[] | string;
}

export type VsCode = WebviewApi<State>;
