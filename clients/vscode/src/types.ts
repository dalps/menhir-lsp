import { Range } from "vscode-languageclient";
import { RawRange } from "./webviews/ast/types";
import { Uri } from "vscode";

export type Entry = { name: string };

export type ModuleEntries = {
  moduleUri: Uri;
  entrypoints: Entry[];
};

export enum InputSourceKind {
  ActiveEditor,
  File,
  TextBox,
}
export type InputSource = { value: InputSourceKind };
export interface Token {
  range: Range;
  rawRange: RawRange;
  text: string;
  action: number;
}
