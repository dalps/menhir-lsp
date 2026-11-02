import { Range } from "vscode-languageclient";
import { RawRange } from "./webviews/ast/types";
import { Uri } from "vscode";

export type Entry = {
  moduleUri: string; // merely a copy of the parent's moduleUri
  name: string;
  range: Range;
};

export type ModuleEntries = {
  moduleUri: string;
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
