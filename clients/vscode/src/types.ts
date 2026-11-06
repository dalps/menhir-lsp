import { Range } from "vscode-languageclient";
import { RawRange } from "./webviews/ast/types";

export type Entry = WithRange &
  // carries a copy of the parent's moduleUri. todo: don't repeat moduleUri
  WithModuleUri & {
    name: string;
  };

export type ModuleEntries = WithModuleUri & {
  entrypoints: Entry[];
};

export enum InputSourceKind {
  ActiveEditor,
  File,
  TextBox,
}

export type InputSource = { value: InputSourceKind };

export type Token = WithRange & {
  rawRange: RawRange;
  text: string;
  action: number;
};

export type DerivationTree =
  | string
  | { nt: string; children: DerivationTree[] };

export type ActionMapping = // todo: don't repeat moduleUri
  WithModuleUri & {
    action: number;
    regexp: Located<string>;
    value: Maybe<LexerAction>;
  };

export interface WithRange {
  range: Range;
}

export interface WithModuleUri {
  moduleUri: string;
}

export type Located<T> = WithRange & {
  value: T;
};

export type Maybe<T> = T | null;

export enum LexerActionKind {
  Skip = "skip",
  Fail = "fail",
  Produce = "token",
}

export interface LexerAction {
  kind: LexerActionKind;
  token?: {
    name: string;
    id: number;
  };
}
