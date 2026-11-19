import { Range } from "vscode-languageclient";
import { RawRange } from "./webviews/ast/types";
import * as vscode from "vscode";

export enum EntryKind {
  Mll = "lexer",
  Mly = "parser",
}

// Used to represent either lexer and parser entrypoints.
export type Entry = WithKind<EntryKind> &
  WithRange &
  // carries a copy of the parent's moduleUri. todo: don't repeat moduleUri
  WithModuleUri & {
    name: string;
  };

export type ModuleEntries = WithKind<EntryKind> &
  WithModuleUri & {
    entrypoints: Entry[];
  };

export enum InputSourceKind {
  ActiveEditor = 1, // Start from 1 so any value can't be falsy
  File,
  TextBox,
}

export type Token = WithRange & {
  rawRange: RawRange;
  text: string;
  action: number;
};

type TerminalWithToken = {
  terminal: string;
  token: Token;
};

export type DerivationTree =
  | MaybeLocated<TerminalWithToken>
  | { nt: MaybeLocated<string>; children: DerivationTree[] };

export type ActionMapping = // todo: don't repeat moduleUri
  WithModuleUri & {
    id: number;
    regexp: Located<string>;
    action: Maybe<LexerAction>;
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

export type MaybeLocated<T> = Partial<WithRange & WithModuleUri> & {
  value: T;
};

export enum LexerActionKind {
  Skip = "skip",
  Fail = "fail",
  Produce = "token",
}

export type Terminal = WithModuleUri &
  WithRange & {
    name: string;
    id: number;
  };

export type WithData<T> = { data: T };

export type QuickPickItemWithData<T> = Partial<WithData<T>> &
  vscode.QuickPickItem;

export type WithKind<K> = { kind: K };

export type LexerAction =
  | Tag<LexerActionKind.Skip>
  | Tag<LexerActionKind.Fail>
  | Variant<LexerActionKind.Produce, Terminal>;

export type Tag<tag extends string> = WithKind<tag>;
export type Variant<tag extends string, A> = WithKind<tag> & { value: A };

export type Result<T, E> = Variant<"ok", T> | Variant<"error", E>;

export type Production = { lhs: string; rhs: Symbol[] };

export type Symbol = WithKind<"t" | "nt"> & { name: string };

export type StackItem = Located<{
  symbol: Symbol;
  state: number;
}>;

export type Stack = StackItem[];

export type Lexbuf = { startOfs: number; currOfs: number };

export type Item = {
  index: number;
  production: Production;
};

export type Env = {
  time: number;
  stack: Stack;
  state: number;
  derivation: DerivationTree[];
  lookahead: Maybe<TerminalWithToken>;
  items: Item[];
  incoming: Maybe<Symbol>;
};

export type WithEnv = { env: Env };

export type Checkpoint =
  | Variant<"inputNeeded", WithEnv>
  | Variant<"lexerError", WithEnv & { message: string }>
  | Variant<"shifting", WithEnv & { terminal: string }>
  | Variant<"reducing", WithEnv & { production: Located<Production> }>
  | Variant<"accepted", WithEnv & DerivationTree>
  | Variant<"rejected", WithEnv & { symbol: string }>;
