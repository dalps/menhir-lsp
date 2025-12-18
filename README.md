# LSP implementation for Menhir grammars

`menhir-lsp` is a Language Server for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/), an LR(1) parser generator developed by Inria. The server offers basic IntelliSense features for Menhir grammars (files with the `.mly` extensions) requested by a client hosted in a text editor. A [client for VS Code](client/) is conveniently provided as a VS Code extension.

## Features

Requests handled by the server:

* [Find References](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references) - with token alias resolution
* [Goto Definition Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition)
* [Hover](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover)
* [Document Symbols](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol)
* [Completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion) for terminals, nonterminals and standard library rules.

## Usage

Installing this server is a simple as:

```
opam install menhir-lsp
```

Then, in order to get IntelliSense for your grammars in VS Code, you need to install the client, available in the VS Code Marketplace. The client will run and communicate with the server automatically.

## Development & contributing

The server code is based on the [Linol Lwt template](https://github.com/c-cube/linol).

It also reuses the [Menhir grammar that parses Menhir itself](https://gitlab.inria.fr/fpottier/menhir/-/tree/master/driver/stage2?ref_type=heads) to get an AST representation of the document annotated with locations. I've tried working with the [Menhir SDK](https://ocaml.org/p/menhirSdk/latest), but the cmly representation of the grammar doesn't have location annotations for the various symbols, which are essential in implementign most LSP features, so vendoring the original code is the best I could come up with.

Please open an issue or a PR for missing features, ideas for improvement, other editor support, general bugs or any other matter.

## License

GNU General Public License v2.0 only
