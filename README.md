# menhir-lsp

`menhir-lsp` is a Language Server for the OCaml dialects [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/), [ocamlyacc](https://ocaml.org/manual/5.4/lexyacc.html) and [Ocamllex](https://ocaml.org/manual/5.4/lexyacc.html). Its goal is to provide rich IntelliSense features in .mll and .mly files. A [client](client/) for VS Code is available in a [VS Code extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client).

## Capabilities

* [Find References](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references) 
* [Jump to Definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition)
* [Hover](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover)
* [Document Symbols](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol)
* [Completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion)
* [Code Actions](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction)
* [Rename](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename)

## Installation & Usage

Installing the server is a simple as:

```
opam install menhir-lsp
```

The server alone doesn't do much, to do its job it needs a client that speaks the LSP running in your editor. On VS Code, proceed by installing [the extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client). In case of other editors, register a new client for them. Once started, the LSP client will spawn the server and communicate with it automatically.

### Manual installation

Run these commands if the opam package is not working or you want to hack on the server.

```
git clone https://github.com/dalps/menhir-lsp && cd menhir-lsp
opam install .
```

## Development notes

The server code is based on the [Linol Lwt template](https://github.com/c-cube/linol).

It also reuses the [Menhir grammar that parses Menhir itself](https://gitlab.inria.fr/fpottier/menhir/-/tree/master/driver/stage2?ref_type=heads) to get the AST representation of the document annotated with locations. Unfortunately the AST offered by the [Menhir SDK](https://ocaml.org/p/menhirSdk/latest) lacks location annotations for various kinds of symbols, so I had to resorted to tweaking the original parser to get things working.

Issues or PRs for missing features, editor support or any improvement are welcome.

## License

GNU General Public License v2.0 only
