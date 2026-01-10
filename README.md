# Menhir LSP

`menhir-lsp` is a Language Server for the OCaml dialects [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) and [Ocamllex](https://ocaml.org/manual/5.4/lexyacc.html). Its goal is to provide rich IntelliSense in `.mly` and `.mll` files in client editors. A [client](client/) for VS Code is available as a [VS Code extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client).

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

The server alone doesn't do much, to do its job it needs a client that talks the LSP running in your editor. If you use VS Code proceed by installing [the Menhir extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client), then open an `.mly` or `.mll` document. The LSP client will launch the server and communicate with it automatically.

For other editors, please refer to their documentation on how to register an LSP client.

### Manual installation

Run these commands should the opam package not work or you want to hack on the server.

```
git clone https://github.com/dalps/menhir-lsp && cd menhir-lsp
opam install .
```

## Development

The server's code depends on [Linol](https://github.com/c-cube/linol), an OCaml package that simplifies the development of Language Servers.

It is also based on the original code of both [Menhir](https://gitlab.inria.fr/fpottier/menhir) and [Ocamllex](https://github.com/ocaml/ocaml/tree/trunk/lex), whose parsers I [modified](/vendor/) in order to annotate their ASTs with source locations and make error recovery possible.

Additional references: [ocaml-lsp](https://github.com/ocaml/ocaml-lsp), [atd-lsp](github.com/ahrefs/atd-lsp), [catala-lsp](https://github.com/CatalaLang/catala-language-server). These were huge help material for implementing common LSP tasks in OCaml.

### Editor support

Currently only VS Code is supported, but not everyone uses VS Code. If you think this is useful and would like to have `menhir-lsp` work in your preferred editor, please consider contributing a new client / extension / adapter through a PR. I'm planning to maintain only the VS Code client since that it is my daily, fairly easy to extend, editor.

Issues for bugs / missing features are welcome.

## License

GNU General Public License v2.0 only