# Menhir LSP

`menhir-lsp` is a Language Server for the OCaml dialects [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) and [ocamllex](https://ocaml.org/manual/5.4/lexyacc.html). Its goal is to provide rich IntelliSense in `.mly` and `.mll` files in client editors. A [client](client/) for VS Code is available as a [VS Code extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client).

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

For other editors, please refer to their documentation on how to register a client.

### Manual installation

Run these commands if the opam package is down or you want to hack on the server.

```
git clone https://github.com/dalps/menhir-lsp && cd menhir-lsp
opam install .
```

## Development notes

The server's code depends on [Linol](https://github.com/c-cube/linol), an OCaml package that simplifies the development of Language Servers.

It is also based on the source codes of both [Menhir](https://gitlab.inria.fr/fpottier/menhir) and [ocamllex](https://github.com/ocaml/ocaml/tree/trunk/lex), [modified](vendor/) so their parsers stores location annotations in the AST and doesn't exit the server's process when a syntax error occurs.

Issues or PRs for missing features, clients for other editors or any bug / improvement are welcome.

## License

GNU General Public License v2.0 only
