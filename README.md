# Menhir LSP

`menhir-lsp` is a Language Server for the OCaml dialects [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) and [Ocamllex](https://ocaml.org/manual/5.4/lexyacc.html). Its goal is to provide first-class language support for their syntaxes in client editors. A [client](client/) for VS Code is available through the [Menhir VS Code extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client).

## Implemented Features

The server supports the following set of LSP feautres in `.mll` and `.mly` files:
* [Find References](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references) 
* [Jump to Definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition)
* [Hover](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover)
* [Document Symbols](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol)
* [Completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion)
* [Code Actions](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction)
* [Rename](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename)
* [Selection Range](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_selectionRange)
* [Document Formatting](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting)

Moreover, `menhir-lsp` provides document symbols and folding ranges in Menhir `.messages` databases that store syntax error messages of your parsers. More info in the [extension's README](/clients/vscode/README.md#.messages).

## Installation & Usage

Install the server through opam:

```
opam install menhir-lsp
```

The server alone doesn't do much, to do its job it needs a client that speaks the Language Server Protocol running in your editor. If you use VS Code proceed by installing [the Menhir extension](https://marketplace.visualstudio.com/items?itemName=dalps.menhir-lsp-client), then open a document with the `.mly` or `.mll` file extension. The LSP client will launch the server and communicate with it automatically.

Many features are available out of the box in any workspace, but to get the most out of `menhir-lsp` - project-wide hover types, references, completions, etc. - make sure your workspace is a [dune project](https://dune.readthedocs.io/en/stable/overview.html#project-layout) and you declare your parser and lexer modules in the [`menhir`](https://dune.readthedocs.io/en/stable/reference/dune/menhir.html#menhir) and [`ocamllex`](https://dune.readthedocs.io/en/stable/reference/dune/ocamllex.html) stanzas of your libraries respectively.

For integration with other editors, please refer to their documentation on how to register a LSP client.

### Manual installation

Run these commands if you encounter a problem with the opam package or you'd like to hack on the server:

```
git clone https://github.com/dalps/menhir-lsp.git && cd menhir-lsp
dune install
```

You can also download a copy of the binary from the [lateset release](https://github.com/dalps/menhir-lsp/releases/latest) on GitHub. Put it under a folder that's under your `PATH` and the client should be able to run it just as well.

## Development

The server's code lives in the [`bin`](bin/) directory. It relies on [Linol](https://github.com/c-cube/linol), an OCaml package that simplifies the development of Language Servers, and the [visitors](https://ocaml.org/p/visitors/latest) OCaml syntax plugin, for automating AST traversals.

It is also based on the original codebases of both [Menhir](https://gitlab.inria.fr/fpottier/menhir) and [Ocamllex](https://github.com/ocaml/ocaml/tree/trunk/lex), whose parsers I [modified](/vendor/) in order to annotate their ASTs with source locations and make error recovery possible.

Additional references: [ocaml-lsp](https://github.com/ocaml/ocaml-lsp), [atd-lsp](github.com/ahrefs/atd-lsp), [catala-lsp](https://github.com/CatalaLang/catala-language-server). These were huge help material for implementing common LSP tasks in OCaml.

### Editor support

Currently only VS Code is supported, but not everyone uses VS Code. If you think this is useful and would like to have `menhir-lsp` work in your preferred editor, please consider contributing a new client / extension / adapter through a PR. I'm planning to maintain only the VS Code client since that it is my daily editor.

Issues for bugs / missing features are welcome.

## License

GNU General Public License v2.0 only