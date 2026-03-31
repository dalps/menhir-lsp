# Menhir and Ocamllex LSP client

VS Code extension providing IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) grammars and [Ocamllex](https://ocaml.org/manual/5.4/lexyacc.html) lexers. In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp). Install it with [opam](https://ocaml.org/packages), the OCaml package manager:

```
opam install menhir-lsp
```

or download the binary from the [latest release](https://github.com/dalps/menhir-lsp/releases/latest) and add it to your PATH.

The extension will activate and launch the server upon opening a file ending with `.mly` or `.mll`.

**Important:** This extension does not provide syntax highlighting for the two languages. The [OCaml Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform) extension already takes care of that; make sure you have it installed before using this. The Menhir extension aims to complement the official one by providing completions and code browsing features in Ocamllex and Menhir specifications.

Enjoy ease of navigation and *rock*-solid language support in your grammars!

## Features

* Context-aware completions for your grammar's terminals and nonterminals, Menhir's standard library rules, named regexes, captured variables, OCaml modules and constants, etc.
* References View (Shift+F12 on any symbol)
* Jump to Definition (Ctrl+Click on any symbol)
* Hover documentation for token aliases and Menhir's standard library rules
* Document Symbols (Ctrl+Shift+O)
* Rename Symbol (F2 on a rule name or a token name)
* Refactorings for Menhir tokens and Ocamllex regular expressions (Cltr+.)
* Selection Ranges (Shift+Alt+← / Shift+Alt+→ to respectively shrink / expand your selection on a regular expression)
* Document Formatting (Shift+Alt+F)
* Fancy file icons

## License

GPL-2.0