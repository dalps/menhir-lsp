# Menhir and Ocamllex LSP client

VS Code extension providing IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) grammars and [Ocamllex](https://ocaml.org/manual/5.4/lexyacc.html) lexers. In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp). Install it with [opam](https://ocaml.org/packages), the OCaml package manager:

```
opam update && opam install menhir-lsp
```

or download the binary from the [latest release](https://github.com/dalps/menhir-lsp/releases/latest) and add it to your PATH.

The extension will activate and launch the server upon opening a file ending with `.mly` or `.mll`. Enjoy ease of navigation and solid language support in your grammars!

## Features

* Context-aware completions for your grammar's terminals and nonterminals, Menhir's standard library rules, named regexes, captured variables, OCaml modules and constants, etc.
* References View (Shift+F12 on any symbol)
* Jump to Definition (Ctrl+Click on any symbol)
* Hover documentation for token aliases and Menhir's standard library rules
* Document Symbols (Ctrl+Shift+O)
* Rename symbol (F2 on a rule name or a token name)
* Refactorings for Menhir tokens and Ocamllex regular expressions (Cltr+.)
* Selection ranges (Shift+Alt+← / Shift+Alt+→ to respectively shrink / expand your selection on a regular expression)
* Fancy file icons

## License

GPL-2.0