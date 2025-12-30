# Menhir LSP client

VS Code extension providing IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) and [ocamllex](https://ocaml.org/manual/5.4/lexyacc.html). In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp), which can be installed with opam:

```
opam install menhir-lsp
```

Enjoy ease of navigation and solid language support in your `.mll` and `.mly` files!

## Features

* Context-aware completions for your grammars' terminals and nonterminals, Menhir's standard library rules, named regexes, symbol captures etc.
* View References (Shift+F12 on any symbol)
* Jump to Definition (Ctrl-click on any symbol)
* Hover documentation for token aliases and standard library rules
* Document Symbols (Ctrl+Shift+O)
* Rename symbol (F2 on any rule names or token names)
* Refactorings for Menhir tokens
* Fancy file icons

## License

GPL-2.0