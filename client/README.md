# Menhir LSP client

VS Code extension providing IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) grammars and [ocamllex](https://ocaml.org/manual/5.4/lexyacc.html) lexers. In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp), which can be installed with opam:

```
opam update && opam install menhir-lsp
```

The extension activates upon opening a file with the `.mly` or `.mll` extension. Enjoy ease of navigation and solid language support in your grammars!

## Features

* Context-aware completions for your grammar's terminals and nonterminals, Menhir standard library's rules, named regexes, symbol captures etc.
* References View (Shift+F12 on any symbol)
* Jump to Definition (Ctrl+Click on any symbol)
* Hover documentation for token aliases and standard library rules
* Document Symbols (Ctrl+Shift+O)
* Rename symbol (F2 on a rule name or a token name)
* Refactorings for Menhir tokens
* Fancy file icons

## License

GPL-2.0