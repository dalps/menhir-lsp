# Menhir LSP client

VS Code extension providing IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) grammars. In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp), which can be installed with opam:

```
opam install menhir-lsp
```

Then language support will be available on all your `.mly` files.

## Features

* Auto-completion for your grammar's terminals and rules, plus Menhir's standard library rules
* References View (Shift+F12 on any symbol)
* Jump to definition (Ctrl-click on any symbol)
* Hover info for token aliases and standard library rules
* Document Symbols (Ctrl+Shift+O)

## License

GPL-2.0