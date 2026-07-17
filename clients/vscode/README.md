# Menhir and Ocamllex LSP Client

VS Code extension providing IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) parsers and [Ocamllex](https://ocaml.org/manual/5.4/lexyacc.html) lexers. In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp). Install it with [opam](https://ocaml.org/packages), the OCaml package manager:

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
* Hover types and documentation for token aliases and Menhir's standard library rules
* Document Symbols (Ctrl+Shift+O)
* Rename Symbol (F2 on a rule name or a token name)
* Refactorings for Menhir tokens and Ocamllex regular expressions (Ctrl+.)
* Selection Ranges (Shift+Alt+← / Shift+Alt+→ to respectively shrink / expand your selection along syntactic nodes)
* Document Formatting (Shift+Alt+F) powered by `ocamlformat`
* Fancy file icons
* Syntax highlighting, entries outline and folding ranges for Menhir [.messages files](https://cambium.inria.fr/~fpottier/menhir/manual.html#sec73)

### Code Actions

#### Menhir Refactorings

* **Define and replace with alias**: replace all occurrences of a token `RBRACE` with an alias `"}"`. The action automatically (re)defines the alias at the site of the token declaration.

#### ocamllex Refactorings

* **Extract named regexp**: extract a named regular expression out of a valid regexp selection. The original regexp is replaced with a reference to a new named regular expression placed at the top of your lexer.

### Commands

Open the command palette (F1) and type `Menhir` to view the available commands.

### .messages Features

When the active editor is a `.messages` database, the counts of sentences and error messages is shown in the status bar below the editor. Clicking on it will bring focus the Outline view where one can see a summary of the entries laid out in a indexed list.

Plus, the following commands are available to quickly navigate the entries.
Each command moves focus relatively to the current cursor position:

* `menhir-lsp-client.nextMessage` (Alt+N): Focus the next message
* `menhir-lsp-client.nextDummyMessage` (Alt+Shift+N): Focus the next `<YOUR SYNTAX ERROR MESSAGE HERE>` message
* `menhir-lsp-client.previousMessage` (Alt+P): Focus the previous message
* `menhir-lsp-client.previousDummyMessage` (Alt+Shift+P): Focus the previous `<YOUR SYNTAX ERROR MESSAGE HERE>` message

## License

GPL-2.0
