# Menhir and Ocamllex LSP Client

This VS Code extension provides IntelliSense for [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/) grammars and [ocamllex](https://ocaml.org/manual/5.4/lexyacc.html) lexers. In order to work, it needs the Menhir language server [menhir-lsp](https://github.com/dalps/menhir-lsp). Install it with [opam](https://ocaml.org/packages), the OCaml package manager:

```
opam install menhir-lsp
```

or download the binary from the [latest release](https://github.com/dalps/menhir-lsp/releases/latest) and add it to your PATH.

The extension activates and starts up the server upon opening a file with the `.mly` or `.mll` extension.

**Important:** this extension does not provide syntax highlighting for the two languages. The [OCaml Platform extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform) already takes care of that; make sure you have it installed before using this. The Menhir extension aims to complement the official one by providing completions and code browsing features in ocamllex and Menhir source code.

Enjoy ease of navigation and *rock*-solid language support in your grammars!

## Features

### Language Features

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

* **Extract named regexp**: extract a named regular expression out of a valid regexp selection. The original regexp is replaced with a reference to a new named regular expression placed at the top of your rules.

### Parsing and Lexing UI (experimental)

You can explore the behavior of your lexer and parser entrypoints interactively in two distinct modes. They can be launched either by clicking the play buttons in the Entrypoint Explorer view in the sidebar or the code lenses placed above each entrypoint of an .mll or .mly file opened in an editor.

The Lexing UI tokenizes an editor's text with a selected lexer rule and highlights the computed tokens. It updates automatically as you edit the editor's contents.

The Parsing UI computes lets you step on the derivation of a conrete syntax tree of an editor's contents for a selected grammar start symbol and a lexer rule that will scan the input. It requires that your parser be compiled to the CMLY format; you can ask dune to produce this format by adding the `--cmly` flag in the [menhir stanza](https://dune.readthedocs.io/en/stable/reference/dune/menhir.html) where your parser is declared, for example:

```
(menhir
 (modules calc_parser)
 (flags --cmly))
```

#### Lexer action mapping

The Parsing UI has no knowledge of what lexer actions do and it does not run any OCaml code, so before scanning any text it needs to know how to interpret each action in terms of the selected grammar's token set. To help establish this model of lexer semantics the server guesses an early map that is presented in the Regexp-Token Mapping view. You should make sure it's correct before taking any step on the input and tweak the bad bindings with the edit buttons. For each branch of the lexer rule you can choose among three actions:

- **Skip**: read the next token
- **Fail**: halt the lexer
- **Produce** a terminal of the grammar

Once you're satisfied with the lexer model, head over to the Derivation Explorer view and use the arrow buttons at the top to step on the current input. The larger arrows pause when a breakpoint is reached, otherwise when no breakpoint is set they jump to the beginning or end of the derivation.

#### Grammar breakpoints

Breakpoints can be set by decorating the grammar's source code with the `[@break]` attribute in the following places:

- on a token declaration (pause when the token is shifted);

```ocaml.menhir
%token <int> INT [@break]
```

- on a rule (pause when any of its productions is reduced);

```ocaml.menhir
expr [@break]:
| ...
```
- on a production (pause when it is reduced).

```ocaml.menhir
expr:
| ...
| e1 = expr TIMES e2 = expr { e1 * e2 } [@break]
| ...
```

### .messages Features

When the active editor is a `.messages` database, the counts of sentences and error messages is shown in the status bar below the editor. Clicking on it will focus the Outline view where you'll see a summary of the entries laid out in a indexed list.

Plus, the following commands are available to quickly navigate the entries.
Each command moves focus relatively to the current cursor position:

* `menhir-lsp-client.nextMessage` (Alt+N): Focus the next message
* `menhir-lsp-client.nextDummyMessage` (Alt+Shift+N): Focus the next `<YOUR SYNTAX ERROR MESSAGE HERE>` message
* `menhir-lsp-client.previousMessage` (Alt+P): Focus the previous message
* `menhir-lsp-client.previousDummyMessage` (Alt+Shift+P): Focus the previous `<YOUR SYNTAX ERROR MESSAGE HERE>` message

### Miscellanea

Open the command palette (F1) and type `Menhir` to view other available utilities.

## License

GPL-2.0
