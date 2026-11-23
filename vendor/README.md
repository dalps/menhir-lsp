# Disclaimer

This directory contains parts of the source code of [Menhir](https://gitlab.inria.fr/fpottier/menhir/) and [ocamllex](https://github.com/ocaml/ocaml/tree/trunk/lex). It is structured as follows:

- [lex/](lex/) contains a subset of modules from the ocamllex code base modified to simplify access to the generated DFA for the VS Code lexing UI;
- [ocamllex/](ocamllex/) contains a subset of modules from the ocamllex code base modified for the purpose of LSP analysis;
- [menhir/](menhir/) contains a subset of modules from the Menhir code base modified for the purpose of LSP analysis.

Copyright comments have been kept intact. All rights of the modules herein go to their respective authors.
