default: watch

build:
    dune build

watch:
    dune build @ocaml-index -w

test:
    dune test -w

install: build
    dune install

format file: install
    clear
    ./_build/install/default/bin/menhirformat {{ file }}

publish:
    opam publish .

bump-lsp version: build
    git tag -a 'menhir-lsp.{{ version }}' -m 'menhir-lsp beta version {{ version }}'
    git push origin 'menhir-lsp.{{ version }}'
    gh release create 'menhir-lsp.{{ version }}' -t 'menhir-lsp.{{ version }}' --generate-notes

bump-fmt version: build
    git tag -a 'menhirformat.{{ version }}' -m 'menhirformat beta version {{ version }}'
    git push origin 'menhirformat.{{ version }}'
    gh release create 'menhirformat.{{ version }}' -t 'menhirformat.{{ version }}' --generate-notes

# 1. check if tag arg is valid
# 2. upload the executable
upload-lsp-binary tag:
    git tag -l 'menhir-lsp.{{ tag }}'
    gh release upload 'menhir-lsp.{{ tag }}' _build/install/default/bin/menhir-lsp#"menhir-lsp v{{ tag }}, x86-64, for GNU/Linux 3.2.0"

upload-fmt-binary tag:
    git tag -l 'menhirformat.{{ tag }}'
    gh release upload 'menhirformat.{{ tag }}' _build/install/default/bin/menhirformat#"menhirformat v{{ tag }}, x86-64, for GNU/Linux 3.2.0"

download-ocamllex-grammar:
    [ -d tmp ] || mkdir tmp
    wget https://raw.githubusercontent.com/ocaml/ocaml/refs/heads/trunk/lex/parser.mly -O tmp/lex_parser.mly
    menhir tmp/lex_parser.mly --only-preprocess-u > vendor/ocamllex/original_parser.mly
    rm -rf tmp

download-menhir-grammar:
    [ -d tmp ] || mkdir tmp
    wget https://gitlab.inria.fr/fpottier/menhir/-/raw/master/driver/stage2/Parser.mly -O tmp/Parser.mly
    menhir tmp/Parser.mly --only-preprocess-u > vendor/menhir/OriginalParser.mly
    rm -rf tmp

download-menhir-stdlib:
    wget https://gitlab.inria.fr/fpottier/menhir/-/raw/master/front/standard.mly

download-menhir-messages:
    wget gitlab.inria.fr/fpottier/menhir/-/raw/master/driver/stage2/ParserMessages.messages -O vendor/menhir/ParserMessages.messages
