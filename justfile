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

bump-menhirlsp version: build
    git tag -a "menhir-lsp.{{ version }}" -m 'beta {{ version }}'
    git push origin "menhir-lsp.{{ version }}"
    gh release create "menhir-lsp.{{ version }}" --generate-notes

bump-menhirformat version: build
    git tag -a "menhirformat.{{ version }}" -m 'beta {{ version }}'
    git push origin {{ version }}
    gh release create {{ version }} --generate-notes

publish-menhirlsp version:
    opam publish menhir-lsp -v {{ version }}

publish-menhirformat version:
    opam publish menhirformat -v {{ version }}

# 1. check if tag arg is valid

# 2. upload the executable
upload-binary tag:
    git tag -l {{ tag }} 
    gh release upload {{ tag }} _build/install/default/bin/menhir-lsp#"menhir-lsp v{{ tag }}, x86-64, for GNU/Linux 3.2.0"

menhir-rand:
    menhir test/calc.mly --infer --random-sentence expr --random-sentence-length 3 --random-self-init

menhir-interpret:
    ledit | menhir test/calc.mly --infer --interpret --interpret-show-cst --trace

menhir-graph:
    menhir --infer --dump test/calc.mly --automaton-graph
    dot -Tpng test/calc.dot > test/calc.png

test-menhirformat:
    for file in `find . -name *.mly`; do echo "Formatting $file"; menhirformat $file; echo "Done formatting $file"; done

# position format: <line>:<number>
test-merlin-completion position file prefix:
    ocamlmerlin single complete-prefix -position {{ position }} -prefix {{ prefix }} -filename {{ file }} < {{ file }} | jq .value

ocamllex-messages-lrgrep:
    lrgrep import-messages vendor/ocamllex/original_parser.messages -o original_parser.lrgrep -g _build/default/vendor/ocamllex/original_parser.cmly 

ocamllex-messages-list:
    menhir vendor/ocamllex/parser.mly --list-errors > vendor/ocamllex/parser.messages 
ocamllex-messages-stats:
    menhir vendor/ocamllex/original_parser.mly --echo-errors vendor/ocamllex/original_parser.messages 

calc-messages-stats:
    menhir ~/menhir/demos/calc-syntax-errors/parser.mly --echo-errors /home/dalpi/menhir/demos/calc-syntax-errors/parserMessages.messages 
