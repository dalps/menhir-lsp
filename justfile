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
