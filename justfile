default: watch

build:
    dune build

watch:
    dune build @ocaml-index -w

install: build
    dune install

menhir-rand:
    menhir test/calc.mly --infer --random-sentence expr --random-sentence-length 3 --random-self-init

menhir-interpret:
    ledit | menhir test/calc.mly --infer --interpret --interpret-show-cst --trace
