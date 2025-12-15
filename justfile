default: watch

build:
    dune build

watch:
    dune build -w

install: build
    dune install
