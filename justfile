default: watch

build:
    dune build

watch:
    dune build @ocaml-index -w

install: build
    dune install

run-dap:
    dune exec menhir-dap

install-dap: build
    dune install menhir-dap

# 1. check if tag arg is valid

# 2. upload the executable
upload-binary tag: install
    git tag -l {{ tag }} 
    gh release upload {{ tag }} _build/install/default/bin/menhir-lsp#"menhir-lsp v{{ tag }}, {{ arch() }}, {{ os() }}"

# shell( 'file _build/default/bin/main.exe | cut -d "," -f 2,7' )

menhir-rand grammar rule len="3":
    menhir {{ grammar }} --infer --random-sentence {{ rule }} --random-sentence-length {{ len }} --random-self-init

menhir-interpret grammar:
    ledit | menhir {{ grammar }} --infer --interpret --interpret-show-cst --trace

menhir-graph grammar:
    menhir --infer --dump {{ grammar }} --automaton-graph
    dot -Tpng {{ without_extension(grammar) }}.dot > {{ without_extension(grammar) }}.png
    rm -f {{ without_extension(grammar) }}.ml {{ without_extension(grammar) }}.mli
