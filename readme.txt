opam init
opam install ocaml-lsp-server
opam install ocamlformat
code .

VScode OCaml Platform
ouvrir la Command Palette (Ctrl-Shift-P sous Linux), sélectionner Ocaml Open REPL

tar xfz proj_graphdb.tgz
cd Proj_GraphDB/
opam init
eval $(opam env)
opam install dune
opam install ppx_deriving
opam install ocamlgraph
dune build
opam install menhir
dune exec Proj_GraphDB f test/tiny.q
opam install utop
