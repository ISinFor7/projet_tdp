opam init
opam install ocaml-lsp-server
opam install ocamlformat
cd Proj_GraphDB/
opam install dune
opam install ppx_deriving
opam install ocamlgraph
dune build
opam install menhir
opam install utop
eval $(opam env)
code .
