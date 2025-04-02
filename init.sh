opam install
opam init
opam install ocaml-lsp-server
opam install ocamlformat
tar xfz proj_graphdb.tgz
cd Proj_GraphDB/
opam init
eval 
opam install dune
opam install ppx_deriving
opam install ocamlgraph
dune build
opam install menhir

opam install utop
code .
