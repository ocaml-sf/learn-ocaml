#!/bin/sh

opam list --installed depext || opam install depext
opam pin add --no-action ocp-indent \
     "https://github.com/OCamlPro/ocp-indent.git"
opam pin add --no-action ocplib-json-typed \
	   "https://github.com/OCamlPro/ocplib-json-typed.git"
opam pin add --no-action ocp-ocamlres \
	   "https://github.com/OCamlPro/ocp-ocamlres.git"
opam pin add --yes --no-action tryocaml-fun-deps src
opam depext ${DEPEXTOPT} tryocaml-fun-deps
if opam list --installed tryocaml-fun-deps
then opam upgrade tryocaml-fun-deps ocp-indent ocplib-json-typed ocp-ocamlres
else opam install tryocaml-fun-deps
fi
