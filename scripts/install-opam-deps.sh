#!/bin/sh

opam list --installed depext || opam install depext
opam pin add --yes --no-action ocp-indent \
     "https://github.com/OCamlPro/ocp-indent.git#master"
opam pin add --yes --no-action ocp-ocamlres \
	   "https://github.com/OCamlPro/ocp-ocamlres.git"
opam pin add --yes --no-action learn-ocaml-deps src
opam install camlp4 --yes
opam depext learn-ocaml-deps
if opam list --installed learn-ocaml-deps
then opam upgrade learn-ocaml-deps ocp-indent ocplib-json-typed ocp-ocamlres
else opam install learn-ocaml-deps
fi
