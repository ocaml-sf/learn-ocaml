#!/usr/bin/env bash


. .travis-ocaml.sh
opam install -y opam-devel
opam install . -y --deps
make PROCESSING_JOBS=1
make opaminstall
