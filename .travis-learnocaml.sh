#!/usr/bin/env bash


. .travis-ocaml.sh
opam install -y opam-devel
sudo cp /home/travis/.opam/4.05.0/lib/opam-devel/* /usr/local/bin
hash -r
opam install . -y --deps
make PROCESSING_JOBS=1
make opaminstall
