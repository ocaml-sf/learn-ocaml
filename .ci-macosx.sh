#!/bin/sh

brew update
brew install opam
opam init -y --compiler=4.06.0
eval $(opam env)

opam install -y . --deps-only
make && make opaminstall
