#!/bin/sh

brew update
brew install opam
brew install libev
opam init -y --compiler=4.05.0
eval $(opam env)

opam install -y . --deps-only --locked
make && make opaminstall
