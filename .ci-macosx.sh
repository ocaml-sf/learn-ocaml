#!/bin/sh

brew update
brew install opam
opam init -y
opam install -y . --deps-only
make && make opaminstall
