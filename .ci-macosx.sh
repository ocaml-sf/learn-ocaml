#!/bin/sh

brew update
brew install opam
opam init
opam install . --deps-only
make && make opaminstall
