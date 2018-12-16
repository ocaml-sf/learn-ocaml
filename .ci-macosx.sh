#!/bin/sh

brew update
brew install opam
opam --version
opam install . --deps-only
make && make opaminstall
