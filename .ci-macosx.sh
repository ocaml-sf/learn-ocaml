#!/bin/sh

set -e

brew update
brew install pkg-config
brew install opam
brew install libev
opam init -y --compiler=4.05.0
eval $(opam env)

opam install -y -j 2 . --deps-only --locked
make && make opaminstall

# See src/main/linking_flags.sh
make detect-libs
