#!/bin/sh

set -e

sw_vers
system_profiler SPSoftwareDataType
uname -a

brew update
brew install pkg-config
brew install opam
brew install libev
opam init -y -a --bare

opam switch create . ocaml-base-compiler --deps-only --locked -y -j 2 -v
eval $(opam env)
make
make opaminstall

# See src/main/linking_flags.sh
make detect-libs
