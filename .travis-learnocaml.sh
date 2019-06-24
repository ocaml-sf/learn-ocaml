#!/usr/bin/env bash

fold_start () { printf 'travis_fold:start:%s\r\033[33;1m%s\033[0m\n' "$1" "$2"; }
fold_end () { printf 'travis_fold:end:%s\r' "$1"; }

fold_start prepare-ocaml-environment "Preparing OCaml environment"
. .travis-ocaml.sh
fold_end prepare-ocaml-environment

fold_start check-build-system "Checking LearnOCaml build system"
# sudo add-apt-repository -y ppa:ansible/bubblewrap
# sudo apt-get -y update
# sudo apt-get install -y bubblewrap
# opam install -y opam-devel
# sudo cp /home/travis/.opam/ocaml-base-compiler.4.05.0/lib/opam-devel/* /usr/local/bin
# hash -r
opam upgrade -y || true
opam install . -y --deps --locked
opam install -y opam-installer
make PROCESSING_JOBS=1
make opaminstall
fold_end check-build-system
