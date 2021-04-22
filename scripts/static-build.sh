#!/usr/bin/env bash
set -ue

LC_ALL=C

cd $(dirname "$0")/..

# Copy git tree without `.git`

## Static linking configuration ##

# The linked C libraries list may need updating on changes to the dependencies.
#
# To get the correct list for manual linking, the simplest way is to set the
# flags to `-verbose`, while on the normal `autolink` mode, then extract them
# from the gcc command-line.

COMMON_LIBS="camlstr base_stubs ssl_threads_stubs ssl crypto cstruct_stubs lwt_unix_stubs unix c"
# `m` and `pthreads` are built-in musl

static_link() {
  local LIBS="$* $COMMON_LIBS"
  echo '(-noautolink'
  echo ' -cclib -Wl,-Bstatic'
  echo ' -cclib -static-libgcc'
  for l in $LIBS; do
      echo " -cclib -l$l"
  done
  echo ')'
}

trap "rm -f src/main/linking_*.sexp" EXIT

static_link >src/main/linking_main.sexp laolao_stubs threads cstruct_stubs bigarray camlrun

static_link >src/main/linking_client.sexp threads bigarray camlrun

static_link >src/main/linking_server.sexp laolao_stubs threadsnat bigarray

## Run build in container

tar c $(git ls-files) src/main/linking_*.sexp | \
  docker run --rm -i \
    ocamlpro/ocaml:4.05 \
    sh -uexc \
      'tar x >&2 && opam switch create . ocaml-system "dune<2" --deps-only >&2 && opam exec make >&2 && tar c -hC _build/install/default/bin .' | \
  tar vx
