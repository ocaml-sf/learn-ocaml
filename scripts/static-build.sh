#!/usr/bin/env bash
set -ue

LC_ALL=C

cd $(dirname "$0")/..

## Run build in container

set -o pipefail
git ls-files -z | xargs -0 tar c | \
  docker run --rm -i \
    ocamlpro/ocaml:5.1 \
    sh -uexc \
      'tar x >&2 &&
       sudo apk add openssl-libs-static bash >&2 &&
       opam switch create . ocaml-system --deps-only --locked >&2 &&
       opam exec make LINKING_MODE=static >&2 &&
       tar c -hC _build/install/default/bin .' | \
  tar vx
