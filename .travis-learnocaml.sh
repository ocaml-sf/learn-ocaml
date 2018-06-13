#!/usr/bin/env bash

bash -c '. .travis-ocaml.sh && make build-deps && make PROCESSING_JOBS=1 && make opaminstall'
