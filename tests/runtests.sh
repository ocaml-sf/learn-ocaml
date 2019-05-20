#!/bin/bash

# Temporary directory
TMP=$(mktemp -d)

# Where to build the repository
BUILD=$TMP/build

# Build the reporistory
learn-ocaml build --repo ../demo-repository -o $BUILD
if [ $? -ne 0 ]; then
    echo Build failed
    exit 1
fi

# Run the server in background
learn-ocaml serve --contents-dir=$BUILD &

# Wait for the server to be initialized
sleep 5

# Send data to the server
learn-ocaml-client --server http://localhost:8080 --json demo.ml

# Cleanup
rm -rf $TMP

kill $!
