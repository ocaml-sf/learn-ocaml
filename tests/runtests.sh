#!/bin/bash

# Temporary directory
TMP=$(mktemp -d)

cp -r ../demo-repository $TMP/test-repo

# Build the reporistory
pushd $TMP
learn-ocaml build --repo test-repo
if [ $? -ne 0 ]; then
    echo Build failed
    exit 1
fi

# Run the server in background
learn-ocaml serve &
popd

# Wait for the server to be initialized
sleep 2

# Get the token
TOKEN=$(find $TMP/sync -name \*.json -printf '%P' | sed 's|/|-|g' | sed 's|-save.json||')

# Send data to the server
learn-ocaml-client --server http://localhost:8080 --token "$TOKEN" --json demo.ml

# Cleanup
rm -rf $TMP

kill $!
