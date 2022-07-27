#!/usr/bin/env bash

# This script updates 'docs/index.md' from 'README.md',
# and changes github.com/…/learn-ocaml/…/docs URLs to relative paths,
# so that github.io pages work independently of github.com src files.

srcdir=$(cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd)

readme=$(readlink -f "$srcdir/../README.md")
index=$(readlink -f "$srcdir/../docs/index.md")

if [[ ! -r "$readme" ]]
then
  echo "Error: '$readme': file does not exist or is unreadable"
  exit 1 
fi

if [[ ! -w "$index" ]]
then
  echo "Error: '$index': file does not exist or is not writable"
  exit 1 
fi

url="https://github.com/ocaml-sf/learn-ocaml/blob/master/docs"

sed -e "s;$url;.;g" "$readme" > "$index"
