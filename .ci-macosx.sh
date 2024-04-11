#!/bin/sh

set -ex

sw_vers
system_profiler SPSoftwareDataType
uname -a

brew update

# homebrew fails to upgrade python due to unlinking failure
# (cf. https://github.com/actions/setup-python/issues/577 )
rm -f /usr/local/bin/2to3
rm -f /usr/local/bin/idle3
rm -f /usr/local/bin/pydoc3
rm -f /usr/local/bin/python3
rm -f /usr/local/bin/python3-config
rm -f /usr/local/bin/2to3-3.*
rm -f /usr/local/bin/idle3.*
rm -f /usr/local/bin/pydoc3.*
rm -f /usr/local/bin/python3.*
rm -f /usr/local/bin/python3.*-config
rm -f /usr/local/lib/libtcl8.6.dylib
rm -f /usr/local/lib/libtk8.6.dylib
rm -f /usr/local/bin/go
rm -f /usr/local/bin/gofmt
rm -f /usr/local/bin/node
rm -f /usr/local/bin/npm
rm -f /usr/local/bin/npx
rm -f -r /usr/local/include/node
rm -f -r /usr/local/share/doc/node
rm -f -r /usr/local/lib/node_modules
rm -f /usr/local/lib/dtrace/node.d
rm -f /usr/local/share/man/man1/node.1
rm -f /usr/local/share/systemtap

brew upgrade

brew install pkg-config
brew install opam
brew install libev
brew install openssl
opam init -y -a --bare

opam switch create . ocaml-base-compiler --deps-only --locked -y -j 2 # -v
eval $(opam env)

# Run unit tests
# Note: we might want to run them in Linux as well in the CI
make test

make

make opaminstall

# See src/main/linking_flags.sh
make detect-libs
