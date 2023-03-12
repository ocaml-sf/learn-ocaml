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
rm -f /usr/local/bin/2to3-3.11
rm -f /usr/local/bin/idle3.11
rm -f /usr/local/bin/pydoc3.11
rm -f /usr/local/bin/python3.11
rm -f /usr/local/bin/python3.11-config
rm -f /usr/local/lib/libtcl8.6.dylib
rm -f /usr/local/lib/libtk8.6.dylib
brew upgrade

brew install pkg-config
brew install opam
brew install libev
brew install openssl
opam init -y -a --bare

opam switch create . ocaml-base-compiler --deps-only --locked -y -j 2 # -v
eval $(opam env)
make
make opaminstall

# See src/main/linking_flags.sh
make detect-libs
