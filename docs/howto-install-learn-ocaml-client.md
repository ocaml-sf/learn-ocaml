How to install the learn-ocaml client
=====================================

This section explains how to install a learn-ocaml client for students.

## Manual compilation

Note: you need a working ```opam``` environment with OCaml ```4.05.0```.

* Install the dependencies using:
  - if you have opam >=2.0 available:
``
opam switch create . --deps-only && opam install opam-installer && eval $(opam env)
``
  - otherwise:
``
make build-deps
``
(You may want to first read the script `scripts/install-opam-deps.sh` to check what it does.)

* Compile the learn-ocaml management tool using:
```
make && make opaminstall
```
