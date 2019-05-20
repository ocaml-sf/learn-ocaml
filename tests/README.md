# Tests

## Run

To run the test suite, please execute `runtests.sh`

## Add a test

Each directory must contains a `repo` sub-directory, which will be built and launched with learn-ocaml.
Other sub-directories are named after exercices, and contain different solutions for it. Each solution (for example test.ml) can be provided with a particular output (test.ml.txt), which will be compared with what the server returned.
