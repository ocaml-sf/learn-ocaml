# Tests

## Run

To run the test suite, please execute `runtests.sh`. It uses docker.

## Add a test

Each directory must contain a `repo` sub-directory, which will be built and launched with learn-ocaml.
Other sub-directories are named after exercices, and contain different solutions for it. Each solution (for example test.ml) must be provided with the desired output (test.ml.json), which will be compared with the server response.
