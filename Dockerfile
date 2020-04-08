FROM ocaml/opam2:alpine as compilation
LABEL Description="learn-ocaml building" Vendor="OCamlPro"

WORKDIR learn-ocaml

COPY learn-ocaml.opam learn-ocaml.opam.locked tryocaml.opam tryocaml.opam.locked ./
RUN sudo chown -R opam:nogroup .

ENV OPAMYES true
RUN echo 'archive-mirrors: [ "https://opam.ocaml.org/cache" ]' >> ~/.opam/config
RUN opam repository set-url default http://opam.ocaml.org
RUN opam switch 4.10
RUN echo 'pre-session-commands: ["sudo" "apk" "add" depexts]' >>~/.opam/config
RUN opam install . --deps-only --locked

ADD static static
ADD translations translations
ADD src src
ADD scripts scripts
ADD Makefile Makefile
ADD demo-repository demo-repository
ADD dune-project dune-project
ADD dune dune
RUN sudo chown -R opam:nogroup .

ENV OPAMVERBOSE 1
RUN opam install . --destdir /home/opam/install-prefix --locked --deps-only
RUN opam exec -- make tryocaml

FROM bitnami/nginx:latest as server
LABEL Description="TryOCaml web-server" Vendor="OCamlPro"

COPY --from=compilation /home/opam/learn-ocaml/www /app
