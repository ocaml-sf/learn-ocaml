FROM ocaml/opam2-staging:alpine
LABEL Description="learn-ocaml running container" Vendor="OCamlPro" Version="0.1"

WORKDIR learn-ocaml

COPY learn-ocaml.opam .
RUN sudo chown opam:nogroup learn-ocaml.opam

ENV OPAMYES true
RUN opam switch 4.05
RUN opam pin learn-ocaml . -n
RUN opam depext learn-ocaml
RUN opam install . --deps-only --locked

ADD . .
RUN sudo chown -R opam:nogroup .

RUN opam install .

ARG port=8080
ENV port=${port}

EXPOSE $port

CMD ["build","serve"]
ENTRYPOINT ["/bin/bash","-c","opam exec -- learn-ocaml --sync-dir=/sync --repo=/repository --port=${port} \"$@\""]
