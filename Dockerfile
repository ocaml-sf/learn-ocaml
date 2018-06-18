FROM ocaml/opam2:alpine-3.7-ocaml-4.05 as compilation
LABEL Description="learn-ocaml building" Vendor="OCamlPro" Version="0.2"

WORKDIR learn-ocaml

COPY learn-ocaml.opam .
RUN sudo chown -R opam:nogroup .

ENV OPAMYES true
RUN opam switch 4.05
RUN opam pin learn-ocaml . -n
RUN opam depext learn-ocaml
RUN opam install . --deps-only --locked

ADD static static
ADD translations translations
ADD src src
ADD scripts scripts
ADD Makefile Makefile
ADD demo-repository demo-repository
RUN echo "bytelink += [\"-custom\"]" >addflags.ocp
RUN sudo chown -R opam:nogroup .

RUN opam install . --destdir /home/opam/install-prefix




FROM alpine:3.7 as program
LABEL Description="learn-ocaml app manager" Vendor="OCamlPro" Version="0.2"

RUN apk update
RUN apk add ncurses-libs dumb-init
RUN addgroup learn-ocaml
RUN adduser learn-ocaml -DG learn-ocaml

VOLUME ["/repository"]
RUN mkdir -p /sync && chown learn-ocaml:learn-ocaml /sync
VOLUME ["/sync"]
EXPOSE 8080

USER learn-ocaml
WORKDIR /home/learn-ocaml

CMD ["build","serve"]
ENTRYPOINT ["dumb-init","learn-ocaml","--sync-dir=/sync","--repo=/repository","--port=8080"]

COPY --from=compilation /home/opam/install-prefix /usr
