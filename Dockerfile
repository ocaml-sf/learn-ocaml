FROM ocaml/opam2:alpine as compilation
LABEL Description="learn-ocaml building" Vendor="OCamlPro"

WORKDIR /home/opam/learn-ocaml

COPY learn-ocaml.opam learn-ocaml.opam.locked learn-ocaml-client.opam ./
RUN sudo chown -R opam:nogroup .

ENV OPAMYES true
RUN echo 'archive-mirrors: [ "https://opam.ocaml.org/cache" ]' >> ~/.opam/config \
  && opam repository set-url default http://opam.ocaml.org \
  && opam switch 4.12 \
  && echo 'pre-session-commands: [ "sudo" "apk" "add" depexts ]' >> ~/.opam/config \
  && opam install . --deps-only --locked

COPY static static
COPY translations translations
COPY src src
COPY scripts scripts
COPY Makefile Makefile
COPY demo-repository demo-repository
COPY dune-project dune-project
COPY dune dune
RUN sudo chown -R opam:nogroup .

ENV OPAMVERBOSE 1
RUN cat /proc/cpuinfo /proc/meminfo
RUN opam install . --destdir /home/opam/install-prefix --locked


FROM alpine:3.13 as client

RUN apk update \
  && apk add ncurses-libs libev dumb-init openssl \
  && addgroup learn-ocaml \
  && adduser learn-ocaml -DG learn-ocaml

VOLUME ["/learnocaml"]

USER learn-ocaml
WORKDIR /learnocaml

COPY --from=compilation /home/opam/install-prefix/bin/learn-ocaml-client /usr/bin

ENTRYPOINT ["dumb-init","learn-ocaml-client"]

LABEL org.opencontainers.image.title="learn-ocaml-client"
LABEL org.opencontainers.image.description="learn-ocaml command-line client"
LABEL org.opencontainers.image.url="https://ocaml-sf.org/"
LABEL org.opencontainers.image.vendor="The OCaml Software Foundation"


FROM alpine:3.13 as program

RUN apk update \
  && apk add ncurses-libs libev dumb-init git openssl \
  && addgroup learn-ocaml \
  && adduser learn-ocaml -DG learn-ocaml

VOLUME ["/repository"]
RUN mkdir -p /sync && chown learn-ocaml:learn-ocaml /sync
VOLUME ["/sync"]
EXPOSE 8080
EXPOSE 8443

USER learn-ocaml
WORKDIR /home/learn-ocaml

COPY --from=compilation /home/opam/install-prefix /usr

ENTRYPOINT ["dumb-init","learn-ocaml","--sync-dir=/sync","--repo=/repository"]
CMD ["build","serve"]

LABEL org.opencontainers.image.title="learn-ocaml"
LABEL org.opencontainers.image.description="learn-ocaml app manager"
LABEL org.opencontainers.image.url="https://ocaml-sf.org/"
LABEL org.opencontainers.image.vendor="The OCaml Software Foundation"
