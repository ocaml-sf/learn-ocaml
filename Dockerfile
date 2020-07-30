FROM ocaml/opam2:alpine-3.7 as compilation
LABEL Description="learn-ocaml building" Vendor="OCamlPro"

WORKDIR learn-ocaml

COPY learn-ocaml.opam learn-ocaml.opam.locked learn-ocaml-client.opam ./
RUN sudo chown -R opam:nogroup .

ENV OPAMYES true
RUN echo 'archive-mirrors: [ "https://opam.ocaml.org/cache" ]' >> ~/.opam/config \
  && opam repository set-url default http://opam.ocaml.org \
  && opam switch 4.05 \
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
RUN opam install . --destdir /home/opam/install-prefix --locked



FROM alpine:3.7 as client

ARG BUILD_DATE
ARG VCS_BRANCH
ARG VCS_REF

LABEL org.label-schema.build-date="${BUILD_DATE}" \
  org.label-schema.name="learn-ocaml-client" \
  org.label-schema.description="learn-ocaml command-line client" \
  org.label-schema.url="https://ocaml-sf.org/" \
  org.label-schema.vendor="The OCaml Software Foundation" \
  org.label-schema.version="${VCS_BRANCH}" \
  org.label-schema.vcs-ref="${VCS_REF}" \
  org.label-schema.vcs-url="https://github.com/ocaml-sf/learn-ocaml" \
  org.label-schema.schema-version="1.0"

RUN apk update \
  && apk add ncurses-libs libev dumb-init \
  && addgroup learn-ocaml \
  && adduser learn-ocaml -DG learn-ocaml

VOLUME ["/learnocaml"]

USER learn-ocaml
WORKDIR /learnocaml

COPY --from=compilation /home/opam/install-prefix/bin/learn-ocaml-client /usr/bin

ENTRYPOINT ["dumb-init","learn-ocaml-client"]



FROM alpine:3.7 as program

ARG BUILD_DATE
ARG VCS_BRANCH
ARG VCS_REF

LABEL org.label-schema.build-date="${BUILD_DATE}" \
  org.label-schema.name="learn-ocaml" \
  org.label-schema.description="learn-ocaml app manager" \
  org.label-schema.url="https://ocaml-sf.org/" \
  org.label-schema.vendor="The OCaml Software Foundation" \
  org.label-schema.version="${VCS_BRANCH}" \
  org.label-schema.vcs-ref="${VCS_REF}" \
  org.label-schema.vcs-url="https://github.com/ocaml-sf/learn-ocaml" \
  org.label-schema.schema-version="1.0"

RUN apk update \
  && apk add ncurses-libs libev gmp dumb-init git \
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

CMD ["build","serve"]
ENTRYPOINT ["dumb-init","learn-ocaml","--sync-dir=/sync","--repo=/repository"]
