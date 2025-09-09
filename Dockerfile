FROM ocaml/opam:alpine-3.20-ocaml-5.1 AS compilation
LABEL Description="learn-ocaml building" Vendor="OCamlPro"

WORKDIR /home/opam/learn-ocaml

COPY learn-ocaml.opam learn-ocaml.opam.locked learn-ocaml-client.opam learn-ocaml-client.opam.locked ./
RUN sudo chown -R opam:nogroup .

ENV OPAMYES=true
RUN echo 'archive-mirrors: [ "https://opam.ocaml.org/cache" ]' >> ~/.opam/config \
  && opam repository set-url default http://opam.ocaml.org \
  && opam switch 5.1 \
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

ENV OPAMVERBOSE=1
RUN cat /proc/cpuinfo /proc/meminfo
RUN opam install . --destdir /home/opam/install-prefix --locked


FROM alpine:3.20 AS client

RUN apk update \
  && apk add ncurses-libs libev dumb-init libssl3 libcrypto3 \
  && addgroup learn-ocaml \
  && adduser learn-ocaml -DG learn-ocaml

VOLUME ["/learnocaml"]

USER learn-ocaml
WORKDIR /learnocaml

COPY --from=compilation /home/opam/install-prefix/bin/learn-ocaml-client /usr/bin

ENTRYPOINT ["dumb-init","/usr/bin/learn-ocaml-client"]


FROM alpine:3.20 AS program

RUN apk update \
  && apk add ncurses-libs libev dumb-init git openssl lsof \
  && addgroup learn-ocaml \
  && adduser learn-ocaml -DG learn-ocaml

VOLUME ["/repository"]
RUN mkdir -p /sync && chown learn-ocaml:learn-ocaml /sync
VOLUME ["/sync"]
EXPOSE 8080
EXPOSE 8443

USER learn-ocaml
WORKDIR /home/learn-ocaml

ARG opam_switch="/home/opam/.opam/5.1"

COPY --from=compilation /home/opam/install-prefix /usr
COPY --from=compilation "$opam_switch/bin"/ocaml* "$opam_switch/bin/"
COPY --from=compilation "$opam_switch/lib/ocaml" "$opam_switch/lib/ocaml/"
COPY --from=compilation "$opam_switch/bin/js_of_ocaml" "$opam_switch/bin/"
COPY --from=compilation "$opam_switch/lib/js_of_ocaml" "$opam_switch/lib/js_of_ocaml"
COPY --from=compilation "$opam_switch/lib/vg" "$opam_switch/lib/vg"
COPY --from=compilation "$opam_switch/lib/gg" "$opam_switch/lib/gg"

# Fixes for ocamlfind
COPY --from=compilation "$opam_switch/lib/findlib.conf" "$opam_switch/lib/"
ENV PATH="${opam_switch}/bin:${PATH}"
ENV OCAMLPATH="/usr/lib"
RUN ln -sf "$opam_switch/lib/vg" "/usr/lib"
RUN ln -sf "$opam_switch/lib/gg" "/usr/lib"

ENTRYPOINT ["dumb-init","/usr/bin/learn-ocaml","--sync-dir=/sync","--repo=/repository"]
CMD ["build","serve"]
