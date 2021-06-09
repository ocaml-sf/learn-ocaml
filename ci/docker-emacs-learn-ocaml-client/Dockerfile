ARG base=ocamlsf/learn-ocaml-client
ARG version=master
FROM ${base}:${version}

WORKDIR /home/learn-ocaml

USER root

RUN apk add --no-cache \
  curl \
  emacs-nox \
  && mkdir -p -v bin \
  && chown -v learn-ocaml:learn-ocaml bin

ENV PATH /home/learn-ocaml/bin:${PATH}

ENV LANG C.UTF-8
# ENV LC_ALL C.UTF-8
# ENV LANGUAGE en_US:en

COPY --chown=learn-ocaml:learn-ocaml .emacs .emacs

USER learn-ocaml

# Do some automatic Emacs installation/byte-compilation:
RUN emacs --version && emacs --batch -l ${HOME}/.emacs

ENTRYPOINT []
CMD ["/bin/sh"]
