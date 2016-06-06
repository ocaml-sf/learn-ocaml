ROOTDIR=../..
include ${ROOTDIR}/Makefile.conf

all: learnocaml_index.js

############################################################################
## Index

INDEX_MLS:=server_index.ml client_index.ml learnocaml_main.ml
INDEX_MLIS:=server_index.mli client_index.mli
INDEX_PACKAGES:=\
  ocplib-json-typed \
  js_of_ocaml \
  js_of_ocaml.syntax \
  js_of_ocaml.tyxml \
  ezjsonm \
  cstruct

INDEX_INCLUDES := -I ${JSLIB_DIR}

learnocaml_index.js: learnocaml_index.byte
	${JS_OF_OCAML} +weak.js +cstruct/cstruct.js $<
	cp $@ ${WWW_DIR}/$@

${INDEX_MLS:.ml=.cmo}: INCLUDES=${INDEX_INCLUDES}
learnocaml_index.byte: ${INDEX_MLS:.ml=.cmo}
	$(OCAMLFIND) ocamlc ${addprefix -package ,${INDEX_PACKAGES}} ${JSLIB_DIR}/jslib.cma -linkpkg $^ -o $@

clean::
	-rm -f learnocaml_index.js learnocaml_index.byte

############################################################################
## Exercise




MLS:=${INDEX_MLS}
MLIS:=${INDEX_MLIS}
PACKAGES:=${INDEX_PACKAGES}

include ../Makefile.generic

