all: build

# config variables ------------------------------------------------------------

# where the static data should be put
PREFIX ?= /usr/local
WWW ?= $(PREFIX)/share/learn-ocaml/www

# end of config variables -----------------------------------------------------

build-deps:
	opam install . --deps-only --locked

.PHONY: build
build:
	@ocp-build init
	@ocp-build

.PHONY: static
static:
	@${MAKE} -C static

.PHONY: install
install: static
	@cp _obuild/learnocaml/learnocaml.byte ${PREFIX}/bin/learn-ocaml
	@cp _obuild/learnocaml-client/learnocaml-client.byte ${PREFIX}/bin/learn-ocaml-client
	@mkdir -p ${WWW}
	@cp -r static/* ${WWW}
	@cp _obuild/*/learnocaml-main.js ${WWW}/js/
	@cp _obuild/*/learnocaml-exercise.js ${WWW}/js/
	@cp _obuild/*/learnocaml-toplevel-worker.js ${WWW}/js/
	@cp _obuild/*/learnocaml-grader-worker.js ${WWW}/js/
	@cp -r demo-repository/* ${PREFIX}/share/learn-ocaml/repository/
	@cp scripts/complete.sh ${PREFIX}/share/bash-completion/completions/learn-ocaml

uninstall:
	@rm -f ${PREFIX}/bin/learn-ocaml
	@rm -rf ${PREFIX}/share/learn-ocaml
	@rm -f ${PREFIX}/share/bash-completion/completions/learn-ocaml

.PHONY: learn-ocaml.install travis
learn-ocaml.install: static
	@echo 'bin: [' >$@
	@echo '  "_obuild/learnocaml/learnocaml.byte" {"learn-ocaml"}' >>$@
	@echo '  "_obuild/learnocaml-client/learnocaml-client.byte" {"learn-ocaml-client"}' >>$@
	@echo ']' >>$@
	@echo 'share: [' >>$@
	@echo '  "scripts/complete.sh"' >>$@
	@$(foreach mod,main exercise toplevel-worker grader-worker,\
	    echo '  "_obuild/learnocaml-$(mod)/learnocaml-$(mod).js" {"www/js/learnocaml-$(mod).js"}' >>$@;)
	@$(foreach f,$(wildcard static/js/*.js static/js/ace/*.js static/*.html static/icons/*.svg static/fonts/*.woff static/css/*.css static/icons/*.gif),\
	    echo '  "$(f)" {"www/${f:static/%=%}"}' >>$@;)
	@(cd static && find js/mathjax -name '*.js' -exec echo '  "static/{}" {"www/{}"}' ';'; ) >>$@
	@echo ']' >>$@

# Generates up-to-date translation template for lang % from the sources
LANGS = $(patsubst translations/%.po,%,$(wildcard translations/*.po))
translations/$(LANGS:=.pot):
	@for f in $(LANGS); do echo >> translations/$$f.po; done
	@rm -f translations/*.pot
	@DUMP_POT=1 ocp-build -j 1
	@for f in $(LANGS); do \
	  mv translations/$$f.pot translations/$$f.pot.bak; \
	  msguniq translations/$$f.pot.bak > translations/$$f.pot; \
	  rm translations/$$f.pot.bak; \
	done

# Updates existing translations (.po) for the latest source template
update-%-translation: translations/%.pot
	@msgmerge -U translations/$*.po translations/$*.pot
	@rm -f translations/$*.pot

opaminstall: build learn-ocaml.install
	@opam-installer --prefix `opam var prefix` learn-ocaml.install

docker-images: Dockerfile learn-ocaml.opam
	@rm -rf docker
	@git clone . docker
	@cp Dockerfile docker
	@docker build -t learn-ocaml-compilation --target compilation docker
	@docker build -t learn-ocaml --target program docker
	@docker build -t learn-ocaml --target client docker
	@echo "Use with 'docker run --rm -v \$$PWD/sync:/sync -v \$$PWD:/repository -p PORT:8080 learn-ocaml -- ARGS'"

# Generates documentation for testing (exclusively Test_lib and Learnocaml_report modules)
doc: build
	mkdir -p _obuild/doc
	odoc css -o _obuild/doc
	odoc compile _obuild/testing/test_lib.cmti --package learn-ocaml
	odoc compile _obuild/learnocaml-state/learnocaml_report.cmti --package learn-ocaml
	odoc html _obuild/testing/test_lib.odoc --output-dir _obuild/doc
	odoc html _obuild/learnocaml-state/learnocaml_report.odoc --output-dir _obuild/doc

clean:
	@ocp-build clean
	-rm -f translations/$*.pot
	@${MAKE} -C  static clean
	-rm -rf ${DEST_DIR}
	-rm -f src/grader/embedded_cmis.ml
	-rm -f src/grader/embedded_grading_cmis.ml
	-rm -f src/ppx-metaquot/ast_lifter.ml
	-rm -f ${patsubst ${EXERCISES_DIR}/%/meta.json, \
			    ${EXERCISES_DIR}/%.*, \
			    ${wildcard ${EXERCISES_DIR}/*/meta.json}}
	-find -name \*~ -delete

travis: # From https://stackoverflow.com/questions/21053657/how-to-run-travis-ci-locally
	BUILDID="build-$$RANDOM";					\
	INSTANCE="travisci/ci-garnet:packer-1512502276-986baf0";	\
	docker run --name $$BUILDID -dit $$INSTANCE /sbin/init &&	\
	docker exec -it $$BUILDID bash -l
