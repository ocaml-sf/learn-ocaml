all: build

# config variables ------------------------------------------------------------

# parallelism when processing the repository
PROCESSING_JOBS ?= 4

# where the result Web site will be put
DEST_DIR ?= ${CURDIR}/www

# the exercise repository to use
REPO_DIR ?= ${CURDIR}/demo-repository

# end of config variables -----------------------------------------------------

EXERCISES_DIR ?= ${REPO_DIR}/exercises
LESSONS_DIR ?= ${REPO_DIR}/lessons
TUTORIALS_DIR ?= ${REPO_DIR}/tutorials

build-deps:
	sh scripts/install-opam-deps.sh

build:
	@ocp-build init
	@ocp-build

process-repo: install
	_obuild/*/learnocaml-process-repository.byte -j ${PROCESSING_JOBS} \
          -exercises-dir ${EXERCISES_DIR} \
          -tutorials-dir ${TUTORIALS_DIR} \
          -dest-dir ${DEST_DIR} \
          -dump-outputs ${EXERCISES_DIR} \
          -dump-reports ${EXERCISES_DIR}

install:
	@mkdir -p ${DEST_DIR}
	@${MAKE} -C  static
	cp -r static/* ${DEST_DIR}
	cp ${LESSONS_DIR}/* ${DEST_DIR}
	@cp _obuild/*/learnocaml-main.js ${DEST_DIR}/js/
	@cp _obuild/*/learnocaml-exercise.js ${DEST_DIR}/js/
	@cp _obuild/*/learnocaml-toplevel-worker.js ${DEST_DIR}/js/
	@cp _obuild/*/learnocaml-grader-worker.js ${DEST_DIR}/js/
	@cp _obuild/*/learnocaml-simple-server.byte .

.PHONY: learn-ocaml.install
learn-ocaml.install:
	@echo 'bin: [' >$@
	@echo '  "_obuild/learnocaml/learnocaml.byte" {"learn-ocaml"}' >>$@
	@echo ']' >>$@
	@echo 'share: [' >>$@
	@echo '  "scripts/complete.sh"' >>$@
	@$(foreach mod,main exercise toplevel-worker grader-worker,\
	    echo '  "_obuild/learnocaml-$(mod)/learnocaml-$(mod).js" {"www/js/learnocaml-$(mod).js"}' >>$@;)
	@$(foreach f,$(wildcard static/js/ace/*.js static/*.html static/icons/*.svg static/fonts/*.woff static/css/*.css static/icons/*.gif),\
	    echo '  "$(f)" {"www/${f:static/%=%}"}' >>$@;)
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
