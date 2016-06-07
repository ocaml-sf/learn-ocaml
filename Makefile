all: build

# config variables ------------------------------------------------------------

# where the result Web site will be put
DEST_DIR ?= ${CURDIR}/www

# the exercise repository to use
REPO_DIR ?= ${CURDIR}/demo-repository

# end of config variables -----------------------------------------------------

EXERCISES_DIR ?= ${REPO_DIR}/exercises
LESSONS_DIR ?= ${REPO_DIR}/lessons

build:
	@ocp-build init
	@ocp-build
	@mkdir -p ${DEST_DIR}
	@${MAKE} -C  static
	cp static/* ${DEST_DIR}
	cp ${LESSONS_DIR}/* ${DEST_DIR}
	@cp _obuild/*/learnocaml-main.js ${DEST_DIR}/
	@cp _obuild/*/learnocaml-exercise.js ${DEST_DIR}/
	@cp _obuild/*/learnocaml-toplevel-worker.js ${DEST_DIR}/
	@cp _obuild/*/learnocaml-grader-worker.js ${DEST_DIR}/
	@mkdir -p $(DEST_DIR)
	_obuild/*/learnocaml-process-repository.byte -j 4 \
          -exercises-dir ${EXERCISES_DIR} \
          -dest-dir ${DEST_DIR} \
          -dump-outputs ${EXERCISES_DIR} \
          -dump-reports ${EXERCISES_DIR}

clean:
	@ocp-build clean
	@${MAKE} -C  static clean
	-rm -rf ${DEST_DIR}
	-rm -f src/grader/embedded_cmis.ml
	-rm -f src/grader/embedded_grading_cmis.ml
	-rm -f src/ppx-metaquot/ast_lifter.ml
	-rm -f ${patsubst ${EXERCISES_DIR}/%/meta.json, \
                            ${EXERCISES_DIR}/%.*, \
                            ${wildcard ${EXERCISES_DIR}/*/meta.json}}
	-find -name \*~ -delete
