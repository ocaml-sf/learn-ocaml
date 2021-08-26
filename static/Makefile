.PHONY: icons

all: icons dune

FILES = $(wildcard \
  js/*.js\
  js/ace/*.js\
  js/jszip/*.js\
  *.html\
  icons/*.svg\
  icons/*.gif\
  icons/*.ico\
  fonts/*.woff\
  css/*.css\
) $(shell find js/mathjax ! -type d)

EDITOR_FILES = $(wildcard \
  editor/*.html\
)

ALWAYS:
file-list: icons ALWAYS
	@echo ${FILES} ${EDITOR_FILES} >$@

icons:
	@${MAKE} -C icons

dune: file-list
	@echo '(install' >$@
	@echo ' (section share)' >>$@
	@echo ' (package learn-ocaml)' >>$@
	@echo ' (files' >>$@
	@$(foreach f,$(FILES),echo '  ($f as ${addprefix www/,$f})' >>$@;)
	@$(foreach f,$(EDITOR_FILES),echo '  ($f as $f)' >>$@;)
	@echo ' )' >>$@
	@echo ')' >>$@

clean:
	@${MAKE} -C icons clean
	rm -f dune
