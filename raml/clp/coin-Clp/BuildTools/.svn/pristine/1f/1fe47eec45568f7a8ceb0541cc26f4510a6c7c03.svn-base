# Copyright (C) 2006, 2007 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

# Author:  Andreas Waechter           IBM    2006-04-13

########################################################################
#                    Documentation installation                        #
########################################################################

DocFiles =  README AUTHORS LICENSE 
DocInstallDir = $(datadir)/coin/doc/$(PACKAGE_NAME)

COIN_HAS_DOXYGEN = @COIN_HAS_DOXYGEN_TRUE@TRUE
COIN_HAS_LATEX = @COIN_HAS_LATEX_TRUE@TRUE

doxygen-docs: 
	if test "$(COIN_HAS_DOXYGEN)" = TRUE; then \
	  if test -d "doxydoc/"; then \
	    doxygen doxydoc/doxygen.conf;\
	  fi;\
	fi

pdf-doxygen-docs: doxygen-docs
	if test "$(COIN_HAS_DOXYGEN)" = TRUE; then \
	  if test -d "doxydoc/latex"; then \
       	    if test "$(COIN_HAS_LATEX)" = TRUE; then \
	      cd doxydoc/latex;\
	      $(MAKE) pdf;\
	      cd -;\
	    fi;\
	  fi;\
	fi

clean-doxygen-docs:
	if test -d "doxydoc/"; then \
	  cd doxydoc ;\
	  rm -rf html latex *.log *.tag;\
	fi

install-doxygen-docs: doxygen-docs
	if test "$(COIN_HAS_DOXYGEN)" = TRUE; then \
	  if test -d "doxydoc/"; then \
	    test -d "$(DESTDIR)$(DocInstallDir)/doxydoc" || $(mkdir_p) "$(DESTDIR)$(DocInstallDir)/doxydoc"; \
	    $(INSTALL_DATA) @coin_doxy_tagname@ "$(DESTDIR)$(DocInstallDir)/@coin_doxy_tagname@";\
	    if test -f "doxydoc/latex/refman.pdf"; then \
	      $(INSTALL_DATA) doxydoc/latex/refman.pdf "$(DESTDIR)$(DocInstallDir)";\
	    fi;\
	    if test -d "doxydoc/html"; then \
	      test -d "$(DESTDIR)$(DocInstallDir)/doxydoc/search/" || $(mkdir_p) "$(DESTDIR)$(DocInstallDir)/doxydoc/search/"; \
	      $(INSTALL_DATA) doxydoc/html/*.* "$(DESTDIR)$(DocInstallDir)/doxydoc";\
	      $(INSTALL_DATA) doxydoc/html/search/*.* "$(DESTDIR)$(DocInstallDir)/doxydoc/search";\
            fi;\
          fi;\
        fi

uninstall-doxygen-docs:
	if test -d "$(DESTDIR)$(DocInstallDir)/doxydoc/"; then \
	  rm -rf "$(DESTDIR)$(DocInstallDir)/doxydoc/"; \
	fi
	if test -f "$(DESTDIR)$(DocInstallDir)/refman.pdf"; then \
	  rm -f "$(DESTDIR)$(DocInstallDir)/refman.pdf"; \
	fi

all-doxygen-docs:
	for dir in $(subdirs) ; do \
	  do_project=true;\
	  for proj in $(COIN_SKIP_DOXYGEN); do\
	    if test $$dir = $$proj; then\
	      do_project=false;\
	    fi;\
	  done;\
	  if test -r $$dir/doxydoc & $$do_project = true; then \
	    (cd $$dir ; $(MAKE) doxygen-docs) \
	  fi ; \
	done ; 

clean-all-doxygen-docs:
	for dir in $(subdirs) ; do \
	  if test -r $$dir/doxydoc ; then \
	    (cd $$dir ; $(MAKE) clean-doxygen-docs) \
	  fi ; \
	done ; 

install-all-doxygen-docs: all-doxygen-docs
	for dir in $(subdirs) ; do \
	  do_project=true;\
	  for proj in $(COIN_SKIP_DOXYGEN); do\
	    if test $$dir = $$proj; then\
	      do_project=false;\
	    fi;\
	  done;\
	  if test -r $$dir/doxydoc & $$do_project = true; then \
	    (cd $$dir ; $(MAKE) install-doxygen-docs) \
	  fi ; \
	done ; 

uninstall-all-doxygen-docs:
	for dir in $(subdirs) ; do \
	  if test -r $$dir/doxydoc ; then \
	    (cd $$dir ; $(MAKE) uninstall-doxygen-docs) \
	  fi ; \
	done ; 

install-doc: $(DocFiles) 
	test -z "$(DocInstallDir)" || $(mkdir_p) "$(DESTDIR)$(DocInstallDir)"
	for file in $(DocFiles); do \
	  if test -f "$$file"; then dir=; else dir="$(srcdir)/"; fi; \
	  if test -f "$$dir$$file"; then $(INSTALL_DATA) "$$dir$$file" "$(DESTDIR)$(DocInstallDir)/$$file"; fi; \
	done 

uninstall-doc:
	for file in $(DocFiles); do \
	  rm -f "$(DESTDIR)$(DocInstallDir)/$$file"; \
	done

########################################################################
#                         Maintainer Stuff                             #
########################################################################

if MAINTAINER_MODE

# Make sure acinclude is using most recent coin.m4
$(srcdir)/acinclude.m4: $(BUILDTOOLSDIR)/coin.m4
	cat $(LIBTOOLM4) $< > $@

# Make sure the autotools scripts are up to date
$(AUX_DIR)/install-sh: $(BUILDTOOLSDIR)/install-sh
	cp $< $@

$(AUX_DIR)/missing: $(BUILDTOOLSDIR)/missing
	cp $< $@

$(AUX_DIR)/config.guess: $(BUILDTOOLSDIR)/config.guess
	cp $< $@

$(AUX_DIR)/config.sub: $(BUILDTOOLSDIR)/config.sub
	cp $< $@

$(AUX_DIR)/depcomp: $(BUILDTOOLSDIR)/depcomp
	cp $< $@

$(AUX_DIR)/ltmain.sh: $(BUILDTOOLSDIR)/ltmain.sh
	cp $< $@

# Take care of updating externals (if Dependencies file exists)
if HAVE_EXTERNALS
$(top_builddir)/Makefile: .Dependencies-stamp

.Dependencies-stamp: $(srcdir)/Dependencies
	cd $(srcdir); BuildTools/set_externals Dependencies
	touch .Dependencies-stamp

update-externals: .Dependencies-stamp
	cd $(srcdir); svn update

endif

endif

if HAVE_EXTERNALS
EXTRA_DIST += Dependencies
DISTCLEANFILES += .Dependencies-stamp
endif

DISTCLEANFILES += $(VPATH_DISTCLEANFILES)

.PHONY: install-doc uninstall-doc update-externals
