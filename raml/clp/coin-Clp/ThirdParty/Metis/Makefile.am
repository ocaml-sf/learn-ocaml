# Copyright (C) 2009 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: Makefile.am 3103 2013-11-21 19:56:05Z stefan $

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = get.metis INSTALL.METIS

lib_LTLIBRARIES = libcoinmetis.la

METISDIR = @METISDIR@

libcoinmetis_la_SOURCES = \
	$(METISDIR)/Lib/balance.c \
	$(METISDIR)/Lib/bucketsort.c \
	$(METISDIR)/Lib/ccgraph.c \
	$(METISDIR)/Lib/coarsen.c \
	$(METISDIR)/Lib/compress.c \
	$(METISDIR)/Lib/debug.c \
	$(METISDIR)/Lib/estmem.c \
	$(METISDIR)/Lib/fm.c \
	$(METISDIR)/Lib/fortran.c \
	$(METISDIR)/Lib/frename.c \
	$(METISDIR)/Lib/graph.c \
	$(METISDIR)/Lib/initpart.c \
	$(METISDIR)/Lib/kmetis.c \
	$(METISDIR)/Lib/kvmetis.c \
	$(METISDIR)/Lib/kwayfm.c \
	$(METISDIR)/Lib/kwayrefine.c \
	$(METISDIR)/Lib/kwayvolfm.c \
	$(METISDIR)/Lib/kwayvolrefine.c \
	$(METISDIR)/Lib/match.c \
	$(METISDIR)/Lib/mbalance2.c \
	$(METISDIR)/Lib/mbalance.c \
	$(METISDIR)/Lib/mcoarsen.c \
	$(METISDIR)/Lib/memory.c \
	$(METISDIR)/Lib/mesh.c \
	$(METISDIR)/Lib/meshpart.c \
	$(METISDIR)/Lib/mfm2.c \
	$(METISDIR)/Lib/mfm.c \
	$(METISDIR)/Lib/mincover.c \
	$(METISDIR)/Lib/minitpart2.c \
	$(METISDIR)/Lib/minitpart.c \
	$(METISDIR)/Lib/mkmetis.c \
	$(METISDIR)/Lib/mkwayfmh.c \
	$(METISDIR)/Lib/mkwayrefine.c \
	$(METISDIR)/Lib/mmatch.c \
	$(METISDIR)/Lib/mmd.c \
	$(METISDIR)/Lib/mpmetis.c \
	$(METISDIR)/Lib/mrefine2.c \
	$(METISDIR)/Lib/mrefine.c \
	$(METISDIR)/Lib/mutil.c \
	$(METISDIR)/Lib/myqsort.c \
	$(METISDIR)/Lib/ometis.c \
	$(METISDIR)/Lib/parmetis.c \
	$(METISDIR)/Lib/pmetis.c \
	$(METISDIR)/Lib/pqueue.c \
	$(METISDIR)/Lib/refine.c \
	$(METISDIR)/Lib/separator.c \
	$(METISDIR)/Lib/sfm.c \
	$(METISDIR)/Lib/srefine.c \
	$(METISDIR)/Lib/stat.c \
	$(METISDIR)/Lib/subdomains.c \
	$(METISDIR)/Lib/timing.c \
	$(METISDIR)/Lib/util.c

DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)`

AM_CPPFLAGS = \
	-I`$(CYGPATH_W) $(srcdir)/$(METISDIR)/Lib`

if DEPENDENCY_LINKING
libcoinmetis_la_LIBADD = $(METIS_PCLIBS)
endif

libcoinmetis_la_LDFLAGS = $(LT_LDFLAGS)

thirdpartyincludedir = $(includedir)/coin/ThirdParty
thirdpartyinclude_HEADERS = \
  $(METISDIR)/Lib/defs.h \
  $(METISDIR)/Lib/macros.h \
  $(METISDIR)/Lib/metis.h \
  $(METISDIR)/Lib/proto.h \
  $(METISDIR)/Lib/rename.h \
  $(METISDIR)/Lib/struct.h

pkgconfiglibdir = $(libdir)/pkgconfig
pkgconfiglib_DATA = coinmetis.pc

test:
	@echo "No test available for Metis."

# Suppress default defines from autoheader
DEFS =

########################################################################
#                         Maintainer Stuff                             #
########################################################################

CLEANFILES =

# Files that are generated and should be cleaned with make distclean
DISTCLEANFILES = $(MAKEOKFILE) strings.h

include BuildTools/Makemain.inc
