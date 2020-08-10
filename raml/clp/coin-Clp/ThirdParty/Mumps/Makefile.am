# Copyright (C) 2007-2010 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: Makefile.am 3102 2013-11-21 19:56:01Z stefan $

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = get.mumps INSTALL.MUMPS

lib_LTLIBRARIES = libcoinmumps.la

MUMPSDIR = @MUMPSDIR@

nodist_libcoinmumps_la_SOURCES = \
	$(MUMPSDIR)/src/tools_common_mod.F \
	$(MUMPSDIR)/src/dmumps_comm_buffer.F \
	$(MUMPSDIR)/src/dmumps_struc_def.F \
	$(MUMPSDIR)/src/mumps_ooc_common.F\
	$(MUMPSDIR)/src/mumps_static_mapping.F \
	$(MUMPSDIR)/src/dmumps_ooc_buffer.F \
	$(MUMPSDIR)/src/dmumps_load.F \
	$(MUMPSDIR)/src/dmumps_ooc.F \
	$(MUMPSDIR)/src/dmumps_part1.F \
	$(MUMPSDIR)/src/dmumps_part2.F \
	$(MUMPSDIR)/src/dmumps_part3.F \
	$(MUMPSDIR)/src/dmumps_part4.F \
	$(MUMPSDIR)/src/dmumps_part5.F \
	$(MUMPSDIR)/src/dmumps_part6.F \
	$(MUMPSDIR)/src/dmumps_part7.F \
	$(MUMPSDIR)/src/dmumps_part8.F \
	$(MUMPSDIR)/src/mumps_part9.F \
	$(MUMPSDIR)/src/mumps_sol_es.F \
	$(MUMPSDIR)/src/mumps_c.c \
	$(MUMPSDIR)/src/mumps_common.c\
	$(MUMPSDIR)/src/mumps_orderings.c\
	$(MUMPSDIR)/src/mumps_io.c\
	$(MUMPSDIR)/src/mumps_io_basic.c\
	$(MUMPSDIR)/src/mumps_io_thread.c\
	$(MUMPSDIR)/src/mumps_io_err.c\
	$(MUMPSDIR)/src/mumps_size.c\
	$(MUMPSDIR)/libseq/mpi.f \
	$(MUMPSDIR)/libseq/mpic.c \
	$(MUMPSDIR)/libseq/elapse.c


DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)`

AM_CPPFLAGS = \
	-I`$(CYGPATH_W) $(srcdir)/$(MUMPSDIR)/src` \
	-I`$(CYGPATH_W) $(srcdir)/$(MUMPSDIR)/libseq` \
	-I`$(CYGPATH_W) $(srcdir)/$(MUMPSDIR)/include`

if DEPENDENCY_LINKING
libcoinmumps_la_LIBADD = $(MUMPS_LIBS) $(MUMPS_PCLIBS)
endif

libcoinmumps_la_LDFLAGS = $(LT_LDFLAGS)

# Some dependecies for modules:

dmumps_load.lo dmumps_part1.lo dmumps_part2.lo dmumps_part3.lo \
dmumps_part4.lo dmumps_part5.lo dmumps_part6.lo dmumps_part8.lo: \
dmumps_comm_buffer.lo

dmumps_part1.lo dmumps_part2.lo dmumps_part3.lo \
dmumps_part4.lo dmumps_part5.lo dmumps_part6.lo: dmumps_load.lo

dmumps_part1.lo dmumps_ooc.lo: dmumps_ooc_buffer.lo

dmumps_part1.lo dmumps_part2.lo dmumps_part3.lo dmumps_part4.lo \
dmumps_part5.lo dmumps_part6.lo dmumps_part8.lo: dmumps_ooc.lo

dmumps_part1.lo dmumps_part5.lo: dmumps_part2.lo

dmumps_load.lo dmumps_ooc.lo dmumps_part1.lo dmumps_part2.lo dmumps_part3.lo \
dmumps_part4.lo dmumps_part5.lo dmumps_part7.lo dmumps_part8.lo: \
dmumps_struc_def.lo

dmumps_ooc_buffer.lo dmumps_ooc.lo: mumps_ooc_common.lo

dmumps_part5.lo dmumps_ooc.lo: mumps_static_mapping.lo

dmumps_part2.lo dmumps_part5.lo dmumps_part8.lo: tools_common_mod.lo mumps_sol_es.lo

# actually only dmumps_part8.lo depends on mumps_sol_es.lo, but having only one target
# on the left side makes automake think that we provide a rule how to build this target
dmumps_part2.lo dmumps_part8.lo: mumps_sol_es.lo

# Module files that need to be deleted
MODEXT = mod
CLEANFILES = *.$(MODEXT)

# Reset DEFS to that they don't show up for the Fortran compiler
DEFS =
CFLAGS += $(MY_DEFS)
FFLAGS += $(MY_FDEFS)

thirdpartyincludedir = $(includedir)/coin/ThirdParty
thirdpartyinclude_HEADERS = \
  $(MUMPSDIR)/include/dmumps_c.h \
  $(MUMPSDIR)/include/mumps_compat.h \
  $(MUMPSDIR)/include/mumps_c_types.h \
  $(MUMPSDIR)/libseq/mpi.h

pkgconfiglibdir = $(libdir)/pkgconfig
pkgconfiglib_DATA = coinmumps.pc

test:
	@echo "No test available for Mumps."

########################################################################
#                         Maintainer Stuff                             #
########################################################################

# Files that are generated and should be cleaned with make distclean
DISTCLEANFILES = $(MAKEOKFILE)

include BuildTools/Makemain.inc
