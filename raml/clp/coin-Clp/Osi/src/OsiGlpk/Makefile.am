# Copyright (C) 2006, 2007 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: Makefile.am 1942 2013-11-21 19:56:36Z stefan $

# Author:  Andreas Waechter           IBM    2006-04-13

AUTOMAKE_OPTIONS = foreign

########################################################################
#                            libOsiGlpk                                #
########################################################################

# Name of the library compiled in this directory.
lib_LTLIBRARIES = libOsiGlpk.la

# List all source files for this library, including headers
libOsiGlpk_la_SOURCES = \
	OsiGlpkSolverInterface.cpp OsiGlpkSolverInterface.hpp

# List all additionally required libraries
if DEPENDENCY_LINKING
libOsiGlpk_la_LIBADD = $(OSILIB_LIBS) ../Osi/libOsi.la
endif

# This is for libtool
libOsiGlpk_la_LDFLAGS = $(LT_LDFLAGS)

# Here list all include flags, relative to this "srcdir" directory.  This
# "cygpath" stuff is necessary to compile with native compilers on Windows.
AM_CPPFLAGS = -I`$(CYGPATH_W) $(srcdir)/../Osi` $(COINUTILS_CFLAGS) $(GLPK_CFLAGS)

# This line is necessary to allow VPATH compilation
DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)` -I$(top_builddir)/src/Osi

########################################################################
#                Headers that need to be installed                     #
########################################################################

# Here list all the header files that are required by a user of the library,
# and that therefore should be installed in 'include/coin'
includecoindir = $(includedir)/coin
includecoin_HEADERS = OsiGlpkSolverInterface.hpp
