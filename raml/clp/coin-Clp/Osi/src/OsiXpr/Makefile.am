# Copyright (C) 2006 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: Makefile.am 1800 2011-08-14 10:37:34Z stefan $

# Author:  Andreas Waechter           IBM    2006-04-13

AUTOMAKE_OPTIONS = foreign

########################################################################
#                             libOsiXpr                                #
########################################################################

# Name of the library compiled in this directory.
lib_LTLIBRARIES = libOsiXpr.la

# List all source files for this library, including headers
libOsiXpr_la_SOURCES = \
	OsiXprSolverInterface.cpp OsiXprSolverInterface.hpp

# This is for libtool (on Windows)
libOsiXpr_la_LDFLAGS = $(LT_LDFLAGS)

# Here list all include flags, relative to this "srcdir" directory.  This
# "cygpath" stuff is necessary to compile with native compilers on Windows.
# "top_srcdir" refers to the basic directory for the main package that is
# being compiled.
AM_CPPFLAGS = \
	-I`$(CYGPATH_W) $(srcdir)/../Osi` \
	-I`$(CYGPATH_W) $(XPRINCDIR)` \
	$(COINUTILS_CFLAGS)

# This line is necessary to allow VPATH compilation
DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)` -I$(top_builddir)/src/Osi

########################################################################
#                Headers that need to be installed                     #
########################################################################

# Here list all the header files that are required by a user of the library,
# and that therefore should be installed in 'include/coin'
includecoindir = $(includedir)/coin
includecoin_HEADERS = OsiXprSolverInterface.hpp
