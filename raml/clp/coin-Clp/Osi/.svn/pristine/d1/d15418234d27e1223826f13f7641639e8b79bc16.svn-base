# Copyright (C) 2006 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

# Author:  Andreas Waechter           IBM    2006-04-13

AUTOMAKE_OPTIONS = foreign

########################################################################
#                             libOsiMsk                                #
########################################################################

# Name of the library compiled in this directory.
lib_LTLIBRARIES = libOsiMsk.la

# List all source files for this library, including headers
libOsiMsk_la_SOURCES = \
	OsiMskSolverInterface.cpp OsiMskSolverInterface.hpp

# This is for libtool (on Windows)
libOsiMsk_la_LDFLAGS = $(LT_LDFLAGS)

# Here list all include flags, relative to this "srcdir" directory.  This
# "cygpath" stuff is necessary to compile with native compilers on Windows.
AM_CPPFLAGS = \
	-I`$(CYGPATH_W) $(srcdir)/../Osi` \
	-I`$(CYGPATH_W) $(MSKINCDIR)` \
	$(COINUTILS_CFLAGS)

# This line is necessary to allow VPATH compilation
DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)` -I$(top_builddir)/src/Osi

########################################################################
#                Headers that need to be installed                     #
########################################################################

# Here list all the header files that are required by a user of the library,
# and that therefore should be installed in 'include/coin'
includecoindir = $(includedir)/coin
includecoin_HEADERS = OsiMskSolverInterface.hpp
