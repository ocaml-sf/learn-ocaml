# Copyright (C) 2009 Stefan Vigerske and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

AUTOMAKE_OPTIONS = foreign

########################################################################
#                             libOsiGrb                                 
########################################################################

# Name of the library compiled in this directory.
lib_LTLIBRARIES = libOsiGrb.la

# List all source files for this library, including headers
libOsiGrb_la_SOURCES = OsiGrbSolverInterface.cpp OsiGrbSolverInterface.hpp

# This is for libtool (on Windows)
libOsiGrb_la_LDFLAGS = $(LT_LDFLAGS)

# Here list all include flags, relative to this "srcdir" directory.  This
# "cygpath" stuff is necessary to compile with native compilers on Windows.
AM_CPPFLAGS = \
	-I`$(CYGPATH_W) $(srcdir)/../Osi` \
	-I`$(CYGPATH_W) $(GRBINCDIR)` \
	$(COINUTILS_CFLAGS)

# This line is necessary to allow VPATH compilation
DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)` -I$(top_builddir)/src/Osi

########################################################################
#                Headers that need to be installed                     #
########################################################################

# Here list all the header files that are required by a user of the library,
# and that therefore should be installed in 'include/coin'
includecoindir = $(includedir)/coin
includecoin_HEADERS = OsiGrbSolverInterface.hpp
