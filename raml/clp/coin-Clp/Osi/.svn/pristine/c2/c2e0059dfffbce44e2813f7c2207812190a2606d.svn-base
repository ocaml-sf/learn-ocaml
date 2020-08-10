# Copyright (C) 2006 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

# Author:  Lou Hafer	SFU	2010-11-20

AUTOMAKE_OPTIONS = foreign

########################################################################
#                    Common Test library for Osi                       #
########################################################################

# Name of the library compiled in this directory.  We want it to be installed
# in $libdir
lib_LTLIBRARIES = libOsiCommonTests.la

# List all source files for this library, including headers

libOsiCommonTests_la_SOURCES = \
    OsiCommonTests.hpp \
	OsiColCutTest.cpp \
	OsiCutsTest.cpp \
	OsiRowCutDebuggerTest.cpp \
	OsiRowCutTest.cpp \
	OsiSimplexAPITest.cpp \
	OsiNetlibTest.cpp \
	OsiUnitTestUtils.cpp \
	OsiSolverInterfaceTest.cpp

# List all additionally required libraries
if DEPENDENCY_LINKING
libOsiCommonTests_la_LIBADD = $(OSILIB_LIBS) ../Osi/libOsi.la
endif

# Libtool flags
libOsiCommonTests_la_LDFLAGS = $(LT_LDFLAGS)

# Here list all include flags, relative to this "srcdir" directory.  This
# "cygpath" stuff is necessary to compile with native compilers on Windows
AM_CPPFLAGS = -I`$(CYGPATH_W) $(srcdir)/../Osi` $(COINUTILS_CFLAGS)

# This line is necessary to allow VPATH compilation
DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)` -I$(top_builddir)/src/Osi

########################################################################
#                Headers that need to be installed                     #
########################################################################

# Here list all the header files that are required by a user of the library,

includecoindir = $(includedir)/coin
includecoin_HEADERS = OsiUnitTests.hpp
