# Copyright (C) 2006 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: Makefile.am 324 2012-11-21 18:57:40Z stefan $

# Author:  Andreas Waechter           IBM    2006-04-13

AUTOMAKE_OPTIONS = foreign

########################################################################
#                List files that should be distributed                 #
########################################################################

EXTRA_DIST = $(EXAMPLE_FILES)

DISTCLEANFILES = $(EXAMPLE_CLEAN_FILES)

datacoindir = $(datadir)/coin/Data/Sample
datacoin_DATA = $(EXAMPLE_FILES)

pkgconfiglibdir = $(libdir)/pkgconfig
pkgconfiglib_DATA = coindatasample.pc

include BuildTools/Makemain.inc

test:
	@echo "No test available."
