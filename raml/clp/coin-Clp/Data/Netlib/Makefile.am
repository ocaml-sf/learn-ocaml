# Copyright (C) 2006 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: Makefile.am 329 2012-11-21 19:02:51Z stefan $

# Author:  Andreas Waechter           IBM    2006-04-13

AUTOMAKE_OPTIONS = foreign

########################################################################
#                List files that should be distributed                 #
########################################################################

EXTRA_DIST = $(EXAMPLE_FILES)

datacoindir = $(datadir)/coin/Data/Netlib

########################################################################
#           Extra targets for uncompressing the .gz files              #
########################################################################

# If configure has been run with --enable-gnu-packages, we assume that
# compressed files can be read and that we don't have to uncompress.
# Otherwise, we uncompress the files
if COIN_HAS_ZLIB
uncompress: skipunzip
datacoin_DATA = $(EXAMPLE_FILES)
else
uncompress: unzip
datacoin_DATA = $(EXAMPLE_UNCOMPRESSED_FILES)
endif

# This target still leaves the original compressed files around
unzip: $(EXAMPLE_UNCOMPRESSED_FILES)

$(EXAMPLE_UNCOMPRESSED_FILES):
	gzip -d -c $@.gz > $@

skipunzip:
	echo "Skipping decompression (package compiled with --enable-gnu-packages)"

pkgconfiglibdir = $(libdir)/pkgconfig
pkgconfiglib_DATA = coindatanetlib.pc

.PHONY: uncompress unzip skipunzip

test:
	@echo "No test available."

########################################################################
#                  List files that should be cleaned                   #
########################################################################

CLEANFILES = $(EXAMPLE_UNCOMPRESSED_FILES)

DISTCLEANFILES = $(EXAMPLE_CLEAN_FILES)

include BuildTools/Makemain.inc
