# Copyright (C) 2003, 2007 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = get.blas INSTALL.BLAS

lib_LTLIBRARIES = libcoinblas.la

libcoinblas_la_SOURCES = \
	dasum.f \
	daxpy.f \
	dcabs1.f \
	dcopy.f \
	ddot.f \
	dgbmv.f \
	dgemm.f \
	dgemv.f \
	dger.f \
	dnrm2.f \
	drot.f \
	drotg.f \
	drotm.f \
	drotmg.f \
	dsbmv.f \
	dscal.f \
	dsdot.f \
	dspmv.f \
	dspr2.f \
	dspr.f \
	dswap.f \
	dsymm.f \
	dsymv.f \
	dsyr2.f \
	dsyr2k.f \
	dsyr.f \
	dsyrk.f \
	dtbmv.f \
	dtbsv.f \
	dtpmv.f \
	dtpsv.f \
	dtrmm.f \
	dtrmv.f \
	dtrsm.f \
	dtrsv.f \
	idamax.f \
	isamax.f \
	izamax.f \
	lsame.f \
	sgemm.f \
	sgemv.f \
	sger.f \
	sscal.f \
	sswap.f \
	ssyr.f \
	strsm.f \
	xerbla.f \
	zaxpy.f \
	zcopy.f \
	zdscal.f \
	zdotc.f \
	zdotu.f \
	zgemm.f \
	zgemv.f \
	zgeru.f \
	zher.f \
	zscal.f \
	zswap.f \
	ztrsm.f

libcoinblas_la_LDFLAGS = $(LT_LDFLAGS)

pkgconfiglibdir = $(libdir)/pkgconfig
pkgconfiglib_DATA = coinblas.pc

test:
	@echo "No test available for Blas."

########################################################################
#                         Maintainer Stuff                             #
########################################################################

CLEANFILES =

# Files that are generated and should be cleaned with make distclean
DISTCLEANFILES = $(MAKEOKFILE)

include BuildTools/Makemain.inc
