# Copyright (C) 2006, 2008 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = get.lapack INSTALL.LAPACK

lib_LTLIBRARIES = libcoinlapack.la

LAPACK_FILES = \
	dbdsqr.f \
	dgebal.f \
	dgebak.f \
	dgebd2.f \
	dgebrd.f \
	dgeev.f \
	dgehd2.f \
	dgehrd.f \
	dgelq2.f \
	dgelqf.f \
	dgels.f \
	dgeqr2.f \
	dgeqrf.f \
	dgesvd.f \
	dgesv.f \
	dgetf2.f \
	dgetrf.f \
	dgetri.f \
	dgetrs.f \
	dggbak.f \
	dggbal.f \
	dgghrd.f \
	dggev.f \
	dhgeqz.f \
	dhseqr.f \
	disnan.f \
	dlabad.f \
	dlabrd.f \
	dlacpy.f \
	dladiv.f \
	dlaebz.f \
	dlae2.f \
	dlaev2.f \
	dlaexc.f \
	dlagtf.f \
	dlagts.f \
	dlag2.f \
	dlahqr.f \
	dlahr2.f \
	dlaisnan.f \
	dlaln2.f \
	dlamch.f \
	dlaneg.f \
	dlange.f \
	dlanhs.f \
	dlanst.f \
	dlansy.f \
	dlanv2.f \
	dlapy2.f \
	dlapy3.f \
	dlaqr0.f \
	dlaqr1.f \
	dlaqr2.f \
	dlaqr3.f \
	dlaqr4.f \
	dlaqr5.f \
	dlarf.f \
	dlarfb.f \
	dlarfg.f \
	dlarft.f \
	dlarfx.f \
	dlarnv.f \
	dlarra.f \
	dlarrb.f \
	dlarrc.f \
	dlarrd.f \
	dlarre.f \
	dlarrf.f \
	dlarrj.f \
	dlarrk.f \
	dlarrr.f \
	dlarrv.f \
	dlartg.f \
	dlartv.f \
	dlaruv.f \
	dlar1v.f \
	dlas2.f \
	dlascl.f \
	dlaset.f \
	dlasq1.f \
	dlasq2.f \
	dlasq3.f \
	dlasq4.f \
	dlasq5.f \
	dlasq6.f \
	dlasr.f \
	dlasrt.f \
	dlaswp.f \
	dlassq.f \
	dlasv2.f \
	dlasyf.f \
	dlasy2.f \
	dlatrd.f \
	dorg2l.f \
	dorg2r.f \
	dorgbr.f \
	dorghr.f \
	dorglq.f \
	dorgl2.f \
	dorgql.f \
	dorgqr.f \
	dorgtr.f \
	dorm2r.f \
	dormbr.f \
	dormhr.f \
	dorml2.f \
	dormlq.f \
	dormql.f \
	dormqr.f \
	dormtr.f \
	dorm2l.f \
	dpotf2.f \
	dpotrf.f \
	dpotrs.f \
	dppsv.f \
	dpptrf.f \
	dpptrs.f \
	dstebz.f \
	dstein.f \
	dstemr.f \
	dsteqr.f \
	dsterf.f \
	dsyev.f \
	dsyevr.f \
	dsyevx.f \
	dsytd2.f \
	dsytf2.f \
	dsytrd.f \
	dsytrf.f \
	dsytri.f \
	dtgevc.f \
	dtrevc.f \
	dtrexc.f \
	dtrti2.f \
	dtrtri.f \
	dtrtrs.f \
	ieeeck.f \
	iladlc.f \
	iladlr.f \
	ilaenv.f \
	iparmq.f \
	sgetf2.f \
	sgetrf.f \
	slamch.f \
	slaswp.f \
	zgetf2.f \
	zgetrf.f \
	zlacgv.f \
	zlacpy.f \
	zlaev2.f \
	zlaswp.f \
	zpotf2.f \
	zrot.f \
	zsymv.f \
	zsyr.f \
	zsytri.f

libcoinlapack_la_SOURCES = $(LAPACK_FILES)

if DEPENDENCY_LINKING
libcoinlapack_la_LIBADD = $(LAPACKLIB_LIBS)
endif

libcoinlapack_la_LDFLAGS = $(LT_LDFLAGS)

dlamch.o: dlamch.f
	$(F77) $(AM_FFLAGS) $(DLAMCH_FFLAGS) -c -o $@ $<

dlamch.obj: dlamch.f
	$(F77) $(AM_FFLAGS) $(DLAMCH_FFLAGS) -c -o $@ `$(CYGPATH_W) '$<'`

dlamch.lo: dlamch.f
	$(LIBTOOL) --tag=F77 --mode=compile $(F77) $(AM_FFLAGS) $(DLAMCH_FFLAGS) -c -o $@ $<

slamch.o: slamch.f
	$(F77) $(AM_FFLAGS) $(DLAMCH_FFLAGS) -c -o $@ $<

slamch.obj: slamch.f
	$(F77) $(AM_FFLAGS) $(DLAMCH_FFLAGS) -c -o $@ `$(CYGPATH_W) '$<'`

slamch.lo: slamch.f
	$(LIBTOOL) --tag=F77 --mode=compile $(F77) $(AM_FFLAGS) $(DLAMCH_FFLAGS) -c -o $@ $<

pkgconfiglibdir = $(libdir)/pkgconfig
pkgconfiglib_DATA = coinlapack.pc

test:
	@echo "No test available for Lapack."

########################################################################
#                         Maintainer Stuff                             #
########################################################################

CLEANFILES =

# Files that are generated and should be cleaned with make distclean
DISTCLEANFILES = $(LAPACK_FILES) $(MAKEOKFILE)

include BuildTools/Makemain.inc
