# Copyright (C) 2006, 2007 International Business Machines.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

# Author:  Andreas Waechter            IBM    2006-04-13

#############################################################################
#                       Names and other basic things                        #
#############################################################################

AC_PREREQ(2.59)

AC_INIT([ThirdPartyLapack],[1.5],[http://projects.coin-or.org/BuildTools/newticket])

AC_COPYRIGHT([
Copyright 2006 International Business Machines and others.
All Rights Reserved.
This file is part of the open source package Coin which is distributed
under the Eclipse Public License.])

# List one file in the package so that the configure script can test
# whether the package is actually there
AC_CONFIG_SRCDIR(LAPACK/SRC/dlarf.f)

# Where should everything be installed by default?  Here, we want it
# to be installed directly in 'bin', 'lib', 'include' subdirectories
# of the directory where configure is run.  The default would be
# /usr/local.
AC_PREFIX_DEFAULT([`pwd`])

AC_COIN_PROJECTDIR_INIT(Lapack)

#############################################################################
#                         Standard build tool stuff                         #
#############################################################################

# Get the system type
AC_CANONICAL_BUILD

#turn off warnings and pedantic mode since they could make trouble on some platforms,
#and we couldn't fix them anyway
coin_skip_warn_fflags=yes

# Check if user wants to produce debugging code
AC_COIN_DEBUG_COMPILE(Lapack)

# Get the name of the Fortran compiler and appropriate compiler options
AC_COIN_PROG_F77

# Get the linker flags (FLIBS) that are required for linking against this library
AC_COIN_F77_WRAPPERS

# Check for Blas
AC_COIN_CHECK_PACKAGE_BLAS(LapackLib)
if test $coin_has_blas != yes; then
  AC_MSG_ERROR([Required package BLAS not found.])
fi

# FLIBS will be needed to link against this library, so add them to LAPACKLIB_PCLIBS
LAPACKLIB_PCLIBS="$LAPACKLIB_PCLIBS $FLIBS"

# Take care that optimization is disbled for DLAMCH.F 
AC_ARG_VAR(DLAMCH_FFLAGS,[Fortran compiler options for DLAMCH.F])
if test "x$DLAMCH_FFLAGS" = x ; then
  #disable optimiziation for dlamch.f by adding -Od or -O0 at end of FFLAGS (hope to overwrite previous -O? option, if present) 
  case $F77 in
    ifort* | */ifort* | IFORT* | */IFORT* )
      case $build in
        *-cygwin* | *-mingw*)
          DLAMCH_FFLAGS="$FFLAGS -Od"
          ;;
        *)
          DLAMCH_FFLAGS="$FFLAGS -O0"
          ;;
      esac
      ;;
    compile_f2c*)
      case "$CC" in
        cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
          DLAMCH_FFLAGS="$FFLAGS -Od"
        ;;
        *)
          DLAMCH_FFLAGS="$FFLAGS -O0"
        ;;
      esac
      ;;
    *)
      DLAMCH_FFLAGS="$FFLAGS -O0"
      ;;
  esac
fi
AC_SUBST(DLAMCH_FFLAGS)

# Initialize autotools
AC_COIN_INIT_AUTO_TOOLS

#############################################################################
#               Create links to the required source files                   #
#############################################################################

lapack_source_files='dbdsqr.f dgebal.f dgebak.f dgebd2.f dgebrd.f dgeev.f dgehd2.f dgehrd.f dgelq2.f dgelqf.f dgels.f dgeqr2.f dgeqrf.f dgesvd.f dgesv.f dgetf2.f dgetrf.f dgetri.f dgetrs.f dggbak.f dggbal.f dgghrd.f dggev.f dhgeqz.f dhseqr.f disnan.f dlabad.f dlabrd.f dlacpy.f dladiv.f dlaebz.f dlae2.f dlaev2.f dlaexc.f dlagtf.f dlagts.f dlag2.f dlahqr.f dlahr2.f dlaisnan.f dlaln2.f dlaneg.f dlange.f dlanhs.f dlanst.f dlansy.f dlanv2.f dlapy2.f dlapy3.f dlaqr0.f dlaqr1.f dlaqr2.f dlaqr3.f dlaqr4.f dlaqr5.f dlarf.f dlarfb.f dlarfg.f 
dlarft.f dlarfx.f dlarnv.f dlarra.f dlarrb.f dlarrc.f dlarrd.f dlarre.f dlarrf.f dlarrj.f dlarrk.f dlarrr.f dlarrv.f dlartg.f dlartv.f dlaruv.f dlar1v.f dlas2.f dlascl.f dlaset.f dlasq1.f dlasq2.f dlasq3.f dlasq4.f dlasq5.f dlasq6.f dlasr.f dlasrt.f dlaswp.f dlassq.f dlasv2.f dlasyf.f dlasy2.f dlatrd.f dorg2l.f dorg2r.f dorgbr.f dorghr.f dorglq.f dorgl2.f dorgql.f dorgqr.f dorgtr.f dorm2r.f dormbr.f dormhr.f dorml2.f dormlq.f dormql.f dormqr.f dormtr.f dorm2l.f dpotf2.f dpotrf.f dpotrs.f dppsv.f dpptrf.f dpptrs.f dstebz.f dstein.f
dstemr.f dsteqr.f dsterf.f dsyev.f dsyevr.f dsyevx.f dsytd2.f dsytf2.f dsytrd.f dsytrf.f dsytri.f dtgevc.f dtrevc.f dtrexc.f dtrti2.f dtrtri.f dtrtrs.f ieeeck.f iladlc.f iladlr.f ilaenv.f iparmq.f sgetf2.f sgetrf.f slaswp.f zgetf2.f zgetrf.f zlacgv.f zlacpy.f zlaev2.f zlaswp.f zpotf2.f zrot.f zsymv.f zsyr.f zsytri.f'
for file in $lapack_source_files; do
  AC_CONFIG_LINKS($file:LAPACK/SRC/$file)
done
lapack_source_files_in_install='dlamch.f slamch.f'
for file_in_install in $lapack_source_files_in_install; do
  AC_CONFIG_LINKS($file_in_install:LAPACK/INSTALL/$file_in_install)
done

AC_CONFIG_FILES([Makefile coinlapack.pc coinlapack-uninstalled.pc])

AC_COIN_FINALIZE
