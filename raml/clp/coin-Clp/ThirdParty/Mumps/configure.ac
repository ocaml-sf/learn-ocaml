# Copyright (C) 2007-2009 International Business Machines.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: configure.ac 3598 2016-07-20 15:31:35Z stefan $

# Author:  Andreas Waechter            IBM    2006-04-13

#############################################################################
#                       Names and other basic things                        #
#############################################################################

AC_PREREQ(2.59)

AC_INIT([ThirdPartyMumps],[1.5],[http://projects.coin-or.org/BuildTools/newticket])

AC_COPYRIGHT([
Copyright 2007-2009 International Business Machines and others.
All Rights Reserved.
This file is part of the open source package Coin which is distributed
under the Eclipse Public License.])

# List one file in the package so that the configure script can test
# whether the package is actually there
AC_CONFIG_SRCDIR(MUMPS/src/dmumps_part1.F)

# Where should everything be installed by default?  Here, we want it
# to be installed directly in 'bin', 'lib', 'include' subdirectories
# of the directory where configure is run.  The default would be
# /usr/local.
AC_PREFIX_DEFAULT([`pwd`])

AC_COIN_PROJECTDIR_INIT(Mumps)

#############################################################################
#                         Standard build tool stuff                         #
#############################################################################

# Get the system type
AC_CANONICAL_BUILD

#turn off warnings and pedantic mode since they could make trouble on some platforms,
#and we couldn't fix them anyway
coin_skip_warn_cflags=yes
coin_skip_warn_fflags=yes

# Check if user wants to produce debugging code
AC_COIN_DEBUG_COMPILE(Mumps)

# Get the name of the C compiler
AC_COIN_PROG_CC

# Get the name of the Fortran compiler and appropriate compiler options
AC_COIN_PROG_F77

# Find out how to call Fortran from C and determine Fortran runtime libraries
AC_COIN_F77_WRAPPERS

# Add FLIBS to MUMPS_PCLIBS, so that they get into the .pc files
MUMPS_PCLIBS="$FLIBS"

# We need to translate the result from autoconf to what MUMPS wants
case "$ac_cv_f77_mangling" in
  "lower case, no underscore, no extra underscore")
  ;;
  "lower case, underscore, no extra underscore")
  MY_DEFS="-DAdd_"
  ;;
  "lower case, no underscore, extra underscore")
  MY_DEFS="-DAdd_"
  ;;
  "lower case, underscore, extra underscore")
  MY_DEFS="-DAdd__"
  ;;
  "upper case, no underscore, no extra underscore")
  MY_DEFS="-DUPPER"
  ;;
  "upper case, no underscore, extra underscore")
  MY_DEFS="-DUPPER"
  ;;
  "upper case, underscore, no extra underscore")
  MY_DEFS="-DUPPER"
  ;;
  "upper case, underscore, extra underscore")
  MY_DEFS="-DUPPER"
  ;;
esac

# Mumps can make use of pthreads
# check for pthread.h header file and library
AC_ARG_ENABLE([pthread-mumps],
  [AC_HELP_STRING([--disable-pthread-mumps],[disable use of pthread library])],
  [enable_pthread_mumps=$enableval],
  [enable_pthread_mumps=no])
if test $enable_pthread_mumps = yes ; then
  AC_COIN_CHECK_HEADER([pthread.h],[],[enable_pthread_mumps=no])
fi
if test $enable_pthread_mumps = yes ; then
  AC_CHECK_LIB([pthread],[pthread_create],
    [MUMPS_PCLIBS="-lpthread $MUMPS_PCLIBS"],
    [enable_pthread_mumps=no])
fi
if test $enable_pthread_mumps = no ; then
  MY_DEFS="$MY_DEFS -DWITHOUT_PTHREAD=1"
fi

MY_DEFS="$MY_DEFS -DALLOW_NON_INIT -DMUMPS_ARITH=MUMPS_ARITH_d"
AC_SUBST(MY_DEFS)

# Initialize automake
AC_COIN_INIT_AUTO_TOOLS

AC_COIN_CHECK_PACKAGE_BLAS(MUMPS)
AC_COIN_CHECK_PACKAGE(Metis, [coinmetis], MUMPS)

if test "$coin_has_metis" = yes; then
  MY_DEFS="$MY_DEFS -Dmetis"
  MY_FDEFS="$MY_FDEFS -Dmetis"
fi

# Verify that we can compile Fortran 90 code
AC_MSG_CHECKING([whether we can compile Fortran 90 code])
AC_LANG_PUSH(Fortran 77)
AC_COMPILE_IFELSE([        MODULE BLA
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: BLUBB
      END MODULE BLA],[can_f90=yes],[can_f90=no])
AC_MSG_RESULT([$can_f90])
AC_LANG_POP(Fortran 77)
if test $can_f90 = no; then
  AC_MSG_ERROR([Your Fortran compiler can't compile Fortran 90, I cannot compile MUMPS])
fi

MUMPSDIR=MUMPS
MUMPSOBJDIR=`pwd`
MUMPSSRCDIR=$abs_source_dir/$coin_mumpsobjdir/$MUMPSDIR

AC_SUBST(MUMPSDIR)
AC_SUBST(MUMPSOBJDIR)
AC_SUBST(MUMPSSRCDIR)

# Fix Fortran preprocessor flags
case $F77 in
  *xlf*)
    fdefs=
    if test -n "$MY_FDEFS"; then
      for flag in $MY_FDEFS; do
        fdefs="$fdefs -WF,$flag"
      done
    fi
    MY_FDEFS="$fdefs"
    ;;
esac
AC_SUBST(MY_FDEFS)

AC_MSG_CHECKING([for additional preprocessor C flags])
AC_MSG_RESULT([$MY_DEFS])
AC_MSG_CHECKING([for additional preprocessor Fortran flags])
AC_MSG_RESULT([$MY_FDEFS])

AC_CONFIG_FILES([Makefile coinmumps.pc coinmumps-uninstalled.pc])

AC_COIN_FINALIZE
