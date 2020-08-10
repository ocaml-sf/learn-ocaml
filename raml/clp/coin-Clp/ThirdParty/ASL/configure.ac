# Copyright (C) 2007, 2009 International Business Machines.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: configure.ac 3726 2017-02-25 01:28:29Z stefan $

# Author:  Andreas Waechter            IBM    2006-04-13
# Revised: Lou Hafer		       SFU    2007-09-15

#############################################################################
#                       Names and other basic things                        #
#############################################################################

AC_PREREQ(2.59)

AC_INIT([ThirdPartyASL],[1.3],[http://projects.coin-or.org/BuildTools/newticket])

AC_COPYRIGHT([
Copyright 2006, 2007, 2009 International Business Machines and others.
All Rights Reserved.
This file is part of the open source package Coin which is distributed
under the Eclipse Public License.])

# List one file in the package so that the configure script can test
# whether the package is actually there
AC_CONFIG_SRCDIR(solvers/asl.h)

# Where should everything be installed by default?  Here, we want it
# to be installed directly in 'bin', 'lib', 'include' subdirectories
# of the directory where configure is run.  The default would be
# /usr/local.
AC_PREFIX_DEFAULT([`pwd`])

#############################################################################
#                         Standard build tool stuff                         #
#############################################################################

# Get the system type
AC_CANONICAL_BUILD

# A bit of initial setup.
AC_COIN_PROJECTDIR_INIT(ASL)

# Turn off warnings and pedantic mode since they could make trouble on some
# platforms, and we can't fix issues in third-party code.
coin_skip_warn_cflags=yes

# Check if user wants to produce debugging code
AC_COIN_DEBUG_COMPILE(ASL)

# We need egrep and ln -s somewhere
AC_PROG_EGREP
AC_PROG_LN_S

# Get the name of the C compiler and compiler flags
AC_COIN_PROG_CC

# Initialize automake and libtool
AC_COIN_INIT_AUTO_TOOLS

# Check if pkg-config is available (to setup addlibs file)
AC_COIN_HAS_PKGCONFIG

#############################################################################
#                          AMPL SOLVER LIBRARY                              #
#############################################################################

# Determine the name of the ASL library

# Set a substitutable variable with the library name, and determine the compilation style.
AC_SUBST(AMPLSOLVER_A)
if test "x$coin_cc_is_cl" = xtrue ; then
  AMPLSOLVER_A=libamplsolver.lib
else
  AMPLSOLVER_A=libamplsolver.a
fi

# Determine the host type to set linker options in the Makefile
AC_SUBST(HOST_TYPE)
case $host in
  *-cygwin* | *-mingw*)
    AM_CONDITIONAL(HOST_IS_CYGWIN, [test 0 = 0])
  ;;
  *)
    AM_CONDITIONAL(HOST_IS_CYGWIN, [test 0 = 1])
  ;;
esac

# Determine flags to be passed on the command line to (n)make. The main
# activity is to groom CFLAGS and set up ASLMAKEFLAGS to pass CFLAGS on the
# (n)make command line.
# NON_STDIO no longer appears in any source file; removed all mention.
# No_Control87 seems to be specific to DEC Alpha. Try leaving it undefined.

if test -z "$ASLMAKEFLAGS" ; then
  asladdcflags=
  case $build in
    *-linux*)
      case $CC in
        pgcc* | */pgcc* | xlc* | */xlc* | blrts_xlc* | */blrts_xlc*)
          asladdcflags="$asladdcflags -DNO_fpu_control";;
      esac
      ;;
  esac

# Various feature checks. Force the language to be C while we do this to avoid
# issues with C++ (re)declarations.
  AC_LANG_PUSH(C)

# The critical function for NO_RUSAGE is getrusage(). Use a standard test.

  AC_MSG_CHECKING([for getrusage])
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([[]],[[getrusage()]])],
    [AC_MSG_RESULT([yes])],
    [asladdcflags="$asladdcflags -DNO_RUSAGE"
     AC_MSG_RESULT([no])])

# Check for long long; we need to define NO_LONG_LONG if it's not present.  ASL
# makefile.u and makefile.vc both deal with this, in their own way.
# Unfortunately, when trying to build with cl under cygwin, the makefile.vc
# solution, comptry.bat, fails. To safely discard comptry.bat, we need to do
# this check. It's hardly likely that long long is missing on any modern
# system, but it's easy to check.
# Also, if it's present, then ASL uses strtoull, which is not supported on Windows systems.
# Instead, there should be a _strtoui64.

  AC_CHECK_TYPE([long long],[
     if test "x$coin_cc_is_cl" = xtrue ; then
       asladdcflags="$asladdcflags -Dstrtoull=_strtoui64"
     fi],
    [asladdcflags="$asladdcflags -DNO_LONG_LONG"],[])

# Check for ssize_t; we need to define NO_SSIZE_T if it's not present.

  AC_CHECK_TYPE([ssize_t],[],
    [asladdcflags="$asladdcflags -DNO_SSIZE_T"],[])

# Check for dlopen, ASL uses it to load userdefined function libraries
  AC_CHECK_LIB(dl,[dlopen],[ASL_PCLIBS="-ldl $ASL_PCLIBS"])

# Back to whatever language we were in.
  AC_LANG_POP(C)

# On BlueGene disable dynamic library loading
  if test x"$enable_bg" = xyes; then
    asladdcflags="$asladdcflags -DNO_FUNCADD"
  fi

# Groom CFLAGS before we merge it with asladdcflags.
# Remove -Werror if present, to make sure we are not interrupting compilation
# of ASL because of warning messages. The cl equivalent is -WX.
# SV: Our buildsystem should never add these flags to the CFLAGS, so why bother checking for them?
#     If the user added some, shouldn't we listen to it and not just remove them again?
  bla=
  if test "x$coin_cc_is_cl" = xtrue ; then
    warnToErr="-WX"
  else
    warnToErr="-Werror"
  fi
  for i in $CFLAGS; do
    if test "$i" != "$warnToErr"; then
      bla="$bla $i"
    fi
  done
  
# Add -fPIC -DPIC if we actually build shared libraries
  if test x$enable_shared != xno; then
    asladdcflags="$asladdcflags -fPIC -DPIC"
  fi
  
  ASLMAKEFLAGS="CFLAGS=\"$bla $asladdcflags\""

# Somehow, the flags specified here cause ASL to segfault on the AIX
# compiler, so take them out.
  case $CC in
    xlc* | */xlc* | mpxlc* | */mpxlc*)
      ASLMAKEFLAGS=`echo $ASLMAKEFLAGS | sed -e 's/-qtune=auto//g' -e 's/-qarch=auto//g' -e 's/-qcache=auto//g' -e 's/-qhot//g' -e 's/-O3/-O/g'`
      ;;
    pgcc* | */pgcc*)
      ASLMAKEFLAGS=`echo $ASLMAKEFLAGS | sed -e 's/-fast/-O1/g'`
      ;;
  esac

# Limit GNU make to one thread. But remember that if we're building with
# cl, ultimately we're going to use nmake, which knows nothing of this.
  if test "x$coin_cc_is_cl" != xtrue ; then
    rm -rf conftest.mak
    ($am_make -v > conftest.grp) 2>&5
    if $EGREP GNU conftest.grp >&5 ; then
      ASLMAKEFLAGS="-j 1 $ASLMAKEFLAGS"
    fi
    rm -f conftest.grp
  fi
fi
# End if-then to construct ASLMAKEFLAGS. Announce the result.

AC_MSG_NOTICE([Command line flags for (n)make \"$ASLMAKEFLAGS\".])
AC_ARG_VAR(ASLMAKEFLAGS, [Compilation flags necessary for compiling the AMPL solver library])

# generate the output files.

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([compile_MS_ASL],[chmod a+x compile_MS_ASL])
AC_CONFIG_FILES([compile_Unix_ASL],[chmod a+x compile_Unix_ASL])
AC_CONFIG_FILES([coinasl.pc coinasl-uninstalled.pc])

AC_COIN_CHECK_LIBM(ASL)
AC_SUBST(ASL_PCLIBS)

AC_COIN_FINALIZE
