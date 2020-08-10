# Copyright (C) 2006,2009 International Business Machines.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: configure.ac 2093 2019-02-22 19:53:14Z stefan $

# Author:  Andreas Waechter            IBM    2006-04-13

#############################################################################
#                       Names and other basic things                        #
#############################################################################

AC_PREREQ(2.59)

AC_INIT([CoinUtils],[2.10],[http://projects.coin-or.org/CoinUtils])

AC_COPYRIGHT([
Copyright 2006 International Business Machines and others.
All Rights Reserved.
This file is part of the open source package Coin which is distributed
under the Eclipse Public License.])

# List one file in the package so that the configure script can test
# whether the package is actually there
AC_CONFIG_SRCDIR(src/CoinError.cpp)

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

# If this project depends on external projects, the Externals file in
# the source root directory contains definition of where to find those
# externals.  The following macro ensures that those externals are
# retrieved by svn if they are not there yet.
AC_COIN_PROJECTDIR_INIT(CoinUtils)

# Check if user wants to produce debugging code
AC_COIN_DEBUG_COMPILE(CoinUtils)

# Get the name of the C++ compiler and appropriate compiler options
AC_COIN_PROG_CXX

# Get the name of the Fortran compiler and appropriate compiler options
AC_COIN_PROG_F77

# Find out how to call Fortran from C and determine Fortran runtime libraries
if test "x$F77" != xunavailable
then
  AC_COIN_F77_WRAPPERS
fi

# Initialize automake and libtool
AC_COIN_INIT_AUTO_TOOLS

############################################################################
#                   Stuff that we need for finite and isnan                #
############################################################################

AC_COIN_CHECK_ISFINITE
AC_COIN_CHECK_ISNAN

#############################################################################
#                         Thread configuration                              #
#############################################################################

# Define new options: 
#    --enable-coinutils-threads 
#    --enable-coinutils-mempool-override-new
#    --enable-coinutils-mempool-maxpooled

AC_LANG_PUSH(C++)

AC_ARG_ENABLE([coinutils-threads],
[AC_HELP_STRING([--enable-coinutils-threads],
                [enables compilation of thread aware CoinUtils (mempool so far)])])

if test "$enable_coinutils_threads" = yes; then
  # Define the preprocessor macro
  AC_DEFINE([COINUTILS_PTHREADS],[1],[Define to 1 if the thread aware version of CoinUtils should be compiled])
  AC_CHECK_LIB([rt],[clock_gettime],
               [COINUTILSLIB_LIBS="-lrt $COINUTILSLIB_LIBS"
                COINUTILSLIB_PCLIBS="-lrt $COINUTILSLIB_PCLIBS"],
	       [AC_MSG_ERROR([--enable-coinutils-threads selected, but -lrt unavailable])])
  AC_CHECK_LIB([pthread],[pthread_create],
               [COINUTILSLIB_LIBS="-lpthread $COINUTILSLIB_LIBS"
                COINUTILSLIB_PCLIBS="-lpthread $COINUTILSLIB_PCLIBS"],
	       [AC_MSG_ERROR([--enable-coinutils-threads selected, but -lpthreads unavailable])])
fi

AC_ARG_ENABLE([coinutils-mempool-override-new],
[AC_HELP_STRING([--enable-coinutils-mempool-override-new],
                [enables the CoinUtils mempool to override global new/delete])])

if test "$enable_coinutils_mempool_override_new" = yes; then
   AC_DEFINE([COINUTILS_MEMPOOL_OVERRIDE_NEW],[1],[Define to 1 CoinUtils should override global new/delete])
fi

AC_ARG_ENABLE([coinutils-mempool-maxpooled],
[AC_HELP_STRING([--enable-coinutils-mempool-maxpooled],
                [Specify the default maximum memory allocation size that is served by the memory pool. If negative (or 'no') then the memory pool is disabled completely. Otherwise its value can be overridden at runtime using the COINUTILS_MEMPOOL_MAXPOOLED environment variable.])])

if test "$enable_coinutils_mempool_maxpooled" = yes; then
   AC_DEFINE([COINUTILS_MEMPOOL_MAXPOOLED],[4096],[Default maximum pooled allocation size])
elif test "$enable_coinutils_mempool_maxpooled" = no; then
   AC_DEFINE([COINUTILS_MEMPOOL_MAXPOOLED],[-1],[Disable CoinUtils memory pool])
elif test x"$enable_coinutils_mempool_maxpooled" = x; then
   AC_DEFINE([COINUTILS_MEMPOOL_MAXPOOLED],[-1],[Disable CoinUtils memory pool])
else
   AC_DEFINE_UNQUOTED([COINUTILS_MEMPOOL_MAXPOOLED],${enable_coinutils_mempool_maxpooled},[Default maximum pooled allocation size])
fi

#############################################################################
#                       Finding certain integer types                       #
#############################################################################

AC_COIN_CHECK_CXX_CHEADER(inttypes)
AC_COIN_CHECK_CXX_CHEADER(stdint)

##### 64bit Integer types
# The problem here is that you can't extend `int64_t' to `unsigned int64_t'.
# So we need distinct CoinInt64 and CoinUInt64. It should be safe to assume
# uint64_t, given int64_t.
CoinInt64=
CoinUInt64=

# try int64_t
AC_CHECK_TYPE([int64_t],[CoinInt64=int64_t ; CoinUInt64=uint64_t],[],[
#ifdef HAVE_CINTTYPES
# include <cinttypes>
#else
# ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
# endif
#endif
])

# We need to use the C compiler in the AC_CHECK_SIZEOF since otherwise the
# MSCV compiler complains about redefinition of "exit". ac_cv_sizeof_<type>
# sometimes adds `^M' to the number, hence the check for `8?'.
AC_LANG_PUSH(C)
# try long long
if test x"$CoinInt64" = x; then
  AC_CHECK_SIZEOF([long long])
  case $ac_cv_sizeof_long_long in
    8 | 8?) CoinInt64="long long"
    	    CoinUInt64="unsigned long long"
	    ;;
  esac
fi

#try long
if test x"$CoinInt64" = x; then
  AC_CHECK_SIZEOF([long])
  case $ac_cv_sizeof_long in
    8 | 8?) CoinInt64="long"
    	    CoinUInt64="unsigned long"
	    ;;
  esac
fi

#try int
if test x"$CoinInt64" = x; then
  AC_CHECK_SIZEOF([int])
  case $ac_cv_sizeof_int in
    8 | 8?) CoinInt64="int"
    	    CoinUInt64="unsigned int"
	    ;;
  esac
fi
AC_LANG_POP(C)

if test x"$CoinInt64" = x; then
  AC_MSG_ERROR([Cannot find integer type with 64 bits])
fi
AC_DEFINE_UNQUOTED([COIN_INT64_T],[$CoinInt64],[Define to 64bit integer type])
AC_DEFINE_UNQUOTED([COIN_UINT64_T],[$CoinInt64],
		   [Define to 64bit unsigned integer type])


##### Integer type for Pointer
CoinIntPtr=

# try intptr_t
AC_CHECK_TYPE([intptr_t],[CoinIntPtr=intptr_t],[],[
#ifdef HAVE_CINTTYPES
# include <cinttypes>
#else
# ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
# endif
#endif
])

AC_LANG_PUSH(C)
# try long long
if test x"$CoinIntPtr" = x; then
  AC_CHECK_SIZEOF([int *])
  AC_CHECK_SIZEOF([long long])
  if test "$ac_cv_sizeof_long_long" = "$ac_cv_sizeof_int_p"; then
    CoinIntPtr="long long"
  fi
fi

# try long
if test x"$CoinIntPtr" = x; then
  AC_CHECK_SIZEOF([long])
  if test "$ac_cv_sizeof_long" = "$ac_cv_sizeof_int_p"; then
    CoinIntPtr="long"
  fi
fi

# try int
if test x"$CoinIntPtr" = x; then
  AC_CHECK_SIZEOF([int])
  if test "$ac_cv_sizeof_int" = "$ac_cv_sizeof_int_p"; then
    CoinIntPtr="int"
  fi
fi
AC_LANG_POP(C)

if test x"$CoinIntPtr" = x; then
  AC_MSG_ERROR([Cannot find integer type capturing pointer])
fi
AC_DEFINE_UNQUOTED([COIN_INTPTR_T],[$CoinIntPtr],[Define to integer type capturing pointer])

if test "x$ac_cv_header_stdint_h" = xyes ; then
  AC_DEFINE([COINUTILS_HAS_STDINT_H], [1], [Define to 1 if stdint.h is available for CoinUtils])
fi

if test "x$ac_cv_header_cstdint" = xyes ; then
  AC_DEFINE([COINUTILS_HAS_CSTDINT], [1], [Define to 1 if cstdint is available for CoinUtils])
fi

#############################################################################
#                           Check whether we have windows.h                 #
#############################################################################
AC_CHECK_HEADERS([windows.h])

#############################################################################
#                           Check whether we have endian.h                  #
#############################################################################
AC_CHECK_HEADERS([endian.h])

#############################################################################
#                             COIN-OR components                            #
#############################################################################

AC_COIN_CHECK_LIBM(CoinUtilsLib)

AC_COIN_CHECK_PACKAGE_BLAS(CoinUtilsLib)
AC_COIN_CHECK_PACKAGE_LAPACK(CoinUtilsLib)
AC_COIN_CHECK_PACKAGE(Glpk,   [coinglpk], [CoinUtilsLib])
AC_COIN_CHECK_PACKAGE(Sample, [coindatasample])
AC_COIN_CHECK_PACKAGE(Netlib, [coindatanetlib])

AC_COIN_CHECK_GNU_ZLIB(CoinUtilsLib)
AC_COIN_CHECK_GNU_BZLIB(CoinUtilsLib)
AC_COIN_CHECK_GNU_READLINE(CoinUtilsLib)

AC_COIN_VPATH_LINK(test/plan.mod)

#############################################################################
#                  Check for doxygen                                        #
#############################################################################

AC_COIN_DOXYGEN()

##############################################################################
#                   Finishing up by writing all the output                   #
##############################################################################

# Here list all the files that configure should create (except for the
# configuration header file)
AC_CONFIG_FILES([Makefile
                 src/Makefile
                 test/Makefile
                 coinutils.pc coinutils-uninstalled.pc])
AC_CONFIG_FILES([doxydoc/doxygen.conf])

# Here put the location and name of the configuration header file
AC_CONFIG_HEADER([src/config.h src/config_coinutils.h])

# Finally, we let configure write all the output...
AC_COIN_FINALIZE
