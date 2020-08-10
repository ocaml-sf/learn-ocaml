# Copyright (C) 2009 International Business Machines.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id: configure.ac 4098 2020-01-31 10:32:04Z stefan $

# Author:  Andreas Waechter            IBM    2009-04-20

#############################################################################
#                       Names and other basic things                        #
#############################################################################

AC_PREREQ(2.59)

AC_INIT([ThirdPartyMetis],[1.3],[http://projects.coin-or.org/BuildTools/newticket])

AC_COPYRIGHT([
Copyright 2009 International Business Machines and others.
All Rights Reserved.
This file is part of the open source package Coin which is distributed
under the Eclipse Public License.])

# List one file in the package so that the configure script can test
# whether the package is actually there
AC_CONFIG_SRCDIR(metis-4.0/Lib/balance.c)

# Where should everything be installed by default?  Here, we want it
# to be installed directly in 'bin', 'lib', 'include' subdirectories
# of the directory where configure is run.  The default would be
# /usr/local.
AC_PREFIX_DEFAULT([`pwd`])

AC_COIN_PROJECTDIR_INIT(Metis)

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
AC_COIN_DEBUG_COMPILE(Metis)

# Get the name of the C compiler
AC_COIN_PROG_CC

# Check if pkg-config is available (to setup addlibs file)
AC_COIN_HAS_PKGCONFIG

# Initialize automake
AC_COIN_INIT_AUTO_TOOLS

# Metis depends on libm
AC_COIN_CHECK_LIBM(Metis)

# We may need to create a dummy header file for Windows
AC_CHECK_HEADER([strings.h], [], [touch strings.h])

# On some systems, there are no drand48 and srand48.
# By defining __VC__, we can make Metis use an alternative function
AC_CHECK_FUNC(drand48,,[CFLAGS="$CFLAGS -D__VC__"])

METISDIR=metis-4.0
AC_SUBST(METISDIR)

AC_CONFIG_FILES([Makefile coinmetis.pc coinmetis-uninstalled.pc])

AC_SUBST(METIS_PCLIBS)

AC_COIN_FINALIZE
