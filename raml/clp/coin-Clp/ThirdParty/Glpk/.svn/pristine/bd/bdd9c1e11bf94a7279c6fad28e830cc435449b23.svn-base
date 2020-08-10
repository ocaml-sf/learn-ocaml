# Copyright (C) 2007-2010 Stefan Vigerske and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

#############################################################################
#                       Names and other basic things                        #
#############################################################################

AC_PREREQ(2.59)

AC_INIT([ThirdPartyGlpk],[1.10],[http://projects.coin-or.org/BuildTools/newticket])

AC_COPYRIGHT([
Copyright 2007-2010 Stefan Vigerske and others.
All Rights Reserved.
This file is part of the open source package Coin which is distributed
under the Eclipse Public License.])

# List one file in the package so that the configure script can test
# whether the package is actually there
AC_CONFIG_SRCDIR(glpk/src/glpk.h)

# Where should everything be installed by default?  Here, we want it
# to be installed directly in 'bin', 'lib', 'include' subdirectories
# of the directory where configure is run.  The default would be
# /usr/local.
AC_PREFIX_DEFAULT([`pwd`])

AC_COIN_PROJECTDIR_INIT(Glpk)

#############################################################################
#                         Standard build tool stuff                         #
#############################################################################

# Get the system type
AC_CANONICAL_BUILD

#turn off warnings and pedantic mode since it seem to make trouble on solaris platform
#and we couldn't fix it anyway
coin_skip_warn_cflags=yes

# Check if user wants to produce debugging code
AC_COIN_DEBUG_COMPILE(Glpk)

# Get the name of the C compiler
AC_COIN_PROG_CC

# Check if pkg-config is available (to setup addlibs file)
AC_COIN_HAS_PKGCONFIG

# Initialize automake
AC_COIN_INIT_AUTO_TOOLS

###########################################################################
#                            check GLPK version                           #
###########################################################################

AC_MSG_CHECKING([GLPK version])
glpkmajorver=`awk '/define GLP_MAJOR_VERSION/ {print $3}' $srcdir/glpk/src/glpk.h`
glpkminorver=`awk '/define GLP_MINOR_VERSION/ {print $3}' $srcdir/glpk/src/glpk.h`
AC_MSG_RESULT([${glpkmajorver}.${glpkminorver}])
if test x$glpkmajorver != x4 || test x$glpkminorver != x48 ; then
  AC_MSG_ERROR(["Incorrect GLPK version. This build system is made for GLPK 4.48. Please update by rerunning get.GLPK"])
fi
AC_SUBST(glpkmajorver)
AC_SUBST(glpkminorver)

###########################################################################
#                 Other packages and C functions                          #
###########################################################################

AC_LANG_PUSH(C)

AC_COIN_CHECK_GMP(Glpk)
if test $coin_has_gmp = yes; then
  AC_DEFINE([HAVE_GMP],[1],[Define to 1 if GMP is available])
fi

AC_CHECK_FUNC([vsnprintf])

AC_LANG_POP(C)

AC_COIN_CHECK_LIBM(Glpk)

###########################################################################
#                     Database connectivity                               #
###########################################################################

#whether we have any means to load shared libraries
enable_dl=no

AC_LANG_PUSH(C)

AC_COIN_CHECK_HEADER([dlfcn.h],[enable_dl=yes])
if test "$enable_dl" = "yes"; then
  #if dlfcn.h is there, check also if libdl is there; if so, set corresponding define for GLPK 
  AC_CHECK_LIB(dl,[dlopen],[AC_DEFINE([HAVE_DLFCN],[1],[Define to 1 if dlfcn support is available])],[enable_dl=no])
  if test $enable_dl = yes; then
    GLPK_LIBS="-ldl $GLPK_LIBS"
    GLPK_PCLIBS="-ldl $GLPK_PCLIBS"
  fi
fi

if test "$enable_dl" = "no"; then
  AC_COIN_CHECK_HEADER([ltdl.h],[enable_dl=yes])
  if test "$enable_dl" = "yes"; then
    #if ltdl.h is there, check also if libltdl is there; if so, set corresponding define for GLPK 
    AC_CHECK_LIB(ldtl,[lt_dlopen],[AC_DEFINE([HAVE_LTDL],[1],[Define to 1 if ltdl support is available])],[enable_dl=no])
    if test $enable_dl = yes; then
      GLPK_LIBS="-lltdl $GLPK_LIBS"
      GLPK_PCLIBS="-lltdl $GLPK_PCLIBS"
    fi
  fi
fi

if test "$enable_dl" = "no"; then
  AC_COIN_CHECK_HEADER([windows.h],[enable_dl=yes])
  if test "$enable_dl" = "yes"; then
    AC_DEFINE([__WOE__],[1],[Define to 1 if windows dll support is available])
  fi
fi

AC_ARG_ENABLE(odbc,
  AC_HELP_STRING([--enable-odbc],[enable MathProg ODBC support [[default=auto]]]),
  [case $enableval in
     yes | unix)
       AC_COIN_CHECK_HEADER([sql.h],,[AC_MSG_ERROR(["need sql.h to enable ODBC support"])])
       enable_odbc="$enableval"
     ;;
     no)
       enable_odbc="no"
     ;;
     *)
       AC_MSG_ERROR([invalid value '$enableval' for --enable-odbc])
     ;;
   esac],
  [AC_COIN_CHECK_HEADER([sql.h],[enable_odbc=yes],[enable_odbc=no])]
)

AC_ARG_ENABLE(mysql,
  AC_HELP_STRING([--enable-mysql],[enable MathProg MySQL support [[default=auto]]]),
  [case $enableval in
     yes)
       AC_COIN_CHECK_HEADER([mysql.h mysql/mysql.h],,[AC_MSG_ERROR(["need mysql.h to enable MySQL support"])])
       enable_mysql="$enableval"
     ;;
     no)
       enable_mysql="no"
     ;;
     *)
       AC_MSG_ERROR([invalid value '$enableval' for --enable-mysql])
     ;;
   esac],
  [AC_COIN_CHECK_HEADER([mysql.h mysql/mysql.h],[enable_mysql=yes],[enable_mysql=no])]
)

AC_LANG_POP(C)

if test "$enable_dl" = "yes" && ( test "$enable_odbc" = "yes" || test "$enable_odbc" = "unix"); then
  AC_MSG_CHECKING([checking name of ODBC library])
  case $build in
    *-cygwin* |  *-mingw32*)
       case "$CC" in
         *cl* )
            odbclibname="odbc32.dll"
         ;;
         *) #FIXME: what are the correct library names for using ODBC with gnu compiler under Windows?
            if test "$enable_odbc" = "yes"; then
              odbclibname="libiodbc.dll"
            else
              odbclibname="libodbc.dll"
            fi
         ;;
       esac
    ;;
    *-mingw64*)
       case "$CC" in
         *cl* )
            odbclibname="odbc64.dll"
         ;;
         *) #FIXME: what are the correct library names for using ODBC with gnu compiler under Windows?
            if test "$enable_odbc" = "yes"; then
              odbclibname="libiodbc.dll"
            else
              odbclibname="libodbc.dll"
            fi
         ;;
       esac
    ;;
    *-darwin*)
       if test "$enable_odbc" = "yes"; then
          odbclibname="libiodbc.dylib"
       else
          odbclibname="libodbc.dylib"
       fi
    ;;
    *)
       if test "$enable_odbc" = "yes"; then
          odbclibname="libiodbc.so"
       else
          odbclibname="libodbc.so"
       fi
    ;;
  esac

  AC_DEFINE_UNQUOTED([ODBC_DLNAME], ["$odbclibname"], ["Set to the name of the ODBC dynamic library"])
  AC_MSG_RESULT([$odbclibname])
fi

if test "$enable_dl" = "yes" && test "$enable_mysql" = "yes" ; then
  AC_MSG_CHECKING([checking name of MySQL library])
  case $build in
    *-cygwin* |  *-mingw*)
       case "$CC" in
         *cl* )
            mysqllibname="libmysql.dll"
         ;;
         *) #FIXME: what is the correct library name for using MySQL with gnu compiler under Windows?
            mysqllibname="libmysqlclient.dll"
            CFLAGS="-I/usr/include/mysql $CFLAGS"
         ;;
       esac
    ;;
    *-darwin*)
       mysqllibname="libmysqlclient.dylib"
       CFLAGS="-I/usr/include/mysql $CFLAGS"
    ;;
    *)
       mysqllibname="libmysqlclient.so"
       CFLAGS="-I/usr/include/mysql $CFLAGS"
    ;;
  esac

  AC_DEFINE_UNQUOTED([MYSQL_DLNAME], ["$mysqllibname"], ["Set to the name of the MySQL dynamic library"])
  AC_MSG_RESULT([$mysqllibname])
fi

GLPKDIR=glpk
AC_SUBST(GLPKDIR)

AC_CONFIG_FILES([Makefile coinglpk.pc coinglpk-uninstalled.pc])
AC_CONFIG_HEADER([config_glpk.h])

AC_SUBST(GLPK_PCLIBS)

AC_COIN_FINALIZE
