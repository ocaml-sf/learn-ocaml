# Copyright (C) 2006, 2009 International Business Machines.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.
#
## $Id$
#
# Author: Andreas Wachter    IBM      2006-04-14

# This file defines the common autoconf macros for COIN
#

# Check requirements
AC_PREREQ(2.59)

###########################################################################
#                           coin_foreach_w                                #
###########################################################################

# the autoconf version used by COIN-OR is so old that it does not have the M4 macro foreach_w.
# thus, we define our own one which applies the foreach macro to the same arguments but with
# all spaces replaced by ',' in the second argument.
# further, we run the loop only if the second argument is not empty
AC_DEFUN([coin_foreach_w], [m4_ifval([$2], [m4_foreach([$1], [m4_bpatsubsts([$2],[[ 	]+],[, ])], [$3])])])

###########################################################################
#                            COIN_CHECK_FILE                              #
###########################################################################

# A simple replacement for AC_CHECK_FILE that works for cross compilation

AC_DEFUN([AC_COIN_CHECK_FILE],
[if test -r $1; then
  $2
  :
else
  $3
  :
fi
])

###########################################################################
#                           COIN_CHECK_VPATH                              #
###########################################################################

# This macro sets the variable coin_vpath_config to true if this is a
# VPATH configuration, otherwise it sets it to false.

AC_DEFUN([AC_COIN_CHECK_VPATH],
[
AC_MSG_CHECKING(whether this is a VPATH configuration)
if test `cd $srcdir; pwd` != `pwd`; then
  coin_vpath_config=yes;
else
  coin_vpath_config=no;
fi
AC_MSG_RESULT($coin_vpath_config)
]) # AC_COIN_CHECK_VPATH


###########################################################################
#                          COIN_PROJECTVERSION                            #
###########################################################################

# This macro is used by COIN_PROJECTDIR_INIT and sets up variables related
# to versioning numbers of the project.

AC_DEFUN([AC_COIN_PROJECTVERSION],
[m4_ifvaln([$1],[
  AC_DEFINE_UNQUOTED(m4_toupper($1_VERSION), ["$PACKAGE_VERSION"],[Version number of project])
  
  [coin_majorver=`echo $PACKAGE_VERSION | sed -n -e 's/^\([0-9]*\).*/\1/gp'`]
  [coin_minorver=`echo $PACKAGE_VERSION | sed -n -e 's/^[0-9]*\.\([0-9]*\).*/\1/gp'`]
  [coin_releasever=`echo $PACKAGE_VERSION | sed -n -e 's/^[0-9]*\.[0-9]*\.\([0-9]*\).*/\1/gp'`]
  if test "x$coin_majorver" = x ; then coin_majorver=9999 ; fi
  if test "x$coin_minorver" = x ; then coin_minorver=9999 ; fi
  if test "x$coin_releasever" = x ; then coin_releasever=9999 ; fi
  AC_DEFINE_UNQUOTED(m4_toupper($1_VERSION_MAJOR),   [$coin_majorver],   [Major Version number of project])
  AC_DEFINE_UNQUOTED(m4_toupper($1_VERSION_MINOR),   [$coin_minorver],   [Minor Version number of project])
  AC_DEFINE_UNQUOTED(m4_toupper($1_VERSION_RELEASE), [$coin_releasever], [Release Version number of project])

  # We use the following variable to have a string with the upper case
  # version of the project name
  COIN_PRJCT=m4_toupper($1)

  # Set the project's SVN revision number. The complicated sed expression
  # (made worse by quadrigraphs) ensures that things like 4123:4168MS end up
  # as a single number.
  AC_CHECK_PROG([have_svnversion],[svnversion],[yes],[no])
  if test "x$have_svnversion" = xyes; then
    AC_SUBST(m4_toupper($1_SVN_REV))
    svn_rev_tmp=`LANG=en_EN svnversion $srcdir 2>/dev/null`
    if test "x$svn_rev_tmp" != xexported -a "x$svn_rev_tmp" != x -a "x$svn_rev_tmp" != "xUnversioned directory"; then
      m4_toupper($1_SVN_REV)=`echo $svn_rev_tmp | sed -n -e 's/^@<:@0-9@:>@*://' -e 's/\(@<:@0-9@:>@\)@<:@^0-9@:>@*$/\1/p'`
      AC_DEFINE_UNQUOTED(m4_toupper($1_SVN_REV), $m4_toupper($1_SVN_REV), [SVN revision number of project])
    fi
  fi
 ])
 
# Capture libtool library version, if given.
 m4_ifval([$2],[coin_libversion=$2],[])
])

###########################################################################
#                         COIN_PROJECTDIR_INIT                            #
###########################################################################

# This macro does everything that is required in the early part in the
# configure script, such as defining a few variables.  This should only be used
# in the main directory of a project directory (the one which holds the src
# directory for the project). The first parameter is the project name. The
# second (optional) is the libtool library version (important for releases,
# less so for stable or trunk).

AC_DEFUN([AC_COIN_PROJECTDIR_INIT],
[
# As backup, we make sure we don't loose an FLIBS if it has been set
# by the user
save_FLIBS="$FLIBS"

# A useful makefile conditional that is always false
AM_CONDITIONAL(ALWAYS_FALSE, false)

# We set the following variable so that we know later in AC_COIN_FINALIZE
# that we are in a project main directory
coin_projectdir=yes

# Set the project's version numbers
AC_COIN_PROJECTVERSION($1, $2)
]) # AC_COIN_PROJECTDIR_INIT

###########################################################################
#                          COIN_DEBUG_COMPILE                             #
###########################################################################

# enable the configure flags --enable-debug and --enable-debug-prjct
# (where prcjt is the name of the project in lower case) and set the
# variable coin_debug_compile to true or false This is used by
# COIN_PROG_CXX, COIN_PROG_CC and COIN_PROG_F77 to determine the
# compilation flags.  This macro also makes the switches
# --with-prjct-verbosity and --with-prjct-checklevel available, which
# define the preprocessor macros COIN_PRJCT_VERBOSITY and
# COIN_PRJCT_CHECKLEVEL to the specified value (default is 0).
#
# The project specific flags are only made available, if one gives the
# name of the project as first argument to this macro.

AC_DEFUN([AC_COIN_DEBUG_COMPILE],
[AC_BEFORE([$0],[AC_COIN_PROG_CXX])dnl
AC_BEFORE([$0],[AC_COIN_PROG_CC])dnl
AC_BEFORE([$0],[AC_COIN_PROG_F77])dnl

AC_MSG_CHECKING([whether we want to compile in debug mode])

AC_ARG_ENABLE([debug],
[AC_HELP_STRING([--enable-debug],
                [compile all projects with debug options tests (implies --disable-shared)])],
[case "${enableval}" in
   yes) coin_debug_compile=true
        if test "${enable_shared+set}" = set; then :; else
          enable_shared=no
        fi 
        ;;
   no)  coin_debug_compile=false
        ;;
   *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug)
        ;;
esac],
[coin_debug_compile=false])

m4_ifvaln([$1],
[AC_ARG_ENABLE(debug-m4_tolower($1),
 [AC_HELP_STRING([--enable-debug-m4_tolower($1)],
                 [compile project $1 with debug compiler flags])],
 [case "${enableval}" in
    yes) coin_debug_compile=true
         ;;
    no)  coin_debug_compile=false
         ;;
    *) AC_MSG_ERROR(bad value ${enableval} for --enable-debug-m4_tolower($1))
         ;;
 esac],[:])
]) # m4_ifvaln([$1],

if test $coin_debug_compile = true; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi

m4_ifvaln([$1],
[AC_ARG_WITH(m4_tolower($1)-verbosity,
             AC_HELP_STRING([--with-m4_tolower($1)-verbosity],
                            [specify the debug verbosity level for project $1]),
             [if test "$withval" = yes; then
                withval=1
              fi
              m4_tolower(coin_$1_verbosity)=$withval],
             [m4_tolower(coin_$1_verbosity)=0])
 AC_DEFINE_UNQUOTED(m4_toupper(COIN_$1_VERBOSITY),
                    m4_tolower($coin_$1_verbosity),
                    [Define to the debug verbosity level (0 is no output)])

 AC_ARG_WITH(m4_tolower($1)-checklevel,
             AC_HELP_STRING([--with-m4_tolower($1)-checklevel],
                            [specify the sanity check level for project $1]),
             [if test "$withval" = yes; then
                withval=1
              fi
              m4_tolower(coin_$1_checklevel)=$withval],
             [m4_tolower(coin_$1_checklevel)=0])
 AC_DEFINE_UNQUOTED(m4_toupper(COIN_$1_CHECKLEVEL),
                    m4_tolower($coin_$1_checklevel),
                    [Define to the debug sanity check level (0 is no test)])
]) # m4_ifvaln([$1],
 
]) # AC_COIN_DEBUG_COMPILE

###########################################################################
#                          COIN_ENABLE_MSVC                               # 
###########################################################################

# This macro is invoked by any PROG_compiler macro to establish the
# --enable-msvc option.
# The job of this macro is to make sure the option is correct and
# to set enable_msvc. Legal values are yes and no (disabled).
# If set, assume the presence of cl/link. It's the user's responsibility to
# make sure their PATH is correct. In particular, that MSVC link is found
# and not cygwin link (we want to do code linking, not file linking).
# It's the responsibility of individual PROG_compiler macros to ensure that
# they correctly set the compiler search list and preprocess, compile, and
# link flags. This is tied to compiler setup because in practice invocations
# of the preprocessor and linker are made through the compiler.
# For backward compatibility, we also check for --enable-doscompile=msvc.

AC_DEFUN([AC_COIN_ENABLE_MSVC],
[AC_REQUIRE([AC_CANONICAL_BUILD])

  # for backward compatibility
  AC_ARG_ENABLE([doscompile],,[enable_doscompile=$enableval],[enable_doscompile=no])
  
  AC_ARG_ENABLE([msvc],
    [AC_HELP_STRING([--enable-msvc],
                    [Prefer (i)cl/ifort/link over GNU on MinGW/Cygwin.])],
    [enable_msvc=$enableval],
    [enable_msvc=no
     if test "$enable_doscompile" = msvc ; then
       enable_msvc=yes
     elif test "$enable_doscompile" != no ; then
       AC_MSG_ERROR([--enable-doscompile=$enable_doscompile not supported anymore.])
     fi
    ])
    
  if test "$enable_msvc" = MD; then
    enable_shared=yes
    enable_msvc=yes
  fi

  if test "$enable_msvc" = yes; then
    case $build in
      *-cygwin* | *-mingw*) ;;
      *) AC_MSG_ERROR([--enable-msvc option makes sense only under Cygwin or MinGW]) ;;
    esac
  fi
])

###########################################################################
#                             COIN_PROG_CXX                               #
###########################################################################

# Find the compile command by running AC_PROG_CXX (with compiler names for
# different operating systems) and put it into CXX (unless it was given by the
# user). Then find an appropriate value for CXXFLAGS. If either of CXXFLAGS or
# PRJCT_CXXFLAGS is defined, that value is used (replace PRJCT with the upper
# case name of this project).  It is possible to provide additional -D flags
# in the variable CXXDEFS, and additional compilation flags with ADD_CXXFLAGS.

AC_DEFUN([AC_COIN_PROG_CXX],
[AC_REQUIRE([AC_COIN_PROG_CC]) #Let's try if that overcomes configuration problem with VC++ 6.0
AC_REQUIRE([AC_COIN_ENABLE_MSVC])
AC_LANG_PUSH(C++)

AC_ARG_VAR(CXXDEFS,[Additional -D flags to be used when compiling C++ code.])
AC_ARG_VAR(ADD_CXXFLAGS,[Additional C++ compiler options])
AC_ARG_VAR(DBG_CXXFLAGS,[Debug C++ compiler options])
AC_ARG_VAR(OPT_CXXFLAGS,[Optimize C++ compiler options])

coin_has_cxx=yes

save_cxxflags="$CXXFLAGS"
# For *-*-solaris*, promote Studio/Workshop compiler to front of list.
case $build in
  *-cygwin* | *-mingw*)
      if test "$enable_msvc" = yes ; then
         comps="icl cl g++"
      else
         comps="g++ icl cl"
      fi ;;
  *-*-solaris*)
  	     comps="CC xlC_r aCC g++ c++ pgCC icpc gpp cxx cc++ cl FCC KCC RCC" ;;
  *-darwin*) comps="clang++ g++ c++ CC" ;;
  *-linux-gnu*)
             comps="g++ c++ pgCC icpc gpp cxx cc++ cl FCC KCC RCC xlC_r aCC CC" ;;
          *) comps="xlC_r aCC CC g++ c++ pgCC icpc gpp cxx cc++ cl FCC KCC RCC" ;;
esac

# We delete the cached value, since the test might not have been
# performed with our choice of compilers earlier
$as_unset ac_cv_prog_CXX || test "${ac_cv_prog_CXX+set}" != set || { ac_cv_prog_CXX=; export ac_cv_prog_CXX; }
# AC_MSG_NOTICE([C++ compiler candidates: $comps])
AC_PROG_CXX([$comps])

#AC_PROG_CXX sets CXX to g++ if it cannot find a working C++ compiler
#thus, we test here whether $CXX is actually working 
AC_LANG_PUSH(C++)
AC_MSG_CHECKING([whether C++ compiler $CXX works]);
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM(, [int i=0;])],
  [AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)
   AC_MSG_ERROR(failed to find a C++ compiler or C++ compiler $CXX does not work)]
)
AC_LANG_POP(C++)

coin_cxx_is_cl=false
# It seems that we need to cleanup something here for the Windows
case "$CXX" in
  clang* | */clang*) ;;
  cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
    sed -e 's/^void exit (int);//' confdefs.h >> confdefs.hh
    mv confdefs.hh confdefs.h
    coin_cxx_is_cl=true
    ;;
esac

# add automake conditional so we can recognize cl compiler in makefile
AM_CONDITIONAL(COIN_CXX_IS_CL, [test $coin_cxx_is_cl = true])

# Autoconf incorrectly concludes that cl recognises -g. It doesn't.
case "$CXX" in
  clang* ) ;;
  cl* | */cl* | CL* | */CL* )
    if test "$ac_cv_prog_cxx_g" = yes ; then
      ac_cv_prog_cxx_g=no
      AC_MSG_NOTICE([Overruling autoconf; cl does not recognise -g.])
    fi ;;
  * )
    if test x"$CYGPATH_W" = x ; then
      CYGPATH_W=echo
    fi
    ;;
esac
CXXFLAGS="$save_cxxflags"

# Check if a project specific CXXFLAGS variable has been set
if test x$COIN_PRJCT != x; then
  eval coin_tmp=\${${COIN_PRJCT}_CXXFLAGS+set}
  if test x$coin_tmp = xset; then
    eval CXXFLAGS=\${${COIN_PRJCT}_CXXFLAGS}
  fi
fi

if test x"$CXXFLAGS" = x; then

# ToDo decide whether we want -DNDEBUG for optimization
  coin_add_cxxflags=
  coin_opt_cxxflags=
  coin_dbg_cxxflags=
  coin_warn_cxxflags=

  if test "$GXX" = "yes"; then
    case "$CXX" in
      icpc* | */icpc*)
        ;;
      *)
# ToDo decide about unroll-loops
        coin_opt_cxxflags="-O3"
        coin_add_cxxflags="-pipe"
        coin_dbg_cxxflags="-g -O0"
        coin_warn_cxxflags="-Wparentheses -Wreturn-type -Wcast-qual -Wall -Wpointer-arith -Wwrite-strings -Wconversion -Wno-unknown-pragmas -Wno-long-long"
    esac
  fi

# Note that we do not need to cover GCC in the following tests.

  if test -z "$coin_opt_cxxflags"; then
    case $build in
      *-cygwin* | *-mingw*)
        case "$CXX" in
          clang* ) ;;
          cl* | */cl* | CL* | */CL*)
            # The MT and MTd options are mutually exclusive
            if test "$coin_disable_shared" = yes || test "$enable_shared" = yes ; then
               coin_opt_cxxflags='-MD -O2'
               coin_dbg_cxxflags='-MDd'
            else
               coin_opt_cxxflags='-MT -O2'
               coin_dbg_cxxflags='-MTd'
            fi
            coin_add_cxxflags='-nologo -EHsc -GR -wd4996 -D_CRT_SECURE_NO_DEPRECATE'
            ;;
          icl* | */icl* | ICL* | */ICL*)
          # The MT and MTd options are mutually exclusive
            if test "$coin_disable_shared" = yes || test "$enable_shared" = yes ; then
              coin_opt_cxxflags='-MD -Ox'
              coin_dbg_cxxflags='-MDd -debug'
            else
              coin_opt_cxxflags='-MT -Ox'
              coin_dbg_cxxflags='-MTd -debug'
            fi
            coin_add_cxxflags='-nologo -EHsc -GR -D_CRT_SECURE_NO_DEPRECATE'
            ;;
        esac
        ;;
      *-linux-*)
        case "$CXX" in
          icpc* | */icpc*)
            coin_opt_cxxflags="-O3 -ip -mp1"
            coin_add_cxxflags=""
            coin_dbg_cxxflags="-g"
            # Check if -i_dynamic is necessary (for new glibc library)
            CXXFLAGS=
            AC_TRY_LINK(,[int i=0; i++;],[],
                        [coin_add_cxxflags="-i_dynamic $coin_add_cxxflags"])
            ;;
          pgCC* | */pgCC*)
            coin_opt_cxxflags="-fast"
            coin_add_cxxflags="-Kieee -pc 64"
            coin_dbg_cxxflags="-g"
            ;;
        esac
        ;;
      *-ibm-*)
        case "$CXX" in
          xlC* | */xlC* | mpxlC* | */mpxlC*)
            coin_opt_cxxflags="-O -qarch=auto -qcache=auto -qtune=auto -qmaxmem=-1"
            coin_add_cxxflags="-bmaxdata:0x80000000 -qrtti=dyna -qsuppress=1500-036 -qsuppress=1500-029 -qsourcetype=c++"
            coin_dbg_cxxflags="-g"
            ;;
        esac
        ;;
      *-hp-*)
        case "$CXX" in
          aCC* | */aCC* )
            coin_opt_cxxflags="-O"
            coin_add_cxxflags="-AA"
            coin_dbg_cxxflags="-g"
            ;;
        esac
        ;;
      *-*-solaris*)
          coin_opt_cxxflags="-O4"
          coin_dbg_cxxflags="-g"
        ;;
    esac
  fi

# Generic flag settings. If these don't work, add a case above.

  if test "$ac_cv_prog_cxx_g" = yes && test -z "$coin_dbg_cxxflags" ; then
    coin_dbg_cxxflags="-g"
  fi

  if test -z "$coin_opt_cxxflags"; then
    # Try if -O option works if nothing else is set
    CXXFLAGS=-O
    AC_TRY_LINK([],[int i=0; i++;],[coin_opt_cxxflags="-O"])
  fi

  # if PM doesn't want the warning messages, take them out
  if test x"$coin_skip_warn_cxxflags" = xyes; then
    coin_warn_cxxflags=
  fi

# Do final setup of flags based on values determined above.

  if test x${DBG_CXXFLAGS+set} != xset; then
    DBG_CXXFLAGS="$coin_dbg_cxxflags $coin_add_cxxflags $coin_warn_cxxflags"
  fi
  if test x${OPT_CXXFLAGS+set} != xset; then
    OPT_CXXFLAGS="$coin_opt_cxxflags $coin_add_cxxflags -DNDEBUG $coin_warn_cxxflags"
  fi

  DBG_CXXFLAGS="$DBG_CXXFLAGS $ADD_CXXFLAGS $CXXDEFS"
  OPT_CXXFLAGS="$OPT_CXXFLAGS $ADD_CXXFLAGS $CXXDEFS"

  if test "$coin_debug_compile" = "true"; then
    CXXFLAGS="$DBG_CXXFLAGS"
  else
    CXXFLAGS="$OPT_CXXFLAGS"
  fi

# Handle the case where CXXFLAGS was set externally.
else
  CXXFLAGS="$CXXFLAGS $ADD_CXXFLAGS $CXXDEFS"
  if test x${DBG_CXXFLAGS+set} != xset; then
    DBG_CXXFLAGS="$CXXFLAGS"
  fi
  if test x${OPT_CXXFLAGS+set} != xset; then
    OPT_CXXFLAGS="$CXXFLAGS"
  fi
fi

# add -DPROJECT_BUILD to signal compiler preprocessor which config header file to include
if test x$COIN_PRJCT != x; then
  CXXFLAGS="$CXXFLAGS -D${COIN_PRJCT}_BUILD"
fi

# Try if CXXFLAGS works
save_CXXFLAGS="$CXXFLAGS"
AC_TRY_LINK([],[int i=0; i++;],[],[CXXFLAGS=])
if test -z "$CXXFLAGS"; then
  AC_MSG_WARN([The flags CXXFLAGS="$save_CXXFLAGS" do not work.  I will now just try '-O', but you might want to set CXXFLAGS manually.])
  CXXFLAGS='-O'
  AC_TRY_LINK([],[int i=0; i++;],[],[CXXFLAGS=])
  if test -z "$CXXFLAGS"; then
    AC_MSG_WARN([This value for CXXFLAGS does not work.  I will continue with empty CXXFLAGS, but you might want to set CXXFLAGS manually.])
  fi
fi

AC_MSG_NOTICE([C++ compiler options are: $CXXFLAGS])

AC_ARG_VAR(MPICXX,[C++ MPI Compiler])
if test x"$MPICXX" = x; then :; else
  AC_MSG_NOTICE([Will use MPI C++ compiler $MPICXX])
  CXX="$MPICXX"
fi

# correct the LD variable in a build with MS or Intel-windows compiler
case "$CXX" in
  clang* ) ;;
  cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
    LD=link
    ;;
esac

AC_LANG_POP(C++)
]) # AC_COIN_PROG_CXX


###########################################################################
#                             COIN_CXXLIBS                                #
###########################################################################

# Determine the C++ runtime libraries required for linking a C++ library
# with a Fortran or C compiler.  The result is available in CXXLIBS.

AC_DEFUN([AC_COIN_CXXLIBS],
[AC_REQUIRE([AC_PROG_CXX])dnl
AC_LANG_PUSH(C++)
AC_ARG_VAR(CXXLIBS,[Libraries necessary for linking C++ code with Fortran compiler])
if test -z "$CXXLIBS"; then
  if test "$GXX" = "yes"; then
    case "$CXX" in
      icpc* | */icpc*)
        CXXLIBS="-lstdc++"
        ;;
      *)
        # clang uses libc++ as the default standard C++ library, not libstdc++
        # this test is supposed to recognize whether the compiler is clang
        # 
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <ciso646>]], [[
#ifndef _LIBCPP_VERSION
       choke me
#endif
          ]])],
          [CXXLIBS="-lc++"],
          [CXXLIBS="-lstdc++ -lm"])
        ;;
    esac
  else
    case $build in
     *-mingw32 | *-cygwin* )
      case "$CXX" in
      clang* ) ;;
      cl* | */cl* | CL* | */CL*)
        CXXLIBS=nothing;;
      esac;;
     *-linux-*)
      case "$CXX" in
      icpc* | */icpc*)
        CXXLIBS="-lstdc++"
             ;;
      pgCC* | */pgCC*)
        CXXLIBS="-lstd -lC -lc"
             ;;
      esac;;
    *-ibm-*)
      CXXLIBS="-lC -lc"
      ;;
    *-hp-*)
      CXXLIBS="-L/opt/aCC/lib -l++ -lstd_v2 -lCsup_v2 -lm -lcl -lc"
      ;;
    *-*-solaris*)
      CXXLIBS="-lCstd -lCrun"
    esac
  fi
fi
if test -z "$CXXLIBS"; then
  AC_MSG_WARN([Could not automatically determine CXXLIBS (C++ link libraries; necessary if main program is in Fortran or C).])
else
  AC_MSG_NOTICE([Assuming that CXXLIBS is \"$CXXLIBS\".])
fi
if test x"$CXXLIBS" = xnothing; then
  CXXLIBS=
fi
AC_LANG_POP(C++)
]) # AC_COIN_CXXLIBS

###########################################################################
#                           COIN_CHECK_HEADER                             #
###########################################################################

# This macro checks for a header file, but it does so without the
# standard header.  This avoids warning messages like:
#
# configure: WARNING: dlfcn.h: present but cannot be compiled
# configure: WARNING: dlfcn.h:     check for missing prerequisite headers?
# configure: WARNING: dlfcn.h: see the Autoconf documentation
# configure: WARNING: dlfcn.h:     section "Present But Cannot Be Compiled"
# configure: WARNING: dlfcn.h: proceeding with the preprocessor's result
# configure: WARNING: dlfcn.h: in the future, the compiler will take precedence

# My guess that the problem lay with CPPFLAGS seems to be correct. With this
# macro reduced to a direct call to AC_CHECK_HEADERS, there are no warnings
# now that CPPFLAGS contains -mno-cygwin when it needs it. -- lh, 061214 --

# AW 070609: I restored the previous version, since otherwise the warnings
# still pop up for the cl compiler

AC_DEFUN([AC_COIN_CHECK_HEADER],
[#if test x"$4" = x; then
#  hdr="#include <$1>"
#else
#  hdr="$4"
#fi
#AC_CHECK_HEADERS([$1],[$2],[$3],[$hdr])
AC_CHECK_HEADERS([$1],[$2],[$3],[$4])
]) # AC_COIN_CHECK_HEADER

###########################################################################
#                       COIN_CHECK_CXX_CHEADER                             #
###########################################################################

# This macro checks for C header files that are used from C++.  For a give
# stub (e.g., math), it first checks if the C++ library (cmath) is available.
# If it is, it defines HAVE_CMATH (or whatever the stub is).  If it is not
# available, it checks for the old C head (math.h) and defines HAVE_MATH_H
# if that one exists.

AC_DEFUN([AC_COIN_CHECK_CXX_CHEADER],
[AC_LANG_PUSH(C++)
AC_COIN_CHECK_HEADER([c$1],[$2],[$3],[$4])
if test "$ac_cv_header_c$1" != "yes"; then
  AC_COIN_CHECK_HEADER([$1.h],[$2],[$3],[$4])
fi
AC_LANG_POP(C++)
]) # AC_COIN_CHECK_CXX_CHEADER

###########################################################################
#                             COIN_PROG_CC                                #
###########################################################################

# Find the compile command by running AC_PROG_CC (with compiler names
# for different operating systems) and put it into CC (unless it was
# given my the user), and find an appropriate value for CFLAGS.  It is
# possible to provide additional -D flags in the variable CDEFS.

AC_DEFUN([AC_COIN_PROG_CC],
[AC_REQUIRE([AC_COIN_ENABLE_MSVC])
AC_LANG_PUSH(C)

# For consistency, we set the C compiler to the same value of the C++
# compiler, if the C++ is set, but the C compiler isn't (only for CXX=cl)
if test x"$CXX" != x; then
  case "$CXX" in
    clang* ) ;;
    cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
      if test x"$CC" = x; then
        CC="$CXX"
        AC_MSG_WARN([C++ compiler name provided as $CXX, but CC is unset. Setting CC to $CXX])
      fi
      ;;
  esac
fi

AC_ARG_VAR(CDEFS,[Additional -D flags to be used when compiling C code.])
AC_ARG_VAR(ADD_CFLAGS,[Additional C compiler options])
AC_ARG_VAR(DBG_CFLAGS,[Debug C compiler options])
AC_ARG_VAR(OPT_CFLAGS,[Optimize C compiler options])

coin_has_cc=yes

save_cflags="$CFLAGS"

# For *-*-solaris*, promote Studio/Workshop cc compiler to front of list.
# Depending on the user's PATH, when Studio/Workshop cc is not present we may
# find /usr/ucb/cc, which is almost certainly not a good choice for the C
# compiler. In this case, put cc after gcc.

case $build in
  *-cygwin* | *-mingw*)
  	     if test "$enable_msvc" = yes ; then
	       comps="icl cl gcc"
	     else
	       comps="gcc icl cl"
	     fi ;;
  *-*-solaris*)
	     AC_CHECK_PROG(sol_cc_compiler,cc,cc,[],[],/usr/ucb/cc)
	     if test "$sol_cc_compiler" = "cc" ; then
	       comps="cc xlc gcc pgcc icc"
	     else
	       comps="xlc gcc pgcc icc cc"
	     fi
	     ;;
  *-*-darwin*) comps="clang gcc cc" ;;
  *-linux-gnu*) comps="gcc cc pgcc icc xlc" ;;
  *-linux-*) comps="xlc gcc cc pgcc icc" ;;
  *)         comps="xlc_r xlc cc gcc pgcc icc" ;;
esac

# We delete the cached value, since the test might not have been
# performed with our choice of compilers earlier
$as_unset ac_cv_prog_CC || test "${ac_cv_prog_CC+set}" != set || { ac_cv_prog_CC=; export ac_cv_prog_CC; }
# AC_MSG_NOTICE([C compiler candidates: $comps])
AC_PROG_CC([$comps])
if test -z "$CC" ; then
  AC_MSG_ERROR([Failed to find a C compiler!])
fi
# Autoconf incorrectly concludes that cl recognises -g. It doesn't.
case "$CC" in
  clang* ) ;;
  cl* | */cl* | CL* | */CL* )
    if test "$ac_cv_prog_cc_g" = yes ; then
      ac_cv_prog_cc_g=no
      AC_MSG_NOTICE([Overruling autoconf; cl does not recognise -g.])
    fi ;;
  * )
    if test x"$CYGPATH_W" = x ; then
      CYGPATH_W=echo
    fi
    ;;
esac
CFLAGS="$save_cflags"

# add automake conditional so we can recognize cl compiler in makefile
coin_cc_is_cl=false
case "$CC" in
  clang* ) ;;
  cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
    coin_cc_is_cl=true
    ;;
esac
AM_CONDITIONAL(COIN_CC_IS_CL, [test $coin_cc_is_cl = true])

# Check if a project specific CFLAGS variable has been set
if test x$COIN_PRJCT != x; then
  eval coin_tmp=\${${COIN_PRJCT}_CFLAGS+set}
  if test x$coin_tmp = xset; then
    eval CFLAGS=\${${COIN_PRJCT}_CFLAGS}
  fi
fi

if test x"$CFLAGS" = x; then

  coin_add_cflags=
  coin_opt_cflags=
  coin_dbg_cflags=
  coin_warn_cflags=

  if test "$GCC" = "yes"; then
    case "$CC" in
      icc* | */icc*)
        ;;
      *)
        coin_opt_cflags="-O3"
        coin_add_cflags="-pipe"
        coin_dbg_cflags="-g -O0"
        coin_warn_cflags="-Wimplicit -Wparentheses -Wsequence-point -Wreturn-type -Wcast-qual -Wall -Wno-unknown-pragmas -Wno-long-long"
    esac
  fi
  if test -z "$coin_opt_cflags"; then
    case $build in
      *-cygwin* | *-mingw*)
        case "$CC" in
          clang* ) ;;
          cl* | */cl* | CL* | */CL*)
            if test "$coin_disable_shared" = yes || test "$enable_shared" = yes ; then
              coin_opt_cflags='-MD -O2'
              coin_dbg_cflags='-MDd'
            else
              coin_opt_cflags='-MT -O2'
              coin_dbg_cflags='-MTd'
            fi
            coin_add_cflags='-nologo -wd4996 -D_CRT_SECURE_NO_DEPRECATE'
            ;;
          icl* | */icl* | ICL* | */ICL*)
            if test "$coin_disable_shared" = yes || test "$enable_shared" = yes ; then
              coin_opt_cflags='-MD -Ox'
              coin_dbg_cflags='-MDd -debug'
            else
              coin_opt_cflags='-MT -Ox'
              coin_dbg_cflags='-MTd -debug'
            fi
            coin_add_cflags='-nologo -D_CRT_SECURE_NO_DEPRECATE'
            ;;
        esac
        ;;
      *-linux-*)
        case "$CC" in
          icc* | */icc*)
            coin_opt_cflags="-O3 -ip -mp1"
            coin_add_cflags=""
            coin_dbg_cflags="-g"
            # Check if -i_dynamic is necessary (for new glibc library)
            CFLAGS=
            AC_TRY_LINK([],[int i=0; i++;],[],
                        [coin_add_cflags="-i_dynamic $coin_add_cflags"])
            ;;
          pgcc* | */pgcc*)
            coin_opt_cflags="-fast"
            coin_add_cflags="-Kieee -pc 64"
            coin_dbg_cflags="-g"
            ;;
        esac
        ;;
      *-ibm-*)
        case "$CC" in
          xlc* | */xlc* | mpxlc* | */mpxlc*)
            coin_opt_cflags="-O -qarch=auto -qcache=auto -qtune=auto -qmaxmem=-1"
            coin_add_cflags="-bmaxdata:0x80000000 -qsuppress=1500-036 -qsuppress=1500-029"
            coin_dbg_cflags="-g"
          ;;
        esac
        ;;
      *-hp-*)
        coin_opt_cflags="-O"
        coin_add_cflags="-Ae"
        coin_dbg_cflags="-g"
        ;;
      *-*-solaris*)
        coin_opt_cflags="-xO4"
        coin_dbg_cflags="-g"
        ;;
      *-sgi-*)
        coin_opt_cflags="-O -OPT:Olimit=0"
        coin_dbg_cflags="-g"
        ;;
    esac
  fi

  if test "$ac_cv_prog_cc_g" = yes && test -z "$coin_dbg_cflags" ; then
    coin_dbg_cflags="-g"
  fi

  if test -z "$coin_opt_cflags"; then
    # Try if -O option works if nothing else is set
    CFLAGS="-O"
    AC_TRY_LINK([],[int i=0; i++;],[coin_opt_cflags="-O"])
  fi

  # if PM doesn't want the warning messages, take them out
  if test x"$coin_skip_warn_cflags" = xyes; then
    coin_warn_cflags=
  fi

  if test x${DBG_CFLAGS+set} != xset; then
    DBG_CFLAGS="$coin_dbg_cflags $coin_add_cflags $coin_warn_cflags"
  fi
  if test x${OPT_CFLAGS+set} != xset; then
    OPT_CFLAGS="$coin_opt_cflags $coin_add_cflags -DNDEBUG $coin_warn_cflags"
  fi

  DBG_CFLAGS="$DBG_CFLAGS $ADD_CFLAGS $CDEFS"
  OPT_CFLAGS="$OPT_CFLAGS $ADD_CFLAGS $CDEFS"

  if test "$coin_debug_compile" = "true"; then
    CFLAGS="$DBG_CFLAGS"
  else
    CFLAGS="$OPT_CFLAGS"
  fi
else
  CFLAGS="$CFLAGS $ADD_CFLAGS $CDEFS"
  if test x${DBG_CFLAGS+set} != xset; then
    DBG_CFLAGS="$CFLAGS"
  fi
  if test x${OPT_CFLAGS+set} != xset; then
    OPT_CFLAGS="$CFLAGS"
  fi
fi

# add -DPROJECT_BUILD to signal compiler preprocessor which config header file to include
if test x$COIN_PRJCT != x; then
  CFLAGS="$CFLAGS -D${COIN_PRJCT}_BUILD"
fi

# Try if CFLAGS works
save_CFLAGS="$CFLAGS"
AC_TRY_LINK([],[int i=0; i++;],[],[CFLAGS=])
if test -z "$CFLAGS"; then
  AC_MSG_WARN([The value CFLAGS="$save_CFLAGS" do not work.  I will now just try '-O', but you might want to set CFLAGS manually.])
  CFLAGS='-O'
  AC_TRY_LINK([],[int i=0; i++;],[],[CFLAGS=])
  if test -z "$CFLAGS"; then
    AC_MSG_WARN([This value for CFLAGS does not work.  I will continue with empty CFLAGS, but you might want to set CFLAGS manually.])
  fi
fi

AC_MSG_NOTICE([C compiler options are: $CFLAGS])

AC_ARG_VAR(MPICC,[C MPI Compiler])
if test x"$MPICC" = x; then :; else
  AC_MSG_NOTICE([Will use MPI C compiler $MPICC])
  CC="$MPICC"
fi

# Correct the LD variable if we are using the MS or Intel-windows compiler
case "$CC" in
  clang* ) ;;
  cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
    LD=link
    ;;
esac

AC_LANG_POP(C)
]) # AC_COIN_PROG_CC

###########################################################################
#                             COIN_PROG_F77                               #
###########################################################################

# Find the compile command by running AC_PROG_F77 (with compiler names
# for different operating systems) and put it into F77 (unless it was
# given by the user), and find an appropriate value for FFLAGS

AC_DEFUN([AC_COIN_PROG_F77],
[AC_REQUIRE([AC_COIN_ENABLE_MSVC])
AC_REQUIRE([AC_COIN_PROG_CC])
AC_REQUIRE([AC_COIN_F77_COMPS])
AC_LANG_PUSH([Fortran 77])

AC_ARG_VAR(ADD_FFLAGS,[Additional Fortran compiler options])
AC_ARG_VAR(DBG_FFLAGS,[Debug Fortran compiler options])
AC_ARG_VAR(OPT_FFLAGS,[Optimize Fortran compiler options])

coin_has_f77=yes

save_fflags="$FFLAGS"

# We delete the cached value, since the test might not have been
# performed with our choice of compilers earlier
$as_unset ac_cv_prog_F77 || test "${ac_cv_prog_F77+set}" != set || { ac_cv_prog_F77=; export ac_cv_prog_F77; }

# This is a real belt-and-suspenders approach. AC_COIN_FIND_F77 will use
# coin_f77_comps to see if there's a program that matches one of the names.
# If there's no such program, F77 = unavailable. If we match the name,
# feed AC_PROG_F77 the same search list, just to be sure it's a functioning
# compiler.
# AC_MSG_NOTICE([Fortran compiler candidates: $coin_f77_comps])
AC_COIN_FIND_F77
if test "$F77" != "unavailable" ; then
  AC_PROG_F77($coin_f77_comps)
else
  AC_MSG_WARN([Failed to find a Fortran compiler!])
fi

FFLAGS="$save_fflags"

# Check if a project specific FFLAGS variable has been set
if test x$COIN_PRJCT != x; then
  eval coin_tmp=\${${COIN_PRJCT}_FFLAGS+set}
  if test x$coin_tmp = xset; then
    eval FFLAGS=\${${COIN_PRJCT}_FFLAGS}
  fi
fi

if test "$F77" != "unavailable" && test x"$FFLAGS" = x ; then

  coin_add_fflags=
  coin_opt_fflags=
  coin_dbg_fflags=
  coin_warn_fflags=

  if test "$G77" = "yes"; then
    coin_opt_fflags="-O3"
    coin_add_fflags="-pipe"
    coin_dbg_fflags="-g -O0"
  else
    case $build in
      *-cygwin* | *-mingw*)
        case $F77 in
          ifort* | */ifort* | IFORT* | */IFORT* )
            if test "$coin_disable_shared" = yes || test "$enable_shared" = yes ; then
              coin_opt_fflags='-MD -O3'
              coin_dbg_fflags='-MDd -debug'
            else
              coin_opt_fflags='-MT -O3'
              coin_dbg_fflags='-MTd -debug'
            fi
            coin_add_fflags='-fpp -nologo'
          ;;
          compile_f2c*)
            if test "$coin_disable_shared" = yes || test "$enable_shared" = yes ; then
              coin_opt_fflags='-MD -O2'
              coin_dbg_fflags='-MDd'
            else
              coin_opt_fflags='-MT -O2'
              coin_dbg_fflags='-MTd'
            fi
            coin_add_fflags='-nologo -wd4996'
          ;;
        esac
        ;;
      *-linux-*)
        case $F77 in
          ifc* | */ifc* | ifort* | */ifort*)
            coin_opt_fflags="-O3 -ip"
            coin_add_fflags="-cm -w90 -w95"
            coin_dbg_fflags="-g -CA -CB -CS"
            # Check if -i_dynamic is necessary (for new glibc library)
            FFLAGS=
            AC_TRY_LINK(,[      write(*,*) 'Hello world'],[],
                        [coin_add_fflags="-i_dynamic $coin_add_fflags"])
            ;;
          pgf77* | */pgf77* | pgf90* | */pgf90*)
            coin_opt_fflags="-fast"
            coin_add_fflags="-Kieee -pc 64"
            coin_dbg_fflags="-g"
          ;;
        esac
        ;;
      *-ibm-*)
        case "$F77" in
          xlf* | */xlf* | mpxlf* | */mpxlf* )
            coin_opt_fflags="-O -qarch=auto -qcache=auto -qtune=auto -qmaxmem=-1"
            coin_add_fflags="-bmaxdata:0x80000000 -qsuppress=1500-036 -qsuppress=1500-029"
            coin_dbg_fflags="-g -C"
            ;;
        esac
        ;;
      *-hp-*)
        coin_opt_fflags="+O3"
        coin_add_fflags="+U77"
        coin_dbg_fflags="-C -g"
        ;;
      *-*-solaris*)
        coin_opt_fflags="-O4"
        coin_dbg_fflags="-g"
        ;;
      *-sgi-*)
        coin_opt_fflags="-O5 -OPT:Olimit=0"
        coin_dbg_fflags="-g"
        ;;
    esac
  fi

  if test "$ac_cv_prog_f77_g" = yes && test -z "$coin_dbg_fflags" ; then
    coin_dbg_fflags="-g"
  fi

  if test -z "$coin_opt_fflags"; then
    # Try if -O option works if nothing else is set
    FFLAGS=-O
    AC_TRY_LINK(,[      integer i], [coin_opt_fflags="-O"])
  fi

  # if PM doesn't want the warning messages, take them out
  if test x"$coin_skip_warn_fflags" = xyes; then
    coin_warn_fflags=
  fi

  if test x${DBG_FFLAGS+set} != xset; then
    DBG_FFLAGS="$coin_dbg_fflags $coin_add_fflags $coin_warn_fflags"
  fi
  if test x${OPT_FFLAGS+set} != xset; then
    OPT_FFLAGS="$coin_opt_fflags $coin_add_fflags $coin_warn_fflags"
  fi

  DBG_FFLAGS="$DBG_FFLAGS $ADD_FFLAGS"
  OPT_FFLAGS="$OPT_FFLAGS $ADD_FFLAGS"

  if test "$coin_debug_compile" = "true"; then
    FFLAGS="$DBG_FFLAGS"
  else
    FFLAGS="$OPT_FFLAGS"
  fi
else
  FFLAGS="$FFLAGS $ADD_FFLAGS"
  if test x${DBG_FFLAGS+set} != xset; then
    DBG_FFLAGS="$FFLAGS"
  fi
  if test x${OPT_FFLAGS+set} != xset; then
    OPT_FFLAGS="$FFLAGS"
  fi
fi

# Try if FFLAGS works
if test "$F77" != "unavailable" ; then
  orig_FFLAGS="FFLAGS"
  AC_TRY_LINK(,[      integer i],[],[FFLAGS=])
  if test -z "$FFLAGS"; then
    AC_MSG_WARN([The flags FFLAGS="$orig_FFLAGS" do not work.  I will now just try '-O', but you might want to set FFLAGS manually.])
    FFLAGS='-O'
    AC_TRY_LINK(,[      integer i],[],[FFLAGS=])
    if test -z "$FFLAGS"; then
      AC_MSG_WARN([The flags FFLAGS=-O do not work. I will continue with empty FFLAGS, but you might want to set FFLAGS manually.])
    fi
  fi
fi

AC_MSG_NOTICE([Fortran compiler options are: $FFLAGS])

AC_ARG_VAR(MPIF77,[Fortran MPI Compiler])
if test x"$MPIF77" = x; then :; else
  AC_MSG_NOTICE([Will use MPI Fortran compiler $MPIF77])
  F77="$MPIF77"
fi

# correct the LD variable if we use the intel fortran compiler in windows
case $build in
  *-cygwin* | *-mingw*)
    case "$F77" in
      ifort* | */ifort* | IFORT* | */IFORT*)
        LD=link
      ;;
    esac
  ;;
esac

AC_LANG_POP([Fortran 77])
]) # AC_COIN_PROG_F77

###########################################################################
#                           COIN_F77_WRAPPERS                             #
###########################################################################

# Calls autoconfs AC_F77_LIBRARY_LDFLAGS and does additional corrections to FLIBS.
# Then calls AC_F77_WRAPPERS to get Fortran namemangling scheme.
#
# To ensure that the FLIBS are determined and corrected before linking against
# Fortran compilers is attempted by other macros, we put it into an extra macro
# and call it via AC_REQUIRE. This way it seems to be called before the macros
# required by AC_F77_WRAPPERS.

AC_DEFUN([_AC_COIN_F77_LIBRARY_LDFLAGS],
[AC_BEFORE([AC_PROG_F77],[$0])dnl

# get FLIBS
AC_F77_LIBRARY_LDFLAGS
orig_FLIBS="$FLIBS"

# If FLIBS has been set by the user, we just restore its value here
if test x"$save_FLIBS" != x; then
  FLIBS="$save_FLIBS"
else
  # This is to correct a missing exclusion in autoconf 2.59
  if test x"$FLIBS" != x; then
    my_flibs=
    for flag in $FLIBS; do
      case $flag in
        -lcrt*.o) ;;
        -lcygwin) ;;
        -lgcc*)   ;;
               *) my_flibs="$my_flibs $flag" ;;
      esac
    done
    FLIBS="$my_flibs"
  fi

  case $build in
  # The following is a fix to define FLIBS for ifort on Windows
  # In its original version, it linked in libifcorert.lib or libifcorertd.lib on Windows/ifort explicitly.
  # However, this seem to create a dependency on libifcorert.dll (or libifcorertd.dll) in the executables.
  # This is seem to be unnecessary, libifcorert(d).lib has been removed from the link line.
  # Further, excluding libc.lib from the default libs seemed to be necessary only for VS < 8.
  # Since the corresponding flag seems to make more trouble than it avoids, it has been removed now.
     *-cygwin* | *-mingw*)
       case "$F77" in
#         ifort* | */ifort* | IFORT* | */IFORT*)
#           FLIBS="-link $LIBS /NODEFAULTLIB:libc.lib"
#           if "$coin_debug_compile" = true ; then
#             FLIBS="-link $LIBS /NODEFAULTLIB:libc.lib /NODEFAULTLIB:libcmt.lib"
#           else
#             FLIBS="-link $LIBS /NODEFAULTLIB:libc.lib /NODEFAULTLIB:libcmtd.lib"
#           fi
#           ;;
         compile_f2c*)
           FLIBS=`$F77 -FLIBS` ;;
       esac;;
     *-hp-*)
         FLIBS="$FLIBS -lm";;
     *-ibm-*)
         FLIBS=`echo $FLIBS | sed 's/-lc)/-lc/g'` ;;
     *-linux-*)
       case "$F77" in
         pgf77* | */pgf77* | pgf90* | */pgf90*)
# ask linker to go through the archives multiple times
# (the Fortran compiler seems to do that automatically...)
           FLIBS="-Wl,--start-group $FLIBS -Wl,--end-group" ;;
       esac
  esac
  ac_cv_f77_libs="$FLIBS"
fi

if test "x$orig_FLIBS" != "x$FLIBS" ; then
  AC_MSG_NOTICE([Corrected Fortran libraries: $FLIBS])
fi
]) # _AC_COIN_F77_LIBRARY_LDFLAGS

AC_DEFUN([AC_COIN_F77_WRAPPERS],
[AC_BEFORE([AC_COIN_PROG_F77],[$0])dnl
AC_REQUIRE([_AC_COIN_F77_LIBRARY_LDFLAGS])dnl

AC_LANG_PUSH([Fortran 77])
AC_F77_WRAPPERS
AC_LANG_POP([Fortran 77])

]) # AC_COIN_F77_WRAPPERS

###########################################################################
#                             COIN_FIND_F77                               #
###########################################################################

# Attempt to preempt autoconf by locating an appropriate F77 program. This
# macro will not trigger a fatal error if a suitable compiler cannot be
# found. It should be called before COIN_PROG_F77 or COIN_TRY_FLINK.

AC_DEFUN([AC_COIN_FIND_F77],
[AC_REQUIRE([AC_COIN_ENABLE_MSVC])
AC_REQUIRE([AC_COIN_F77_COMPS])
AC_MSG_NOTICE([Trying to determine Fortran compiler name])
AC_CHECK_TOOLS([F77],[$coin_f77_comps],[unavailable])
])

# Auxilliary macro to make sure both COIN_PROG_F77 and COIN_FIND_F77 use
# the same search lists for compiler names.
# For *-*-solaris*, promote Studio/Workshop compilers to front of list.
AC_DEFUN([AC_COIN_F77_COMPS],
[case $build in
  *-cygwin* | *-mingw*)
     if test "$enable_msvc" = yes ; then
       coin_f77_comps="ifort fl32 compile_f2c"
     else
       coin_f77_comps="gfortran ifort g95 g77 fl32 compile_f2c"
     fi ;;
  *-*-solaris*)
     coin_f77_comps="f95 f90 g95 f77 xlf_r fort77 gfortran g77 pgf90 pgf77 ifort ifc frt af77" ;;
  *-linux-gnu*)
     coin_f77_comps="gfortran ifort g95 fort77 f77 g77 pgf90 pgf77 ifc frt af77 xlf_r" ;;
  *) coin_f77_comps="xlf_r fort77 gfortran ifort g95 f77 g77 pgf90 pgf77 ifc frt af77" ;;
 esac
])

###########################################################################
#                          COIN_INIT_AUTOMAKE                             #
###########################################################################

# This macro calls the regular INIT_AUTOMAKE and MAINTAINER_MODE
# macros, and defines additional variables and makefile conditionals,
# that are used in the maintainer parts of the makfile.  It also
# checks if the correct versions of the autotools are used.
#
# This also defines the AC_SUBST variables:
# abs_source_dir     absolute path to source code for this package
# abs_bin_dir        absolute path to the directory where binaries are
#                    going to be installed (prefix/bin)
# abs_lib_dir        absolute path to the directory where libraries are
#                    going to be installed (prefix/lib)
# abs_include_dir    absolute path to the directory where the header files
#                    are installed (prefix/include)

AC_DEFUN([AC_COIN_INIT_AUTOMAKE],
[AC_REQUIRE([AC_PROG_EGREP])
AC_REQUIRE([AC_PROG_LN_S])

# AC_MSG_NOTICE([Beginning automake initialisation.])
# Stuff for automake
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

coin_have_externals=no
if test "$enable_maintainer_mode" = yes; then

  # If maintainer mode is chosen, we make sure that the correct versions
  # of the tools are used, and that we know where libtool.m4 is (to
  # recreate acinclude.m4)

  AC_SUBST(LIBTOOLM4)
  LIBTOOLM4=
  # Normally, $HOME
  AUTOTOOLS_DFLT=$HOME

  AC_CACHE_CHECK([whether we are using the correct autotools],
                 [ac_cv_use_correct_autotools],
                 [ac_cv_use_correct_autotools=check])

  if test $ac_cv_use_correct_autotools = check; then
    ac_cv_use_correct_autotools=yes
    # Check if we have autoconf
    AC_CHECK_PROG([have_autoconf],[autoconf],[yes],[no])
    if test $have_autoconf = no; then
      AC_MSG_ERROR([You specified you want to use maintainer mode, but I cannot find autoconf in your path.])
    fi

    # Check whether autoconf is the correct version
    correct_version='2.59'
    grep_version=`echo  $correct_version | sed -e 's/\\./\\\\\\./g'`
    AC_MSG_CHECKING([whether we are using the correct version ($correct_version) of autoconf])
    autoconf --version > confauto.out 2>&1
    if $EGREP $grep_version confauto.out >/dev/null 2>&1; then
      AC_MSG_RESULT([yes])
    else
      rm -f confauto.out
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([You don't have the correct version of autoconf as the first one in your path.])
    fi
    rm -f confauto.out

    # Check if the executable autoconf is picked up from the correct location
    AC_MSG_CHECKING([whether autoconf is coming from the correct location])
    autoconf_dir=`which autoconf | sed -e 's=/autoconf=='`
    autoconf_dir=`cd $autoconf_dir; pwd`
    if test x$AUTOTOOLS_DIR = x; then
      want_dir=$AUTOTOOLS_DFLT/bin
    else
      want_dir=$AUTOTOOLS_DIR/bin
    fi
    if test $autoconf_dir = `cd $want_dir; pwd`; then
      AC_MSG_RESULT([yes])
    else
      rm -f confauto.out
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([The autoconf executable should be picked up from \$AUTOTOOLS_DFLT/bin or \$AUTOTOOLS_DIR/bin.])
    fi

    # Check if we have automake
    AC_CHECK_PROG([have_automake],[automake],[yes],[no])
    if test $have_automake = no; then
      AC_MSG_ERROR([You specified you want to use maintainer mode, but I cannot find automake in your path.])
    fi
  
    # Check whether automake is the correct version
    correct_version='1.9.6'
    grep_version=`echo  $correct_version | sed -e 's/\\./\\\\\\./g'`
    AC_MSG_CHECKING([whether we are using the correct version ($correct_version) of automake])
    automake --version > confauto.out 2>&1
    if $EGREP $grep_version confauto.out >/dev/null 2>&1; then
      AC_MSG_RESULT([yes])
    else
      rm -f confauto.out
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([You don't have the correct version of automake as the first one in your path.])
    fi
    rm -f confauto.out

    # Check if the executable automake is picked up from the correct location
    AC_MSG_CHECKING([whether automake is coming from the correct location])
    automake_dir=`which automake | sed -e 's=/automake=='`
    automake_dir=`cd $automake_dir; pwd`
    if test x$AUTOTOOLS_DIR = x; then
      want_dir=$AUTOTOOLS_DFLT/bin
    else
      want_dir=$AUTOTOOLS_DIR/bin
    fi
    if test $automake_dir = `cd $want_dir; pwd`; then
      AC_MSG_RESULT([yes])
    else
      rm -f confauto.out
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([The automake executable should be picked up from \$AUTOTOOLS_DFLT/bin or \$AUTOTOOLS_DIR/bin.])
    fi

    # Check if this is the correct version of libtool (with escaped dots)
    if test x$AUTOTOOLS_DIR = x; then
      want_dir=$AUTOTOOLS_DFLT/share
    else
      want_dir=$AUTOTOOLS_DIR/share
    fi
    correct_version='1.5.22'
    grep_version=`echo  $correct_version | sed -e 's/\\./\\\\\\./g'`
    AC_COIN_CHECK_FILE([$want_dir/libtool/ltmain.sh],
	               [have_ltmain=yes],
                       [have_ltmain=no])
    AC_MSG_CHECKING([whether we are using the correct version ($correct_version) of libtool.])
    if test $have_ltmain = yes; then
    if $EGREP $grep_version $want_dir/libtool/ltmain.sh >/dev/null 2>&1; then
        AC_MSG_RESULT([yes])
      else
        AC_MSG_RESULT([no])
        AC_MSG_ERROR([You don't have the correct version of libtool.])
      fi
    else
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([I cannot find the ltmain.sh file.])
    fi
  fi

  # Check if we can find the libtool file
  if test x$AUTOTOOLS_DIR = x; then
    want_dir=$AUTOTOOLS_DFLT/share
  else
    want_dir=$AUTOTOOLS_DIR/share
  fi
  AC_COIN_CHECK_FILE([$want_dir/aclocal/libtool.m4],
                     [LIBTOOLM4="$want_dir/aclocal/libtool.m4"],
                     [AC_MSG_ERROR([I cannot find the libtool.m4 file.])])

  # Check if we have an Dependencies file
  if test -r $srcdir/Dependencies; then
    coin_have_externals=yes
  fi
  # Check if subversion is installed and understands https
  AC_CHECK_PROG([have_svn],[svn],[yes],[no])
  if test x$have_svn = xyes; then
    AC_CACHE_CHECK([whether svn understands https],
                   [ac_cv_svn_understands_https],
                   [svn --version > confauto.out 2>&1
                    if $EGREP https confauto.out >/dev/null 2>&1; then
                      ac_cv_svn_understands_https=yes
                    else
                      ac_cv_svn_understands_https=no
                      have_svn=no
                      ac_cv_prog_have_svn=no
                    fi
                    rm -f confauto.out])
  fi

  # Find the location of the BuildTools directory
  BUILDTOOLSDIR=
  if test -r $srcdir/BuildTools/coin.m4; then
    BUILDTOOLSDIR=$srcdir/BuildTools
  elif test -r $srcdir/../BuildTools/coin.m4; then
    BUILDTOOLSDIR=$srcdir/../BuildTools
  elif test -r $srcdir/../../BuildTools/coin.m4; then
    BUILDTOOLSDIR=$srcdir/../../BuildTools
  elif test -r $srcdir/../../../BuildTools/coin.m4; then
    BUILDTOOLSDIR=$srcdir/../../../BuildTools
  else
    AC_MSG_ERROR(Cannot find the BuildTools directory, better disable maintainer mode.)
  fi
  AC_SUBST(BUILDTOOLSDIR)
  
  # for running automake by make, we need to have Makemain.inc available at the place where it usually can be found during run_autotools
  if test "$BUILDTOOLSDIR" != "$srcdir/BuildTools" ; then
    $LN_S `cd $BUILDTOOLSDIR; pwd` "$srcdir/BuildTools"
  fi

  # The following variable is set to the name of the directory where
  # the autotool scripts are located
  AC_SUBST(AUX_DIR)
  AUX_DIR=$ac_aux_dir
fi

# helpful variable for the base directory of this package
abs_source_dir=`cd $srcdir; pwd`
AC_SUBST(abs_source_dir)

# Stuff for example Makefiles
if test x$prefix = xNONE; then
  full_prefix=$ac_default_prefix
else
  full_prefix=$prefix
fi

AC_SUBST(abs_lib_dir)
abs_lib_dir=$full_prefix/lib
AC_SUBST(abs_include_dir)
abs_include_dir=$full_prefix/include
AC_SUBST(abs_bin_dir)
abs_bin_dir=$full_prefix/bin

AM_CONDITIONAL(HAVE_EXTERNALS,
               test $coin_have_externals = yes && test x$have_svn = xyes)

# AC_MSG_NOTICE([End automake initialisation.])

]) # AC_COIN_INIT_AUTOMAKE

###########################################################################
#                          COIN_CREATE_LIBTOOL                            #
###########################################################################

# This does all the tests necessary to create the libtool script in the
# package base directory.  If this is used, then the COIN_INIT_AUTO_TOOLS
# test in the subdirectories will be able to skip the libtool tests and
# just use the one in the package base directory.

m4_define([AC_COIN_CREATE_LIBTOOL],
[AC_CANONICAL_BUILD

# Check if user wants to produce debugging code
AC_COIN_DEBUG_COMPILE

# Get the name of the C compiler and appropriate compiler options
AC_COIN_PROG_CC

# Get the name of the C++ compiler and appropriate compiler options
AC_COIN_PROG_CXX

# Get the name of the Fortran compiler and appropriate compiler options
AC_COIN_PROG_F77

# Initialize automake and libtool
# AC_MSG_NOTICE([Calling INIT_AUTO_TOOLS from CREATE_LIBTOOL.])
AC_COIN_INIT_AUTO_TOOLS
# AC_MSG_NOTICE([Finished INIT_AUTO_TOOLS from CREATE_LIBTOOL.])
])

###########################################################################
#                         COIN_INIT_AUTO_TOOLS                            #
###########################################################################

# Initialize the auto tools automake and libtool, with all
# modifications we want for COIN packages.
#
# RPATH_FLAGS        link flags for hardcoding path to shared objects

# This is a trick to have this code before AC_COIN_PROG_LIBTOOL
AC_DEFUN([AC_COIN_DISABLE_STATIC],
[
coin_disable_shared=no
# Test if force_shared has been set
if test "x$1" = xforce_shared; then
  if test x$enable_shared = xno; then
    AC_MSG_ERROR([Shared libraries are disabled by user, but this is not feasible with the given options])
  fi
  enable_shared=yes;
else
  case $build in
    *-cygwin* | *-mingw*)
      coin_disable_shared=yes
      if test x"$enable_shared" = xyes; then
        case "$CC" in
          clang* )
            AC_MSG_WARN([Building of DLLs not supported in this configuration.])
            ;;
          cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
            AC_MSG_NOTICE([Building of DLLs not supported in this configuration.])
            ;;
          *gcc*)
            if test x"$enable_dependency_linking" = xyes; then
              coin_disable_shared=no
            else
              AC_MSG_WARN([Dependency linking seems to be disabled, so shared libraries (DLLs) will not be built])
            fi
            ;;
          *)
            AC_MSG_WARN([Building of DLLs not supported in this configuration.])
            ;;
        esac
      fi
    ;;
    *-aix*)
      coin_disable_shared=yes
      platform=AIX
      if test x"$enable_shared" = xyes; then
        AC_MSG_WARN([Shared objects are not supported.])
      fi
    ;;
  esac
fi
if test x"$coin_disable_shared" = xyes; then
  if test x"$enable_shared" = xyes; then
    :
  else
    # we don't disable shared, because it was not selected anyway
    coin_disable_shared=no
  fi
  enable_shared=no
fi
# By default, we only want the shared objects to be compiled
AC_DISABLE_STATIC
])

m4_define([AC_COIN_INIT_AUTO_TOOLS],
[{AC_BEFORE([AC_COIN_PROG_CXX],[$0])
AC_BEFORE([AC_COIN_PROG_CC],[$0])
AC_BEFORE([AC_COIN_PROG_F77],[$0])

# START
AC_COIN_DISABLE_STATIC([$1])

# Initialize automake
AC_COIN_INIT_AUTOMAKE

LIBTOOL=
if test -f ../libtool; then
  coin_config_dir=..
  LIBTOOL='$(SHELL) $(top_builddir)/../libtool'
fi
if test "x$LIBTOOL" = x; then
  if test -f ../../libtool; then
    coin_config_dir=../..
    LIBTOOL='$(SHELL) $(top_builddir)/../../libtool'
  fi
fi

if test "x$LIBTOOL" = x; then
# AC_MSG_NOTICE([Creating libtool script (calling COIN_PROG_LIBTOOL).])
  # Stuff for libtool
  AC_COIN_PROG_LIBTOOL
# AC_MSG_NOTICE([Finished COIN_PROG_LIBTOOL.])
  # set RPATH_FLAGS to the compiler link flags required to hardcode location
  # of the shared objects
  AC_COIN_RPATH_FLAGS([$abs_lib_dir])

else

  AC_MSG_NOTICE([Using libtool script in directory $coin_config_dir])
  # get all missing information from the config.log file 

  # output variables and defines
  as_save_IFS=$IFS
  IFS='
'
  for oneline in `cat $coin_config_dir/config.status`; do
    case "$oneline" in
         # First some automake conditionals
      s,@am__fastdep* | s,@AR@* | s,@CPP@*  | s,@CPPFLAGS@* | s,@CXXCPP@*  | \
      s,@RANLIB@* | s,@STRIP@* | s,@ac_ct_AR@* | s,@ac_ct_RANLIB@* | \
      s,@ac_ct_STRIP@* | s,@host* | s,@LN_S@* | s,@RPATH_FLAGS@* | \
      s,@ac_c_preproc_warn_flag@* |  s,@ac_cxx_preproc_warn_flag@* ) 
        command=`echo $oneline | sed -e 's/^s,@//' -e 's/@,/="/' -e 's/,;t t/"/'`
#        echo "$command"
        eval "$command"
        ;;
      s,@DEFS@* )
        command=`echo $oneline | sed -e 's/^s,@DEFS@,/defsline="/' -e 's/,;t t/"/'`
#        echo "$command"
        eval "$command"
        ;;
    esac
  done
  IFS=$as_save_IFS

  # And some defines (assuming here that the packages base dir
  # doesn't have a config.h file
  for word in $defsline; do
#    echo word $word 
    case $word in
      -DHAVE_@<:@A-Z_@:>@*_H=1 | -DSTDC_HEADERS=1 )
        i=`echo $word | sed -e 's/-D/#define /' -e 's/=/ /'`
#        echo dd $i
        echo $i >>confdefs.h
        ;;
    esac
  done
fi

# AC_MSG_NOTICE([End of INIT_AUTO_TOOLS.])

AC_ARG_ENABLE([dependency-linking],
  [AC_HELP_STRING([--disable-dependency-linking],
                  [disable linking library dependencies into shared libraries])],
  [dependency_linking="$enableval"],
  [dependency_linking=auto])

if test "$dependency_linking" = auto; then
  # On Cygwin and AIX, building DLLs doesn't work
  dependency_linking=no
  if test x"$coin_disable_shared" = xno; then
    case $build in
      *-cygwin* | *-mingw*)
        case "$CC" in
          clang* )
            dependency_linking=yes
            ;;
          cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
            dependency_linking=no
            ;;
          *gcc*)
            dependency_linking=yes
            ;;
          *)
            dependency_linking=yes
            ;;
        esac
        ;;
      *)
        dependency_linking=yes
        ;;
    esac
  fi
fi

if test "$dependency_linking" = yes ;
then
  LT_LDFLAGS="-no-undefined"
else
  LT_LDFLAGS=
fi

AM_CONDITIONAL(DEPENDENCY_LINKING, [test "$dependency_linking" = yes])

# Check if we want to set the library version
AC_MSG_CHECKING([if library version is set])
if test x"$coin_libversion" != x; then
  LT_LDFLAGS="$LT_LDFLAGS -version-info $coin_libversion"
  AC_MSG_RESULT([$coin_libversion])
else
  AC_MSG_RESULT([no])
fi

AC_SUBST(LT_LDFLAGS)

#END
}])

###########################################################################
#                      COIN_PATCH_LIBTOOL_CYGWIN                          #
###########################################################################

# Patches to libtool for cygwin. Lots for cl, a few for GCC.
# For cl:
# - cygpath is not correctly quoted in fix_srcfile_path
# - paths generated for .lib files is not run through cygpath -w


AC_DEFUN([AC_COIN_PATCH_LIBTOOL_CYGWIN],
[ case "$CXX" in
    clang* )
      # we assume that libtool patches for CLANG are the same as for GNU compiler - correct???
      AC_MSG_NOTICE(Applying patches to libtool for CLANG compiler)
      sed -e 's|fix_srcfile_path=\"`cygpath -w \"\$srcfile\"`\"|fix_srcfile_path=\"\\\`'"$CYGPATH_W"' \\\"\\$srcfile\\\"\\\`\"|' \
	  -e 's|"lib /OUT:\\$oldlib\\$oldobjs\\$old_deplibs"|"\\$AR \\$AR_FLAGS \\$oldlib\\$oldobjs\\$old_deplibs~\\$RANLIB \\$oldlib"|' \
	  -e 's|libext="lib"|libext="a"|' \
      libtool > conftest.bla
      ;;
    cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*) 
      AC_MSG_NOTICE(Applying patches to libtool for cl compiler)
      sed -e 's|fix_srcfile_path=\"`cygpath -w \"\$srcfile\"`\"|fix_srcfile_path=\"\\\`'"$CYGPATH_W"' \\\"\\$srcfile\\\"\\\`\"|' \
	  -e 's|fix_srcfile_path=\"\"|fix_srcfile_path=\"\\\`'"$CYGPATH_W"' \\\"\\$srcfile\\\"\\\`\"|' \
	  -e 's%compile_deplibs=\"\$dir/\$old_library \$compile_deplibs\"%compile_deplibs="'\`"$CYGPATH_W"' \$dir/\$old_library | sed -e '"'"'sY\\\\\\\\Y/Yg'"'"\`' \$compile_deplibs\"'% \
	  -e 's%compile_deplibs=\"\$dir/\$linklib \$compile_deplibs\"%compile_deplibs="'\`"$CYGPATH_W"' \$dir/\$linklib | sed -e '"'"'sY\\\\\\\\Y/Yg'"'"\`' \$compile_deplibs\"'% \
	  -e 's%lib /OUT:%lib -OUT:%' \
	  -e "s%cygpath -w%$CYGPATH_W%" \
	  -e 's%$AR x \\$f_ex_an_ar_oldlib%bla=\\$(lib -nologo -list \\$('"$CYGPATH_W \$[]1"') '"$mydos2unix"' | xargs echo); echo \\$bla; for i in \\$bla; do lib -nologo -extract:\\$i \\$('"$CYGPATH_W \$[]1"'); done%' \
	  -e 's%$AR t "$f_ex_an_ar_oldlib"%lib -nologo -list \$('"$CYGPATH_W \$[]1"') '"$mydos2unix"'%' \
	  -e 's%f_ex_an_ar_oldlib="\($?*1*\)"%f_ex_an_ar_oldlib='\`"$CYGPATH_W"' \1`%' \ 
	  -e 's%^archive_cmds=.*%archive_cmds="\\$CC -o \\$lib \\$libobjs \\$compiler_flags \\\\\\`echo \\\\\\"\\$deplibs\\\\\\" | \\$SED -e '"\'"'s/ -lc\\$//'"\'"'\\\\\\` -link -dll~linknames="%' \
	  -e 's%old_archive_cmds="lib -OUT:\\$oldlib\\$oldobjs\\$old_deplibs"%old_archive_cmds="if test -r \\$oldlib; then bla=\\"\\$oldlib\\"; else bla=; fi; lib -OUT:\\$oldlib \\\\\\$bla\\$oldobjs\\$old_deplibs"%' \
      libtool > conftest.bla
      ;;
    *)
      AC_MSG_NOTICE(Applying patches to libtool for GNU compiler)
      sed -e 's|fix_srcfile_path=\"`cygpath -w \"\$srcfile\"`\"|fix_srcfile_path=\"\\\`'"$CYGPATH_W"' \\\"\\$srcfile\\\"\\\`\"|' \
	  -e 's|"lib /OUT:\\$oldlib\\$oldobjs\\$old_deplibs"|"\\$AR \\$AR_FLAGS \\$oldlib\\$oldobjs\\$old_deplibs~\\$RANLIB \\$oldlib"|' \
	  -e 's|libext="lib"|libext="a"|' \
      libtool > conftest.bla
      ;;
  esac
  mv conftest.bla libtool
  chmod 755 libtool
]) # COIN_PATCH_LIBTOOL_CYGWIN

###########################################################################
#                    COIN_PATCH_LIBTOOL_SOLARIS                           #
###########################################################################
# If we want to do a 64-bit build with GCC on Solaris, the system search
# libraries need to point to 64-bit subdirectories. If they do not already do
# that, fix them. This patch is evolving, as are GCC compilers.  GCC 4.2.1
# reports the correct search list, given the correct call. GCC 4.1.1 does not.
# `Correct call' means -m64 is specified. `Correct search list' seems to amount
# to prepending the list of 64-bit subdirectories to the 32-bit directories.
# Both SPARC and x86 have this issue, but a different hardware id string is
# required depending on the underlying CPU. The macro executes isainfo to get
# the string. This will fail, of course, if we're cross-compiling. The
# alternative is to fail on a regular basis each time a new CPU identifier is
# needed. This macro will also fail if the search list reported with
# -print-search-dirs differs between the C, C++, and Fortran compilers; each
# have their own setting in libtool.  If GCC reports the correct search list
# given the -m64 flag, the best solution is to define CC='gcc -m64', and
# similarly for CXX, F77, so that libtool will make the correct call.
###########################################################################
AC_DEFUN([AC_COIN_PATCH_LIBTOOL_SOLARIS],
[ if test "$GCC" = yes && \
     (echo $CXXFLAGS $CFLAGS $FFLAGS | $EGREP 'm64' >/dev/null 2>&1) ; then
    hdwisa=`isainfo | sed -e 's/\(@<:@^ @:>@*\) .*$/\1/'`
    if `$EGREP 'sys_lib_search_path_spec=' libtool | $EGREP -v $hdwisa >/dev/null 2>&1` ; then
      AC_MSG_NOTICE([Applying patches to libtool for 64-bit GCC compilation])
      fixlibtmp=`$CC -m64 -print-search-dirs | $EGREP '^libraries:'`
      fixlibtmp=`echo $fixlibtmp | sed -e 's/libraries: =//' -e 's/:/ /g'`
      if `echo "$fixlibtmp" | $EGREP -v $hdwisa  >/dev/null 2>&1` ; then
	# AC_MSG_NOTICE(Compensating for broken gcc)
	for lib in $fixlibtmp ; do
	  if test -d "${lib}${hdwisa}" ; then
	    syslibpath64="$syslibpath64 ${lib}${hdwisa}/"
	  fi
	done
	syslibpath64="${syslibpath64} ${fixlibtmp}"
      else
	syslibpath64="$fixlibtmp"
      fi
      sed -e 's|sys_lib_search_path_spec=".*"|sys_lib_search_path_spec="'"$syslibpath64"'"|' libtool > conftest.bla
      mv conftest.bla libtool
      chmod 755 libtool  
    fi
    # AC_MSG_NOTICE(Result is )
    # $EGREP 'sys_lib_search_path_spec=' libtool
  fi ])	# COIN_PATCH_LIBTOOL_SOLARIS

###########################################################################
#                           COIN_PROG_LIBTOOL                             #
###########################################################################

# Setup the libtool stuff together with any modifications to make it
# work on additional platforms

AC_DEFUN([AC_COIN_PROG_LIBTOOL],
[# No longer needed now that CPPFLAGS is correctly set -- lh, 061214 --
 # AC_REQUIRE([AC_COIN_DLFCN_H])

# NEW: If libtool exists in the directory higher up, we use that one
#      instead of creating a new one

# It turns out that the code for AC_PROG_LIBTOOL is somehow AC_REQUIRED
# out in front of this macro body. You'll notice that LIBTOOL is already
# defined here.  We'll have to count on this macro not being called if libtool
# already exists, or at least move the libtool fixes outside the conditional.
# AC_MSG_NOTICE([Entering coin_prog_libtool, LIBTOOL = "$LIBTOOL".])
# This test is therefore removed.  -- lh, 061214 --
# if test "x$LIBTOOL" = x; then

# AC_MSG_NOTICE([Calling PROG_LIBTOOL.])
  AC_PROG_LIBTOOL
# AC_MSG_NOTICE([Finished PROG_LIBTOOL.])
  AC_SUBST(ac_c_preproc_warn_flag)
  AC_SUBST(ac_cxx_preproc_warn_flag)

  AC_MSG_NOTICE([Build is "$build".])
  AC_CHECK_PROG([dos2unix], [dos2unix], [dos2unix])
  if test "$dos2unix" = dos2unix ; then
    mydos2unix="| dos2unix"
  fi
  case $build in
    *-mingw*)
      CYGPATH_W=echo
      ;;
  esac

  case $build in
    # Here we need to check if -m32 is specified.  If so, we need to correct
    # sys_lib_search_path_spec
    *-cygwin* | *-mingw*)
      AC_COIN_PATCH_LIBTOOL_CYGWIN
      ;;
    *x86_64-*)
      if test "$GCC" = yes && (echo $CXXFLAGS $CFLAGS $FFLAGS | $EGREP 'm32' >& /dev/null); then 
        AC_MSG_NOTICE(Applying patches to libtool for 32bit compilation)
        sed -e 's|sys_lib_search_path_spec=".*"|sys_lib_search_path_spec="/lib /usr/lib"|' libtool > conftest.bla
        mv conftest.bla libtool
        chmod 755 libtool  
      fi
      ;;

    *-solaris*)
      AC_COIN_PATCH_LIBTOOL_SOLARIS
      ;;
    # Cygwin. Ah, cygwin. Too big and ugly to inline; see the macro.
    *-darwin*)
      AC_MSG_NOTICE(Applying patches to libtool for Darwin)
      sed -e 's/verstring="${wl}-compatibility_version ${wl}$minor_current ${wl}-current_version ${wl}$minor_current.$revision"/verstring="-compatibility_version $minor_current -current_version $minor_current.$revision"/' \
        -e 's/ -dynamiclib / -dynamiclib -single_module /g' \
      libtool > conftest.bla

      mv conftest.bla libtool
      chmod 755 libtool
      ;;
  esac
# This fi matches the commented `if test "x$LIBTOOL" = x;' up at the head of
# the macro. -- lh, 061214 --
# fi

# AC_MSG_NOTICE([End libtool initialisation.])
]) # AC_COIN_PROG_LIBTOOL

# This is a trick to force the check for the dlfcn header to be done before
# the checks for libtool
# No longer needed now that CPPFLAGS is correctly set.  -- lh, 061214 --
# ACDEFUN([AC_COIN_DLFCN_H],
# [AC_LANG_PUSH(C)
# AC_COIN_CHECK_HEADER([dlfcn.h])
# AC_LANG_POP(C)
# ]) # AC_COIN_DLFCN_H

###########################################################################
#                            COIN_RPATH_FLAGS                             #
###########################################################################

# This macro, in case shared objects are used, defines a variable
# RPATH_FLAGS that can be used by the linker to hardwire the library
# search path for the given directories.  This is useful for example
# Makefiles

AC_DEFUN([AC_COIN_RPATH_FLAGS],
[RPATH_FLAGS=

if test $enable_shared = yes; then
  case $build in
    *-linux-*)
      if test "$GXX" = "yes"; then
        RPATH_FLAGS=
        for dir in $1; do
          RPATH_FLAGS="$RPATH_FLAGS -Wl,--rpath -Wl,$dir"
        done
      fi ;;
    *-darwin*)
        RPATH_FLAGS=nothing ;;
    *-ibm-*)
      case "$CXX" in
      xlC* | */xlC* | mpxlC* | */mpxlC*)
        RPATH_FLAGS=nothing ;;
      esac ;;
    *-hp-*)
        RPATH_FLAGS=nothing ;;
    *-mingw32)
        RPATH_FLAGS=nothing ;;
    *-*-solaris*)
        RPATH_FLAGS=
        for dir in $1; do
          RPATH_FLAGS="$RPATH_FLAGS -R$dir"
        done
  esac

  if test "$RPATH_FLAGS" = ""; then
    AC_MSG_WARN([Could not automatically determine how to tell the linker about automatic inclusion of the path for shared libraries.  The test examples might not work if you link against shared objects.  You will need to set the LD_LIBRARY_PATH, DYLP_LIBRARY_PATH, or LIBDIR variable manually.])
  fi
  if test "$RPATH_FLAGS" = "nothing"; then
    RPATH_FLAGS=
  fi
fi

AC_SUBST(RPATH_FLAGS)
]) # AC_COIN_RPATH_FLAGS

###########################################################################
#                        COIN_LINK_INPUT_CMD                              #
###########################################################################

# This macro determines which command should be used to "link" files
# that are input to the generated executables.  On Windows, the codes
# using the native Windows system libraries, cannot understand symbolic
# links, and a copy should be used instead of 'ln -s'.
# The result is stored in coin_link_input_cmd

AC_DEFUN([AC_COIN_LINK_INPUT_CMD],
[AC_REQUIRE([AC_PROG_LN_S])
AC_BEFORE([AC_COIN_PROG_CC], [$0])
AC_BEFORE([AC_COIN_ENABLE_MSVC], [$0])

AC_MSG_CHECKING([which command should be used to link input files])
coin_link_input_cmd="$LN_S"
case "$CC" in
  clang* ) ;;
  cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
    coin_link_input_cmd=cp ;;
esac
AC_MSG_RESULT($coin_link_input_cmd)
])

###########################################################################
#                              COIN_FINALIZE                              #
###########################################################################

# This macro should be called at the very end of the configure.ac file.
# It creates the output files (by using AC_OUTPUT), and might do some other
# things (such as generating links to data files in a VPATH configuration).
# It also prints the "success" message.
# Note: If the variable coin_skip_ac_output is set to yes, then no output
# files are written.

AC_DEFUN([AC_COIN_FINALIZE],
[
AC_REQUIRE([AC_COIN_LINK_INPUT_CMD])
if test x$coin_skip_ac_output != xyes; then

  # library extension
  AC_SUBST(LIBEXT)
  case "$CC" in
    clang* )
         LIBEXT=a ;;
    cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
         LIBEXT=lib ;;
      *) LIBEXT=a ;;
  esac

  # Define VPATH_DISTCLEANFILES to be everything that needs to be
  # cleaned for distclean in a vpath configuration
  AC_SUBST(VPATH_DISTCLEANFILES)
  VPATH_DISTCLEANFILES="$coin_vpath_link_files"

  # Take out subdirectories if their configuration concluded that they
  # don't need to be compiled
  if test x"$coin_ac_skip_subdirs" != x; then
    new_subdirs=
    for i in $subdirs; do
      skipme=no
      for j in $coin_ac_skip_subdirs; do
        if test $i = $j; then
          skipme=yes;
        fi
      done
      if test $skipme = no; then
        new_subdirs="$new_subdirs $i"
      fi
    done
    subdirs="$new_subdirs"
  fi

  # need to come before AC_OUTPUT
  if test x$coin_projectdir != xyes; then
    # write coin_subdirs to a file so that project configuration knows where to find uninstalled projects
    echo $coin_subdirs > coin_subdirs.txt
  else
    # substitute for OBJDIR, needed to setup .pc file for uninstalled project
    ABSBUILDDIR="`pwd`"
    AC_SUBST(ABSBUILDDIR)
  fi
  
  AC_OUTPUT

  if test x"$coin_vpath_link_files" = x; then : ; else
    lnkcmd="$coin_link_input_cmd"
    if test "$lnkcmd" = cp; then
      AC_MSG_NOTICE(Copying data files for VPATH configuration)
    else
      AC_MSG_NOTICE(Creating VPATH links for data files)
    fi
    for file in $coin_vpath_link_files; do
      dir=`AS_DIRNAME(["./$file"])`
      if test -d $dir; then : ; else
        AS_MKDIR_P($dir)
      fi
      rm -f $file
      $lnkcmd $abs_source_dir/$file $file
    done
  fi

  AC_MSG_NOTICE([In case of trouble, first consult the troubleshooting page at https://projects.coin-or.org/BuildTools/wiki/user-troubleshooting])
  if test x$coin_projectdir = xyes; then
    AC_MSG_NOTICE([Configuration of $PACKAGE_NAME successful])
  else
    AC_MSG_NOTICE([Main configuration of $PACKAGE_NAME successful])
  fi
else
  AC_MSG_NOTICE([No configuration of $PACKAGE_NAME necessary])
fi

]) #AC_COIN_FINALIZE

###########################################################################
#                             COIN_VPATH_LINK                             #
###########################################################################

# This macro queues source files that need to be available in the build
# directory. In a VPATH configuration, the files will be made available by
# symbolic link or copy (when the platform does not support links). The list
# is processed by COIN_FINALIZE. The parameter is a whitespace-separated
# list of files.

AC_DEFUN([AC_COIN_VPATH_LINK],
[
AC_REQUIRE([AC_COIN_CHECK_VPATH])
# Allow for newlines in the parameter
if test $coin_vpath_config = yes; then
  cvl_tmp="$1"
  for file in $cvl_tmp ; do
    coin_vpath_link_files="$coin_vpath_link_files $file"
  done
fi
]) #AC_COIN_VPATH_LINK

###########################################################################
#                       COIN_ENABLE_GNU_PACKAGES                          #
###########################################################################

# This macro defined the --enable-gnu-packages flag.  This can be used
# to check if a user wants to compile GNU packges (such as readline)
# into the executable.  By default, GNU packages are disabled.

AC_DEFUN([AC_COIN_ENABLE_GNU_PACKAGES],
[AC_ARG_ENABLE([gnu-packages],
               [AC_HELP_STRING([--enable-gnu-packages],
                               [compile with GNU packages (disabled by default)])],
	       [coin_enable_gnu=$enableval],
	       [coin_enable_gnu=no])
]) # AC_COIN_ENABLE_GNU_PACKAGES

#######################################################################
#                           COIN_CHECK_LIBM                           #
#######################################################################

# For a (space separated) list of arguments X, this macro adds the flags
# for linking against the math library to a X_LIBS and X_PCLIBS.

AC_DEFUN([AC_COIN_CHECK_LIBM],
[AC_BEFORE([AC_COIN_PROG_CC],[$0])

if test $coin_cc_is_cl != true ; then
  coin_foreach_w([myvar], [$1], [
    m4_toupper(myvar)_LIBS="-lm $m4_toupper(myvar)_LIBS"
    m4_toupper(myvar)_PCLIBS="-lm $m4_toupper(myvar)_PCLIBS"
    m4_toupper(myvar)_LIBS_INSTALLED="-lm $m4_toupper(myvar)_LIBS_INSTALLED"
  ])
fi

]) # AC_COIN_CHECK_LIBM

###########################################################################
#                           COIN_CHECK_GNU_ZLIB                           #
###########################################################################

# This macro checks for the libz library.  If found, it sets the automake
# conditional COIN_HAS_ZLIB and defines the C preprocessor variable
# COIN_HAS_ZLIB.  Further, for a (space separated) list of arguments X,
# it adds the linker flag to the variables X_LIBS, X_PCLIBS, and X_LIBS_INSTALLED.

# TODO the macro name should be changed to AC_COIN_CHECK_ZLIB

AC_DEFUN([AC_COIN_CHECK_GNU_ZLIB],
[
AC_BEFORE([AC_COIN_PROG_CXX],[$0])
AC_BEFORE([AC_COIN_PROG_CC],[$0])
AC_BEFORE([AC_COIN_PROG_F77],[$0])
AC_BEFORE([$0],[AC_COIN_FINALIZE])

coin_has_zlib=no

AC_ARG_ENABLE([zlib],
              [AC_HELP_STRING([--disable-zlib],[do not compile with compression library zlib])],
              [coin_enable_zlib=$enableval],
              [coin_enable_zlib=yes])

if test $coin_enable_zlib = yes; then
  AC_COIN_CHECK_HEADER([zlib.h],[coin_has_zlib=yes])

  if test $coin_has_zlib = yes; then
    AC_CHECK_LIB([z],[gzopen],[:],[coin_has_zlib=no])
  fi

  if test $coin_has_zlib = yes; then
    coin_foreach_w([myvar], [$1], [
                    m4_toupper(myvar)_LIBS="-lz $m4_toupper(myvar)_LIBS"
                    m4_toupper(myvar)_PCLIBS="-lz $m4_toupper(myvar)_PCLIBS"
                    m4_toupper(myvar)_LIBS_INSTALLED="-lz $m4_toupper(myvar)_LIBS_INSTALLED"
                   ])
    AC_DEFINE([COIN_HAS_ZLIB],[1],[Define to 1 if zlib is available])
  fi
fi

AM_CONDITIONAL(COIN_HAS_ZLIB,test x$coin_has_zlib = xyes)
]) # AC_COIN_CHECK_GNU_ZLIB


###########################################################################
#                          COIN_CHECK_GNU_BZLIB                           #
###########################################################################

# This macro checks for the libbz2 library.  If found, it defines the C
# preprocessor variable COIN_HAS_BZLIB.  Further, for a (space separated) list
# of arguments X, it adds the linker flag to the variables X_LIBS, X_PCLIBS, and X_LIBS_INSTALLED.

# TODO the macro name should be changed to AC_COIN_CHECK_BZLIB

AC_DEFUN([AC_COIN_CHECK_GNU_BZLIB],
[
AC_BEFORE([AC_COIN_PROG_CXX],[$0])
AC_BEFORE([AC_COIN_PROG_CC],[$0])
AC_BEFORE([AC_COIN_PROG_F77],[$0])
AC_BEFORE([$0],[AC_COIN_FINALIZE])

AC_ARG_ENABLE([bzlib],
              [AC_HELP_STRING([--disable-bzlib],[do not compile with compression library bzlib])],
              [coin_enable_bzlib=$enableval],
              [coin_enable_bzlib=yes])

coin_has_bzlib=no
if test $coin_enable_bzlib = yes; then
  AC_COIN_CHECK_HEADER([bzlib.h],[coin_has_bzlib=yes])

  if test $coin_has_bzlib = yes; then
    AC_CHECK_LIB([bz2],[BZ2_bzReadOpen],[:],[coin_has_bzlib=no])
  fi

  if test $coin_has_bzlib = yes; then
    coin_foreach_w([myvar], [$1], [
                    m4_toupper(myvar)_LIBS="-lbz2 $m4_toupper(myvar)_LIBS"
                    m4_toupper(myvar)_PCLIBS="-lbz2 $m4_toupper(myvar)_PCLIBS"
                    m4_toupper(myvar)_LIBS_INSTALLED="-lbz2 $m4_toupper(myvar)_LIBS_INSTALLED"
                  ])
    AC_DEFINE([COIN_HAS_BZLIB],[1],[Define to 1 if bzlib is available])
  fi
fi
]) # AC_COIN_CHECK_GNU_BZLIB


###########################################################################
#                         COIN_CHECK_GNU_READLINE                         #
###########################################################################

# This macro checks for GNU's readline.  It verifies that the header
# readline/readline.h is available, and that the -lreadline library
# contains "readline".  It is assumed that #include <stdio.h> is included
# in the source file before the #include<readline/readline.h>
# If found, it defines the C preprocessor variable COIN_HAS_READLINE.
# Further, for a (space separated) list of arguments X, it adds the
# linker flag to the variable X_LIBS, X_PCLIBS, and X_LIBS_INSTALLED.

AC_DEFUN([AC_COIN_CHECK_GNU_READLINE],
[AC_REQUIRE([AC_COIN_ENABLE_GNU_PACKAGES])
AC_BEFORE([AC_COIN_PROG_CXX],[$0])
AC_BEFORE([AC_COIN_PROG_CC],[$0])
AC_BEFORE([AC_COIN_PROG_F77],[$0])
AC_BEFORE([$0],[AC_COIN_FINALIZE])

coin_has_readline=no
if test $coin_enable_gnu = yes; then
  AC_COIN_CHECK_HEADER([readline/readline.h],
                       [coin_has_readline=yes],[],
                       [#include <stdio.h>])

  coin_save_LIBS="$LIBS"
  LIBS=
  # First we check if tputs and friends are available
  if test $coin_has_readline = yes; then
    AC_SEARCH_LIBS([tputs],[ncurses termcap curses],[],
                   [coin_has_readline=no])
  fi

  # Now we check for readline
  if test $coin_has_readline = yes; then
    AC_CHECK_LIB([readline],[readline],[:],[coin_has_readline=no])
  fi

  if test $coin_has_readline = yes; then
    coin_foreach_w([myvar], [$1], [
                    m4_toupper(myvar)_LIBS="-lreadline $LIBS $m4_toupper(myvar)_LIBS"
                    m4_toupper(myvar)_PCLIBS="-lreadline $LIBS $m4_toupper(myvar)_PCLIBS"
                    m4_toupper(myvar)_LIBS_INSTALLED="-lreadline $LIBS $m4_toupper(myvar)_LIBS_INSTALLED"
                   ])
    AC_DEFINE([COIN_HAS_READLINE],[1],[Define to 1 if readline is available])
  fi

  LIBS="$coin_save_LIBS"
fi
]) # AC_COIN_CHECK_GNU_READLINE

###########################################################################
#                              COIN_CHECK_GMP                             #
###########################################################################

# This macro checks for the gmp library.  If found, it defines the C
# preprocessor variable COIN_HAS_GMP.  Further, for a (space separated) list
# of arguments X, it adds the linker flag to the variables X_LIBS, X_PCLIBS, and X_LIBS_INSTALLED.

AC_DEFUN([AC_COIN_CHECK_GMP],
[
AC_BEFORE([AC_COIN_PROG_CXX],[$0])
AC_BEFORE([AC_COIN_PROG_CC],[$0])
AC_BEFORE([AC_COIN_PROG_F77],[$0])
AC_BEFORE([$0],[AC_COIN_FINALIZE])

AC_ARG_ENABLE([gmp],
              [AC_HELP_STRING([--disable-gmp],[do not compile with GNU multiple precision library])],
              [coin_enable_gmp=$enableval],
              [coin_enable_gmp=yes])

coin_has_gmp=no
if test $coin_enable_gmp = yes; then
  AC_COIN_CHECK_HEADER([gmp.h],[AC_CHECK_LIB([gmp],[__gmpz_init],[coin_has_gmp=yes])])
  
  if test $coin_has_gmp = yes ; then
    coin_foreach_w([myvar], [$1], [
                    m4_toupper(myvar)_LIBS="-lgmp $m4_toupper(myvar)_LIBS"
                    m4_toupper(myvar)_PCLIBS="-lgmp $m4_toupper(myvar)_PCLIBS"
                    m4_toupper(myvar)_LIBS_INSTALLED="-lgmp $m4_toupper(myvar)_LIBS_INSTALLED"
                   ])
    AC_DEFINE([COIN_HAS_GMP],[1],[Define to 1 if GMP is available])
  fi
fi
]) # AC_COIN_CHECK_GMP

###########################################################################
#                            COIN_CHECK_ISFINITE                          #
###########################################################################

# This macro checks for a usable implementation of a function to check
# whether a given floating point number is finite.
# If a function is found, then the macro defines the symbol
# toupper($1)_C_FINITE to the name of this function.

AC_DEFUN([AC_COIN_CHECK_ISFINITE],[

AC_LANG_PUSH(C++)

AC_COIN_CHECK_CXX_CHEADER(math)
AC_COIN_CHECK_CXX_CHEADER(float)
AC_COIN_CHECK_CXX_CHEADER(ieeefp)

COIN_C_FINITE=
AC_CHECK_DECL([isfinite],[COIN_C_FINITE=isfinite],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])
if test -z "$COIN_C_FINITE"; then
  AC_CHECK_DECL([finite],[COIN_C_FINITE=finite],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])
  if test -z "$COIN_C_FINITE"; then
    AC_CHECK_DECL([_finite],[COIN_C_FINITE=_finite],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])
  fi
fi
if test -z "$COIN_C_FINITE"; then
  AC_MSG_WARN(Cannot find C-function for checking Inf.)
else
  AC_DEFINE_UNQUOTED(COIN_C_FINITE,[$COIN_C_FINITE],
                     [Define to be the name of C-function for Inf check])
fi

AC_LANG_POP(C++)
])

###########################################################################
#                              COIN_CHECK_ISNAN                           #
###########################################################################

# This macro checks for a usable implementation of a function to check
# whether a given floating point number represents NaN.
# If a function is found, then the macro defines the symbol COIN_C_ISNAN
# to the name of this function.

AC_DEFUN([AC_COIN_CHECK_ISNAN],[

AC_LANG_PUSH(C++)

AC_COIN_CHECK_CXX_CHEADER(math)
AC_COIN_CHECK_CXX_CHEADER(float)
AC_COIN_CHECK_CXX_CHEADER(ieeefp)

COIN_C_ISNAN=
AC_CHECK_DECL([isnan],[COIN_C_ISNAN=isnan],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])

# It appears that for some systems (e.g., Mac OSX), cmath will provide only
# std::isnan, and bare isnan will be unavailable. Typically we need a parameter
# in the test to allow C++ to do overload resolution.

if test -z "$COIN_C_ISNAN"; then
  AC_CHECK_DECL([std::isnan(42.42)],[COIN_C_ISNAN=std::isnan],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])
fi

if test -z "$COIN_C_ISNAN"; then
  AC_CHECK_DECL([_isnan],[COIN_C_ISNAN=_isnan],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])
fi
if test -z "$COIN_C_ISNAN"; then
  AC_CHECK_DECL([isnand],[COIN_C_ISNAN=isnand],,[
#ifdef HAVE_CMATH
# include <cmath>
#else
# ifdef HAVE_MATH_H
#  include <math.h>
# endif
#endif
#ifdef HAVE_CFLOAT
# include <cfloat>
#else
# ifdef HAVE_FLOAT_H
#  include <float.h>
# endif
#endif
#ifdef HAVE_CIEEEFP
# include <cieeefp>
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>
# endif
#endif])
fi
if test -z "$COIN_C_ISNAN"; then
  AC_MSG_WARN(Cannot find C-function for checking NaN.)
else
  AC_DEFINE_UNQUOTED(COIN_C_ISNAN,[$COIN_C_ISNAN],
                     [Define to be the name of C-function for NaN check])
fi

AC_LANG_POP(C++)
])

###########################################################################
#                             COIN_DATA_PATH                              #
###########################################################################

# This macro defines a preprocessor macro with the absolute path to a
# subdirectory of Data.  The argument of this macro is the name of the
# subdirectory (in correct case), and the name of the macro is
# COIN_DATA_DIR_PATH, where dir is replaced by the capitalized name of
# the directory.  The path ends with a separator ("/" for linux and
# '\\' for Windows).  The default value for this path can be
# overwritten with the input variable with the same name
# (COIN_DATA_DIR_PATH).  At this point we chech only for the
# $srcdir/../Data subdirectory.

AC_DEFUN([AC_COIN_DATA_PATH],
[AC_MSG_CHECKING([absolute path to data directory $1])

AC_ARG_VAR(m4_toupper(COIN_DATA_$1_PATH),[Set to absolute path to Data/$1 subdirectory])

if test x"$m4_toupper(COIN_DATA_$1_PATH)" = x; then
  if test -d $srcdir/../Data/$1 ; then
    m4_toupper(COIN_DATA_$1_PATH)=`cd $srcdir/../Data/$1; pwd`
  else
    m4_toupper(COIN_DATA_$1_PATH)=`cd $srcdir/../../Data/$1; pwd`
  fi
fi

# Under Cygwin, use Windows path.  Add separator
case $build in
  *-cygwin*)
    m4_toupper(COIN_DATA_$1_PATH)=`cygwin -w $m4_toupper(COIN_DATA_$1_PATH)`\\
    ;;
  *)
    m4_toupper(COIN_DATA_$1_PATH)="$m4_toupper(COIN_DATA_$1_PATH)/"
    ;;
esac

if test -d $m4_toupper(COIN_DATA_$1_PATH); then
  AC_DEFINE_UNQUOTED(m4_toupper(COIN_DATA_$1_PATH),["$m4_toupper(COIN_DATA_$1_PATH)"],
            [Define to absolute path for Data subdirectory $1])
  AC_MSG_RESULT($m4_toupper(COIN_DATA_$1_PATH))
else
  AC_MSG_ERROR(Directory $m4_toupper(COIN_DATA_$1_PATH) does not exist)
fi
]) # AC_COIN_DATA_PATH

###########################################################################
#                       COIN_LINK_FROM_FILELIST                           #
###########################################################################

# This macro creates links (or copies, if necessary) to files listed
# as content in a text file (second argument) into a target directory
# (first argument), which is created if it doesn't exist yet.  If s link
# already exists, nothing happens.

AC_DEFUN([AC_COIN_LINKCOPY_FROM_FILELIST],
[cmd="$3"
if test -r $srcdir/$2 ; then
  my_target_dir="$1"
  my_link_files=`cat $srcdir/$2`
  my_dirname=`AS_DIRNAME($2)`
#  if test -e $my_target_dir; then : ; else
#    AS_MKDIR_P($my_target_dir)
#  fi
  for i in $my_link_files; do
    #rm -rf $my_target_dir/$i
    if test -e $my_target_dir/$i; then : ; else
      dirn2=`AS_DIRNAME($my_target_dir/$i)`
      if test -d $dirn2; then : ; else
        AS_MKDIR_P($dirn2)
      fi
      $cmd $abs_source_dir/$my_dirname/$i $my_target_dir/$i
    fi
  done
else
  AC_MSG_WARN([File list file $2 missing!])
fi
])

AC_DEFUN([AC_COIN_LINK_FROM_FILELIST], 
[
AC_REQUIRE([AC_COIN_LINK_INPUT_CMD])
echo Creating links in $1 ...
AC_COIN_LINKCOPY_FROM_FILELIST($1, $2, $coin_link_input_cmd)
])

###########################################################################
#                       COIN_COPY_FROM_FILELIST                           #
###########################################################################

# Like COIN_LINK_FROM_FILELIST, but copies the files.

AC_DEFUN([AC_COIN_COPY_FROM_FILELIST], 
[
echo Creating copies in $1 ...
AC_COIN_LINKCOPY_FROM_FILELIST($1, $2, [cp])
])

###########################################################################
#                          COIN_EXAMPLE_FILES                             #
###########################################################################

# This macro determines the names of the example files (using the
# argument in an "ls" command) and sets up the variables EXAMPLE_FILES
# and EXAMPLE_CLEAN_FILES.  If this is a VPATH configuration, it also
# creates soft links to the example files.

AC_DEFUN([AC_COIN_EXAMPLE_FILES],
[AC_REQUIRE([AC_COIN_CHECK_VPATH])
AC_REQUIRE([AC_COIN_ENABLE_MSVC])
AC_REQUIRE([AC_PROG_LN_S])

files=`cd $srcdir; ls $1`
# We need to do the following loop to make sure that are no newlines
# in the variable
for file in $files; do
  EXAMPLE_FILES="$EXAMPLE_FILES $file"
done
if test $coin_vpath_config = yes; then
  lnkcmd=
  if test "$enable_msvc" = yes; then
    lnkcmd=cp
  fi
  case "$CC" in
    clang* ) ;;
    cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
      lnkcmd=cp ;;
  esac
  if test "x$lnkcmd" = xcp; then
    AC_MSG_NOTICE([Copying example files ($1)])
  else
    AC_MSG_NOTICE([Creating links to the example files ($1)])
    lnkcmd="$LN_S"
  fi
  for file in $EXAMPLE_FILES; do
    rm -f $file
    $lnkcmd $srcdir/$file $file
  done
  EXAMPLE_CLEAN_FILES="$EXAMPLE_CLEAN_FILES $1"
else
  EXAMPLE_CLEAN_FILES="$EXAMPLE_CLEAN_FILES"
fi

# In case there are compressed files, we create a variable with the
# uncompressed names
EXAMPLE_UNCOMPRESSED_FILES=
for file in $EXAMPLE_FILES; do
  case $file in
    *.gz)
      EXAMPLE_UNCOMPRESSED_FILES="$EXAMPLE_UNCOMPRESSED_FILES `echo $file | sed -e s/.gz//`"
      ;;
  esac
done

AC_SUBST(EXAMPLE_UNCOMPRESSED_FILES)
AC_SUBST(EXAMPLE_FILES)
AC_SUBST(EXAMPLE_CLEAN_FILES)
]) #AC_COIN_EXAMPLE_FILES

###########################################################################
#                      COIN_CHECK_USER_LIBRARY                            #
###########################################################################
# This macro sets up usage of a user library with header files. The assumption
# is that the header file(s) and library do not reside in standard system
# directories, hence both the include directory and link flags must be
# specified. There are two mandatory arguments and two optional arguments.
#
# The first argument (mandatory) should be a name (LibraryName) for the
# library.  The second argument (mandatory) should be an abbreviation in
# upper case letters (LBRY) for the library. Ultimately, the macro will
# specify two variables, LBRYINCDIR and LBRYLIB, to be substituted in files
# generated during configuration; a preprocessor symbol COIN_HAS_LBRY; and a
# matching automake conditional COIN_HAS_LBRY. LBRYINCDIR should specify the
# directory containing include files for the library. LBRYLIB should specify
# the flags necessary to link to the library. A variable coin_has_lbry will
# be set to true or false, as appropriate. A variable lbry_libcheck will be
# be set to yes or no; no indicates link checks should not be attempted.
#
# The macro defines three configure arguments, --with-libraryname-incdir,
# --with-libraryname-lib, and --disable-libraryname-libcheck, by converting
# LibraryName to lower case.
#
# LBRYINCDIR and LBRYLIB can be specified as environment variables or as
# part of the configure command using --with-libraryname-incdir and
# --with-libraryname-lib, respectively. Command line arguments override
# environment variables.
#
# If a third argument is given, it should specify a file in LBRYINCDIR.  The
# macro will check for the presence of the file. If a fourth argument is given,
# it should specify a function name, `fname'.  The macro will attempt to link a
# trivial program containing a parameterless call to the function, `fname()',
# using the LBRYLIB flags. The link check uses C as the language; this has been
# adequate to date but has limitations. It is possible to disable the link
# check by specifying --disable-libraryname-libcheck. This is a workaround for
# instances where the link check does not work properly, for whatever reason.
# If you're trying to link to a Fortran library, consider using F77_FUNC or
# FC_FUNC to obtain a mangled fname appropriate for use from C code. For a C++
# library, you're on your own unless the library declares some function with
# extern "C" linkage. Otherwise, you'll have to somehow find the mangled C++
# name.
# A fifth argument can be specified to include linker flags that may be required
# to sucessfully perform the linking check.
#
# An optional sixth argument can be given to specify a list of targets.
# For each target X, the variables X_LIBS and X_PCLIBS will be extended by $LBRYLIB,
# if the library has been found and seems to work.

AC_DEFUN([AC_COIN_CHECK_USER_LIBRARY],
[ AC_REQUIRE([AC_COIN_PROJECTDIR_INIT])
  AC_MSG_CHECKING(if user provides library for $1)

# Check for header file directory

  AC_ARG_WITH(m4_tolower($1)-incdir,
      AS_HELP_STRING([--with-m4_tolower($1)-incdir],
		     [specify the header file directory for library $1]),
      [$2INCDIR=`cd $withval; pwd`])

# Check for library directory

  AC_ARG_WITH(m4_tolower($1)-lib,
      AS_HELP_STRING([--with-m4_tolower($1)-lib],
		     [specify the flags used to link with the library $1]),
      [$2LIB=$withval])

# Switch to disable library check if requested

  AC_ARG_ENABLE(m4_tolower($1)-libcheck,
      AS_HELP_STRING([--disable-m4_tolower($1)-libcheck],
		     [skip the link check at configuration time]),
      [m4_tolower($1)_libcheck=$enableval],
      [m4_tolower($1)_libcheck=yes])

# At this point, if we're going to use the library, both LBRYINCDIR and
# LBRYLIB must be defined and not empty.

  if test x"$$2INCDIR" != x || test x"$$2LIB" != x; then
    if test x"$$2INCDIR" = x || test x"$$2LIB" = x; then
      AC_MSG_ERROR([You need to specify both an include directory and link flags to use library $1. Use --with-m4_tolower($1)-incdir of environment variable $$2INCDIR to specify the include directory. Use --with-m4_tolower($1)-lib or environment variable $$2LIB to specify link flags.])
    fi
    m4_tolower(coin_has_$2)=true
    AC_MSG_RESULT(yes)
  else
    m4_tolower(coin_has_$2)=false
    AC_MSG_RESULT(no)
  fi

# If we have instructions for use, consider header and link checks.

  if test $m4_tolower(coin_has_$2) = true; then

# If argument 3 (file) is given, check for the file. Typically this will be a
# header file, but that's not assumed.

    m4_ifval([$3],
        [AC_COIN_CHECK_FILE([$$2INCDIR/$3],[],
	     [AC_MSG_ERROR([Cannot find file $3 in $$2INCDIR])])])

# Now see if we can link the function. There are arguments for and against
# assuming argument 3 is a header file declaring the function. A correct
# function declaration is the main argument in favour. Having to cope with
# possible dependencies or other oddities are the main arguments against.
# Force the use of C as the best single choice amongst C++, C, and Fortran.
# Obviously, this has limits.

    m4_ifvaln([$4],
        [if test x"$m4_tolower($1)_libcheck" != xno; then
	   coin_save_LIBS="$LIBS"
	   LIBS="$$2LIB $5"
	   coin_$2_link=no
	   AC_LANG_PUSH(C)
	   for fnm in $4 ; do
	     AC_MSG_CHECKING([whether symbol $fnm is available with $2])
	     AC_LINK_IFELSE([AC_LANG_PROGRAM([[]],[[$fnm()]])],
		 [AC_MSG_RESULT(yes)
		  coin_$2_link=yes
		  break],
		 [AC_MSG_RESULT(no)])
	   done
	   AC_LANG_POP(C)
           LIBS="$coin_save_LIBS"
	   if test x"$coin_$2_link" != xyes ; then
	     AC_MSG_ERROR([Cannot find symbol(s) $4 with $2])
	   fi
	 fi])

# If we make it this far, we've verified the file and linked the function. Add
# the necessary link flags to $6_{PC}LIBS and define the preprocessor symbol
# COIN_HAS_LBRY.

    coin_foreach_w([myvar], [$6], [
      m4_toupper(myvar)_LIBS="$$2LIB $m4_toupper(myvar)_LIBS"
      m4_toupper(myvar)_PCLIBS="$$2LIB $m4_toupper(myvar)_PCLIBS"
      m4_toupper(myvar)_LIBS_INSTALLED="$$2LIB $m4_toupper(myvar)_LIBS_INSTALLED"
    ])
    
    AC_DEFINE(COIN_HAS_$2,[1],[Define to 1 if the $1 package is available])
  fi

# Arrange for configure to substitute LBRYINCDIR and LBRYLIB and create the
# automake conditional. These actions must occur unconditionally.

  AC_SUBST($2INCDIR)
  AC_SUBST($2LIB)
  AM_CONDITIONAL(COIN_HAS_$2, test $m4_tolower(coin_has_$2) = true)
]) #AC_COIN_CHECK_USER_LIBRARY 

###########################################################################
#                            COIN_TRY_FLINK                               #
###########################################################################

# Auxilliary macro to test if a Fortran function name can be linked,
# given the current settings of LIBS.  We determine from the context, what
# the currently active programming language is, and cast the name accordingly.
# The first argument is the name of the function/subroutine, in small letters,
# the second argument are the actions taken when the test works, and the
# third argument are the actions taken if the test fails.

AC_DEFUN([AC_COIN_TRY_FLINK],
[case $ac_ext in
  f)
    AC_TRY_LINK(,[      call $1],[$2],[$3])
    ;;
  c)
    AC_F77_FUNC($1,cfunc$1)
    if test x"$coin_need_flibs" = xyes; then
      flink_try=no;
    else
      AC_TRY_LINK([void $cfunc$1();],[$cfunc$1()],
                  [flink_try=yes],[flink_try=no])
    fi
    if test $flink_try = yes; then
      $2
    else
      if test x"$FLIBS" != x; then
        flink_save_libs="$LIBS"
        LIBS="$LIBS $FLIBS"
        AC_TRY_LINK([void $cfunc$1();],[$cfunc$1()],
                    [LIBS="$flink_save_libs"
                     coin_need_flibs=yes
                     $2
                    ],
                    [LIBS="$flink_save_libs"
                     $3])
      else
        $3
      fi
    fi
    ;;
  cc|cpp)
    AC_F77_FUNC($1,cfunc$1)
    if test x"$coin_need_flibs" = xyes; then
      flink_try=no;
    else
      AC_TRY_LINK([extern "C" {void $cfunc$1();}],[$cfunc$1()],
                  [flink_try=yes],[flink_try=no])
    fi
    if test $flink_try = yes; then
      $2
    else
      if test x"$FLIBS" != x; then
        flink_save_libs="$LIBS"
        LIBS="$LIBS $FLIBS"
        AC_TRY_LINK([extern "C" {void $cfunc$1();}],[$cfunc$1()],
                    [LIBS="$flink_save_libs"
                     coin_need_flibs=yes
                     $2
                    ],
                    [LIBS="$flink_save_libs"
                     $3])
      else
        $3
      fi
    fi
    ;;
esac
]) # AC_COIN_TRY_FLINK

###########################################################################
#                           COIN_DOXYGEN                                  #
###########################################################################
#
# This macro determines the configuration information for doxygen, the tool
# used to generate online documentation of COIN code. It takes one parameter,
# a list of projects (mixed-case, to match the directory names) that should
# be processed as external tag files. E.g., COIN_DOXYGEN([Clp Osi]).
#
# This macro will define the following variables:
#  coin_have_doxygen	Yes if doxygen is found, no otherwise
#  coin_doxy_usedot     Defaults to `yes'; --with-dot will still check to see
#			if dot is available
#  coin_doxy_tagname	Name of doxygen tag file (placed in doxydoc directory)
#  coin_doxy_logname    Name of doxygen log file (placed in doxydoc directory)
#  coin_doxy_tagfiles   List of doxygen tag files used to reference other
#                       doxygen documentation
#  coin_doxy_excludes	Directories to exclude from doxygen processing

AC_DEFUN([AC_COIN_DOXYGEN],
[

AC_MSG_NOTICE([configuring doxygen documentation options])

# Check to see if doxygen is available.

AC_CHECK_PROG([coin_have_doxygen],[doxygen],[yes],[no])
AC_CHECK_PROG([coin_have_latex],[latex],[yes],[no])

# Look for the dot tool from the graphviz package, unless the user has
# disabled it.

AC_ARG_WITH([dot],
  AS_HELP_STRING([--with-dot],
		 [use dot (from graphviz) when creating documentation with
		  doxygen if available; --without-dot to disable]),
  [],[withval=yes])
if test x"$withval" = xno ; then
  coin_doxy_usedot=NO
  AC_MSG_CHECKING([for dot ])
  AC_MSG_RESULT([disabled])
else
  AC_CHECK_PROG([coin_doxy_usedot],[dot],[YES],[NO])
fi

# Generate a tag file name and a log file name

AC_SUBST([coin_doxy_tagname],[doxydoc/${PACKAGE}_doxy.tag])
AC_SUBST([coin_doxy_logname],[doxydoc/${PACKAGE}_doxy.log])
AM_CONDITIONAL(COIN_HAS_DOXYGEN, [test $coin_have_doxygen = yes])
AM_CONDITIONAL(COIN_HAS_LATEX, [test $coin_have_latex = yes])

# Process the list of project names and massage them into possible doxygen
# doc'n directories. Prefer 1) classic external, source processed using
# a project-specific doxygen.conf, we use the tag file; 2) classic
# external, source processed using package doxygen.conf; 3) installed
# doxydoc. Alternatives 1) and 2) are only possible if the directory will be
# configured, which we can't know unless this is the package base configure,
# since coin_subdirs is only set there. Hence it's sufficient to check for
# membership. If we use a tag file from a classic external, exclude the
# source from doxygen processing when doxygen runs in the base directory.

coin_doxy_tagfiles=
coin_doxy_excludes=
tmp="$1"
for proj in $tmp ; do
  lc_proj=`echo $proj | [tr [A-Z] [a-z]]`
  AC_MSG_CHECKING([for doxygen doc'n for $proj ])
  doxytag=${lc_proj}_doxy.tag
  doxyfound=no
  # proj will be configured, hence doxydoc present in build tree
  doxysrcdir="${srcdir}/../${proj}"
  # AC_MSG_NOTICE([Considering $doxysrcdir (base)])
  if test -d "$doxysrcdir" ; then
    # with a doxydoc directory?
    doxydir="$doxysrcdir/doxydoc"
    # AC_MSG_NOTICE([Considering $doxydir (base)])
    # AC_MSG_NOTICE([Subdirs: $coin_subdirs)])
    if test -d "$doxydir" ; then
      # use tag file; don't process source
      doxydir="../${proj}/doxydoc"
      coin_doxy_tagfiles="$coin_doxy_tagfiles $doxydir/$doxytag=../../$doxydir/html"
      AC_MSG_RESULT([$doxydir (tag)])
      coin_doxy_excludes="$coin_doxy_excludes */${proj}"
    else
      # will process the source -- nothing further to be done here
      AC_MSG_RESULT([$doxysrcdir (src)])
    fi
    doxyfound=yes
  fi
  # Not built, fall back to installed tag file
  if test $doxyfound = no ; then
    eval doxydir="${datadir}/coin/doc/${proj}/doxydoc"
    # AC_MSG_NOTICE([Considering $doxydir (install)])
    # AC_MSG_NOTICE([Subdirs: $coin_subdirs)])
    coin_doxy_tagfiles="$coin_doxy_tagfiles $doxydir/$doxytag=$doxydir/html"
    AC_MSG_RESULT([$doxydir (tag)])
  fi
done
AC_SUBST([coin_doxy_tagfiles])
AC_SUBST([coin_doxy_excludes])

]) # AC_COIN_DOXYGEN


###########################################################################
#                           COIN_HAS_PKGCONFIG                            #
###########################################################################

# This macro checks whether a pkg-config tool with a minimal version number
# is available.  If so, then the variable PKGCONFIG is set to its path.
# If not, PKGCONFIG is set to "".  The minimal version number can be given
# as first parameter, by default it is 0.16.0, since COIN-OR .pc files now
# include an URL field, which breaks pkg-config version <= 0.15.
# This macro is a modified version of PKG_PROG_PKG_CONFIG in pkg.m4.
# Further, the AM_CONDITIONAL COIN_HAS_PKGCONFIG is set and PKGCONFIG is
# AC_SUBST'ed.  Finally, if this setup belongs to a project directory, then
# the search path for .pc files is assembled from the value of
# $PKG_CONFIG_PATH, the values of --prefix, --coin-instdir, and the directories
# named in a file ../coin_subdirs.txt or ../../coin_subdirs.txt in a variable
# COIN_PKG_CONFIG_PATH, which is also AC_SUBST'ed. For a path xxx given in the
# coin-subdirs.txt, also the directory xxx/pkgconfig is added, if existing.

AC_DEFUN([AC_COIN_HAS_PKGCONFIG],
[AC_ARG_VAR([PKG_CONFIG], [path to pkg-config utility])

AC_ARG_ENABLE([pkg-config],
  [AC_HELP_STRING([--disable-pkg-config],[disable use of pkg-config (if available)])],
  [use_pkgconfig="$enableval"],
  [if test x$coin_cc_is_cl = xtrue; then
     use_pkgconfig=no
   else
     use_pkgconfig=yes
   fi])

if test $use_pkgconfig = yes ; then
  if test "x$ac_cv_env_PKG_CONFIG_set" != "xset"; then
    AC_CHECK_TOOL([PKG_CONFIG], [pkg-config])
  fi
  if test -n "$PKG_CONFIG"; then
    _pkg_min_version=m4_default([$1], [0.16.0])
    AC_MSG_CHECKING([pkg-config is at least version $_pkg_min_version])
    if $PKG_CONFIG --atleast-pkgconfig-version $_pkg_min_version; then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
      PKG_CONFIG=""
    fi
  fi

  # check if pkg-config supports the short-errors flag
  if test -n "$PKG_CONFIG" && \
    $PKG_CONFIG --atleast-pkgconfig-version 0.20; then
    pkg_short_errors=" --short-errors "
  else
    pkg_short_errors=""
  fi
fi

AM_CONDITIONAL([COIN_HAS_PKGCONFIG], [test -n "$PKG_CONFIG"])
AC_SUBST(PKG_CONFIG)

# assemble pkg-config search path for installed projects
COIN_PKG_CONFIG_PATH="$PKG_CONFIG_PATH"

# let's assume that when installing into $prefix, then the user may have installed some other coin projects there before, so it's worth to have a look into there
# best would actually to use ${libdir}, since .pc files get installed into ${libdir}/pkgconfig,
# unfortunately, ${libdir} expands to ${exec_prefix}/lib and ${exec_prefix} to ${prefix}...
if test "x${prefix}" = xNONE ; then
  COIN_PKG_CONFIG_PATH="${ac_default_prefix}/lib64/pkgconfig:${ac_default_prefix}/lib/pkgconfig:${ac_default_prefix}/share/pkgconfig:${COIN_PKG_CONFIG_PATH}"
else
  COIN_PKG_CONFIG_PATH="${prefix}/lib64/pkgconfig:${prefix}/lib/pkgconfig:${prefix}/share/pkgconfig:${COIN_PKG_CONFIG_PATH}"
fi

AC_ARG_WITH([coin-instdir],
  AC_HELP_STRING([--with-coin-instdir],
                 [prefix of installation directory for precompiled COIN packages]),
  [if test -d "$withval"; then : ; else
     AC_MSG_ERROR([argument for --with-coin-instdir not a directory])
   fi
   COIN_PKG_CONFIG_PATH="$withval/lib/pkgconfig:$withval/share/pkgconfig:${COIN_PKG_CONFIG_PATH}"
  ],[])

AC_SUBST(COIN_PKG_CONFIG_PATH)

# assemble additional pkg-config search paths for uninstalled projects
if test x$coin_projectdir = xyes ; then
  # if we are in a project setup, then in a classic setup, we want to find uninstalled projects
  # their (relative) location is written to coin_subdirs.txt by the configure in the project base directory
  # unfortunately, if the user set prefix, then we do not know where the project base directory is located
  # but it is likely to be either .. (if we are a usual coin project) or ../.. (if we are a unusual coin project like ThirdParty or Data)
  COIN_PKG_CONFIG_PATH_UNINSTALLED=
  if test -f ../coin_subdirs.txt ; then
    for i in `cat ../coin_subdirs.txt` ; do
      if test -d ../$i ; then
        COIN_PKG_CONFIG_PATH_UNINSTALLED="`cd ../$i; pwd`:${COIN_PKG_CONFIG_PATH_UNINSTALLED}"
      fi
      if test -d ../$i/pkgconfig ; then
        COIN_PKG_CONFIG_PATH_UNINSTALLED="`cd ../$i/pkgconfig; pwd`:${COIN_PKG_CONFIG_PATH_UNINSTALLED}"
      fi
    done
  fi

  if test -f ../../coin_subdirs.txt ; then
    for i in `cat ../../coin_subdirs.txt` ; do
      if test -d ../../$i ; then
        COIN_PKG_CONFIG_PATH_UNINSTALLED="`cd ../../$i; pwd`:${COIN_PKG_CONFIG_PATH_UNINSTALLED}"
      fi
      if test -d ../../$i/pkgconfig ; then
        COIN_PKG_CONFIG_PATH_UNINSTALLED="`cd ../../$i/pkgconfig; pwd`:${COIN_PKG_CONFIG_PATH_UNINSTALLED}"
      fi
    done
  fi

  if test -f ../../../coin_subdirs.txt ; then
    for i in `cat ../../../coin_subdirs.txt` ; do
      if test -d ../../../$i ; then
        COIN_PKG_CONFIG_PATH_UNINSTALLED="`cd ../../../$i; pwd`:${COIN_PKG_CONFIG_PATH_UNINSTALLED}"
      fi
      if test -d ../../../$i/pkgconfig ; then
        COIN_PKG_CONFIG_PATH_UNINSTALLED="`cd ../../../$i/pkgconfig; pwd`:${COIN_PKG_CONFIG_PATH_UNINSTALLED}"
      fi
    done
  fi

  AC_SUBST(COIN_PKG_CONFIG_PATH_UNINSTALLED)
fi

if test -n "$PKG_CONFIG" && test x$coin_cc_is_cl = xtrue; then
  AC_MSG_WARN([Using pkg-config together with MS or Intel Compiler on Windows is not support by example Makefiles. Consider using --disable-pkg-config.])
fi

])

###########################################################################
#                           COIN_PKG_CHECK_PROJECT_EXISTS                 #
###########################################################################

# COIN_PKG_CHECK_PROJECT_EXISTS(PROJECT, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
#
# Check to see whether a particular project exists.  Similar
# to PKG_CHECK_MODULES(), but set only the variables $1_VERSION and $1_PKG_ERRORS variables
#
AC_DEFUN([AC_COIN_PKG_CHECK_PROJECT_EXISTS],
[AC_REQUIRE([AC_COIN_HAS_PKGCONFIG])
if test -n "$PKG_CONFIG" ; then
  if $PKG_CONFIG --exists "m4_tolower($1)"; then
    m4_toupper($1)[]_VERSION=`$PKG_CONFIG --modversion "m4_tolower($1)" 2>/dev/null`
    m4_ifval([$2], [$2], [:])
  else
    m4_toupper($1)_PKG_ERRORS=`$PKG_CONFIG $pkg_short_errors --errors-to-stdout --print-errors "m4_tolower($1)"`
    $3
  fi
else
  AC_MSG_ERROR("Cannot check for existance of module $1 without pkg-config")
fi
])

###########################################################################
#                           COIN_PKG_CHECK_MODULE_EXISTS                  #
###########################################################################

# COIN_PKG_CHECK_MODULES_EXISTS(MODULE, PACKAGES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
#
# Check to see whether a particular set of packages exists.
# Similar to PKG_CHECK_MODULES(), but set only the variable $1_VERSIONS and $1_PKG_ERRORS
#
AC_DEFUN([AC_COIN_PKG_CHECK_MODULE_EXISTS],
[AC_REQUIRE([AC_COIN_HAS_PKGCONFIG])
if test -n "$PKG_CONFIG" ; then
  if $PKG_CONFIG --exists "$2"; then
    m4_toupper($1)[]_VERSIONS=`$PKG_CONFIG --modversion "$2" 2>/dev/null | tr '\n' ' '`
    $3
  else
    m4_toupper($1)_PKG_ERRORS=`$PKG_CONFIG $pkg_short_errors --errors-to-stdout --print-errors "$2"`
    $4
  fi
else
  AC_MSG_ERROR("Cannot check for existance of module $1 without pkg-config")
fi
])

###########################################################################
#                           COIN_PKG_HAS_MODULE                           #
###########################################################################

# COIN_PKG_HAS_MODULE(MODULE, PACKAGES, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
#
# Checks whether pkg-config files for a given set of packages is available.
# If so, sets MODULE_CFLAGS, MODULE_LIBS, and MODULES_DATA and executes ACTION-IF-FOUND.
# If not, then ACTION-IF-NOT-FOUND is executed.
# A reason for not finding a package is stored in MODULE_PKG_ERRORS
#
# --------------------------------------------------------------
AC_DEFUN([AC_COIN_PKG_HAS_MODULE],
[AC_REQUIRE([AC_COIN_HAS_PKGCONFIG])

AC_COIN_PKG_CHECK_MODULE_EXISTS([$1],[$2],
  [ cflags=`$PKG_CONFIG --cflags "$2" 2>/dev/null`
    # pkg-config cannot handle spaces, so CYGPATH_W cannot be put into .pc files
	# thus, we modify the cflags extracted from pkg-config by putting CYGPATH_W behind -I's
	# but only do this if is not trivial
    if test "$CYGPATH_W" != "echo" ; then
      # need to put into brackets since otherwise autoconf replaces the brackets in the sed command
      [cflags=`echo $cflags | sed -e 's/-I\([^ ]*\)/-I\`${CYGPATH_W} \1\`/g'`]
    fi
    m4_toupper($1)[]_CFLAGS="$cflags"
    m4_toupper($1)[]_LIBS=`$PKG_CONFIG --libs "$2" 2>/dev/null`
    m4_toupper($1)[]_DATA=`$PKG_CONFIG --variable=datadir "$2" 2>/dev/null`
    $3
  ],
  [ $4 ])

])# PKG_CHECK_MODULES

###########################################################################
#                           COIN_MAIN_PACKAGEDIR                          #
###########################################################################

# This macro substitutes COIN_MAIN_SUBDIR.
# If $2/$1 or $1 is in COIN_SKIP_PROJECTS, do nothing.
# If --with-$1-lib, --with-$1-incdir, or --with-$1-datadir is given, then assume that the package is installed.
# Otherwise, if the directory $2/$1 and the file $2/$1/$3 exist, check whether $2/$1/configure exists.
#   If so, include this directory into the list of directories where configure and make recourse into.
# tolower(coin_has_$1) is set to "no" if the project source is not available or will not be compiled.
# Otherwise, it will be set to "yes".

AC_DEFUN([AC_COIN_MAIN_PACKAGEDIR],[
AC_MSG_CHECKING([whether source of project $1 is available and should be compiled])

m4_tolower(coin_has_$1)=notGiven
coin_reason=

# check if user wants to skip project in any case
AC_ARG_VAR([COIN_SKIP_PROJECTS],[Set to the subdirectories of projects that should be skipped in the configuration])
if test x"$COIN_SKIP_PROJECTS" != x; then
  for dir in $COIN_SKIP_PROJECTS; do
    if test $dir = "$1"; then
      m4_tolower(coin_has_$1)="no"
      coin_reason="$1 has been specified in COIN_SKIP_PROJECTS"
    fi
    m4_ifval($2,[
    if test $dir = "$2/$1"; then
      m4_tolower(coin_has_$1)="no"
      coin_reason="$2/$1 has been specified in COIN_SKIP_PROJECTS"
    fi])
  done
fi

if test "$m4_tolower(coin_has_$1)" != no; then
  AC_ARG_WITH([m4_tolower($1)],,
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)="no"
       coin_reason="--without-m4_tolower($1) has been specified"
     fi
    ])
fi

if test "$m4_tolower(coin_has_$1)" != no; then
  AC_ARG_WITH([m4_tolower($1)-lib],
    AC_HELP_STRING([--with-m4_tolower($1)-lib],
                   [linker flags for using project $1]),
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)="no"
       coin_reason="--without-m4_tolower($1)-lib has been specified"
     else
       m4_tolower(coin_has_$1)="no"
       coin_reason="--with-m4_tolower($1)-lib has been specified"
     fi],
    [])
fi

if test "$m4_tolower(coin_has_$1)" != no; then
  AC_ARG_WITH([m4_tolower($1)-incdir],
    AC_HELP_STRING([--with-m4_tolower($1)-incdir],
                   [directory with header files for using project $1]),
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)="no"
       coin_reason="--without-m4_tolower($1)-incdir has been specified"
     else
       m4_tolower(coin_has_$1)="no"
       coin_reason="--with-m4_tolower($1)-incdir has been specified"
     fi],
    [])
fi

if test "$m4_tolower(coin_has_$1)" != no; then
  AC_ARG_WITH([m4_tolower($1)-datadir],
    AC_HELP_STRING([--with-m4_tolower($1)-datadir],
                   [directory with data files for using project $1]),
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)="no"
       coin_reason="--without-m4_tolower($1)-datadir has been specified"
     else
       m4_tolower(coin_has_$1)="no"
       coin_reason="--with-m4_tolower($1)-datadir has been specified"
     fi],
    [])
fi

m4_if(m4_tolower($1), blas, [
  if test $m4_tolower(coin_has_$1) != no; then
    #--with-blas can overwrite --with-blas-lib, and can be set to BUILD to enforce building blas
    AC_ARG_WITH([blas],
      AC_HELP_STRING([--with-blas], [specify BLAS library (or BUILD to enforce use of ThirdParty/Blas)]),
        [if test x"$withval" = "xno" ; then
           coin_has_blas="no"
           coin_reason="--without-blas has been specified"
         elif test x"$withval" != "xBUILD" ; then
           coin_has_blas="no"
           coin_reason="--with-blas has been specified"
         fi],
        [])
  fi
])

m4_if(m4_tolower($1), lapack, [
  if test $m4_tolower(coin_has_$1) != no; then
    #--with-lapack can overwrite --with-lapack-lib, and can be set to BUILD to enforce building lapack
    AC_ARG_WITH([lapack],
      AC_HELP_STRING([--with-lapack], [specify LAPACK library (or BUILD to enforce use of ThirdParty/Lapack)]),
        [if test x"$withval" = "xno" ; then
           coin_has_lapack="no"
           coin_reason="--without-lapack has been specified"
         elif test x"$withval" != "xBUILD" ; then
           coin_has_lapack="no"
           coin_reason="--with-lapack has been specified"
         fi],
        [])
  fi
])

# check if project is available in present directory
if test "$m4_tolower(coin_has_$1)" = notGiven; then
  m4_tolower(coin_has_$1)=no
  if test -d $srcdir/m4_ifval($2,[$2/],)$1; then
    coin_reason="source in m4_ifval($2,[$2/],)$1"
    # If a third argument is given, then we have to check if one one the files given in that third argument is present.
    # If none of the files in the third argument is available, then we consider the project directory as non-existing.
    # However, if no third argument is given, then this means that there should be no check, and existence of the directory is sufficient.
    m4_ifvaln([$3],
      [for i in $srcdir/m4_ifval($2,[$2/],)$1/$3; do
         if test -r $i; then
           m4_tolower(coin_has_$1)="yes"
         else
           m4_tolower(coin_has_$1)="no"
           coin_reason="source file $i not available"
           break
         fi
       done],
      [ m4_tolower(coin_has_$1)="yes" ])
  fi
fi

if test -z "$coin_reason" ; then
  AC_MSG_RESULT([$m4_tolower(coin_has_$1)])
else
  AC_MSG_RESULT([$m4_tolower(coin_has_$1), $coin_reason])
fi

if test "$m4_tolower(coin_has_$1)" = yes ; then
  if test -r $srcdir/m4_ifval($2,[$2/],)$1/configure; then
    coin_subdirs="$coin_subdirs m4_ifval($2,[$2/],)$1"
    AC_CONFIG_SUBDIRS(m4_ifval($2,[$2/],)$1)
  fi
fi
])

###########################################################################
#                            COIN_CHECK_PACKAGE                           #
###########################################################################

# This macro checks for the existance of a COIN-OR package and provides compiler and linker flags to compile against this package.
# A package can consists of one or more COIN-OR or other projects.
# It defines the PACKAGE_CFLAGS, PACKAGE_LIBS, PACKAGE_DEPENDENCIES, and PACKAGE_DATA variables, referring to the compiler and linker
# flags to use when linking against this module, the libraries the package depends on, and the directories where the module data resists.
# The difference between PACKAGE_LIBS and PACKAGE_DEPENDENCIES is that PACKAGE_DEPENDENCIES does not contain arguments starting with '-',
# so it can be used to setup the _DEPENDENCIES variable in a Makefile.am.
# It also defines a COIN_HAS_PACKAGE preprocessor macro and makefile conditional.
# Further, tolower(coin_has_$1) is set to "yes".
# If a list of build targets using this projects is given in the third argument,
# then the compiler and linker variables and .pc file setup variable corresponding to this build target
# are extended with the values for this package.
# That is, for each build target X, the variables X_CFLAGS, X_LIBS, X_DEPENDENCIES, X_PCLIBS, X_PCREQUIRES are setup,
# whereas the last two specify the values to put into the "Libs:" and "Requires:" fields of the .pc file, resp.
#
# The first argument should be the name (PACKAGE) of the package (in correct lower
# and upper case).
# The second argument should be a (space separated) list of projects which this
# package consists of. Optionally, required version numbers can be added.
# The optional third argument should be a (space separated) list of build targets
# which use this package, if available.
#
# It is also possible to specify a preinstalled version of this package
# or to specify only the linker and compiler flags and data directory.
#
# If the user did not specify --with-$1-... flags and pkg-config is not available,
# COIN_CHECK_PACKAGE_FALLBACK($1, $2, $3) is called.

AC_DEFUN([AC_COIN_CHECK_PACKAGE],
[AC_REQUIRE([AC_COIN_HAS_PKGCONFIG])
AC_MSG_CHECKING([for COIN-OR package $1])

m4_tolower(coin_has_$1)=notGiven

# check if user wants to skip package in any case
if test x"$COIN_SKIP_PROJECTS" != x; then
  for dir in $COIN_SKIP_PROJECTS; do
    if test $dir = "$1"; then
      m4_tolower(coin_has_$1)=skipping
    fi
  done
fi

if test "$m4_tolower(coin_has_$1)" != skipping; then
  AC_ARG_WITH([m4_tolower($1)],,
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)=skipping
     fi
    ])
fi

m4_toupper($1_LIBS)=
m4_toupper($1_CFLAGS)=
m4_toupper($1_DATA)=
m4_toupper($1_DEPENDENCIES)=
m4_toupper($1_PCLIBS)=
m4_toupper($1_PCREQUIRES)=
AC_SUBST(m4_toupper($1_LIBS))
AC_SUBST(m4_toupper($1_CFLAGS))
AC_SUBST(m4_toupper($1_DATA))
AC_SUBST(m4_toupper($1_DEPENDENCIES))
AC_SUBST(m4_toupper($1_LIBS_INSTALLED))
AC_SUBST(m4_toupper($1_CFLAGS_INSTALLED))
AC_SUBST(m4_toupper($1_DATA_INSTALLED))
coin_foreach_w([myvar], [$3], [
  AC_SUBST(m4_toupper(myvar)_CFLAGS)
  AC_SUBST(m4_toupper(myvar)_LIBS)
  AC_SUBST(m4_toupper(myvar)_PCLIBS)
  AC_SUBST(m4_toupper(myvar)_PCREQUIRES)
  AC_SUBST(m4_toupper(myvar)_DEPENDENCIES)
  AC_SUBST(m4_toupper(myvar)_CFLAGS_INSTALLED)
  AC_SUBST(m4_toupper(myvar)_LIBS_INSTALLED)
])

#check if user provided LIBS, CFLAGS, or DATA for package or disables use of package
if test $m4_tolower(coin_has_$1) != skipping; then
  AC_ARG_WITH([m4_tolower($1)-lib],
    AC_HELP_STRING([--with-m4_tolower($1)-lib],
                   [linker flags for using package $1]),
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)=skipping
     else
       m4_tolower(coin_has_$1)=yes
       m4_toupper($1_LIBS)="$withval"
       m4_toupper($1_PCLIBS)="$withval"
       coin_foreach_w([myvar], [$3], [
         m4_toupper(myvar)_PCLIBS="$withval $m4_toupper(myvar)_PCLIBS"
         m4_toupper(myvar)_LIBS="$withval $m4_toupper(myvar)_LIBS"
       ])
       # if project flags are given by user and we build without pkg-config, then we need to setup the _INSTALLED variables
       if test -z "$PKG_CONFIG" ; then
         m4_toupper($1_LIBS_INSTALLED)="$withval"
         coin_foreach_w([myvar], [$3], [m4_toupper(myvar)_LIBS_INSTALLED="$withval $m4_toupper(myvar)_LIBS_INSTALLED"])
       fi
     fi
    ],
    [])
fi

if test $m4_tolower(coin_has_$1) != skipping; then
  AC_ARG_WITH([m4_tolower($1)-incdir],
    AC_HELP_STRING([--with-m4_tolower($1)-incdir],
                   [directory with header files for using package $1]),
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)=skipping
     else
       m4_tolower(coin_has_$1)=yes
       m4_toupper($1_CFLAGS)="-I`${CYGPATH_W} $withval`"
       coin_foreach_w([myvar], [$3], [m4_toupper(myvar)_CFLAGS="-I`${CYGPATH_W} $withval` $m4_toupper(myvar)_CFLAGS"])
       # if project flags are given by user and we build without pkg-config, then we need to setup the _INSTALLED variables
       if test -z "$PKG_CONFIG" ; then
         m4_toupper($1_CFLAGS_INSTALLED)="$m4_toupper($1_CFLAGS)"
         coin_foreach_w([myvar], [$3], [m4_toupper(myvar)_CFLAGS_INSTALLED="$m4_toupper($1_CFLAGS) $m4_toupper(myvar)_CFLAGS_INSTALLED"])
       fi
     fi
    ],
    [])
fi

if test $m4_tolower(coin_has_$1) != skipping; then
  AC_ARG_WITH([m4_tolower($1)-datadir],
    AC_HELP_STRING([--with-m4_tolower($1)-datadir],
                   [directory with data files for using package $1]),
    [if test "$withval" = no ; then
       m4_tolower(coin_has_$1)=skipping
     else
       m4_tolower(coin_has_$1)=yes
       m4_toupper($1_DATA)="$withval"
       # if project flags are given by user and we build without pkg-config, then we need to setup the _INSTALLED variables
       if test -z "$PKG_CONFIG" ; then
         m4_toupper($1_DATA_INSTALLED)="$withval"
       fi
     fi
    ],
    [])
fi

if test $m4_tolower(coin_has_$1) = notGiven; then
  if test -n "$PKG_CONFIG" ; then
    # set search path for pkg-config
    # need to export variable to be sure that the following pkg-config gets these values
    coin_save_PKG_CONFIG_PATH="$PKG_CONFIG_PATH"
    PKG_CONFIG_PATH="$COIN_PKG_CONFIG_PATH:$COIN_PKG_CONFIG_PATH_UNINSTALLED"
    export PKG_CONFIG_PATH
    
    # let pkg-config do it's magic
    AC_COIN_PKG_HAS_MODULE([$1],[$2],
      [ m4_tolower(coin_has_$1)=yes
        AC_MSG_RESULT([yes: $m4_toupper($1)_VERSIONS])

        # adjust linker flags for (i)cl compiler
        # for the LIBS, we replace everything of the form "/somepath/name.lib" by "`$(CYGPATH_W) /somepath/`name.lib | sed -e s|\|/|g" (where we have to use excessive many \ to get the \ into the command line for cl)
        if test x$coin_cxx_is_cl = xtrue || test x$coin_cc_is_cl = xtrue ;
        then
          m4_toupper($1_LIBS)=`echo " $m4_toupper($1_LIBS) " | [sed -e 's/ \(\/[^ ]*\/\)\([^ ]*\)\.lib / \`$(CYGPATH_W) \1 | sed -e "s|\\\\\\\\\\\\\\\\\\\\|\/|g"\`\2.lib /g']`
        fi
        m4_toupper($1_PCREQUIRES)="$2"
   
        # augment X_PCREQUIRES, X_CFLAGS, and X_LIBS for each build target X in $3
        coin_foreach_w([myvar], [$3], [
          m4_toupper(myvar)_PCREQUIRES="$2 $m4_toupper(myvar)_PCREQUIRES"
          m4_toupper(myvar)_CFLAGS="$m4_toupper($1)_CFLAGS $m4_toupper(myvar)_CFLAGS"
          m4_toupper(myvar)_LIBS="$m4_toupper($1)_LIBS $m4_toupper(myvar)_LIBS"
        ])
      ],
      [ m4_tolower(coin_has_$1)=notGiven
        AC_MSG_RESULT([not given: $m4_toupper($1)_PKG_ERRORS])
      ])

    # reset PKG_CONFIG_PATH variable 
    PKG_CONFIG_PATH="$coin_save_PKG_CONFIG_PATH"
    export PKG_CONFIG_PATH

  else
    AC_MSG_RESULT([skipped check via pkg-config, redirect to fallback])
    AC_COIN_CHECK_PACKAGE_FALLBACK([$1], [$2], [$3])
  fi

else
  AC_MSG_RESULT([$m4_tolower(coin_has_$1)])
fi

if test $m4_tolower(coin_has_$1) != skipping &&
   test $m4_tolower(coin_has_$1) != notGiven ; then
  AC_DEFINE(m4_toupper(COIN_HAS_$1),[1],[Define to 1 if the $1 package is available])

  AC_ARG_ENABLE([interpackage-dependencies],
    AC_HELP_STRING([--disable-interpackage-dependencies], [disables deduction of Makefile dependencies from package linker flags]),
    [], [enable_interpackage_dependencies=yes])
    
  if test $enable_interpackage_dependencies = yes ; then
     # construct dependencies variables from LIBS variables
     # we add an extra space in LIBS so we can substitute out everything starting with " -"
     # remove everything of the form -framework xxx as used on Mac and mkl* and libiomp5* and wsock32.lib as used on Windows
     # then remove everything of the form -xxx
     # also remove everything of the form `xxx`yyy (may have been added for cygwin/cl)
     m4_toupper($1)_DEPENDENCIES=`echo " $m4_toupper($1)_LIBS" | [sed -e 's/ mkl[^ ]*//g' -e 's/ libiomp5[^ ]*//g' -e 's/ wsock32[^ ]*//g' -e 's/ -framework  *[^ ]*//g' -e 's/ -[^ ]*//g' -e 's/\`[^\`]*\`[^ ]* //g']`
     coin_foreach_w([myvar], [$3], [
       m4_toupper(myvar)_DEPENDENCIES=`echo " $m4_toupper(myvar)_LIBS " | [sed -e 's/ mkl[^ ]*//g' -e 's/ libiomp5[^ ]*//g' -e 's/ wsock32[^ ]*//g' -e 's/ -framework  *[^ ]*//g' -e 's/ -[^ ]*//g' -e 's/\`[^\`]*\`[^ ]* //g']`
     ])
  fi

  if test 1 = 0 ; then  #change this test to enable a bit of debugging output
    if test -n "$m4_toupper($1)_CFLAGS" ; then
      AC_MSG_NOTICE([$1 CFLAGS are $m4_toupper($1)_CFLAGS])
    fi
    if test -n "$m4_toupper($1)_LIBS" ; then
      AC_MSG_NOTICE([$1 LIBS   are $m4_toupper($1)_LIBS])
    fi
    if test -n "$m4_toupper($1)_DEPENDENCIES" ; then
      AC_MSG_NOTICE([$1 DEPENDENCIES are $m4_toupper($1)_DEPENDENCIES])
    fi
    if test -n "$m4_toupper($1)_DATA" ; then
      AC_MSG_NOTICE([$1 DATA   is  $m4_toupper($1)_DATA])
    fi
    if test -n "$m4_toupper($1)_PCLIBS" ; then
      AC_MSG_NOTICE([$1 PCLIBS are $m4_toupper($1)_PCLIBS])
    fi
    if test -n "$m4_toupper($1)_PCREQUIRES" ; then
      AC_MSG_NOTICE([$1 PCREQUIRES are $m4_toupper($1)_PCREQUIRES])
    fi
    coin_foreach_w([myvar], [$3], [
      AC_MSG_NOTICE([myvar CFLAGS are $m4_toupper(myvar)_CFLAGS])
      AC_MSG_NOTICE([myvar LIBS   are $m4_toupper(myvar)_LIBS])
      AC_MSG_NOTICE([myvar DEPENDENCIES are $m4_toupper(myvar)_DEPENDENCIES])
    ])
  fi
fi

# Define the Makefile conditional
AM_CONDITIONAL(m4_toupper(COIN_HAS_$1),
               [test $m4_tolower(coin_has_$1) != notGiven &&
                test $m4_tolower(coin_has_$1) != skipping])

]) # AC_COIN_CHECK_PACKAGE

###########################################################################
#                       COIN_CHECK_PACKAGE_FALLBACK                       #
###########################################################################

# This macro is used by COIN_CHECK_PACKAGE, if it fails to find a package
# because pkg-config was disabled or is not available.
#
# For each project xxx specified in $2, it searches for a xxx-uninstalled.pc
# file in the directories specified in $COIN_PKG_CONFIG_PATH_UNINSTALLED. The
# latter variable is setup by COIN_HAS_PKGCONFIG and consists of the content
# of the coin_subdirs.txt file which has been created by configure in the
# base directory.  The content of xxx-uninstalled.pc is parsed in order
# to defines the variables PACKAGE_CFLAGS, PACKAGE_LIBS, and PACKAGE_DATA,
# referring to the compiler and linker flags to use when linking against this
# package and the directory where the package data resists.  Further, for each
# build target X specified in the third argument, the variables X_CFLAGS and
# X_LIBS are extended with the compiler and linker flags of this package and
# the variables X_PCLIBS and X_PCREQUIRES are extended by the list of linker
# flags and dependent projects as needed to setup a .pc file.  The macros
# checks also dependencies of $2.  Note that the PACKAGE_DATA variable is
# set to the content of datadir of the first .pc file that is parsed.
# Finally, for each X in the third argument, also variables
# X_CFLAGS_INSTALLED and X_LIBS_INSTALLED are setup. They contain the compiler
# and linker flags for X when all projects have been installed. Their content
# is assembled from the .pc files that correspond to installed projects. I.e.,
# whenever a file proj-uninstalled.pc is parsed, then also a corresponding
# proj.pc file is parsed for compiler and linker flags, if available in the
# same directory.
# Similar, a variable PACKAGE_DATA_INSTALLED is setup to the content of datadir
# of the first .pc file that is parsed.
#
# If .pc files for all projects in $2 and their dependencies is found,
# tolower(coin_has_$1) is set to "yes".  Otherwise, if some dependency
# is not found, tolower(coin_has_$1) is set to "notGiven".  Further, a
# COIN_HAS_PACKAGE preprocessor macro and a makefile conditional are defined.
#
# The first argument should be the name (PACKAGE) of the package (in correct
# lower and upper case).  The second argument should be the base names of the
# projects .pc file which define this package.  The optional third argument
# should be a (space separated) list of build targets which use this package,
# if available.
#
# $1 is not checked for $COIN_SKIP_PROJECTS, since we only look into
# $COIN_PKG_CONFIG_PATH_UNINSTALLED.  When the content of this variable was
# setup in the base directory, $COIN_SKIP_PROJECTS has already been considered.

AC_DEFUN([AC_COIN_CHECK_PACKAGE_FALLBACK],
[AC_REQUIRE([AC_COIN_HAS_PKGCONFIG])
AC_MSG_CHECKING([for COIN-OR package $1 (fallback)])

m4_tolower(coin_has_$1)=notGiven
m4_toupper($1_LIBS)=
m4_toupper($1_LIBS_INSTALLED)=
m4_toupper($1_CFLAGS)=
m4_toupper($1_CFLAGS_INSTALLED)=
m4_toupper($1_DATA)=
m4_toupper($1_DATA_INSTALLED)=
m4_toupper($1_PCLIBS)=
m4_toupper($1_PCREQUIRES)=

# initial list of dependencies is "$2", but we need to filter out version number specifications (= x, <= x, >= x, != x)
projtoprocess="m4_bpatsubsts([$2], [<?>?!?=[ 	]*[^ 	]+])"

# we first expand the list of projects to process by adding all dependencies just behind the project which depends on it
# further, we collect the list of corresponding .pc files, but do this in reverse order, because we need this order afterwards
# the latter we also do with .pc files corresponding to the installed projects, which will be needed to setup Makefiles for examples
# also, we setup the DATA variable
allproj=""
allpcfiles=""
allpcifiles=""
while test "x$projtoprocess" != x ; do

  for proj in $projtoprocess ; do
    # if $proj is available and configured, then a project-uninstalled.pc file should have been created, so search for it
    pcfile=""
    save_IFS="$IFS"
    IFS=":"
    for dir in $COIN_PKG_CONFIG_PATH_UNINSTALLED ; do
      # the base directory configure should have setup coin_subdirs.txt in a way that it does not contain projects that should be skipped, so we do not need to test this here again
      if test -r "$dir/${proj}-uninstalled.pc" ; then
        pcfile="$dir/$proj-uninstalled.pc"
        if test -r "$dir/${proj}.pc" ; then
          pcifile="$dir/${proj}.pc"
        else
          AC_MSG_WARN([Found $pcfile, but $dir/${proj}.pc is not available. This may break Makefile's of examples.])
          pcifile=
        fi
        break
      fi
    done
    IFS="$save_IFS"

    if test "x$pcfile" != x ; then
      # read dependencies from $pcfile and filter it
      projrequires=[`sed -n -e 's/Requires://gp' "$pcfile" | sed -e 's/<\{0,1\}>\{0,1\}=[ 	]\{0,\}[^ 	]\{1,\}//g'`]

      # add projrequires to the front of the list of projects that have to be processed next
      # at the same time, remove $proj from this list
      projtoprocess=[`echo $projtoprocess | sed -e "s/$proj/$projrequires/"`]

      # read DATA from $pcfile, if _DATA is still empty
      if test "x$m4_toupper($1_DATA)" = x ; then
        projdatadir=
        [pcfilemod=`sed -e '/[a-zA-Z]:/d' -e 's/datadir=\(.*\)/echo projdatadir=\\\\"\1\\\\"/g' $pcfile`]
        eval `sh -c "$pcfilemod"`
        m4_toupper($1_DATA)="$projdatadir"
      fi

      allproj="$allproj $proj"
      allpcfiles="$pcfile:$allpcfiles"

    else
      AC_MSG_RESULT([no, dependency $proj not available])
      allproj=fail
      break 2
    fi
    
    if test "x$pcifile" != x ; then
      allpcifiles="$pcifile:$allpcifiles"
      
      # read DATA_INSTALLED from $pcifile, if _DATA_INSTALLED is still empty
      if test "x$m4_toupper($1_DATA_INSTALLED)" = x ; then
        projdatadir=
        [pcifilemod=`sed -e '/[a-zA-Z]:/d' -e 's/datadir=\(.*\)/echo projdatadir=\\\\"\1\\\\"/g' $pcifile`]
        eval `sh -c "$pcifilemod"`
        if test "${CYGPATH_W}" != "echo"; then
          projdatadir="\`\$(CYGPATH_W) ${projdatadir} | sed -e 's/\\\\\\\\/\\\\\\\\\\\\\\\\/g'\`"
        fi
        m4_toupper($1_DATA_INSTALLED)="$projdatadir"
      fi
      
    fi

    break
  done

  # remove spaces on begin of $projtoprocess
  projtoprocess=`echo $projtoprocess | sed -e 's/^[ ]*//'`

done

if test "$allproj" != fail ; then

  # now go through the list of .pc files and assemble compiler and linker flags
  # important is here to obey the reverse order that has been setup before,
  # since then libraries that are required by several others should be after these other libraries
  pcfilesprocessed=""

  save_IFS="$IFS"
  IFS=":"
  for pcfile in $allpcfiles ; do

    # if $pcfile has been processed already, skip this round
    if test "x$pcfilesprocessed" != x ; then
      for pcfiledone in $pcfilesprocessed ; do
        if test "$pcfiledone" = "$pcfile" ; then
          continue 2
        fi
      done
    fi

    # modify .pc file to a shell script that prints shell commands for setting the compiler and library flags:
    #   replace "Libs:" by "echo projlibs="
    #   replace "Cflags:" by "echo projcflags="
    #   remove every line starting with <some word>:
    [pcfilemod=`sed -e 's/Libs:\(.*\)$/echo projlibs=\\\\"\1\\\\"/g' -e 's/Cflags:\(.*\)/echo projcflags=\\\\"\1\\\\"/g' -e '/^[a-zA-Z]*:/d' $pcfile`]

    # set projcflags and projlibs variables by running $pcfilemod
    # under mingw, the current IFS seem to make the : in the paths of the gfortran libs go away, so we temporarily set IFS back to its default
    projcflags=
    projlibs=
    IFS="$save_IFS"
    eval `sh -c "$pcfilemod"`
    IFS=":"

    # add CYGPATH_W cludge into include flags and set CFLAGS variable
    if test "${CYGPATH_W}" != "echo"; then
      projcflags=[`echo "$projcflags" | sed -e 's/-I\([^ ]*\)/-I\`${CYGPATH_W} \1\`/g'`]
    fi
    m4_toupper($1_CFLAGS)="$projcflags $m4_toupper($1_CFLAGS)"

    # set LIBS variable
    m4_toupper($1_LIBS)="$projlibs $m4_toupper($1_LIBS)"

    # remember that we have processed $pcfile
    pcfilesprocessed="$pcfilesprocessed:$pcfile"

  done
  IFS="$save_IFS"


  # now go through the list of .pc files for installed projects and assemble compiler and linker flags
  # important is here again to obey the reverse order that has been setup before,
  # since then libraries that are required by several others should be after these other libraries
  pcfilesprocessed=""

  save_IFS="$IFS"
  IFS=":"
  for pcfile in $allpcifiles ; do

    # if $pcfile has been processed already, skip this round
    if test "x$pcfilesprocessed" != x ; then
      for pcfiledone in $pcfilesprocessed ; do
        if test "$pcfiledone" = "$pcfile" ; then
          continue 2
        fi
      done
    fi

    # modify .pc file to a shell script that prints shell commands for setting the compiler and library flags:
    #   replace "Libs:" by "echo projlibs="
    #   replace "Cflags:" by "echo projcflags="
    #   remove every line starting with <some word>:
    [pcfilemod=`sed -e 's/Libs:\(.*\)$/echo projlibs=\\\\"\1\\\\"/g' -e 's/Cflags:\(.*\)/echo projcflags=\\\\"\1\\\\"/g' -e '/^[a-zA-Z]*:/d' $pcfile`]

    # set projcflags and projlibs variables by running $pcfilemod
    # under mingw, the current IFS seem to make the : in the paths of the gfortran libs go away, so we temporarily set IFS back to its default
    projcflags=
    projlibs=
    IFS="$save_IFS"
    eval `sh -c "$pcfilemod"`
    IFS=":"

    # add CYGPATH_W cludge into include flags and set CFLAGS variable
    if test "${CYGPATH_W}" != "echo"; then
      projcflags=[`echo "$projcflags" | sed -e 's/-I\([^ ]*\)/-I\`${CYGPATH_W} \1\`/g'`]
    fi
    m4_toupper($1_CFLAGS_INSTALLED)="$projcflags $m4_toupper($1_CFLAGS_INSTALLED)"

    # set LIBS variable
    m4_toupper($1_LIBS_INSTALLED)="$projlibs $m4_toupper($1_LIBS_INSTALLED)"
    
    # remember that we have processed $pcfile
    pcfilesprocessed="$pcfilesprocessed:$pcfile"

  done
  IFS="$save_IFS"


  # finish up
  m4_tolower(coin_has_$1)=yes
  AC_MSG_RESULT([yes])
  AC_DEFINE(m4_toupper(COIN_HAS_$1),[1],[Define to 1 if the $1 package is available])

  # adjust linker flags for (i)cl compiler
  # for the LIBS, we replace everything of the form "/somepath/name.lib" by "`$(CYGPATH_W) /somepath/`name.lib | sed -e s|\|/|g" (where we have to use excessive many \ to get the \ into the command line for cl),
  # for the LIBS_INSTALLED, we replace everything of the form "/somepath/" by "`$(CYGPATH_W) /somepath/`",
  #    everything of the form "-lname" by "libname.lib", and
  #    everything of the form "-Lpath" by "-libpath:`$(CYGPATH_W) path`
  if test x$coin_cxx_is_cl = xtrue || test x$coin_cc_is_cl = xtrue ;
  then
    m4_toupper($1_LIBS)=`echo " $m4_toupper($1_LIBS) " | [sed -e 's/ \(\/[^ ]*\/\)\([^ ]*\)\.lib / \`$(CYGPATH_W) \1 | sed -e "s|\\\\\\\\\\\\\\\\\\\\|\/|g"\`\2.lib /g']`
    m4_toupper($1_LIBS_INSTALLED)=`echo " $m4_toupper($1_LIBS_INSTALLED)" | [sed -e 's/ \(\/[^ ]*\/\)/ \`$(CYGPATH_W) \1\`/g' -e 's/ -l\([^ ]*\)/ lib\1.lib/g' -e 's/ -L\([^ ]*\)/ -libpath:\`$(CYGPATH_W) \1\`/g']`
  fi

  m4_toupper($1_PCREQUIRES)="$2"
  coin_foreach_w([myvar], [$3], [
    m4_toupper(myvar)_PCREQUIRES="$2 $m4_toupper(myvar)_PCREQUIRES"
    m4_toupper(myvar)_CFLAGS="$m4_toupper($1)_CFLAGS $m4_toupper(myvar)_CFLAGS"
    m4_toupper(myvar)_LIBS="$m4_toupper($1)_LIBS $m4_toupper(myvar)_LIBS"
    m4_toupper(myvar)_CFLAGS_INSTALLED="$m4_toupper($1)_CFLAGS_INSTALLED $m4_toupper(myvar)_CFLAGS_INSTALLED"
    m4_toupper(myvar)_LIBS_INSTALLED="$m4_toupper($1)_LIBS_INSTALLED $m4_toupper(myvar)_LIBS_INSTALLED"
  ])

fi

AM_CONDITIONAL(m4_toupper(COIN_HAS_$1),
               [test $m4_tolower(coin_has_$1) != notGiven &&
                test $m4_tolower(coin_has_$1) != skipping])

]) # AC_COIN_CHECK_PACKAGE_FALLBACK

###########################################################################
#                         COIN_CHECK_PACKAGE_BLAS                         #
###########################################################################

# This macro checks for a library containing the BLAS library.  It
# 1. checks the --with-blas argument
# 2. if --with-blas=BUILD has been specified goes to point 5
# 3. if --with-blas has been specified to a working library, sets BLAS_LIBS
#    to its value
# 4. tries standard libraries
# 5. calls COIN_CHECK_PACKAGE(Blas, [coinblas], [$1]) to check for
#    ThirdParty/Blas
# The makefile conditional and preprocessor macro COIN_HAS_BLAS is defined.
# BLAS_LIBS is set to the flags required to link with a Blas library.
# For each build target X in $1, X_LIBS is extended with $BLAS_LIBS.
# In case 3 and 4, the flags to link to Blas are added to X_PCLIBS too.
# In case 5, Blas is added to X_PCREQUIRES.

AC_DEFUN([AC_COIN_CHECK_PACKAGE_BLAS],
[
AC_ARG_WITH([blas],
            AC_HELP_STRING([--with-blas],
                           [specify BLAS library (or BUILD to enforce use of ThirdParty/Blas)]),
            [use_blas="$withval"], [use_blas=])

# if user specified --with-blas-lib, then we should give COIN_CHECK_PACKAGE
# preference
AC_ARG_WITH([blas-lib],,[use_blas=BUILD])

# Check if user supplied option makes sense
if test x"$use_blas" != x; then
  if test "$use_blas" = "BUILD"; then
    # we come to this later
    :
  elif test "$use_blas" != "no"; then
    coin_save_LIBS="$LIBS"
    LIBS="$use_blas $LIBS"
    if test "$F77" != unavailable ; then
      coin_need_flibs=no
      AC_MSG_CHECKING([whether user supplied BLASLIB=\"$use_blas\" works])
      AC_COIN_TRY_FLINK([daxpy],
                        [if test $coin_need_flibs = yes ; then
                           use_blas="$use_blas $FLIBS"
                         fi
                         AC_MSG_RESULT([yes: $use_blas])],
                        [AC_MSG_RESULT([no])
                         AC_MSG_ERROR([user supplied BLAS library \"$use_blas\" does not work])])
      use_blas="$use_blas $FLIBS"
    else
      AC_MSG_NOTICE([checking whether user supplied BLASLIB=\"$use_blas\" works with C linkage])
      AC_LANG_PUSH(C)
      AC_CHECK_FUNC([daxpy],[],[AC_MSG_ERROR([user supplied BLAS library \"$use_blas\" does not work])])
      AC_LANG_POP(C)
    fi
    LIBS="$coin_save_LIBS"
  fi
else
# Try to autodetect the library for blas based on build system
  #AC_MSG_CHECKING([default locations for BLAS])
  case $build in
    *-sgi-*) 
      AC_MSG_CHECKING([whether -lcomplib.sgimath has BLAS])
      coin_need_flibs=no
      coin_save_LIBS="$LIBS"
      LIBS="-lcomplib.sgimath $LIBS"
      AC_COIN_TRY_FLINK([daxpy],
                        [use_blas="-lcomplib.sgimath"
                         if test $coin_need_flibs = yes ; then
                           use_blas="$use_blas $FLIBS"
                         fi
                         AC_MSG_RESULT([yes: $use_blas])
                        ],
                        [AC_MSG_RESULT([no])])
      LIBS="$coin_save_LIBS"
      ;;

# Ideally, we'd use -library=sunperf, but it's an imperfect world. Studio
# cc doesn't recognise -library, it wants -xlic_lib. Studio 12 CC doesn't
# recognise -xlic_lib. Libtool doesn't like -xlic_lib anyway. Sun claims
# that CC and cc will understand -library in Studio 13. The main extra
# function of -xlic_lib and -library is to arrange for the Fortran run-time
# libraries to be linked for C++ and C. We can arrange that explicitly.
    *-*-solaris*)
      AC_MSG_CHECKING([for BLAS in libsunperf])
      coin_need_flibs=no
      coin_save_LIBS="$LIBS"
      LIBS="-lsunperf $FLIBS $LIBS"
      AC_COIN_TRY_FLINK([daxpy],
                        [use_blas='-lsunperf'
                         if test $coin_need_flibs = yes ; then
                           use_blas="$use_blas $FLIBS"
                         fi
                         AC_MSG_RESULT([yes: $use_blas])
                        ],
                        [AC_MSG_RESULT([no])])
      LIBS="$coin_save_LIBS"
      ;;
      
    *-cygwin* | *-mingw*)
      case "$CC" in
        clang* ) ;;
        cl* | */cl* | CL* | */CL* | icl* | */icl* | ICL* | */ICL*)
          coin_save_LIBS="$LIBS"
          LIBS="mkl_intel_c.lib mkl_sequential.lib mkl_core.lib $LIBS"
          if test "$F77" != unavailable ; then
            AC_MSG_CHECKING([for BLAS in MKL (32bit)])
            AC_COIN_TRY_FLINK([daxpy],
                              [use_blas='mkl_intel_c.lib mkl_sequential.lib mkl_core.lib'
                               AC_MSG_RESULT([yes: $use_blas])
                              ],
                              [AC_MSG_RESULT([no])])
          else
            AC_MSG_NOTICE([for BLAS in MKL (32bit) using C linkage])
            AC_LANG_PUSH(C)
            AC_CHECK_FUNC([daxpy],[use_blas='mkl_intel_c.lib mkl_sequential.lib mkl_core.lib'])
            AC_LANG_POP(C)
          fi
          LIBS="$coin_save_LIBS"
          
          if test "x$use_blas" = x ; then
            LIBS="mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib $LIBS"
            if test "$F77" != unavailable ; then
              AC_MSG_CHECKING([for BLAS in MKL (64bit)])
              AC_COIN_TRY_FLINK([daxpy],
                                [use_blas='mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib'
                                 AC_MSG_RESULT([yes: $use_blas])
                                ],
                                [AC_MSG_RESULT([no])])
            else
              AC_MSG_NOTICE([for BLAS in MKL (64bit) using C linkage])
              # unset cached outcome of test with 32bit MKL
              unset ac_cv_func_daxpy
              AC_LANG_PUSH(C)
              AC_CHECK_FUNC([daxpy],[use_blas='mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib'])
              AC_LANG_POP(C)
            fi
            LIBS="$coin_save_LIBS"
          fi
          ;;
      esac
      ;;
      
     *-darwin*)
      AC_MSG_CHECKING([for BLAS in Veclib])
      coin_need_flibs=no
      coin_save_LIBS="$LIBS"
      LIBS="-framework Accelerate $LIBS"
      AC_COIN_TRY_FLINK([daxpy],
                        [use_blas='-framework Accelerate'
                         if test $coin_need_flibs = yes ; then
                           use_blas="$use_blas $FLIBS"
                         fi
                         AC_MSG_RESULT([yes: $use_blas])
                        ],
                        [AC_MSG_RESULT([no])])
      LIBS="$coin_save_LIBS"
      ;;
  esac

  if test -z "$use_blas" ; then
    AC_MSG_CHECKING([whether -lblas has BLAS])
    coin_need_flibs=no
    coin_save_LIBS="$LIBS"
    LIBS="-lblas $LIBS"
    AC_COIN_TRY_FLINK([daxpy],
                      [use_blas='-lblas'
                       if test $coin_need_flibs = yes ; then
                         use_blas="$use_blas $FLIBS"
                       fi
                       AC_MSG_RESULT([yes: $use_blas])
                      ],
                      [AC_MSG_RESULT([no])])
    LIBS="$coin_save_LIBS"
  fi

  # If we have no other ideas, consider building BLAS.
  if test -z "$use_blas" ; then
    use_blas=BUILD
  fi
fi

if test "x$use_blas" = xBUILD ; then
  AC_COIN_CHECK_PACKAGE(Blas, [coinblas], [$1])
  
elif test "x$use_blas" != x && test "$use_blas" != no; then
  coin_has_blas=yes
  AM_CONDITIONAL([COIN_HAS_BLAS],[test 0 = 0])
  AC_DEFINE([COIN_HAS_BLAS],[1], [If defined, the BLAS Library is available.])
  BLAS_LIBS="$use_blas"
  BLAS_CFLAGS=
  BLAS_DATA=
  AC_SUBST(BLAS_LIBS)
  AC_SUBST(BLAS_CFLAGS)
  AC_SUBST(BLAS_DATA)
  coin_foreach_w([myvar], [$1], [
    m4_toupper(myvar)_PCLIBS="$BLAS_LIBS $m4_toupper(myvar)_PCLIBS"
    m4_toupper(myvar)_LIBS="$BLAS_LIBS $m4_toupper(myvar)_LIBS"
    m4_toupper(myvar)_LIBS_INSTALLED="$BLAS_LIBS $m4_toupper(myvar)_LIBS_INSTALLED"
  ])
  
else
  coin_has_blas=no
  AM_CONDITIONAL([COIN_HAS_BLAS],[test 0 = 1])
fi

coin_foreach_w([myvar], [$1], [
  AC_SUBST(m4_toupper(myvar)_PCLIBS)
  AC_SUBST(m4_toupper(myvar)_LIBS)
  AC_SUBST(m4_toupper(myvar)_LIBS_INSTALLED)
])

]) # AC_COIN_CHECK_PACKAGE_BLAS

###########################################################################
#                       COIN_CHECK_PACKAGE_LAPACK                         #
###########################################################################

# This macro checks for a library containing the LAPACK library.  It
# 1. checks the --with-lapack argument
# 2. if --with-lapack=BUILD has been specified goes to point 5
# 3. if --with-lapack has been specified to a working library, sets
#    LAPACK_LIBS to its value
# 4. tries standard libraries
# 5. calls COIN_CHECK_PACKAGE(Lapack, [coinlapack], [$1]) to check for
#    ThirdParty/Lapack
# The makefile conditional and preprocessor macro COIN_HAS_LAPACK is defined.
# LAPACK_LIBS is set to the flags required to link with a Lapack library.
# For each build target X in $1, X_LIBS is extended with $LAPACK_LIBS.
# In case 3 and 4, the flags to link to Lapack are added to X_PCLIBS too.
# In case 5, Lapack is added to X_PCREQUIRES.
#
# TODO: Lapack usually depends on Blas, so if we check for a system lapack library,
#   shouldn't we include AC_COIN_CHECK_PACKAGE_BLAS first?
#   However, if we look for coinlapack via AC_COIN_CHECK_PACKAGE(Lapack, [coinlapack], [$1]),
#   then we will get Blas as dependency of coinlapack.

AC_DEFUN([AC_COIN_CHECK_PACKAGE_LAPACK],
[
AC_ARG_WITH([lapack],
            AC_HELP_STRING([--with-lapack],
                           [specify LAPACK library (or BUILD to enforce use of ThirdParty/Lapack)]),
            [use_lapack=$withval], [use_lapack=])

#if user specified --with-lapack-lib, then we should give COIN_HAS_PACKAGE preference
AC_ARG_WITH([lapack-lib],,[use_lapack=BUILD])

# Check if user supplied option makes sense
if test x"$use_lapack" != x; then
  if test "$use_lapack" = "BUILD"; then
    # we come to this later
    :
  elif test "$use_lapack" != no; then
    use_lapack="$use_lapack $BLAS_LIBS"
    coin_save_LIBS="$LIBS"
    LIBS="$use_lapack $LIBS"
    if test "$F77" != unavailable; then
      AC_MSG_CHECKING([whether user supplied LAPACKLIB=\"$use_lapack\" works])
      coin_need_flibs=no
      AC_COIN_TRY_FLINK([dsyev],
                        [if test $coin_need_flibs = yes ; then
                           use_lapack="$use_lapack $FLIBS"
                         fi
                         AC_MSG_RESULT([yes: $use_lapack])
                        ],
                        [AC_MSG_RESULT([no])
                         AC_MSG_ERROR([user supplied LAPACK library \"$use_lapack\" does not work])])
    else
      AC_MSG_NOTICE([whether user supplied LAPACKLIB=\"$use_lapack\" works with C linkage])
      AC_LANG_PUSH(C)
      AC_CHECK_FUNC([dsyev],[],[AC_MSG_ERROR([user supplied LAPACK library \"$use_lapack\" does not work])])
      AC_LANG_POP(C)
    fi
    LIBS="$coin_save_LIBS"
  fi
else
  if test x$coin_has_blas = xyes; then
    # First try to see if LAPACK is already available with BLAS library
    coin_save_LIBS="$LIBS"
    LIBS="$BLAS_LIBS $LIBS"
    if test "$F77" != unavailable; then
      coin_need_flibs=no
      AC_MSG_CHECKING([whether LAPACK is already available with BLAS library])
      AC_COIN_TRY_FLINK([dsyev],
                        [use_lapack="$BLAS_LIBS"
                         if test $coin_need_flibs = yes ; then
                           use_lapack="$use_lapack $FLIBS"
                         fi
                         AC_MSG_RESULT([yes: $use_lapack])
                        ],
                        [AC_MSG_RESULT([no])])
    else
      AC_MSG_NOTICE([checking whether LAPACK is already available with BLAS library])
      AC_LANG_PUSH(C)
      AC_CHECK_FUNC([dsyev],[use_lapack="$BLAS_LIBS"])
      AC_LANG_POP(C)
    fi
    LIBS="$coin_save_LIBS"
  fi
  if test -z "$use_lapack"; then
    # Try to autodetect the library for lapack based on build system
    case $build in
      # TODO: Is this check actually needed here, since -lcomplib.sigmath should have been recognized as Blas library,
      #       and above it is checked whether the Blas library already contains Lapack
      *-sgi-*) 
        AC_MSG_CHECKING([whether -lcomplib.sgimath has LAPACK])
        coin_save_LIBS="$LIBS"
        coin_need_flibs=no
        LIBS="-lcomplib.sgimath $BLAS_LIBS $LIBS"
        AC_COIN_TRY_FLINK([dsyev],
                          [use_lapack="-lcomplib.sgimath $BLAS_LIBS"
                           if test $coin_need_flibs = yes ; then
                             use_lapack="$use_lapack $FLIBS"
                           fi
                           AC_MSG_RESULT([yes: $use_lapack])
                          ],
                          [AC_MSG_RESULT([no])])
        LIBS="$coin_save_LIBS"
        ;;

      # See comments in COIN_CHECK_PACKAGE_BLAS.
      # TODO: Is this check actually needed here, since -lsunperf should have been recognized as Blas library,
      #       and above it is checked whether the Blas library already contains Lapack
      *-*-solaris*)
        AC_MSG_CHECKING([for LAPACK in libsunperf])
        coin_need_flibs=no
        coin_save_LIBS="$LIBS"
        LIBS="-lsunperf $BLAS_LIBS $FLIBS $LIBS"
        AC_COIN_TRY_FLINK([dsyev],
                          [use_lapack='-lsunperf $BLAS_LIBS'
                           if test $coin_need_flibs = yes ; then
                             use_lapack="$use_lapack $FLIBS"
                           fi
                           AC_MSG_RESULT([yes: $use_lapack])
                          ],
                          [AC_MSG_RESULT([no])])
        LIBS="$coin_save_LIBS"
        ;;
        # On cygwin, do this check only if doscompile is disabled. The prebuilt library
        # will want to link with cygwin, hence won't run standalone in DOS.
    esac
  fi

  if test -z "$use_lapack" ; then
    AC_MSG_CHECKING([whether -llapack has LAPACK])
    coin_need_flibs=no
    coin_save_LIBS="$LIBS"
    LIBS="-llapack $BLAS_LIBS $LIBS"
    AC_COIN_TRY_FLINK([dsyev],
                      [use_lapack='-llapack'
                       if test $coin_need_flibs = yes ; then
                         use_lapack="$use_lapack $FLIBS"
                       fi
                       AC_MSG_RESULT([yes: $use_lapack])
                      ],
                      [AC_MSG_RESULT([no])])
    LIBS="$coin_save_LIBS"
  fi

  # If we have no other ideas, consider building LAPACK.
  if test -z "$use_lapack" ; then
    use_lapack=BUILD
  fi
fi

if test "x$use_lapack" = xBUILD ; then
  AC_COIN_CHECK_PACKAGE(Lapack, [coinlapack], [$1])

elif test "x$use_lapack" != x && test "$use_lapack" != no; then
  coin_has_lapack=yes
  AM_CONDITIONAL([COIN_HAS_LAPACK],[test 0 = 0])
  AC_DEFINE([COIN_HAS_LAPACK],[1], [If defined, the LAPACK Library is available.])
  LAPACK_LIBS="$use_lapack"
  LAPACK_CFLAGS=
  LAPACK_DATA=
  AC_SUBST(LAPACK_LIBS)
  AC_SUBST(LAPACK_CFLAGS)
  AC_SUBST(LAPACK_DATA)
  coin_foreach_w([myvar], [$1], [
    m4_toupper(myvar)_PCLIBS="$LAPACK_LIBS $m4_toupper(myvar)_PCLIBS"
    m4_toupper(myvar)_LIBS="$LAPACK_LIBS $m4_toupper(myvar)_LIBS"
    m4_toupper(myvar)_LIBS_INSTALLED="$LAPACK_LIBS $m4_toupper(myvar)_LIBS_INSTALLED"
  ])
  
else
  coin_has_lapack=no
  AM_CONDITIONAL([COIN_HAS_LAPACK],[test 0 = 1])
fi

coin_foreach_w([myvar], [$1], [
  AC_SUBST(m4_toupper(myvar)_PCLIBS)
  AC_SUBST(m4_toupper(myvar)_LIBS)
  AC_SUBST(m4_toupper(myvar)_LIBS_INSTALLED)
])

]) # AC_COIN_CHECK_PACKAGE_LAPACK
