# Copyright (C) 2006 International Business Machines and others.
# All Rights Reserved.
# This file is distributed under the Eclipse Public License.

## $Id$

# Author:  Andreas Waechter           IBM    2006-04-13

AUTOMAKE_OPTIONS = foreign

########################################################################
#                             libCoinUtils                             #
########################################################################

# Name of the library compiled in this directory.
# We want it to be installed in the $libdir directory
lib_LTLIBRARIES = libCoinUtils.la

# List all source files for this library, including headers
libCoinUtils_la_SOURCES = \
	config_coinutils.h \
	CoinUtilsConfig.h \
	Coin_C_defines.h \
	CoinAlloc.cpp CoinAlloc.hpp \
	CoinBuild.cpp CoinBuild.hpp \
	CoinDenseVector.cpp CoinDenseVector.hpp \
	CoinDistance.hpp \
	CoinError.cpp CoinError.hpp \
	CoinFactorization.hpp \
	CoinFactorization1.cpp \
	CoinFactorization2.cpp \
	CoinFactorization3.cpp \
	CoinFactorization4.cpp \
	CoinSimpFactorization.hpp \
	CoinSimpFactorization.cpp \
	CoinDenseFactorization.hpp \
	CoinDenseFactorization.cpp \
	CoinOslFactorization.hpp \
	CoinOslFactorization.cpp \
	CoinOslFactorization2.cpp \
	CoinOslFactorization3.cpp \
	CoinOslC.h \
	CoinFileIO.cpp CoinFileIO.hpp \
	CoinFinite.cpp CoinFinite.hpp \
	CoinFloatEqual.hpp \
	CoinHelperFunctions.hpp \
	CoinIndexedVector.cpp CoinIndexedVector.hpp \
	CoinLpIO.cpp CoinLpIO.hpp \
	CoinMessage.cpp CoinMessage.hpp \
	CoinMessageHandler.cpp CoinMessageHandler.hpp \
	CoinModel.cpp CoinModel.hpp \
	CoinStructuredModel.cpp CoinStructuredModel.hpp \
	CoinModelUseful.cpp CoinModelUseful.hpp \
	CoinModelUseful2.cpp \
	CoinMpsIO.cpp CoinMpsIO.hpp \
	CoinPackedMatrix.cpp CoinPackedMatrix.hpp \
	CoinPackedVector.cpp CoinPackedVector.hpp \
	CoinPackedVectorBase.cpp CoinPackedVectorBase.hpp \
	CoinParam.cpp CoinParamUtils.cpp CoinParam.hpp \
	CoinPostsolveMatrix.cpp \
	CoinPragma.hpp \
	CoinPrePostsolveMatrix.cpp \
	CoinPresolveDoubleton.cpp CoinPresolveDoubleton.hpp \
	CoinPresolveDual.cpp CoinPresolveDual.hpp \
	CoinPresolveDupcol.cpp CoinPresolveDupcol.hpp \
	CoinPresolveEmpty.cpp CoinPresolveEmpty.hpp \
	CoinPresolveFixed.cpp CoinPresolveFixed.hpp \
	CoinPresolveForcing.cpp CoinPresolveForcing.hpp \
	CoinPresolveHelperFunctions.cpp \
	CoinPresolveImpliedFree.cpp CoinPresolveImpliedFree.hpp \
	CoinPresolveIsolated.cpp CoinPresolveIsolated.hpp \
	CoinPresolveMatrix.cpp CoinPresolveMatrix.hpp \
	CoinPresolvePsdebug.cpp CoinPresolvePsdebug.hpp \
	CoinPresolveMonitor.cpp CoinPresolveMonitor.hpp \
	CoinPresolveSingleton.cpp CoinPresolveSingleton.hpp \
	CoinPresolveSubst.cpp CoinPresolveSubst.hpp \
	CoinPresolveTighten.cpp CoinPresolveTighten.hpp \
	CoinPresolveTripleton.cpp CoinPresolveTripleton.hpp \
	CoinPresolveUseless.cpp CoinPresolveUseless.hpp \
	CoinPresolveZeros.cpp CoinPresolveZeros.hpp \
	CoinRational.cpp CoinRational.hpp \
	CoinSearchTree.cpp CoinSearchTree.hpp \
	CoinShallowPackedVector.cpp CoinShallowPackedVector.hpp \
	CoinSignal.hpp \
	CoinSmartPtr.hpp \
	CoinSnapshot.cpp CoinSnapshot.hpp \
	CoinSort.hpp \
	CoinTime.hpp \
	CoinTypes.hpp \
	CoinUtility.hpp \
	CoinWarmStart.hpp \
	CoinWarmStartBasis.cpp CoinWarmStartBasis.hpp \
	CoinWarmStartVector.cpp CoinWarmStartVector.hpp \
	CoinWarmStartDual.cpp CoinWarmStartDual.hpp \
	CoinWarmStartPrimalDual.cpp CoinWarmStartPrimalDual.hpp

# List all additionally required libraries
if DEPENDENCY_LINKING
libCoinUtils_la_LIBADD = $(COINUTILSLIB_LIBS)
endif

# This is for libtool
libCoinUtils_la_LDFLAGS = $(LT_LDFLAGS)

# Here list all include flags, relative to this "srcdir" directory.  This
# "cygpath" stuff is necessary to compile with native compilers on Windows
AM_CPPFLAGS = $(GLPK_CFLAGS)

# This line is necessary to allow VPATH compilation
DEFAULT_INCLUDES = -I. -I`$(CYGPATH_W) $(srcdir)`

########################################################################
#                Headers that need to be installed                     #
########################################################################

# Here list all the header files that are required by a user of the library,
# and that therefore should be installed in 'install/coin'
includecoindir = $(includedir)/coin
includecoin_HEADERS = \
	Coin_C_defines.h \
	CoinAlloc.hpp \
	CoinBuild.hpp \
	CoinDenseVector.hpp \
	CoinDistance.hpp \
	CoinError.hpp \
	CoinFactorization.hpp \
	CoinSimpFactorization.hpp \
	CoinDenseFactorization.hpp \
	CoinOslFactorization.hpp \
	CoinFileIO.hpp \
	CoinFinite.hpp \
	CoinFloatEqual.hpp \
	CoinHelperFunctions.hpp \
	CoinIndexedVector.hpp \
	CoinLpIO.hpp \
	CoinMessage.hpp \
	CoinMessageHandler.hpp \
	CoinModel.hpp \
	CoinStructuredModel.hpp \
	CoinModelUseful.hpp \
	CoinMpsIO.hpp \
	CoinPackedMatrix.hpp \
	CoinPackedVector.hpp \
	CoinPackedVectorBase.hpp \
	CoinParam.hpp \
	CoinPragma.hpp \
	CoinPresolveDoubleton.hpp \
	CoinPresolveDual.hpp \
	CoinPresolveDupcol.hpp \
	CoinPresolveEmpty.hpp \
	CoinPresolveFixed.hpp \
	CoinPresolveForcing.hpp \
	CoinPresolveImpliedFree.hpp \
	CoinPresolveIsolated.hpp \
	CoinPresolveMatrix.hpp \
	CoinPresolveMonitor.hpp \
	CoinPresolvePsdebug.hpp \
	CoinPresolveSingleton.hpp \
	CoinPresolveSubst.hpp \
	CoinPresolveTighten.hpp \
	CoinPresolveTripleton.hpp \
	CoinPresolveUseless.hpp \
	CoinPresolveZeros.hpp \
	CoinRational.hpp \
	CoinSearchTree.hpp \
	CoinShallowPackedVector.hpp \
	CoinSignal.hpp \
	CoinSmartPtr.hpp \
	CoinSnapshot.hpp \
	CoinSort.hpp \
	CoinTime.hpp \
	CoinTypes.hpp \
	CoinUtility.hpp \
	CoinWarmStart.hpp \
	CoinWarmStartBasis.hpp \
	CoinWarmStartVector.hpp \
	CoinWarmStartDual.hpp \
	CoinWarmStartPrimalDual.hpp

#######################################################################
# Create the Config.h file that has all public defines and install it #
#######################################################################

install-exec-local:
	$(install_sh_DATA) config_coinutils.h $(DESTDIR)$(includecoindir)/CoinUtilsConfig.h

uninstall-local:
	rm -f $(DESTDIR)$(includecoindir)/CoinUtilsConfig.h
