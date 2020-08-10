/* $Id: CoinAbcSmallFactorization4.cpp 1910 2013-01-27 02:00:13Z stefan $ */
// Copyright (C) 2002, International Business Machines
// Corporation and others, Copyright (C) 2012, FasterCoin.  All Rights Reserved.
// This code is licensed under the terms of the Eclipse Public License (EPL).

#include "CoinAbcCommonFactorization.hpp"
#ifndef ABC_JUST_ONE_FACTORIZATION
#define CoinAbcTypeFactorization CoinAbcSmallFactorization
#define ABC_SMALL 4
#include "CoinAbcBaseFactorization.hpp"
// If we are twiddling this one has data
#define FACTORS_HERE
#include "CoinAbcBaseFactorization4.cpp"
#endif
