// Authors: Matthew Saltzman and Ted Ralphs
// Copyright 2015, Matthew Saltzman and Ted Ralphs
// Licensed under the Eclipse Public License 1.0

#include <algorithm>
#include <cmath>
#ifdef __clang__
//labs() is in cstdlib with clang
#include <cstdlib>
#endif

#include "CoinRational.hpp"

// Based on Python code from
// http://www.johndcook.com/blog/2010/10/20/best-rational-approximation/
// (with permission).
//
// Returns closest (or almost, anyway) rational to val with denominator less
// than or equal to maxdnom.  Return value is true if within tolerance, false
// otherwise.
bool CoinRational::nearestRational_(double val, double maxdelta, long maxdnom)
{
   double intpart;
   double fracpart = fabs(modf(val, &intpart));
   // Consider using remainder() instead?
   
   long a = 0, b = 1, c = 1, d = 1;
   
   while ( b <= maxdnom && d <= maxdnom) {
      double mediant = (a + c)/(double(b + d));
      
      if ( fracpart == mediant ) {
	 if ( b + d <= maxdnom ) {
	    numerator_ = a + c;
	    denominator_ = b + d;
	 } else if ( d > b ) {
	    numerator_ = c;
	    denominator_ = d;
	 } else {
	    numerator_ = a;
	    denominator_ = b;
	 }
      } else if ( val > mediant ) {
	 a = a + c;
	 b = b + d;
      } else {
	 c = a + c;
	 d = b + d;
      }
      
      if ( b > maxdnom ) {
	 numerator_ = c;
	 denominator_ = d;
      } else {
	 numerator_ = a;
	 denominator_ = b;
      }
   }
   
   numerator_ += labs(intpart) * denominator_;
   if ( val < 0 )
      numerator_ *= -1;
   
   return fabs(val - numerator_/double(denominator_)) <= maxdelta;
}

