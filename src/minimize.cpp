/* 
# Copyright (C) 2015-2016 Johannes Rueckert
#
# This file is part of darch.
#
# darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch. If not, see <http://www.gnu.org/licenses/>.
*/

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List minimizeCpp(NumericVector x, Function f, int length, double red,
  List dims, NumericMatrix data, NumericMatrix target, int epochSwitch,
  Function matMult)
{
  // don't reevaluate within 0.1 of the limit of the current bracket
  const double INT = 0.1;
  // extrapolate maximum 3 times the current step-size
  const double EXT = 3.0;
  // max 20 function evaluations per line search
  const int MAX = 20;
  // maximum allowed slope ratio
  const int RATIO = 10;
  const double SIG = 0.1;
  const double RHO = SIG/2;
  
  int lsFailed = 0;
  
  // temporary variables
  NumericVector nv;
  
  // loop variable
  int iNegInc = (length < 0 ? 1 : 0), iPosInc = -iNegInc + 1, i = iNegInc;
  NumericVector ret(f(x, dims, data, target, epochSwitch));
  
  double f0 = !NumericVector::is_na(ret(0)) ? ret(0) : INFINITY;
  NumericVector fX(1, f0);
  
  IntegerVector range = Range(1, ret.length() - 1);
  NumericVector df0 = ret[range];
  NumericVector s(-df0);
  
  double d0 = as<double>(matMult(NumericMatrix(1, df0.length(), df0.begin()), s));

  double x3 = red / (1 - d0);
  int iterations = abs(length), m;
  double f0Best, x1, x2, x4 = 0, d1, d2, d3, d4 = 0, f1, f2, f3, f4 = 0, a, b, tmp;
  NumericVector df0Best, df3;
  NumericVector x0Best;
  bool success, nanOrInf;
  
  while (i < iterations)
  {
    i = i + iPosInc;
    
    x0Best = clone(x);
    f0Best = f0;
    df0Best = clone(df0);
    
    m = (length > 0 ? MAX : std::min(MAX, -length-i));
    
    while (true)
    {
      x2 = 0;
      f2 = f0;
      d2 = d0;
      f3 = f0;
      df3 = clone(df0);
      success = false;
      
      while(!success && m > 0)
      {
        m--;
        i = i + iNegInc;
        
        ret = f(x + x3 * s, dims, data, target, epochSwitch);
        
        f3 = !NumericVector::is_na(ret[0]) ? ret[0] : INFINITY;
        
        range = Range(1, ret.length() - 1);
        df3 = ret[range];
        
        nanOrInf = false;
        for (int j = 0; j < df3.length(); j++)
        {
          tmp = df3[j];
          if (tmp == INFINITY || NumericVector::is_na(tmp))
          {
            nanOrInf = true;
            break;
          }
        }
        
        if (f3 == INFINITY || nanOrInf)
        {
          x3 = (x2 + x3) / 2;
        }
        else
        {
          success = true;
        }
      }
      
      if (f3 < f0Best)
      {
        // keep best values
        x0Best = x + x3 * s + 0;
        f0Best = f3;
        df0Best = clone(df3);
      }
      
      d3 = as<double>(matMult(NumericMatrix(1, df3.length(), df3.begin()), s));
      
      // are we done extrapolating?
      if (d3 > SIG*d0 || f3 > f0+x3*RHO*d0 || m == 0)
      {
        break;
      }
      
      // move point 2 to point 1
      x1 = x2;
      f1 = f2;
      d1 = d2;
      // move point 3 to point 2
      x2 = x3;
      f2 = f3;
      d2 = d3;
      
      a = 6*(f1-f2)+3*(d2+d1)*(x2-x1);
      b = 3*(f2-f1)-(2*d1+d2)*(x2-x1);
      
      x3 = x1 - d1 * (x2 - x1) * (x2 - x1) / (b + sqrt(b * b - a * d1 *(x2 - x1)));
      
      // numeroc problem or wrong sign?
      if (NumericVector::is_na(x3) || x3 == INFINITY || x3 < 0)
      {
        // extrapolate maximum amount
        x3 = x2 * EXT;
      }
      // new point beyond extrapolation limit?
      else if (x3 > (x2 * EXT))
      {                  
        // extrapolate maximum amount
        x3 = x2 * EXT;
      }
      // new point too close to previous point?
      else if (x3 < (x2 + INT * (x2 - x1)))
      {
        x3 = x2 + INT * (x2 - x1);
      }
    }
    
    // keep interpolating
    while ((fabs(d3) > (-SIG * d0) || f3 > (f0 + x3 * RHO * d0)) && m > 0)
    {
      // choose subinterval
      if (d3 > 0 || f3 > (f0 + x3 * RHO * d0))
      {
        // move point 3 to point 4
        x4 = x3;
        f4 = f3;
        d4 = d3;
      }
      else
      {
        // move point 3 to point 2
        x2 = x3;
        f2 = f3;
        d2 = d3;
      }
      
      if (f4 > f0)
      {
        // quadratic interpolation
        x3 = x2 - (0.5 * d2 * (x4 - x2) * (x4 - x2)) / (f4 - f2 - d2 * (x4 - x2));
      }
      else
      {
        // cubic interpolation
        a = 6 * (f2 - f4) / (x4 - x2) + 3 * (d4 + d2);
        b = 3 * (f4 - f2) - (2 * d2 + d4) * (x4 - x2);
        // num. error possible, ok!
        x3 = x2 + (sqrt(b * b - a * d2 * (x4 - x2) * (x4 - x2)) - b) / a;
      }
      
      if (NumericVector::is_na(x3) || fabs(x3) == INFINITY)
      {
        // if we had a numerical problem then bisect
        x3 = (x2 + x4) / 2;
      }
      
      // don't accept too close
      x3 = std::max(std::min(x3, x4 - INT * (x4 - x2)) , x2 + INT * (x4 - x2));
      ret = f(x + x3 * s, dims, data, target, epochSwitch);
      f3 = ret[0];
      range = Range(1, ret.length() - 1);
      df3 = ret[range];
      
      // keep best values
      if (f3 < f0Best)
      {
        x0Best = x + x3 * s;
        f0Best = f3;
        df0Best = clone(df3);
      }
      
      m--;
      // count epochs?!
      i = i + iNegInc;
      // new slope
      d3 = as<double>(matMult(NumericMatrix(1, df3.length(), df3.begin()), s));
    }
    
    // if line search succeeded
    if (fabs(d3) < (-SIG * d0) && f3 < (f0 + x3 * RHO * d0))
    {
      x = x + x3 * s;
      f0 = f3;
      fX.push_back(f0);
      // Polack-Ribiere CG direction
      s = (as<double>(matMult(NumericMatrix(1, df3.length(), df3.begin()), df3))
        - as<double>(matMult(NumericMatrix(1, df0.length(), df0.begin()), df3)))
        / (as<double>(matMult(NumericMatrix(1, df0.length(), df0.begin()), df0))) * s - df3;
      // swap derivatives
      df0 = clone(df3);
      d3 = d0;
      d0 = as<double>(matMult(NumericMatrix(1, df0.length(), df0.begin()), s));
      
      // new slope must be negative
      if (d0 > 0)
      {
        s = -df0;
        // otherwise use steepest direction
        d0 = as<double>(matMult(-NumericMatrix(1, s.length(), s.begin()), s));
      }
      
      // slope ratio but max RATIO
      x3 = x3 * std::min((double)RATIO, d3/(d0 - std::numeric_limits<double>::min()));
      // this line search did not fail
      lsFailed = 1;
    }
    else
    {
      // restore best point so far
      x = clone(x0Best);
      f0 = f0Best;
      df0 = clone(df0Best);
      
      // line search failed twice in a row
      if (lsFailed || i > abs(length))
      {
        // or we ran out of time, so we give up
        break;
      }
      
      s = -df0;
      // try steepest
      d0 = as<double>(matMult(-NumericMatrix(1, s.length(), s.begin()), s));
      x3 = 1 / (1 - d0);
      // this line search failed
      lsFailed = 1;
    }
  }
  
  return List::create(x, fX, i);
}
