/* 
# Copyright (C) 2013-2016 Martin Drees
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

// edits in-place
// [[Rcpp::export]]
void rpropGradientsCpp(NumericMatrix gg, NumericMatrix gradients) {
  int ncols = gg.ncol();
  int nrows = gg.nrow();
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
      if (gg(j, i) < 0) gradients(j, i) = 0;
    }
  }
}

// edits in-place
// [[Rcpp::export]]
void rpropDeltaCpp(NumericMatrix gg, NumericMatrix delta, double inc,
  double dec, double minDelta, double maxDelta)
{
  int ncols = gg.ncol();
  int nrows = gg.nrow();
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
      if (gg(j, i) < 0) delta(j, i) = std::max(delta(j, i) * dec, minDelta);
      else if (gg(j, i) > 0) delta(j, i) = std::min(delta(j, i) * inc, maxDelta);
    }
  }
}

// edits in-place
// [[Rcpp::export]]
void rpropDeltaWiRpropPlus(NumericMatrix gg, NumericMatrix deltaW,
  NumericMatrix gradients, NumericMatrix delta, double newE, double oldE)
{
  int ncols = gg.ncol();
  int nrows = gg.nrow();
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
      if (gg(j, i) >= 0)
        deltaW(j, i) = ((gradients(j, i) < 0) - (gradients(j, i) > 0)) *
          delta(j, i);
      else if (newE > oldE) deltaW(j, i) = -deltaW(j, i);
      else deltaW(j, i) = 0;
    }
  }
}