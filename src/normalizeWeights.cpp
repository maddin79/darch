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

// edits in-place
// [[Rcpp::export]]
void normalizeWeightsCpp(NumericMatrix weights, float bound)
{
  int ncols = weights.ncol();
  
  double colSum;
  NumericVector column;
  for (int i = 0; i < ncols; i++)
  {
    column = weights.column(i);
    colSum = sqrt(sum(column * column));
    
    // Normalize by comparing square sum to bound
    if (colSum > bound)
    {
      weights(_, i) = column / colSum * bound; 
    }
  }
}
