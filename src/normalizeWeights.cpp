/* 
# Copyright (C) 2015-2018 Johannes Rueckert
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
#include <RcppParallel.h>
#include "helpers.h"

using namespace RcppParallel;
using namespace Rcpp;

struct NormalizeWeights : public Worker
{
  RMatrix<double> weights;
  
  int colLength;
  
  float bound;
  
  NormalizeWeights(NumericMatrix weights, float bound) 
    : weights(weights), bound(bound) {
    colLength = weights.nrow();
  }
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    double colSum;
    
    for (int i = begin_col; i < end_col; i++)
    {
      colSum = normalizeWeightsSum(weights.column(i));
      
      // Normalize after comparing square sum to bound
      if (colSum > bound)
      {
        for (int j = 0; j < colLength; j++)
        {
          weights(j, i) = weights(j, i) / colSum * bound;
        }
      }
    }
    
  }
};

// edits in-place
// [[Rcpp::export]]
void normalizeWeightsCpp(NumericMatrix weights, float bound)
{
  int ncols = weights.ncol();
  NormalizeWeights worker(weights, bound);
  
  parallelFor(0, ncols, worker);
}
