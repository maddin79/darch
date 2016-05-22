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
void maxoutUnitCpp(NumericMatrix activations,
  NumericMatrix derivatives, int poolSize, NumericVector dropoutMask)
{
  NumericMatrix subMatrix;
  int nrows = activations.nrow();
  int ncols = activations.ncol();
  int pools = ncols / poolSize;
  int poolStart, poolEnd, maxIndex;
  float activation, currentActivation, derivative;
  bool dropout = (dropoutMask.length() > 0);

  for (int i = 0; i < pools; i++)
  {
    poolStart = poolSize * i;
    poolEnd = poolStart + (poolSize - 1);
    subMatrix = activations(_, Range(poolStart, poolEnd));
    
    for (int j = 0; j < nrows; j++)
    {
      activation = -INFINITY;
      derivative = 0;
      maxIndex = 0;
      
      // Find maximum within pool
      for (int k = 0; k < poolSize; k++)
      {
        currentActivation = subMatrix(j, k);
        
        if (currentActivation > activation &&
          (!dropout || dropoutMask(poolStart + k) == 1))
        {
          activation = currentActivation;
          derivative = derivatives(j, poolStart + k);
          maxIndex = k;
        }
        
        // Set everything to 0
        activations(j, poolStart + k) = 0;
        derivatives(j, poolStart + k) = 0;
      }
    
      // Restore maximum
      if (activation != -INFINITY)
      {
        activations(j, poolStart + maxIndex) = activation;
        derivatives(j, poolStart + maxIndex) = derivative;
      }
    }
  }
}

// edits in-place
// [[Rcpp::export]]
void maxoutWeightUpdateCpp(NumericMatrix inc, int poolSize)
{
  NumericMatrix subMatrix;
  int ncols = inc.ncol();
  int nrows = inc.nrow();
  int poolStart;
  int poolEnd;
  int pools = nrows / poolSize;
  
  for (int i = 0; i < pools; i++)
  {
    poolStart = poolSize * i;
    poolEnd = poolStart + (poolSize - 1);
    subMatrix = inc(Range(poolStart, poolEnd), _);
    
    double sumInc = 0;
    for (int col = 0; col < ncols; col++)
    {
      // Only one element of the pool had its weight changed
      sumInc = sum(subMatrix(_, col));
      
      for (int j = poolStart; j <= poolEnd; j++)
      {
        // All weights within a pool are kept the same
        inc(j, col) = sumInc;
      }
    }
  }
}