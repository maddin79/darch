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

using namespace RcppParallel;
using namespace Rcpp;

struct MaxoutUnit : public Worker
{
  RMatrix<double> activations;
  
  RMatrix<double> derivatives;
  
  const RVector<double> dropoutMask;
  
  const int poolSize;
  
  int colLength;
  
  MaxoutUnit(NumericMatrix activations, NumericMatrix derivatives,
                 const int poolSize, const NumericVector dropoutMask) 
    : activations(activations), derivatives(derivatives),
      dropoutMask(dropoutMask), poolSize(poolSize)
  {
    colLength = activations.nrow();
  }
  
  void operator()(std::size_t begin_pool, std::size_t end_pool)
  {
    int poolStart, poolEnd, maxIndex;
    float activation, currentActivation, derivative;
    bool dropout = (dropoutMask.length() > 0);
    
    for (int i = begin_pool; i < end_pool; i++)
    {
      poolStart = poolSize * i;
      poolEnd = poolStart + (poolSize - 1);
      
      for (int j = 0; j < colLength; j++)
      {
        activation = -INFINITY;
        derivative = 0;
        maxIndex = 0;
        
        // Find maximum within pool
        for (int k = 0; k < poolSize; k++)
        {
          currentActivation = activations(j, poolStart + k);
          
          if (currentActivation > activation &&
              (!dropout || dropoutMask[poolStart + k] == 1))
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
};

// edits in-place
// [[Rcpp::export]]
void maxoutUnitCppParallel(NumericMatrix activations, NumericMatrix derivatives,
                           const int poolSize, const NumericVector dropoutMask)
{
  int ncols = activations.ncol();
  int pools = ncols / poolSize;
  
  MaxoutUnit worker(activations, derivatives, poolSize, dropoutMask);
  
  parallelFor(0, pools, worker);
}

// edits in-place
// [[Rcpp::export]]
void maxoutUnitCpp(NumericMatrix activations,
  NumericMatrix derivatives, int poolSize, NumericVector dropoutMask)
{
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
    
    for (int j = 0; j < nrows; j++)
    {
      activation = -INFINITY;
      derivative = 0;
      maxIndex = 0;
      
      // Find maximum within pool
      for (int k = 0; k < poolSize; k++)
      {
        currentActivation = activations(j, poolStart + k);
        
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

struct MaxoutWeightUpdate : public Worker
{
  RMatrix<double> inc;
  
  const int poolSize;
  
  int colLength;
  
  MaxoutWeightUpdate(NumericMatrix inc, const int poolSize) 
    : inc(inc), poolSize(poolSize)
  {
    colLength = inc.nrow();
  }
  
  void operator()(std::size_t begin_pool, std::size_t end_pool)
  {
    int poolStart, poolEnd, maxIndex;
    int ncols = inc.ncol();
    double sumInc;
    
    for (int i = begin_pool; i < end_pool; i++)
    {
      poolStart = poolSize * i;
      poolEnd = poolStart + (poolSize - 1);
      
      for (int col = 0; col < ncols; col++)
      {
        sumInc = 0;
        
        // Collect sum of changes, only one has changed
        for (int j = poolStart; j <= poolEnd; j++)
        {
          sumInc += inc(j, col);
        }
        
        for (int j = poolStart; j <= poolEnd; j++)
        {
          // All weights within a pool are kept the same
          inc(j, col) = sumInc;
        }
      }
    }
  }
};

// edits in-place
// [[Rcpp::export]]
void maxoutWeightUpdateCpp(NumericMatrix inc, int poolSize)
{
  int ncols = inc.ncol();
  int pools = ncols / poolSize;
  
  MaxoutWeightUpdate worker(inc, poolSize);
  
  parallelFor(0, pools, worker);
}