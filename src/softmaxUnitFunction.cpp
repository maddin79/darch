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
#include "unitFunction.h"

using namespace RcppParallel;
using namespace Rcpp;

struct SoftmaxUnit : UnitFunction
{
  SoftmaxUnit(const NumericMatrix input, NumericMatrix activations,
    NumericMatrix derivatives) : UnitFunction(input, activations, derivatives)
  {}
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    double colSum;
    
    for (int i = begin_col; i < end_col; i++)
    {
      std::transform(activations.column(i).begin(), activations.column(i).end(),
                     activations.column(i).begin(), ::exp);
      
      colSum = std::accumulate(activations.column(i).begin(),
                               activations.column(i).end(), 0.0);
      
      for (int j = 0; j < ncolInput; j++)
      {
        activations(j, i) = activations(j, i) / colSum;
        derivatives(j, i) = activations(j, i) * (1 - activations(j, i));
      }
    }
  }
};

// [[Rcpp::export]]
List softmaxUnitCpp(NumericMatrix input)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations = transpose(input);
  NumericMatrix derivatives(Dimension(ncols, nrows));
  
  SoftmaxUnit worker(input, activations, derivatives);
  
  parallelFor(0, nrows, worker);
  
  return List::create(transpose(activations), transpose(derivatives));
}
