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

struct ApplyDropoutMask : public Worker
{
  const RMatrix<double> input;
  
  int colLength;
  
  RMatrix<double> output;
  
  RVector<double> mask;
  
  ApplyDropoutMask(const NumericMatrix input, NumericMatrix output, NumericVector mask) 
    : input(input), output(output), mask(mask) {
    colLength = input.nrow();
  }
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    for (int i = begin_col; i < end_col; i++)
    {
      RMatrix<double>::Column colOutput = output.column(i);
      RMatrix<double>::Column colInput = input.column(i);
      for (int j = 0; j < colLength; j++)
      {
        colOutput[j] = colInput[j] * mask[i];
      }
    }
    
  }
};

// does not edit in-place
// [[Rcpp::export]]
NumericMatrix applyDropoutMaskCpp(NumericMatrix data, NumericVector mask)
{
  NumericMatrix cData(clone(data));
  int ncols = cData.ncol();
  
  ApplyDropoutMask worker(data, cData, mask);
  
  parallelFor(0, ncols, worker);
  
  return cData;
}
