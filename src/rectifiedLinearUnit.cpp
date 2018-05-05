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

struct RectifiedLinearUnit : UnitFunction {
  
  RectifiedLinearUnit(const NumericMatrix input, NumericMatrix activations,
    NumericMatrix derivatives) : UnitFunction(input, activations, derivatives)
  {}
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    for (int i = begin_col; i < end_col; i++)
    {
      for (int j = 0; j < nrowInput; j++)
      {
        if (input(j, i) <= 0)
        {
          activations(j, i) = 0;
          derivatives(j, i) = 0;
        }
        else
        {
          derivatives(j, i) = 1;
        }
      }
    }
  }
};

// [[Rcpp::export]]
List rectifiedLinearUnitCpp(NumericMatrix input)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations = clone(input);
  NumericMatrix derivatives = NumericMatrix(Dimension(nrows, ncols));
  
  RectifiedLinearUnit worker(input, activations, derivatives);
  
  parallelFor(0, ncols, worker);
  
  return List::create(activations, derivatives);
}
