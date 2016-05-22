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

// does not edit in-place
// [[Rcpp::export]]
NumericMatrix applyDropoutMaskCpp(NumericMatrix data, NumericVector mask)
{
  NumericMatrix cData(clone(data));
  int ncols = cData.ncol();

  for (int i = 0; i < ncols; i++)
  {
    cData.column(i) = cData.column(i) * mask[i];
  }

  return cData;
}
