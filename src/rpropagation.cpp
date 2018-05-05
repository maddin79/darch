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

struct RpropGradients : public Worker
{
  const RMatrix<double> gg;
  
  RMatrix<double> gradients;
  
  int colLength;
  
  RpropGradients(const NumericMatrix gg, NumericMatrix gradients) 
    : gg(gg), gradients(gradients) {
    colLength = gg.nrow();
  }
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    for (int i = begin_col; i < end_col; i++)
    {
      for (int j = 0; j < colLength; j++)
      {
        if (gg(j, i) < 0) gradients(j, i) = 0;
      }
    }
  }
};

// edits in-place
// [[Rcpp::export]]
void rpropGradientsCpp(NumericMatrix gg, NumericMatrix gradients)
{
  int ncols = gg.ncol();
  RpropGradients worker(gg, gradients);
  
  parallelFor(0, ncols, worker);
}

struct RpropDelta : public Worker
{
  const RMatrix<double> gg;
  
  RMatrix<double> delta;
  
  double inc, dec, minDelta, maxDelta;
  
  int colLength;
  
  RpropDelta(const NumericMatrix gg, NumericMatrix delta, double inc,
                 double dec, double minDelta, double maxDelta) :
      gg(gg), delta(delta), inc(inc), dec(dec), minDelta(minDelta),
      maxDelta(maxDelta) {
    colLength = gg.nrow();
  }
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    for (int i = begin_col; i < end_col; i++)
    {
      for (int j = 0; j < colLength; j++)
      {
        if (gg(j, i) < 0) delta(j, i) = std::max(delta(j, i) * dec, minDelta);
        else if (gg(j, i) > 0) delta(j, i) = std::min(delta(j, i) * inc, maxDelta);
      }
    }
  }
};

// edits in-place
// [[Rcpp::export]]
void rpropDeltaCpp(NumericMatrix gg, NumericMatrix delta, double inc,
                           double dec, double minDelta, double maxDelta)
{
  int ncols = gg.ncol();
  RpropDelta worker(gg, delta, inc, dec, minDelta, maxDelta);
  
  parallelFor(0, ncols, worker);
}

struct RpropDeltaWiRpropPlus : public Worker
{
  const RMatrix<double> gg, gradients, delta;
  
  RMatrix<double> deltaW;
  
  const double newE, oldE;
  
  int colLength;
  
  RpropDeltaWiRpropPlus(const NumericMatrix gg, NumericMatrix deltaW,
             const NumericMatrix gradients, const NumericMatrix delta,
             const double newE, const double oldE) :
      gg(gg), deltaW(deltaW), gradients(gradients), delta(delta), newE(newE),
      oldE(oldE)
  {
    colLength = gg.nrow();
  }
  
  void operator()(std::size_t begin_col, std::size_t end_col)
  {
    for (int i = begin_col; i < end_col; i++)
    {
      for (int j = 0; j < colLength; j++)
      {
        if (gg(j, i) >= 0)
          deltaW(j, i) =
            ((gradients(j, i) < 0) - (gradients(j, i) > 0)) * delta(j, i);
        else if (newE > oldE) deltaW(j, i) = -deltaW(j, i);
        else deltaW(j, i) = 0;
      }
    }
  }
};

// edits in-place
// [[Rcpp::export]]
void rpropDeltaWiRpropPlusCpp(const NumericMatrix gg,
      NumericMatrix deltaW, const NumericMatrix gradients,
      const NumericMatrix delta, const double newE, const double oldE)
{
  int ncols = gg.ncol();
  RpropDeltaWiRpropPlus worker(gg, deltaW, gradients, delta, newE, oldE);
  
  parallelFor(0, ncols, worker);
}
