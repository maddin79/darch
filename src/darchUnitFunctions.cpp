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

// TODO call these directly, requires removal of '...' parameter to unit
// functions

// [[Rcpp::export]]
List sigmoidUnitCpp(NumericMatrix input)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations(Dimension(nrows, ncols));
  NumericMatrix derivatives(Dimension(nrows, ncols));
  
  for (int i = 0; i < ncols; i++)
  {
    activations(_, i) = 1/(1 + exp(-input.column(i)));
    derivatives(_, i) = activations.column(i) * (1 - activations.column(i));
  }
  
  return List::create(activations, derivatives);
}

// [[Rcpp::export]]
List softmaxUnitCpp(NumericMatrix input)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations = transpose(input);
  NumericMatrix derivatives(Dimension(ncols, nrows));
  
  for (int i = 0; i < nrows; i++)
  {
    activations(_, i) = exp(activations.column(i) - max(activations.column(i)));
    activations(_, i) = activations.column(i) / sum(activations.column(i));
    derivatives(_, i) = activations.column(i) * (1 - activations.column(i));
  }
  
  return List::create(transpose(activations), transpose(derivatives));
}

// [[Rcpp::export]]
List rectifiedLinearUnitCpp(NumericMatrix input)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations = clone(input);
  NumericMatrix derivatives = NumericMatrix(Dimension(nrows, ncols));
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
      derivatives(j, i) = 1;
      if (input(j, i) <= 0)
      {
        activations(j, i) = 0;
        derivatives(j, i) = 0;
      }
    }
  }
  
  return List::create(activations, derivatives);
}

// [[Rcpp::export]]
List softplusUnitCpp(NumericMatrix input)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations(Dimension(nrows, ncols));
  NumericMatrix derivatives(Dimension(nrows, ncols));
  
  for (int i = 0; i < ncols; i++)
  {
    activations(_, i) = log(1 + exp(input.column(i)));
    derivatives(_, i) = 1/(1 + exp(-input.column(i)));
  }
  
  return List::create(activations, derivatives);
}

// [[Rcpp::export]]
List exponentialLinearUnitCpp(NumericMatrix input, double alpha)
{
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericMatrix activations = NumericMatrix(Dimension(nrows, ncols));
  NumericMatrix derivatives = NumericMatrix(Dimension(nrows, ncols));
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
      if (input(j, i) <= 0)
      {
        activations(j, i) = alpha * (exp(input(j, i)) - 1);
        derivatives(j, i) = activations(j, i) + alpha;
      }
      else
      {
        activations(j, i) = input(j, i);
        derivatives(j, i) = 1;
      }
    }
  }
  
  return List::create(activations, derivatives);
}
