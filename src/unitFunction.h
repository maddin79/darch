#include <Rcpp.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;

#ifndef UNIT_FUNCTION_H
#define UNIT_FUNCTION_H

struct UnitFunction : public Worker
{
  const RMatrix<double> input;
  
  int ncolInput;
  int nrowInput;
  
  RMatrix<double> activations;
  RMatrix<double> derivatives;
  
  UnitFunction(const NumericMatrix input, NumericMatrix activations,
               NumericMatrix derivatives) : input(input), activations(activations),
               derivatives(derivatives)
  {
    nrowInput = input.nrow();
    ncolInput = input.ncol();
  }
};

#endif