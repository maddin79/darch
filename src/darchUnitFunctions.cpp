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
  NumericMatrix derivatives = NumericMatrix(Dimension(nrows, ncols)) + 1;
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
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
  NumericMatrix activations = clone(input);
  NumericMatrix derivatives = NumericMatrix(Dimension(nrows, ncols)) + 1;
  
  for (int i = 0; i < ncols; i++)
  {
    for (int j = 0; j < nrows; j++)
    {
      if (input(j, i) <= 0)
      {
        activations(j, i) = alpha * (exp(activations(j, i)) - 1);
        derivatives(j, i) = input(j, i) + alpha;
      }
    }
  }
  
  return List::create(activations, derivatives);
}
