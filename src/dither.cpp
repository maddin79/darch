#include <Rcpp.h>
using namespace Rcpp;

// edits in-place
// [[Rcpp::export]]
void ditherCpp(NumericMatrix data)
{
  int ncols = data.ncol();
  NumericVector column, dither;
  float sdColumn, variance;

  for (int i = 0; i < ncols; i++)
  {
    column = data.column(i);
    sdColumn = sd(column);
    variance = sdColumn * sdColumn;
    dither = runif(ncols, -variance, variance);
    data(_, i) = column + dither;
  }
}
