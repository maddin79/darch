#include <Rcpp.h>
using namespace Rcpp;

// does not edit in-place
// [[Rcpp::export]]
NumericMatrix applyDropoutMaskCpp(NumericMatrix data, NumericVector mask)
{
  NumericMatrix cData(clone(data));
  int nrows = cData.nrow();

  for (int i = 0; i < nrows; i++)
  {
    cData.row(i) = cData.row(i) * mask;
  }

  return cData;
}
