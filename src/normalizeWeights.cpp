#include <Rcpp.h>
using namespace Rcpp;

// edits in-place
// [[Rcpp::export]]
void normalizeWeightsCpp(NumericMatrix weights, float bound)
{
  int ncols = weights.ncol();
  
  double colSum;
  NumericVector column;
  for (int i = 0; i < ncols; i++)
  {
    column = weights.column(i);
    colSum = sqrt(sum(column * column));
    
    // Normalize by comparing square sum to bound
    if (colSum > bound)
    {
      weights(_, i) = column / colSum * bound; 
    }
  }
}
