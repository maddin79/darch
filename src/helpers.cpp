#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

double cppSD(RMatrix<double>::Column column)
{
  std::vector<double> inVec(column.begin(), column.end());
  int n = inVec.size();
  double sum = std::accumulate(inVec.begin(), inVec.end(), 0.0);
  double mean = sum / inVec.size();
  
  for(std::vector<double>::iterator iter = inVec.begin();
      iter != inVec.end(); 
      ++iter)
  {
    double temp = (*iter - mean)*(*iter - mean);
    *iter = temp;
  }
  
  double sd = std::accumulate(inVec.begin(), inVec.end(), 0.0);
  return std::sqrt( sd / (n-1) );
}

double normalizeWeightsSum(RMatrix<double>::Column column)
{
  std::vector<double> inVec(column.begin(), column.end());
  double sum = 0;
  
  for(std::vector<double>::iterator iter = inVec.begin();
      iter != inVec.end(); 
      ++iter)
  {
    sum += *iter * *iter;
  }
  
  return std::sqrt(sum);
}