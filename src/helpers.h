#include <RcppParallel.h>

double cppSD(RcppParallel::RMatrix<double>::Column column);
double normalizeWeightsSum(RcppParallel::RMatrix<double>::Column column);