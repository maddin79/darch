\docType{methods}
\name{generateWeights}
\alias{generateWeights}
\title{Generates a weight matrix.}
\usage{
  generateWeights(numUnits1, numUnits2)
}
\arguments{
  \item{numUnits1}{Number of units in the lower layer.}

  \item{numUnits2}{Number of units in the upper layer.}
}
\description{
  This function is the standard method for generating
  weights for instances of \code{\link{Net}}. When using
  another function to generate weights, the function must
  be like this one.
}
\seealso{
  \code{\link{Net}}
}

