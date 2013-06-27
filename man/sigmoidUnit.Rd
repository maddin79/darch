\docType{methods}
\name{sigmoidUnit}
\alias{sigmoidUnit}
\title{Sigmoid unit function.}
\usage{
  sigmoidUnit(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
  A list with the activation of the unit in the first
  entry.
}
\description{
  The function calculates the activation and returns the
  result through the sigmoid transfer function.
}
\seealso{
  \code{\link{DArch}}, \code{\link{binSigmoidUnit}},
  \code{\link{sigmoidUnitDerivative}},
  \code{\link{linearUnitDerivative}},
  \code{\link{softmaxUnit}},
  \code{\link{softmaxUnitDerivative}}
}

