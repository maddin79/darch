\docType{methods}
\name{sigmoidUnitDerivative}
\alias{sigmoidUnitDerivative}
\title{Sigmoid unit function with unit derivatives.}
\usage{
  sigmoidUnitDerivative(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
  A list with the activation in the first entry and the
  derivative of the transfer function in the second entry
}
\description{
  The function calculates the activation and returns a list
  which the first entry is the result through the sigmoid
  transfer function and the second entry is the derivative
  of the transfer function.
}
\seealso{
  \code{\link{DArch}}, \code{\link{sigmoidUnit}},
  \code{\link{binSigmoidUnit}}, \code{\link{linearUnit}},
  \code{\link{linearUnitDerivative}},
  \code{\link{softmaxUnit}},
  \code{\link{softmaxUnitDerivative}},
}

