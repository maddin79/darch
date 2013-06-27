\docType{methods}
\name{linearUnitDerivative}
\alias{linearUnitDerivative}
\title{Linear unit function with unit derivatives.}
\usage{
  linearUnitDerivative(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
  A list with the linear activation in the first entry and
  the derivative of the activation in the second entry
}
\description{
  The function calculates the activation of the units and
  returns a list, in which the first entry is the linear
  activation of the units and the second entry is the
  derivative of the transfer function.
}
\seealso{
  \code{\link{DArch}}, \code{\link{sigmoidUnit}},
  \code{\link{binSigmoidUnit}},
  \code{\link{sigmoidUnitDerivative}},
  \code{\link{linearUnit}}, \code{\link{softmaxUnit}},
  \code{\link{softmaxUnitDerivative}}
}

