\docType{methods}
\name{softmaxUnitDerivative}
\alias{softmaxUnitDerivative}
\title{Softmax unit function with unit derivatives.}
\usage{
softmaxUnitDerivative(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
A list with the softmax activation in the first entry and
the derivative of the transfer function in the second entry
}
\description{
The function calculates the activation of the units and
returns a list, in which the first entry is the result
through the softmax transfer function and the second entry
is the derivative of the transfer function.
}
\seealso{
\code{\link{DArch}}, \code{\link{sigmoidUnit}},
\code{\link{binSigmoidUnit}},
\code{\link{sigmoidUnitDerivative}},
\code{\link{linearUnit}},
\code{\link{linearUnitDerivative}},
\code{\link{softmaxUnit}}
}

