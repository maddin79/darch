\docType{methods}
\name{softmaxUnit}
\alias{softmaxUnit}
\title{Softmax unit function.}
\usage{
softmaxUnit(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
A list with the softmax activation in the first entry
}
\description{
The function calculates the activation of the units and
returns a list, in which the first entry is the result
through the softmax transfer function.
}
\seealso{
\code{\link{DArch}}, \code{\link{sigmoidUnit}},
\code{\link{binSigmoidUnit}},
\code{\link{sigmoidUnitDerivative}},
\code{\link{linearUnit}},
\code{\link{linearUnitDerivative}},
\code{\link{softmaxUnitDerivative}}
}

