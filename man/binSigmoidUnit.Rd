\docType{methods}
\name{binSigmoidUnit}
\alias{binSigmoidUnit}
\title{Binary sigmoid unit function.}
\usage{
binSigmoidUnit(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
A list with the binary activation of the unit in the first
entry.
}
\description{
The function calculates the activation and the output from
the sigmoid transfer function. It returns a binary matrix
where a entry is 1 if the value is bigger than a random
number generated with \code{\link{runif}}.
}
\seealso{
\code{\link{DArch}}, \code{\link{sigmoidUnit}},
\code{\link{sigmoidUnitDerivative}},
\code{\link{linearUnit}},
\code{\link{linearUnitDerivative}},
\code{\link{softmaxUnit}},
\code{\link{softmaxUnitDerivative}}
}

