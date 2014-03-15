\docType{methods}
\name{linearUnit}
\alias{linearUnit}
\title{Linear unit function.}
\usage{
linearUnit(data,weights)
}
\arguments{
  \item{data}{The data matrix for the calculation}

  \item{weights}{The weight and bias matrix for the
  calculation}
}
\value{
A list with the linear activation of the unit in the first
entry.
}
\description{
The function calculates the activation of the units and
returns it.
}
\seealso{
\code{\link{DArch}}, \code{\link{sigmoidUnit}},
\code{\link{binSigmoidUnit}},
\code{\link{sigmoidUnitDerivative}},
\code{\link{linearUnitDerivative}},
\code{\link{softmaxUnit}},
\code{\link{softmaxUnitDerivative}}
}

