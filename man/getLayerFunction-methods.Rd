\docType{methods}
\name{getLayerFunction}
\alias{getLayerFunction}
\alias{getLayerFunction,DArch-method}
\title{Returns the neuron function of a layer from the
\code{\link{DArch}} object.}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{index}{The index of the layer.Default is 1.}
}
\description{
  The function returns the neuron function of the layer
  with the given \code{index}.
}
\seealso{
  \code{\link{DArch}} \code{\link{sigmoidUnit}},
  \code{\link{binSigmoidUnit}},
  \code{\link{sigmoidUnitDerivative}},
  \code{\link{linearUnitDerivative}},
  \code{\link{softmaxUnit}},
  \code{\link{softmaxUnitDerivative}}
}

