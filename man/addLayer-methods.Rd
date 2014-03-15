\docType{methods}
\name{addLayer}
\alias{addLayer}
\alias{addLayer,DArch-method}
\title{Adds a layer to the DArch object}
\usage{
addLayer(darch, weights, biases, unitFunction)

\S4method{addLayer}{DArch}(darch, weights, biases, unitFunction)
}
\arguments{
  \item{darch}{An instance of the class
  \code{\link{DArch}}.}

  \item{weights}{The weights for the layer.}

  \item{biases}{The biases for the layer.}

  \item{unitFunction}{The functions of the units in the
  layer.}
}
\description{
Adds a layer to the given DArch object. The parameter
weights and biases will be put together in one matrix.
}
\seealso{
\code{\link{DArch}}, \code{\link{sigmoidUnit}},
\code{\link{binSigmoidUnit}},
\code{\link{sigmoidUnitDerivative}},
\code{\link{linearUnit}},
\code{\link{linearUnitDerivative}},
\code{\link{softmaxUnit}},
\code{\link{softmaxUnitDerivative}},
}

