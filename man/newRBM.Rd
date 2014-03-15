\name{newRBM}
\alias{newRBM}
\title{Constructor function for RBM object.}
\usage{
newRBM(numVisible, numHidden, batchSize, ff = FALSE, logLevel = INFO,
  genWeightFunc = generateWeights)
}
\arguments{
  \item{numVisible}{Number of visible units.}

  \item{numHidden}{Number of hidden units.}

  \item{batchSize}{Size of the batches}

  \item{ff}{Indicates whether the \code{\link[ff]{ff}}
  package is used for the weights, biases and outputs}

  \item{logLevel}{The logging level.  See
  \code{\link{setLogLevel}} for details.}

  \item{genWeightFunc}{The function for generating the
  weight matrices}
}
\value{
The new RBM object
}
\description{
TODO: Doc ...
}

