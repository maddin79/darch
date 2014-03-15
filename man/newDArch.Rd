\name{newDArch}
\alias{newDArch}
\title{Constructor function for \code{\link{DArch}} objects.}
\usage{
newDArch(layers,batchSize,ff=FALSE,
logLevel=INFO, genWeightFunc=generateWeights)
}
\arguments{
  \item{layers}{Array of layer sizes.}

  \item{batchSize}{Size of the batches}

  \item{ff}{Indicates whether the \code{\link[ff]{ff}}
  package is used for the weights, biases and outputs}

  \item{logLevel}{The logging level.  See
  \code{\link{setLogLevel}} for details.}

  \item{genWeightFunc}{The function for generating the
  weight matrices}
}
\value{
The new DArch object
}
\description{
Generate a new \code{\link{DArch}} object with the given
parameters.
}
\details{
It is recommended to use this function for generating a new
\code{\link{DArch}} object, because this function generates
and sets all the necessary parameters like the internally
used \code{\link{RBM}} networks, the list of statistiks
(\code{stats}) etc.
}

