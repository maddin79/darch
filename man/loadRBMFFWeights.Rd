\docType{methods}
\name{loadRBMFFWeights}
\alias{loadRBMFFWeights}
\title{Loads weigths and biases for a RBM network from a ffData file.}
\usage{
  loadRBMFFWeights(rbm,name)
}
\arguments{
  \item{rbm}{A instance of the class \code{\link{RBM}}.}

  \item{name}{The name of the file without the ending
  ".net".}
}
\value{
  \code{rbm} - The RBM with the loaded weights and biases.
}
\description{
  Loads the weigths and the biases for the given RBM object
  from the filename given through the parameter
  \code{name}. See \code{\link{ffload}} for more details
}
\seealso{
  \code{\link{ffload}}, \code{\link{saveRBM}},
  \code{\link{loadRBM}}, \code{\link{saveRBMFFWeights}}
}

