\docType{methods}
\name{saveRBMFFWeights}
\alias{saveRBMFFWeights}
\alias{saveRBMFFWeights,RBM-method}
\title{Saves weights and biases of a RBM network into a ffData file.}
\usage{
saveRBMFFWeights(rbm,name="saveName")

\S4method{saveRBMFFWeights}{RBM}(rbm, name = "saveName")
}
\arguments{
  \item{rbm}{A instance of the class \code{\link{RBM}}.}

  \item{name}{The name for the file.}
}
\description{
Saves the weigths and the biases for the given RBM object
to the filename given through the parameter \code{name}.
}
\details{
The weights and biases are saved in one file with the name
given through the parameter \code{name} and the string
"-WB". See \code{\link{ffsave}} for more details.
}
\seealso{
\code{\link{ffsave}}, \code{\link{loadRBM}},
\code{\link{saveRBM}}, \code{\link{loadRBMFFWeights}}
}

