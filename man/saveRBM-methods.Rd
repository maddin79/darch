\docType{methods}
\name{saveRBM}
\alias{saveRBM}
\alias{saveRBM,RBM-method}
\title{Saves a RBM network}
\arguments{
  \item{rbm}{A instance of the class \code{\link{RBM}}.}

  \item{name}{The name for the file. Default value is
  "rbm".}
}
\description{
  Saves the RBM object to the filename given through the
  parameter \code{name} plus the ending ".net".
}
\details{
  If the field \code{ff} of the RBM object is \code{TRUE}
  then the weights are saved in seperate ff-files through
  the funktion \code{\link{saveRBMFFWeights}}.
}
\seealso{
  \code{\link{loadRBM}}, \code{\link{saveRBMFFWeights}}
  \code{\link{loadRBMFFWeights}}
}

