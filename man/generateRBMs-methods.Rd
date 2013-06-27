\docType{methods}
\name{generateRBMs}
\alias{generateRBMs}
\alias{generateRBMs,DArch-method}
\title{Generates the rbm's for the pre-training.}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{layers}{An array with the sizes of the layers}

  \item{genWeightFunc}{The function for generating the
  weight matrices}
}
\value{
  The DArch object with the generated rbm's
}
\description{
  Used the layer sizes from the DArch object to create the
  RBM objects for the pre-training.
}
\seealso{
  \code{\link{DArch}} \code{\link{RBM}}
}

