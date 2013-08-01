\docType{methods}
\name{resetDArch}
\alias{resetDArch}
\alias{resetDArch,DArch-method}
\title{Resets the weights and biases of the \code{\link{DArch}} object}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}
}
\description{
  This function resets the weights and biases of the
  \code{\link{DArch}} object and all \code{\link{RBM}}
  objects if the parameter \code{resetRBMs} is \code{TRUE}.
}
\details{
  When the parameter \code{resetRBMs} is \code{FALSE} then
  the trained weights and biases are copied from the
  \code{\link{RBM}} objects to the layers.
}
\seealso{
  \code{\link{DArch}}
}

