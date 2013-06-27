\docType{methods}
\name{quadraticError}
\alias{quadraticError}
\title{Quadratic error function}
\usage{
  quadraticError(original, estimate)
}
\arguments{
  \item{original}{The original data matrix}

  \item{estimate}{The calculated data matrix}
}
\value{
  A list with the name of the error function in the first
  entry and the error value in the second entry
}
\description{
  The function calculates the quadradic error from the
  \code{original} and \code{estimate} parameters.
}
\seealso{
  \code{\link{mseError}}, \code{\link{crossEntropyError}}
}

