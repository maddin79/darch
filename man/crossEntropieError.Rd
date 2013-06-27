\docType{methods}
\name{crossEntropieError}
\alias{crossEntropieError}
\title{Cross entropie error function}
\usage{
  crossEntropieError(original, estimate)
}
\arguments{
  \item{original}{The original data}

  \item{estimate}{The calculated data}
}
\value{
  A list with the name of the error function in the first
  entry and the error value in the second entry
}
\description{
  Calculates the cross entropie error from the parameters
  \code{original} and \code{estimate}
}
\seealso{
  \code{\link{quadraticError}}, \code{\link{mseError}}
}

