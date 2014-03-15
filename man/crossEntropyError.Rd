\docType{methods}
\name{crossEntropyError}
\alias{crossEntropyError}
\title{Cross entropy error function}
\usage{
crossEntropyError(original, estimate)
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
The function calculates the cross entropy error from the
\code{original} and \code{estimate} parameters.
}
\seealso{
\code{\link{quadraticError}}, \code{\link{mseError}}
}

