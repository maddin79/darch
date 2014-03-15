\docType{methods}
\name{mseError}
\alias{mseError}
\title{Mean quared error function}
\usage{
mseError(original, estimate)
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
The function calculates the mean quared error (MSE) from
the \code{original} and \code{estimate} parameters.
}
\seealso{
\code{\link{quadraticError}},
\code{\link{crossEntropyError}}
}

