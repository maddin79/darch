\docType{methods}
\name{runDArch}
\alias{runDArch}
\title{Execute the darch}
\usage{
  runDArch(darch, data)
}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{data}{The input data to execute the darch on.}
}
\value{
  The DArch object with the calculated outputs
}
\description{
  Runs the darch in a feed forward manner and saves the
  generated outputs for every layer in the list
  \code{executeOutput} from the darch. To get the outputs
  call
}
\seealso{
  \code{\link{DArch}}
}

