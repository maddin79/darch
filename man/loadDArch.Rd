\docType{methods}
\name{loadDArch}
\alias{loadDArch}
\title{Loads a DArch network}
\usage{
  loadDArch(name="darch")
}
\arguments{
  \item{name}{The name of the file without the ending
  ".net".}
}
\value{
  \code{darch} - The loaded deep architecture
}
\description{
  Loads the DArch object from the filename given through
  the parameter \code{name} plus the ending ".net".
}
\details{
  Make sure when you load a DArch object that every file
  written by the \code{\link{saveDArch}}-Funktion,
  specially when the parameter \code{ff} of the saved DArch
  object is \code{TRUE}, are in the working directory.
}
\seealso{
  \code{\link{saveDArch}}, \code{\link{loadRBMFFWeights}}
}

