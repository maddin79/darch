\docType{methods}
\name{addExecOutput}
\alias{addExecOutput}
\alias{addExecOutput,DArch-method}
\title{Adds an execution output for a DArch object}
\usage{
addExecOutput(darch, output)

\S4method{addExecOutput}{DArch}(darch, output)
}
\arguments{
  \item{darch}{An instance of the class
  \code{\link{DArch}}.}

  \item{output}{The output of the layer.}
}
\description{
This method can be used to save the execution outputs of
every layer for the DArch object. The outputs are saved in
a list and every time this function is called, the list is
extended of one field with the new output.
}
\seealso{
\code{\link{DArch}}
}

