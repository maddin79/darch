\docType{methods}
\name{getExecOutput}
\alias{getExecOutput}
\alias{getExecOutput,DArch-method}
\title{Returns the execution output of the layer from the \code{\link{DArch}} object}
\usage{
getExecOutput(darch,index=1)

\S4method{getExecOutput}{DArch}(darch, index = NULL)
}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{index}{The index of the layer.}
}
\description{
Returns the execution output of the layer by the given
\code{index}. If the index is not set, the output of the
last layer will be returned.
}
\seealso{
\code{\link{DArch}}
}

