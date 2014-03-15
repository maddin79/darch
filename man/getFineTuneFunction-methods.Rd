\docType{methods}
\name{getFineTuneFunction}
\alias{getFineTuneFunction}
\alias{getFineTuneFunction,DArch-method}
\title{Returns the fine tune function for the \code{\link{DArch}}
object.}
\usage{
getFineTuneFunction(darch)

\S4method{getFineTuneFunction}{DArch}(darch)
}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}
}
\description{
Returns the fine tune function which is executed by the
function \code{\link{fineTuneDArch}}.
}
\seealso{
\code{\link{DArch}}, \code{\link{backpropagation}},
\code{\link{rpropagation}},
\code{\link{minimizeAutoencoder}},
\code{\link{minimizeClassifier}}
}

