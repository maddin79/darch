\name{Net}
\alias{Net}
\alias{Net-class}
\title{Abtract class for neural networks.}
\description{
  This is a abstract class for neural networks. It provides
  some functionalitys used in more than one network type.
}
\section{Slot}{
  \describe{ \item{\code{batchSize}:}{Object of class
  \code{"numeric"}. The batch size for the training and
  test data during the learning.}
  \item{\code{errorFunction}:}{Object of class
  \code{"function"}. Function for error calculation.}
  \item{\code{ff}:}{Object of class \code{"logical"}.
  Indicates if the package \code{\link[ff]{ff}} is used to
  save the network data.}
  \item{\code{genWeightFunction}:}{Object of class
  \code{"function"}. A function for generate random
  initialised weight matrix. } \item{\code{logger}:}{Object
  of class \code{"logger"}. The log4r object to show the
  log messages.} }
}
\author{
  Martin Drees
}
\seealso{
  \code{\linkS4class{DArch}}, \code{\linkS4class{RBM}}
}

