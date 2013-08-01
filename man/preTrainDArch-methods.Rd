\docType{methods}
\name{preTrainDArch}
\alias{preTrainDArch}
\alias{preTrainDArch,DArch-method}
\title{Pre trains a \code{\link{DArch}} network}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{trainData}{The data matrix for the training}

  \item{maxEpoch}{The number of epochs}

  \item{numCD}{The number of CD iterations}

  \item{...}{Additional parameters for the function
  \code{\link{trainRBM}}}
}
\description{
  This function pre trains a \code{\link{DArch}} network
  with the contrastive divergence method
}
\details{
  The function runs for every \code{\link{RBM}} in the
  attribute \code{rbmList} the traning function
  \code{\link{trainRBM}} copies after the training the
  weights and biases into the corresponding layer of the
  \code{\link{DArch}} network.
}
\seealso{
  \code{\link{DArch}}, \code{\link{RBM}},
  \code{\link{trainRBM}}
}

