\docType{methods}
\name{backpropagation}
\alias{backpropagation}
\title{Backpropagation learning function}
\usage{
  backpropagation(darch,trainData,targetData,epoch)
}
\arguments{
  \item{darch}{An instance of the class
  \code{\link{DArch}}.}

  \item{trainData}{The data for training.}

  \item{targetData}{The targets for the data}

  \item{epoch}{Number of epochs for the training}
}
\value{
  The trained deep architecture
}
\description{
  This function provides the backpropagtion algorithm for
  deep architectures.
}
\details{
  The function is getting the learning parameters from the
  provided \code{\link{DArch}} object. It uses the
  attributes \code{momentum}, \code{finalMomentum} and
  \code{momentumSwitch} for the calculation of the new
  weights with momentum. The parameter \code{epoch} is
  provided for the change from \code{momentum} to
  \code{finalMomentum} and is compared to
  \code{momentumSwitch}. The attributes
  \code{learnRateWeights} and \code{learnRateBiases} will
  be used for updating the weights.
}
\references{
  Rumelhart, D., G. E. Hinton, R. J. Williams, Learning
  representations by backpropagating errors, Nature 323, S.
  533-536, DOI: 10.1038/323533a0, 1986.
}
\seealso{
  \code{\link{DArch}} \code{\link{rpropagation}}
  \code{\link{minimizeAutoencoder}}
  \code{\link{minimizeClassifier}}
}

