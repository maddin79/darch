\docType{methods}
\name{trainRBM}
\alias{trainRBM}
\alias{trainRBM,RBM-method}
\title{Trains a \code{\link{RBM}} with contrastive divergence}
\arguments{
  \item{rbm}{A instance of the class \code{\link{RBM}}.}

  \item{trainData}{The data matrix for the training}

  \item{maxEpoch}{The number of training iterations}

  \item{numCD}{Number of contrastive divergence iterations}

  \item{...}{Additional parameters for the unit functions}
}
\description{
  The function trains a restricted bolzmann machine
  (\code{\link{RBM}}) with the contrastive divergence
  method.
}
\details{
  This function is build on the basis of the code from G.
  Hinton et. al.
  (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html
  - last visit 06.06.2013) for the pre training of deep
  belief nets. The original code is located in the files
  'rbm.m' and 'rbmhidlinear.m'. It iterates in every epoche
  over the batches and calculates the updates for the
  weights. If it is the first CD iteration or the CD
  iterations are finished, the hidden units are calculated
  with the real value activations of the visible units,
  otherwise with the binary activations. To tell the unit
  functions the actual state of the training, the function
  generates a array with the following running parameters
  and passes them to the units: Maximal epochs: "maxEpoch",
  Actual epochs: "actualEpoch", Number of batches:
  "numBatches", Actual batch: "actualBatch", Maximal CD
  iterations: "numCD", Actual CD iteration: "actualCD", CD
  is finished: "finishCD". (see source code from
  \code{\link{sigmUnitFuncSwitch}} for an example).
}
\seealso{
  \code{\link{RBM}}
}

