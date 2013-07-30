\docType{methods}
\name{sigmUnitFuncSwitch}
\alias{sigmUnitFuncSwitch}
\title{Calculates the neuron output with the sigmoid function}
\usage{
  sigmUnitFuncSwitch(rbm, dataList, biases, weights,
    runParams)
}
\arguments{
  \item{rbm}{A instance of the class \code{\link{RBM}}.}

  \item{dataList}{A list with the data matrices for the
  calculations.}

  \item{biases}{The biases for the calculations}

  \item{weights}{The weight matrix for the calculations}

  \item{runParams}{Parameters which indicates the status of
  the training. \code{"actualCD"} and \code{"finishCD"} are
  needed (see \code{\link{trainRBM}})}
}
\value{
  The real value and binary activations for the units
}
\description{
  Calculates the neuron output with the sigmoid function
  either from the real value or binary input saved in the
  list \code{dataList}.
}
\details{
  The return value is a list with the output of the sigmoid
  function as first entry and binary representation
  calculated through a comparison of the output with random
  numbers. The random numbers a generated with the function
  \code{\link{runif}}. If the parameter
  \code{runParams["actualCD"]} or
  \code{runParams["finishCD"]} is equal one, the
  calculation is made with the real value data
  (\code{dataList[[1]]}), otherwise with the binary
  representations (\code{dataList[[2]]}).
}
\seealso{
  \code{\link{DArch}}
}

