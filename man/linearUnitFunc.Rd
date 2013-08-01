\docType{methods}
\name{linearUnitFunc}
\alias{linearUnitFunc}
\title{Calculates the linear neuron output no transfer function}
\usage{
  linearUnitFunc(rbm, dataList, biases, weights, runParams)
}
\arguments{
  \item{rbm}{A instance of the class \code{\link{RBM}}.}

  \item{dataList}{A list with the data matrices for the
  calculations.}

  \item{biases}{The biases for the calculations}

  \item{weights}{The weight matrix for the calculations}

  \item{runParams}{Parameters which indicates the status of
  the training.}
}
\value{
  The real value and binary activations for the units
}
\description{
  Calculates the linear neuron output with no transfer
  function from real value input saved in the first entry
  of the list \code{dataList}.
}
\details{
  The return value is a list with the output of the neurons
  as first entry and binary representation calculated
  through a comparison of the output with random numbers.
  The random numbers a generated with the function
  \code{\link{rnorm}}.
}
\seealso{
  \code{\link{DArch}}
}

