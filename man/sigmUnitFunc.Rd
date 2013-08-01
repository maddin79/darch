\docType{methods}
\name{sigmUnitFunc}
\alias{sigmUnitFunc}
\title{Calculates the neuron output with the sigmoid function}
\usage{
  sigmUnitFunc(rbm, dataList, biases, weights, runParams)
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
  Calculates the neuron output with the sigmoid function
  from binary input saved in the second entry of the list
  \code{dataList}.
}
\details{
  The return value is a list with the output of the sigmoid
  function as first entry and binary representation
  calculated through a comparison of the output with random
  numbers. The random numbers a generated with the function
  \code{\link{runif}}.
}
\seealso{
  \code{\link{DArch}}
}

