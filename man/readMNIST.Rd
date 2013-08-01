\docType{methods}
\name{readMNIST}
\alias{readMNIST}
\title{Function for generating ff files of the MNIST Database}
\usage{
  readMNIST(folder)
}
\arguments{
  \item{folder}{The location of the MNIST-Database files.}
}
\description{
  This function reads the MNIST-Database, randomized it and
  saved it in the files "train" for the training data and
  "test" for test data.
}
\details{
  When the data is read the variables for the training data
  is \code{trainData} and \code{trainLabels} and for the
  test data \code{testData} and \code{testLabels}. To start
  the function The files "train-images-idx3-ubyte",
  "train-labels-idx1-ubyte', "t10k-images-idx3-ubyte", and
  "t10k-labels-idx1-ubyte" have to be in the folder given
  by the parameter \code{folder}. The folder name must end
  with a slash.
}

