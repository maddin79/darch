\docType{methods}
\name{fineTuneDArch}
\alias{fineTuneDArch}
\alias{fineTuneDArch,DArch-method}
\title{Fine tuning function for the deep architecture.}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{trainData}{The training data}

  \item{targetData}{The expected output for the training
  data}

  \item{...}{Additional parameters for the training
  function}

  \item{maxEpoch}{The number of training iterations}

  \item{isBin}{Indicates whether the output data must be
  interpreted as boolean value. Default is \code{FALSE}. If
  it is true, every value over 0.5 is interpreted as 1 and
  under as 0.}

  \item{isClass}{Indicates whether the training is for a
  classification net.  When \code{TRUE} then statistics for
  classification will be determind. Default is \code{TRUE}}

  \item{validData}{Data for validating the network. Default
  is \code{NULL}}

  \item{validTargets}{The expected output for the training
  data Default is \code{NULL}}

  \item{testData}{Data for testing the network. Default is
  \code{NULL}}

  \item{testTargets}{The expected output for the training
  data Default is \code{NULL}}

  \item{stopErr}{Stop criteria for the error on the train
  data. Default is \code{-Inf}}

  \item{stopClassErr}{Stop criteria for the classification
  error on the train data. Default is \code{101}}

  \item{stopValidErr}{Stop criteria for the error on the
  validation data. Default is \code{-Inf}.}

  \item{stopValidClassErr}{Stop criteria for the
  classification error on the validation data. Default is
  \code{101} .}
}
\description{
  BEARBEITEN
}
\seealso{
  \code{\link{DArch}}
}

