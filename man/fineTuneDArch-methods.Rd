\docType{methods}
\name{fineTuneDArch}
\alias{fineTuneDArch}
\alias{fineTuneDArch,DArch-method}
\title{Fine tuning function for the deep architecture.}
\usage{
fineTuneDArch(darch, trainData, targetData, ..., maxEpoch = 1,
  isBin = FALSE, isClass = TRUE, validData = NULL, validTargets = NULL,
  testData = NULL, testTargets = NULL, stopErr = -Inf,
  stopClassErr = 101, stopValidErr = -Inf, stopValidClassErr = 101)

\S4method{fineTuneDArch}{DArch}(darch, trainData, targetData, ...,
  maxEpoch = 1, isBin = FALSE, isClass = TRUE, validData = NULL,
  validTargets = NULL, testData = NULL, testTargets = NULL,
  stopErr = -Inf, stopClassErr = 101, stopValidErr = -Inf,
  stopValidClassErr = 101)
}
\arguments{
  \item{darch}{A instance of the class
  \code{\link{DArch}}.}

  \item{trainData}{The training data matrix}

  \item{targetData}{The expected output matrix for the
  training data}

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
The fine tuning function for deep architectures. This
function use the function saved in the attribute
\code{fineTuneFunction} to train the deep architecture.
}
\details{
The function trains the given network \code{darch} with the
function saved in the attribute \code{fineTuneFunction} of
the \code{\link{DArch}}-Object. The data (\code{trainData},
\code{validData}, \code{testData}) and belonging classes of
the data (\code{targetData}, \code{validTargets},
\code{testTargets}) can be hand over either as matrix or as
ff-matrix (see package \link{ff} for details).  The data
and classes for validation and testing are optional. If
they are provided the network will be executed with this
datasets and statistics will be calculated. This statistics
are saved in the \code{stats} attribute (see
\code{\link{Net}}). The attribue \code{isBin} indicates
whether the output data must be interpreted as binary
value. If true every value over 0.5 is interpreted as 1
otherwise as 0. Also it is possible to set stop criteria
for the training on the error (\code{stopErr},
\code{stopValidErr}) or the correct classifications
(\code{stopClassErr}, \code{stopValidClassErr}) of the
training or validation dataset.
}
\seealso{
\code{\link{DArch}}, \code{\link{Net}},
\code{\link{backpropagation}}, \code{\link{rpropagation}},
\code{\link{minimizeAutoencoder}},
\code{\link{minimizeClassifier}}
}

