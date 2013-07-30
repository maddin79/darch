\name{DArch}
\alias{DArch}
\alias{DArch-class}
\title{Class for deep architectures}
\description{
  This class implements deep architectures and provides the
  ability to train them with a pre training using
  contrastive divergence and fine tuning with
  backpropagation, resilient backpropagation and conjugate
  gradients.
}
\details{
  The class is inherits all attributes from the class
  \code{link{Net}}. When creating a new instance with the
  constructor \code{\link{newDArch}} (recommended), the
  darch-object contained the number of layers -1 restricted
  bolzmann machines (\code{\link{RBM}}), which are used for
  the unsupervised pre training of the network. The
  \code{\link{RBM}}s are saved in the attribute
  \code{rbmList} and can be fetched over the getter method
  (\code{\link{getRBMList}}. The two attributes
  \code{fineTuneFunction} and \code{executeFunction}
  containing the functions for the fine tuning (default:
  \code{\link{backpropagation}}) and for the execution
  (default: \code{\link{runDArch}}. The training of the
  network is performed by the two learning functions
  \code{\link{preTrainDArch}} and
  \code{\link{fineTuneDArch}}. The first function trains
  the network with the unsupervised method contrastive
  divergence. The second function used the function in the
  attribute \code{fineTuneFunction} for the fine tuning.
  After an execution of the network, the outputs of every
  layer can be found in the attribute \code{executeOutput}.
}
\section{Slots}{
  \describe{ \item{\code{rbmList}:}{A list which contains
  all rbm's for the pre-training.} \item{\code{layers}:}{A
  list with the layer information. In the first field are
  the weights and in the second field is the unit function.
  } \item{\code{learnRateBiases}:}{The learning rate for
  the bias weights.}
  \item{\code{fineTuneFunction}:}{Contains the function for
  the fine tuning.} \item{\code{executeFunction}:}{Contains
  the function for executing the network.}
  \item{\code{executeOutput}:}{A list which contains the
  outputs of every layer after an execution of the
  network.} \item{\code{cancel}:}{Boolean value which
  indicates if the network training is canceled.}
  \item{\code{executeOutput}:}{A string containing the
  message why the network training is stopped.} }
}
\author{
  Martin Drees
}
\seealso{
  \code{\linkS4class{Net}}, \code{\linkS4class{RBM}}
}

