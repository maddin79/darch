\name{RBM}
\alias{RBM}
\alias{RBM-class}
\title{Class for Restricted-Bolzmann-Machine}
\description{
  This class represents a Restricted-Bolzmann-Machine
}
\section{Slots}{
  \describe{ \item{\code{learnRateBiasVisible}:}{Object of
  class \code{"numeric"}.  Learning rate of the visible
  biases.} \item{\code{learnRateBiasHidden}:}{Object of
  class \code{"numeric"}.  Learning rate of the hidden
  biases.} \item{\code{weightCost}:}{Object of class
  \code{"numeric"}.  Weigth cost for the update of the
  weigths.} \item{\code{numHidden}:}{Object of class
  \code{"numeric"}.  Number of hidden units.}
  \item{\code{numVisible}:}{Object of class
  \code{"numeric"}.  Number of visible units.}
  \item{\code{weights}:}{Object of class \code{"matrix"}.
  Weight matrix.} \item{\code{weightInc}:}{Object of class
  \code{"matrix"}.  Matrix of update values for the
  Weight.} \item{\code{output}:}{Object of class
  \code{"matrix"}.  Output matrix of the RBM.}
  \item{\code{visibleBiases}:}{Object of class
  \code{"array"}.  Visible biases array.}
  \item{\code{visibleBiasesInc}:}{Object of class
  \code{"array"}.  Array of update values for the visible
  biases} \item{\code{visibleUnitFunction}:}{Object of
  class \code{"function"}.  Unit function for the visible
  units.} \item{\code{visibleUnitStates}:}{Object of class
  \code{"list"}.  States of the visible units.}
  \item{\code{hiddenBiases}:}{Object of class
  \code{"array"}.  Hidden biases array.}
  \item{\code{hiddenBiasesInc}:}{Object of class
  \code{"array"}.  Array of update values for the hidden
  biases.} \item{\code{hiddenUnitFunction}:}{Object of
  class \code{"function"}.  Unit function for the hidden
  units.} \item{\code{hiddenUnitStates}:}{Object of class
  \code{"list"}.  States of the hidden units.}
  \item{\code{updateFunction}:}{Object of class
  \code{"function"}.  Function for updating the weights and
  biases.} \item{\code{posPhaseData}:}{Object of class
  \code{"list"}.  Attribute to save the positive phase data
  during the training.} \item{\code{ffWeights}:}{Object of
  class \code{"ff_matrix"}.  Weight ff matrix. Used when
  the ff attribute is \code{TRUE}.}
  \item{\code{ffOutput}:}{Object of class
  \code{"ff_matrix"}.  Output ff matrix of the RBM. Used
  when the ff attribute is \code{TRUE}.}
  \item{\code{ffHiddenBiases}:}{Object of class
  \code{"ff_array"}.  Hidden biases ff array. Used when the
  ff attribute is \code{TRUE}.}
  \item{\code{ffVisibleBiases}:}{Object of class
  \code{"ff_array"}.  Hidden biases ff array. Used when the
  ff attribute is \code{TRUE}.} }
}
\author{
  Martin Drees
}
\seealso{
  \code{\linkS4class{Net}}, \code{\linkS4class{DArch}},
  \code{\link{trainRBM}}
}

