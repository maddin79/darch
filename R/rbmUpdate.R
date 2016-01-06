# Copyright (C) 2013-2015 Martin Drees
#
# This file is part of darch.
#
# darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch. If not, see <http://www.gnu.org/licenses/>.

#' Function for updating the weights and biases of an \code{\link{RBM}}
#' 
#' This function updates the weights and biases for an \code{\link{RBM}} 
#' network. It is saved in the attribute \code{updateFunction} of the 
#' \code{\link{RBM}} object and called from the training function 
#' \code{\link{trainRBM}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage rbmUpdate(rbm)
#' @return The updated \code{\link{RBM}}.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @docType methods
#' @rdname rbmUpdate
#' @export
rbmUpdate <- function(rbm, matMult = getDarchParam("matMult", `%*%`, ...),
  ...)
{
  # get parameters
  momentum <- getMomentum(rbm)
  weightInc <- getWeightInc(rbm)
  weights <- getWeights(rbm)
  visibleBiasInc <- getVisibleBiasesInc(rbm)
  visibleBiases <- getVisibleBiases(rbm)
  hiddenBiasInc <- getHiddenBiasesInc(rbm)
  hiddenBiases <- getHiddenBiases(rbm)
  data <- getPosPhaseData(rbm)[[1]]
  posHiddenProbs <-  getPosPhaseData(rbm)[[2]][[1]]
  negativData <- getVisibleUnitStates(rbm)[[1]]
  negHiddenProbs <- getHiddenUnitStates(rbm)[[1]]
  learnRate <- rbm@learnRate * (1 - momentum)
  weightDecay <- rbm@weightDecay
  batchSize <- getBatchSize(rbm)
  
  # positive phase
  posProducts <- matMult(t(data), posHiddenProbs)
  posHiddienAct <- apply(posHiddenProbs,2,sum)
  posVisibleAct <- apply(data,2,sum)
  
  # negative phase
  negProducts  <- matMult(t(negativData), negHiddenProbs)
  negHiddienAct <- apply(negHiddenProbs,2,sum)
  negVisibleAct <- apply(negativData,2,sum)
  
  # update
  weightInc <- momentum*weightInc + learnRate * ((posProducts - negProducts)/batchSize - weightDecay*weights)
  # TODO does the weight decay work?
  visibleBiasInc <- momentum*visibleBiasInc + (learnRate/batchSize) * (posVisibleAct-negVisibleAct - visibleBiases*weightDecay)
  hiddenBiasInc <- momentum*hiddenBiasInc + (learnRate/batchSize) * (posHiddienAct-negHiddienAct - hiddenBiases*weightDecay)
  
  setWeights(rbm) <- weights + weightInc
  setVisibleBiases(rbm) <- visibleBiases + visibleBiasInc
  setHiddenBiases(rbm) <- hiddenBiases + hiddenBiasInc
  setWeightInc(rbm) <- weightInc
  setVisibleBiasesInc(rbm) <- visibleBiasInc
  setHiddenBiasesInc(rbm) <- hiddenBiasInc
  rbm@learnRate <- rbm@learnRate * rbm@learnRateScale
  
  return(rbm)
}