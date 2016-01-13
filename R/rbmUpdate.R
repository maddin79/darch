# Copyright (C) 2013-2016 Martin Drees
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
  weightsInc <- rbm@weightsInc
  weights <- rbm@weights
  visibleBiasInc <- rbm@visibleBiasesInc
  visibleBiases <- rbm@visibleBiases
  hiddenBiasInc <- rbm@hiddenBiasesInc
  hiddenBiases <- rbm@hiddenBiases
  data <- rbm@posPhaseData[[1]]
  posHiddenProbs <-  rbm@posPhaseData[[2]][[1]]
  negativeData <- rbm@visibleUnitStates[[1]]
  negHiddenProbs <- rbm@hiddenUnitStates[[1]]
  learnRate <- rbm@learnRate * (1 - momentum)
  weightDecay <- rbm@weightDecay
  batchSize <- rbm@batchSize
  
  # positive phase
  posProducts <- matMult(t(data), posHiddenProbs)
  posHiddienAct <- colSums(posHiddenProbs)
  posVisibleAct <- colSums(data)
  
  # negative phase
  negProducts  <- matMult(t(negativeData), negHiddenProbs)
  negHiddienAct <- colSums(negHiddenProbs)
  negVisibleAct <- colSums(negativeData)
  
  # update
  weightsInc <- momentum*weightsInc + learnRate * ((posProducts - negProducts)/batchSize - weightDecay*weights)
  # TODO does the weight decay work?
  visibleBiasInc <- momentum*visibleBiasInc + (learnRate/batchSize) * (posVisibleAct-negVisibleAct - visibleBiases*weightDecay)
  hiddenBiasInc <- momentum*hiddenBiasInc + (learnRate/batchSize) * (posHiddienAct-negHiddienAct - hiddenBiases*weightDecay)
  
  weights <- weights + weightsInc
  
  if (rbm@normalizeWeights)
  {
    normalizeWeightsCpp(weights, rbm@normalizeWeightsBound)
  }
  
  rbm@weights <- weights
  rbm@visibleBiases <- visibleBiases + visibleBiasInc
  rbm@hiddenBiases <- hiddenBiases + hiddenBiasInc
  rbm@weightsInc <- weightsInc
  rbm@visibleBiasesInc <- visibleBiasInc
  rbm@hiddenBiasesInc <- hiddenBiasInc
  rbm@learnRate <- rbm@learnRate * rbm@learnRateScale
  
  rbm
}