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

#' Function for updating the weights and biases of an \code{RBM}
#' 
#' This function updates the weights and biases for an \code{\linkS4class{RBM}} 
#' network. It is saved in the attribute \code{updateFunction} of the 
#' \code{RBM} object and called from the training function 
#' \code{\link{trainRBM}}.
#' 
#' @param rbm A instance of the class \code{\linkS4class{RBM}}.
#' @param matMult Matrix multiplication function.
#' @param ... Additional arguments.
#' @return The updated \code{\linkS4class{RBM}}.
#' @export
#' @seealso \code{\linkS4class{RBM}}
#' @keywords internal
rbmUpdate <- function(rbm, matMult = getParameter(".matMult", net = rbm))
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
  learnRate <- (getParameter(".rbm.learnRate", net = rbm)
    * getParameter(".rbm.learnRateScale", net = rbm) ^ rbm@epochs
    * (1 - momentum))
  weightDecay <- getParameter(".rbm.weightDecay", net = rbm)
  batchSize <- getParameter(".rbm.batchSize", net = rbm)
  
  # positive phase
  posProducts <- matMult(t(data), posHiddenProbs)
  posHiddienAct <- colSums(posHiddenProbs)
  posVisibleAct <- colSums(data)
  
  # negative phase
  negProducts  <- matMult(t(negativeData), negHiddenProbs)
  negHiddienAct <- colSums(negHiddenProbs)
  negVisibleAct <- colSums(negativeData)
  
  # update
  weightsInc <- momentum * weightsInc + learnRate *
    (((posProducts - negProducts) / batchSize) - weightDecay * weights)
  visibleBiasInc <- momentum * visibleBiasInc + (learnRate / batchSize) *
    (posVisibleAct - negVisibleAct - visibleBiases * weightDecay)
  hiddenBiasInc <- momentum * hiddenBiasInc + (learnRate / batchSize) *
    (posHiddienAct - negHiddienAct - hiddenBiases * weightDecay)
  
  weights <- weights + weightsInc
  
  if (getParameter(".normalizeWeights", net = rbm))
  {
    normalizeWeightsCpp(weights, getParameter(".normalizeWeightsBound",
      net = rbm))
  }
  
  rbm@weights <- weights
  rbm@visibleBiases <- visibleBiases + visibleBiasInc
  rbm@hiddenBiases <- hiddenBiases + hiddenBiasInc
  rbm@weightsInc <- weightsInc
  rbm@visibleBiasesInc <- visibleBiasInc
  rbm@hiddenBiasesInc <- hiddenBiasInc
  
  rbm
}