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

#' Updates the weight using weight decay.
#' 
#' Multiplies the weights by (1 - \code{weightDecay}) before applying the
#' scheduled weight changes.
#' 
#' @param darch \linkS4class{DArch} instance.
#' @param layerIndex Layer index within the network.
#' @param weightsInc Matrix containing scheduled weight updates from the
#'  fine-tuning algorithm.
#' @param biasesInc Bias weight updates.
#' @param weightDecay Weights are multiplied by (1 - \code{weightDecay}) before
#'  each update. Corresponds to the \code{darch.weightDecay} parameter of
#'  \link{darch.default}.
#' @return updated \linkS4class{DArch} instance
#' @export
weightDecayWeightUpdate <- function(darch, layerIndex, weightsInc, biasesInc)
{
  weights <- getLayerWeights(darch, layerIndex)
  
  inc <- rbind(weightsInc, biasesInc)
  
  if (darch@dropConnect && darch@dropoutHidden > 0)
  {
    inc <- applyDropoutMask(inc, getDropoutMask(darch, layerIndex))
  }
  
  weights <- (weights * (1 - darch@weightDecay) + inc)
  setLayerWeights(darch, layerIndex) <- weights
  
  darch
}

#' Updates the weight on maxout layers
#' 
#' On maxout layers, only the weights of active units are altered, additionally
#' all weights within a pool must be the same.
#' 
#' @param darch \linkS4class{DArch} instance.
#' @param layerIndex Layer index within the network.
#' @param weightsInc Matrix containing scheduled weight updates from the
#'  fine-tuning algorithm.
#' @param biasesInc Bias weight updates.
#' @param weightDecay Weights are multiplied by (1 - \code{weightDecay}) before
#'  each update. Corresponds to the \code{darch.weightDecay} parameter of
#'  \link{darch.default}.
#' @return updated \linkS4class{DArch} instance
#' @export
maxoutWeightUpdate <- function(darch, layerIndex, weightsInc, biasesInc,
  poolSize = getDarchParam("darch.layerFunction.maxout.poolSize", 2, darch))
{
  weights <- getLayerWeights(darch, layerIndex)
  
  inc <- rbind(weightsInc, biasesInc)
  
  if (darch@dropConnect && darch@dropoutHidden > 0)
  {
    inc <- applyDropoutMask(inc, getDropoutMask(darch, layerIndex))
  }
  
  ncols <- ncol(weights)
  nrows <- nrow(weights)-1
  
  # if this is the first pass and poolSize is greater than 1
  # TODO: is poolSize = 1 allowed?
  if (!all(weights[1,] == weights[poolSize,]))
  {
    # Walk through the pools, set all weights within each pool to be the same
    for (i in 1:(nrows / poolSize))
    {
      poolStart <- poolSize * (i - 1) + 1
      poolEnd <- poolStart + (poolSize - 1)
      randomRow <- sample(poolStart:poolEnd, 1)
      
      weights[poolStart:poolEnd,] <- matrix(rep(weights[randomRow,], poolSize),
                                            nrow=poolSize, byrow=T)
    }
  }
  
  # Walk through the pools to change the weight increment
  for (i in 1:(nrows / poolSize))
  {
    poolStart <- poolSize * (i - 1) + 1
    poolEnd <- poolStart + (poolSize - 1)
    
    inc[poolStart:poolEnd,] <-
      matrix(rep(colSums(inc[poolStart:poolEnd,, drop=F]), poolSize),
                 nrow=poolSize, byrow=T)
  }
  
  weights <- (weights * (1 - darch@weightDecay) + inc)
  setLayerWeights(darch, layerIndex) <- weights
  
  darch
}