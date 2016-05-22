# Copyright (C) 2015-2016 Johannes Rueckert
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

# TODO reduce code duplication

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
#' @param ... Additional parameters, not used.
#' @param weightDecay Weights are multiplied by (1 - \code{weightDecay}) before
#'  each update. Corresponds to the \code{darch.weightDecay} parameter of
#'  \link{darch.default}.
#' @param debug Internal debugging flag.
#' @return updated weights
#' @family weight update functions
#' @examples
#' \dontrun{
#' model <- darch(Species ~ ., iris, c(0, 50, 0),
#'  darch.weightUpdateFunction = "weightDecayWeightUpdate")
#' }
#' @export
weightDecayWeightUpdate <- function(darch, layerIndex, weightsInc, biasesInc,
  ..., weightDecay = getParameter(".darch.weightDecay", 0, darch),
  debug = getParameter(".debug", F, darch))
{
  if (getParameter(".darch.trainLayers")[layerIndex] == F)
  {
    return(darch@layers[[layerIndex]][["weights"]])
  }
  
  dropout <- getParameter(".darch.dropout")
  dropConnect <- getParameter(".darch.dropout.dropConnect")
  
  # TODO add dropout, dropConnect, normalizeWeights to parameter list
  weights <- darch@layers[[layerIndex]][["weights"]]
  
  inc <- rbind(weightsInc, biasesInc)
  
  if (dropConnect && dropout[layerIndex + 1] > 0)
  {
    inc <- applyDropoutMaskCpp(inc, getDropoutMask(darch, layerIndex))
  }
  
  weights <- (weights * (1 - weightDecay) + inc)
  
  if (getParameter(".normalizeWeights"))
  {
    normalizeWeightsCpp(weights, getParameter(".normalizeWeightsBound"))
  }
  
  weights
}

#' Updates the weight on maxout layers
#' 
#' On maxout layers, only the weights of active units are altered, additionally
#' all weights within a pool must be the same.
#' 
#' @inheritParams weightDecayWeightUpdate
#' @param poolSize Size of maxout pools, see parameter
#'   \code{darch.maxout.poolSize} of \code{\link{darch}}.
#' @return The updated weights.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, c(0, 50, 0),
#'  darch.unitFunction = c("maxoutUnit", "softmaxUnit"),
#'  darch.maxout.poolSize = 5, darch.maxout.unitFunction = "sigmoidUnit",
#'  darch.weightUpdateFunction = c("weightDecayWeightUpdate", "maxoutWeightUpdate"))
#' }
#' @references
#'  Goodfellow, Ian J., David Warde-Farley, Mehdi Mirza, Aaron C. Courville,
#'  and Yoshua Bengio (2013). "Maxout Networks". In: Proceedings of the 30th
#'  International Conference on Machine Learning, ICML 2013, Atlanta, GA, USA,
#'  16-21 June 2013, pp. 1319-1327.
#'  URL: http://jmlr.org/proceedings/papers/v28/goodfellow13.html
#' @family weight update functions
#' @export
maxoutWeightUpdate <- function(darch, layerIndex, weightsInc, biasesInc, ...,
  weightDecay = getParameter(".darch.weightDecay", 0, darch),
  poolSize = getParameter(".darch.maxout.poolSize", 2, darch))
{
  if (getParameter(".darch.trainLayers")[layerIndex] == F)
  {
    return(darch@layers[[layerIndex]][["weights"]])
  }
  
  dropout <- getParameter(".darch.dropout")
  dropConnect <- getParameter(".darch.dropout.dropConnect")
  weights <- darch@layers[[layerIndex]][["weights"]]
  
  inc <- rbind(weightsInc, biasesInc)
  
  if (dropConnect && dropout[layerIndex + 1] > 0)
  {
    inc <- applyDropoutMaskCpp(inc, getDropoutMask(darch, layerIndex))
  }
  
  nrows <- nrow(weights) - 1
  
  # if this is the first pass
  if (is.null(darch@layers[[layerIndex]][["maxout.init"]]))
  {
    darch@layers[[layerIndex]][["maxout.init"]] <- T
    # Walk through the pools, set all weights within each pool to be the same
    for (i in 1:(nrows / poolSize))
    {
      poolStart <- poolSize * (i - 1) + 1
      poolEnd <- poolStart + (poolSize - 1)
      randomRow <- sample(poolStart:poolEnd, 1)
      
      weights[poolStart:poolEnd,] <- matrix(rep(weights[randomRow,], poolSize),
        nrow = poolSize, byrow = T)
    }
  }
  
  # Walk through the pools to change the weight increment
  maxoutWeightUpdateCpp(inc, poolSize)
  
  weights <- (weights * (1 - weightDecay) + inc)
  
  if (getParameter(".normalizeWeights"))
  {
    normalizeWeightsCpp(weights, getParameter(".normalizeWeightsBound"))
  }
  
  weights
}