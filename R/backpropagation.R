# Copyright (C) 2013-2016 Martin Drees
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

#' @include darch.Class.R
NULL

#' Backpropagation learning function
#'
#' This function provides the backpropagation algorithm for deep architectures.
#'
#' The only backpropagation-specific, user-relevant parameters are
#' \code{bp.learnRate} and \code{bp.learnRateScale}; they can be passed to the
#' \code{\link{darch}} function when enabling \code{backpropagation} as the
#' fine-tuning function. \code{bp.learnRate} defines the backpropagation
#' learning rate and can either be specified as a single scalar or as a vector
#' with one entry for each weight matrix, allowing for per-layer learning rates.
#' \code{bp.learnRateScale} is a single scalar which contains a scaling factor
#' for the learning rate(s) which will be applied after each epoch.
#'
#' Backpropagation supports dropout and uses the weight update function as
#' defined via the \code{darch.weightUpdateFunction} parameter of
#' \code{\link{darch}}.
#'
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @param trainData The training data (inputs).
#' @param targetData The target data (outputs).
#' @param bp.learnRate Learning rates for backpropagation, length is either one
#'   or the same as the number of weight matrices when using different learning
#'   rates for each layer.
#' @param bp.learnRateScale The learn rate is multiplied by this value after
#'   each epoch.
#' @param nesterovMomentum See \code{darch.nesterovMomentum} parameter of
#'   \code{\link{darch}}.
#' @param dropout See \code{darch.dropout} parameter of \code{\link{darch}}.
#' @param dropConnect See \code{darch.dropout.dropConnect} parameter of
#'   \code{\link{darch}}.
#' @param matMult Matrix multiplication function, internal parameter.
#' @param debugMode Whether debug mode is enabled, internal parameter.
#' @param ... Further parameters.
#' @return The trained deep architecture
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.fineTuneFunction = "backpropagation")
#' }
#' @seealso \code{\link{darch}}
#'   
#' @references Rumelhart, D., G. E. Hinton, R. J. Williams, Learning 
#'   representations by backpropagating errors, Nature 323, S. 533-536, DOI: 
#'   10.1038/323533a0, 1986.
#'   
#' @family fine-tuning functions
#' @export
backpropagation <- function(darch, trainData, targetData,
  bp.learnRate = getParameter(".bp.learnRate",
    rep(1, times = length(darch@layers))),
  bp.learnRateScale = getParameter(".bp.learnRateScale"),
  nesterovMomentum = getParameter(".darch.nesterovMomentum"),
  dropout = getParameter(".darch.dropout",
    rep(0, times = length(darch@layers) + 1), darch),
  dropConnect = getParameter(".darch.dropout.dropConnect"),
  matMult = getParameter(".matMult"),
  debugMode = getParameter(".debug", F), ...)
{
  layers <- darch@layers
  numLayers <- length(layers)
  delta <- list()
  outputs <- list()
  derivatives <- list()
  
  dropoutInput <- dropout[1]
  
  if (!getParameter(".bp.init", F, darch))
  {
    darch@parameters[[".bp.init"]] <- T
  }
  
  # apply input dropout mask to data
  if (dropoutInput > 0)
  {
    trainData <- applyDropoutMaskCpp(trainData, getDropoutMask(darch, 0))
  }
  
  momentum <- getMomentum(darch)
  
  # 1. Forwardpropagate
  data <- trainData
  numRows <- nrow(data)
  weights <- vector(mode = "list", length = numLayers)
  numRowsWeights <- vector(mode = "numeric", length = numLayers)
  
  for (i in 1:numLayers)
  {
    numRowsWeights[i] <- nrow(layers[[i]][["weights"]])
    
    # Initialize backprop layer variables
    numColsWeights <- ncol(layers[[i]][["weights"]])
    if (is.null(layers[[i]][["bp.init"]]))
    {
      layers[[i]][["bp.init"]] <- T
      layers[[i]][["bp.weightsInc"]] <-
        matrix(0, numRowsWeights[i] - 1, numColsWeights)
      layers[[i]][["bp.biasesInc"]] <- matrix(0, 1, numColsWeights)
    }
    
    data <- cbind(data,rep(1,numRows))
    func <- layers[[i]][["unitFunction"]]
    
    # Nesterov accelerated gradient
    if (nesterovMomentum)
    {
      nesterov <-
        rbind(layers[[i]][["bp.weightsInc"]], layers[[i]][["bp.biasesInc"]]) *
        momentum
      weights[[i]] <- layers[[i]][["weights"]] + nesterov
    }
    else
    {
      weights[[i]] <- layers[[i]][["weights"]]
    }
    
    # apply dropout masks to weights and / or outputs
    # TODO extract method
    if ((i < numLayers || dropConnect) && dropout[i + 1] > 0)
    {
      dropoutMask <- getDropoutMask(darch, i)
      
      if (dropConnect)
      {
        weights[[i]] <- applyDropoutMaskCpp(weights[[i]], dropoutMask)
        
        ret <- func(matMult(data, weights[[i]]), net = darch,
                    dropoutMask = dropoutMask)
      }
      else
      {
        ret <- func(matMult(data, weights[[i]]), net = darch,
                    dropoutMask = dropoutMask)
        
        ret[[1]] <- applyDropoutMaskCpp(ret[[1]], dropoutMask)
        ret[[2]] <- applyDropoutMaskCpp(ret[[2]], dropoutMask)
      }
    }
    else
    {
      ret <- func(matMult(data, weights[[i]]), net = darch)
    }
    
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
    
    if (debugMode)
    {
      futile.logger::flog.debug("Layer %s: Activation standard deviation: %s",
                                i, sd(data))
      futile.logger::flog.debug("Layer %s: Derivatives standard deviation: %s",
                                i, sd(derivatives[[i]]))
    }
  }
  #rm(data,numRows)
  
  error <- (targetData - outputs[[numLayers]])
  delta[[numLayers]] <- error * derivatives[[numLayers]]
  
  weights[[1]] <- weights[[1]][1:(numRowsWeights[1] - 1),, drop=F]
  # 4. Backpropagate the error
  # TODO i in numLayers:2?
  for (i in (numLayers - 1):1) {
    # remove bias row
    weights[[i + 1]] <-
      weights[[i + 1]][1:(numRowsWeights[i + 1] - 1),, drop = F]
    
    error <-  matMult(delta[[i + 1]], t(weights[[i + 1]]))
    delta[[i]] <- error * derivatives[[i]]
  }
  
  bp.learnRate <-
    bp.learnRate * bp.learnRateScale ^ darch@epochs * (1 - momentum)

  # 5.  Update the weights
  for (i in numLayers:1)
  {
    output <- if (i > 1) outputs[[i - 1]] else trainData
    
    weightsInc <-
      (t(bp.learnRate[i] * matMult(t(delta[[i]]), output)) / nrow(delta[[i]])
      + momentum * layers[[i]][["bp.weightsInc"]])
    biasesInc <-
      (bp.learnRate[i] * (rowSums(t(delta[[i]]))) / nrow(delta[[i]])
      + momentum * layers[[i]][["bp.biasesInc"]])
    
    if (debugMode)
    {
      futile.logger::flog.debug("Layer %s: Weight change ratio: %s",
        i, norm(rbind(weightsInc, biasesInc)) / norm(layers[[i]][["weights"]]))
      
    }
    
    layers[[i]][["weights"]] <-
      (layers[[i]][["weightUpdateFunction"]](darch, i, weightsInc,
      biasesInc))
    layers[[i]][["bp.weightsInc"]] <- weightsInc
    layers[[i]][["bp.biasesInc"]] <- biasesInc
  }

  darch@layers <- layers
  darch
}

printDarchParams.backpropagation <- function(darch,
  lf = futile.logger::flog.info)
{
  lf("[backprop] Using backpropagation for fine-tuning")
  
  printParams(c("bp.learnRate", "bp.learnRateScale"), "backprop")
  
  lf("[backprop] See ?backpropagation for documentation")
}