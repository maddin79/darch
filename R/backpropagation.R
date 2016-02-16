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

#' @include darch.R
NULL

#' Backpropagation learning function
#' 
#' This function provides the backpropagation algorithm for deep architectures.
#' 
#' The function is getting the learning parameters from the provided
#' \code{\linkS4class{DArch}} object. It uses the attributes \code{momentum},
#' \code{finalMomentum} and \code{momentumSwitch} for the calculation of the new
#' weights with momentum. The attributes \code{learnRateWeights} and
#' \code{learnRateBiases} will be used for updating the weights. To use the
#' backpropagation function as the fine tuning function the layer functions of
#' the darch \code{\linkS4class{DArch}} object must set to the versions which
#' calculates also the derivatives of the function result.
#' 
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @param trainData The data for training.
#' @param targetData The targets for the data.
#' @param ... Further parameters.
#' @return The trained deep architecture
#' 
#' @seealso \code{\linkS4class{DArch}}, \code{\link{rpropagation}},
#'   \code{\link{minimizeAutoencoder}} \code{\link{minimizeClassifier}}
#'   \code{\link{minimizeClassifier}}
#' 
#' @references Rumelhart, D., G. E. Hinton, R. J. Williams, Learning
#'   representations by backpropagating errors, Nature 323, S. 533-536, DOI:
#'   10.1038/323533a0, 1986.
#' 
#' @family fine-tuning functions
#' @export
backpropagation <- function(darch, trainData, targetData,
  nesterovMomentum = getDarchParam("darch.nesterovMomentum", T, darch),
  matMult = getDarchParam("matMult", `%*%`, darch),
  debugMode = getDarchParam("debug", F, darch), ...)
{
  layers <- darch@layers
  numLayers <- length(layers)
  delta <- list()
  outputs <- list()
  derivatives <- list()
  
  dropout <- darch@dropout
  dropoutInput <- dropout[1]
  dropoutEnabled <- any(dropout > 0)
  
  if (!getDarchParam(".init.bp", F, darch))
  {
    darch@params[[".init.bp"]] <- T
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
  weights <- vector(mode = "list", length=numLayers)
  numRowsWeights <- vector(mode = "numeric", length=numLayers)
  for (i in 1:numLayers)
  {
    numRowsWeights[i] <- nrow(layers[[i]][["weights"]])
    
    # Initialize backprop layer variables
    numColsWeights <- ncol(layers[[i]][["weights"]])
    if (is.null(layers[[i]][["bp.init"]]))
    {
      layers[[i]][["bp.init"]] <- T
      layers[[i]][["bp.weightsInc"]] <-
        matrix(0, numRowsWeights[i]-1, numColsWeights)
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
    if ((i < numLayers || darch@dropConnect) && dropout[i + 1] > 0)
    {
      dropoutMask <- getDropoutMask(darch, i)
      
      if (darch@dropConnect)
      {
        weights[[i]] <- applyDropoutMaskCpp(weights[[i]], dropoutMask)
        
        ret <- func(matMult(data, weights[[i]]), darch = darch,
                    dropoutMask = dropoutMask)
      }
      else
      {
        ret <- func(matMult(data, weights[[i]]), darch = darch,
                    dropoutMask = dropoutMask)
        
        ret[[1]] <- applyDropoutMaskCpp(ret[[1]], dropoutMask)
        ret[[2]] <- applyDropoutMaskCpp(ret[[2]], dropoutMask)
      }
    }
    else
    {
      ret <- func(matMult(data, weights[[i]]), darch=darch)
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
  for(i in (numLayers-1):1){
    # remove bias row
    weights[[i+1]] <- weights[[i+1]][1:(numRowsWeights[i+1] - 1),, drop=F]
    
    error <-  matMult(delta[[i+1]], t(weights[[i+1]]))
    delta[[i]] <- error * derivatives[[i]]
  }
  
  learnRate <- darch@learnRate * (1 - momentum)

  # 5.  Update the weights
  for(i in numLayers:1)
  {
    output <- if (i > 1) outputs[[i-1]] else trainData
    
    weightsInc <-
      (t(learnRate * matMult(t(delta[[i]]), output)) / nrow(delta[[i]])
      + momentum * layers[[i]][["bp.weightsInc"]])
    biasesInc <-
      (learnRate * (rowSums(t(delta[[i]]))) / nrow(delta[[i]])
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
  lf("[backprop] See ?backpropagation for documentation")
}