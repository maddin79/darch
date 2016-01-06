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
backpropagation <- function(darch, trainData, targetData, ...)
{
  matMult <- getDarchParam("matMult", `%*%`, darch)
  layers <- getLayers(darch)
  numLayers <- length(layers)
  delta <- list()
  outputs <- list()
  derivatives <- list()
  stats <- getStats(darch)
  
  dropoutInput <- getDropoutInputLayer(darch)
  dropoutHidden <- getDropoutHiddenLayers(darch)
  
  # apply input dropout mask to data
  if (dropoutInput > 0)
  {
    trainData <- applyDropoutMask(trainData, getDropoutMask(darch, 0))
  }
  
  # 1. Forwardpropagate
  data <- trainData
  numRows <- nrow(data)
  weights <- list()
  for (i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    func <- getLayerFunction(darch, i)
    weights[[i]] <- getLayerWeights(darch, i)
    
    # apply dropout masks to weights and outputs
    if (dropoutHidden > 0 && (i < numLayers || darch@dropConnect))
    {
      # this is done to allow activation functions to avoid considering values
      # that are later going to be dropped
      weights[[i]] <- applyDropoutMask(weights[[i]], getDropoutMask(darch, i))
      
      ret <- func(matMult(data, weights[[i]]), darch=darch)
      
      if (!darch@dropConnect && i < numLayers)
      {
        ret[[1]] <- applyDropoutMask(ret[[1]], getDropoutMask(darch, i))
        ret[[2]] <- applyDropoutMask(ret[[2]], getDropoutMask(darch, i))
      }
    }
    else
    {
      ret <- func(matMult(data, weights[[i]]), darch=darch)
    }
    
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
  }
  #rm(data,numRows)

  # 2. Calculate the Error on the network output
  #E <- getErrorFunction(darch)(targetData, outputs[[numLayers]])
  #flog.debug(paste("Error",E[[1]],E[[2]]))
  
  error <- (targetData - outputs[[numLayers]])
  delta[[numLayers]] <- error * derivatives[[numLayers]]
  
  nrow <- nrow(weights[[1]])
  weights[[1]] <- weights[[1]][1:(nrow - 1),, drop=F]
  # 4. Backpropagate the error
  for(i in (numLayers-1):1){
    nrow <- nrow(weights[[i+1]])
    # remove bias row
    weights[[i+1]] <- weights[[i+1]][1:(nrow - 1),, drop=F]
    
    error <-  matMult(delta[[i+1]], t(weights[[i+1]]))
    delta[[i]] <- error * derivatives[[i]]
  }
  
  momentum <- getMomentum(darch)
  learnRate <- darch@learnRate * (1 - momentum)

  # 5.  Update the weights
  for(i in numLayers:1)
  {
    # Check if the weightsInc and biasesInc fields in the layer list exist
    ncol <- ncol(weights[[i]])
    if (is.null(layers[[i]][["bp.init"]]))
    {
      setLayerField(darch, i, "bp.init") <- T
      layers[[i]][["bp.weightsInc"]] <- matrix(0,nrow(weights[[i]]),ncol)
      layers[[i]][["bp.biasesInc"]] <- matrix(0,1,ncol)
    }

    if (i > 1){
      output <- outputs[[i-1]]
    }else{
      output <- trainData
    }
    
    weightsInc <-
      (t(learnRate * matMult(t(delta[[i]]), output)) / nrow(delta[[i]])
      + momentum * layers[[i]][["bp.weightsInc"]][])
    biasesInc <-
      (learnRate * (rowSums(t(delta[[i]]))) / nrow(delta[[i]])
      + momentum * layers[[i]][["bp.biasesInc"]][])
    
    darch <- getWeightUpdateFunction(darch, i)(darch, i, weightsInc, biasesInc)
    setLayerField(darch, i, "bp.weightsInc") <- weightsInc
    setLayerField(darch, i, "bp.biasesInc") <- biasesInc
  }

  setStats(darch) <- stats
  return(darch)
}
