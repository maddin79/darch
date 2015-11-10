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
  matMult <- get("matMult", darch.env)
  layers <- getLayers(darch)
  numLayers <- length(layers)
  delta <- list()
  outputs <- list()
  derivatives <- list()
  stats <- getStats(darch)
  
  # apply input dropout mask to data
  # TODO same input dropout mask for all data in a batch?
  trainData <- applyDropoutMask(trainData, getDropoutMask(darch, 0))
  
  # 1. Forwardpropagate
  data <- trainData
  numRows <- dim(data)[1]
  for (i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    func <- getLayerFunction(darch, i)
    weights <- getLayerWeights(darch, i)
    
    # apply dropout masks to weights, unless we're on the last layer; this is
    # done to allow activation functions to avoid considering values that are
    # later going to be dropped
    if (i < numLayers)
    {
      weights <- applyDropoutMask(weights, getDropoutMask(darch, i))
    }
    
    ret <- func(data,weights)
    
    # apply dropout masks to output, unless we're on the last layer
    if (i < numLayers)
    {
      ret[[1]] <- applyDropoutMask(ret[[1]], getDropoutMask(darch, i))
      ret[[2]] <- applyDropoutMask(ret[[2]], getDropoutMask(darch, i))
    }
    
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
  }
  rm(data,numRows)

  # 2. Calculate the Error on the network output
  # TODO if we use dropout in the output layer, multiply dropout mask in here
  error <- (targetData - outputs[[numLayers]][])
  delta[[numLayers]] <- error * derivatives[[numLayers]]

  E <- getErrorFunction(darch)(targetData,outputs[[numLayers]][])
  #flog.debug(paste("Error",E[[1]],E[[2]]))

  # 4. Backpropagate the error
  for(i in (numLayers-1):1){
	  weights <- layers[[i+1]][[1]][]
    # remove bias row
	  weights <- weights[1:(nrow(weights)-1),,drop=F]
    
	  error <-  matMult(delta[[i+1]], t(weights))
	  delta[[i]] <- error * derivatives[[i]]
  }

  # 5.  Update the weights
  learnRateBiases <- getLearnRateBiases(darch)
  learnRateWeights <- getLearnRateWeights(darch)
  for(i in numLayers:1){
    weights <- layers[[i]][[1]][]
    biases <- weights[nrow(weights),,drop=F]
    weights <- weights[1:(nrow(weights)-1),,drop=F]

    # Check if the weightInc field in the layer list exists.
    if (length(layers[[i]]) < 3){
      layers[[i]][[3]] <- matrix(0,nrow(weights),ncol(weights))
    }

    if (i > 1){
      output <- outputs[[i-1]]
    }else{
      output <- trainData
    }

    weightsInc <- t(learnRateWeights * matMult(t(delta[[i]]), output))
    
    # apply dropout mask to momentum
    weightsChange <- weightsInc + (getMomentum(darch) * layers[[i]][[3]][]
      * getDropoutMask(darch, i-1))

    weights <- weights + weightsChange
    biasesInc <- learnRateBiases * (rowSums(t(delta[[i]])))
    biases <- biases + biasesInc
    setLayerWeights(darch,i) <- rbind(weights,biases)
    setLayerField(darch,i,3) <- weightsInc
  }

  setStats(darch) <- stats
  return(darch)
}
