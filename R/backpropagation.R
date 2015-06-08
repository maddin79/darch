# Copyright (C) 2015 darch2
# based on code by Martin Drees, copyright (C) 2013 Darch
#
# This file is part of darch2.
#
# Darch2 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch2.  If not, see <http://www.gnu.org/licenses/>.
#
#
#' Backpropagation learning function
#'
#' This function provides the backpropagation algorithm for deep architectures.
#'
#' @details The function is getting the learning parameters from the provided
#' \code{\link{DArch}} object. It uses the attributes \code{momentum},
#' \code{finalMomentum} and \code{momentumSwitch} for the calculation of the
#' new weights with momentum. The parameter \code{epoch} is provided for the
#' change from \code{momentum} to \code{finalMomentum} and is compared to
#' \code{momentumSwitch}.
#' The attributes \code{learnRateWeights} and \code{learnRateBiases} will be
#' used for updating the weights. To use the backpropagation function as the
#' fine tuning function the layer functions of the darch \code{\link{DArch}}
#' object must set to the versions which calculates also the derivatives of
#' the function result.
#'
#' @param darch An instance of the class \code{\link{DArch}}.
#' @param trainData The data for training.
#' @param targetData The targets for the data
#' @param epoch Number of epochs for the training
#' @return The trained deep architecture
#'
#' @usage backpropagation(darch,trainData,targetData,epoch)
#'
#' @seealso \code{\link{DArch}}
#'          \code{\link{rpropagation}}
#'          \code{\link{minimizeAutoencoder}}
#'          \code{\link{minimizeClassifier}}
#'          \code{\link{minimizeClassifier}}
#'
#' @references
#' Rumelhart, D., G. E. Hinton, R. J. Williams, Learning representations by
#' backpropagating errors, Nature 323, S. 533-536, DOI: 10.1038/323533a0, 1986.
#'
#' @docType methods
#' @rdname backpropagation
#' @include darch.R
#' @export
backpropagation <- function(darch,trainData,targetData,epoch){

#   # Standardabweichung
#   stdabw <- function(x) {n=length(x) ; sqrt(var(x) * (n-1) / n)}

  layers <- getLayers(darch)
  numLayers <- length(layers)
  delta <- list()
  outputs <- list()
  derivatives <- list()
  stats <- getStats(darch)
  
  dropoutMasks <- list()
  
  # generate dropout masks
  dropoutMasks[[1]] <- generateDropoutMask(nrow(getLayerWeights(darch, 1)[])-1, darch@dropoutInput)
  for (i in 2:numLayers)
  {
    dropoutMasks[[i]] <- generateDropoutMask(nrow(getLayerWeights(darch, i)[])-1, darch@dropoutHidden)
  }
  
  # If the batch size is 1, the data must be converted to a matrix
  if(is.null(dim(trainData))){
    trainData <- t(as.matrix(trainData))
  }
  
  # apply input dropout mask to data
  # TODO same input dropout mask for all data in a batch?
  trainData <- applyDropoutMask(trainData, dropoutMasks[[1]], byrow=T)
  
  # 1. Forwardpropagate
  data <- trainData
  numRows <- dim(data)[1]
  for(i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    func <- layers[[i]][[2]]
    weights <-layers[[i]][[1]][]
    
    # apply dropout masks to weights, unless we're on the last layer
    if (i < numLayers)
    {
      weights <- applyDropoutMask(weights, dropoutMasks[[i+1]], byrow=T)
    }
    
    ret <- func(data,weights)
    
    # apply dropout masks to output, unless we're on the last layer, in case
    # the activation function does not return 0 for an input of 0
    if (i < numLayers)
    {
      ret[[1]] <- applyDropoutMask(ret[[1]], dropoutMasks[[i+1]], byrow=T)
      ret[[2]] <- applyDropoutMask(ret[[2]], dropoutMasks[[i+1]], byrow=T)
    }
    
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
  }
  rm(data,numRows)

  # Old Forwardpropagate
  #darch <- getExecuteFunction(darch)(darch,trainData)
  #outputs <- getExecOutputs(darch)

  # 2. Calculate the Error on the network output
  # output <- cbind(outputs[[numLayers-1]][],rep(1,dim(outputs[[numLayers-1]])[1]))
  # derivative <- layers[[numLayers]][[3]](output,layers[[numLayers]][[1]][])
  # TODO if we use dropout in the output layer, multiply dropout mask in here
  error <- (targetData - outputs[[numLayers]][])
  delta[[numLayers]] <- error * derivatives[[numLayers]]

  E <- getErrorFunction(darch)(targetData,outputs[[numLayers]][])
  flog.debug(paste("Error",E[[1]],E[[2]]))

  # Use only entries bigger than index 3 in the stats-list
  if(length(stats) < 5){
    stats[[5]] <- c(E[[2]])
#     stats[[5]] <- list()
  }else{
    stats[[5]] <- c(stats[[5]],E[[2]])
  }

  # 4. Backpropagate the error
  for(i in (numLayers-1):1){
	  weights <- layers[[i+1]][[1]][]
    # remove bias row
	  weights <- weights[1:(nrow(weights)-1),,drop=F]
    
    # current weights are the weights between layers i+1 and i+2;
    # the weight matrix is nxm, where n = neurons in layer i+1 and m = neurons
    # in layer i+2
    weights <- applyDropoutMask(weights, dropoutMasks[[i+1]], byrow=F)
	  
    # only apply i+2 mask if we're not between the last two layers
    if (i+2 <= numLayers)
    {
      weights <- applyDropoutMask(weights, dropoutMasks[[i+2]], byrow=T)
    }
    
	  error <-  matMul(delta[[i+1]], t(weights))
	  delta[[i]] <- error * derivatives[[i]]
  }

  # 5.  Update the weights
  learnRateBiases <- getLearnRateBiases(darch)
  learnRateWeights <- getLearnRateWeights(darch)
  for(i in numLayers:1){
    weights <- layers[[i]][[1]][]
    biases <- t(as.matrix(weights[nrow(weights),]))
    weights <- as.matrix(weights[1:(nrow(weights)-1),])
    # Check if the weightInc field in the layer list exists.
    if(length(layers[[i]]) < 3){
      layers[[i]][[3]] <- matrix(0,nrow(weights),ncol(weights))
#       stats[[5]][[i]] <- list()
    }

    # Set momentum; TODO: unused!
    if(epoch > getMomentumSwitch(darch)){
      momentum <- getFinalMomentum(darch)
    }else{
      momentum <- getMomentum(darch)
    }

    if(i > 1){
      output <- outputs[[i-1]]
    }else{
      output <- trainData
    }

    weightsInc <- t(learnRateWeights * matMul(t(delta[[i]]), output))

#     absDW <- c(abs(weightsInc))
#     absDWstd <- stdabw(absDW)
#     stats[[5]][[i]][[epoch]] <- c(mean(absDW),absDWstd)
    
    # apply dropout mask to weight changes
    weightsChange <- weightsInc + (getMomentum(darch) * layers[[i]][[3]][])

    # dropout mask has to be applied for both involved layers, except on the
    # last layer (each layer is essentially the weight matrix between two
    # layers)
    if (i < numLayers)
    {
      weightsChange <- applyDropoutMask(weightsChange, dropoutMasks[[i+1]], T)
    }
    
    weightsChange <- applyDropoutMask(weightsChange, dropoutMasks[[i]], F)

    weights <- weights + weightsChange
    biasesInc <- learnRateBiases * (rowSums(t(delta[[i]])))
    biases <- biases + biasesInc
    setLayerWeights(darch,i) <- rbind(weights,biases)
    setLayerField(darch,i,3) <- weightsInc
    #     layers[[i]][[1]][] <- rbind(weights,biases)
    #     layers[[i]][[3]] <- weightsInc
  }

  #   setLayers(darch) <- layers

  setStats(darch) <- stats
  return(darch)
}
