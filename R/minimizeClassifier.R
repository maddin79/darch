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

#' Conjugate gradient for a classification network
#' 
#' This function trains a \code{\link{DArch}} classifier network with the conjugate
#' gradient method. 
#' 
#' @details
#' This function is build on the basis of the code from G. Hinton et. al.
#' (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html - last visit 
#' 06.06.2013) for the fine tuning of deep belief nets. The original code is 
#' located in the files 'backpropclassify.m', 'CG_MNIST.m' and 
#' 'CG_CLASSIFY_INIT.m'. 
#' It implements the fine tuning for a classification net with backpropagation
#' using a direct translation of the \code{\link{minimize}} function from C. 
#' Rassmussen (available at http://www.gatsby.ucl.ac.uk/~edward/code/minimize/ 
#' - last visit 06.06.2013) to R.
#' The parameter \code{cg.switchLayers} is for the switch between two training 
#' type. Like in the original code, the top two layers can be trained alone 
#' until \code{epoch} is equal to \code{epochSwitch}. Afterwards the entire 
#' network will be trained.
#'
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param trainData The training data matrix
#' @param targetData The labels for the training data
#' @param cg.length Numbers of line search 
#' @param cg.switchLayers Indicates when to train the full network instead of only 
#' the upper two layers
#' 
#' @return The trained \code{\link{DArch}} object.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname minimizeClassifier
#' @include darch.R
#' @export
minimizeClassifier <- function(darch, trainData, targetData, cg.length = 2,
  cg.switchLayers = 0, matMult = getDarchParam("matMult", `%*%`, darch), ...)
{
  # Function for gradients ###############################
  fr <- function(par,darch,dims,data,target,epochSwitch){
    startPos <- 1
    endPos <- 0
    numRows <- dim(data)[1]
    length <- length(dims)
    outputs <- list()
    gradients <- list()
    derivatives <- list()
    weights <- list()
    
    if (epochSwitch){
      d <- data	
      # Calculating the outputs
      for(i in 1:length){
        d <- cbind(d,rep(1,numRows))
        endPos <- endPos + dims[[i]][1]*dims[[i]][2]
        weights[[i]] <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
        startPos <- endPos+1
        ret <- getLayerFunction(darch, i)(matMult(d, weights[[i]]), darch=darch)
        
        if (darch@dropoutHidden > 0 && !darch@dropConnect && i < length)
        {
          outputs[[i]] <- applyDropoutMask(ret[[1]], getDropoutMask(darch, i))
          derivatives[[i]] <- applyDropoutMask(ret[[2]], getDropoutMask(darch, i))
        }
        else
        {
          outputs[[i]] <- ret[[1]]
          derivatives[[i]] <- ret[[2]]
        }
        
        d <- outputs[[i]]
      }
      
      output <- outputs[[length]]
      f = -sum(target*log(output))
      
      ix <- output-target
      out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
      gradients[[length]] <- matMult(t(out), ix)
      
      for(i in (length-1):1){
        derivatives[[i]] <- cbind(derivatives[[i]],rep(1,nrow(derivatives[[i]])))
        ix <- (matMult(ix, t(weights[[i+1]])))* derivatives[[i]] # outputs[[i]]*(1-outputs[[i]])
        ix <- ix[,1:(dim(ix)[2]-1)]
        if (i > 1){
          out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
          gradients[[i]] <- matMult(t(out), ix)
        }else{
          d <- cbind(data,rep(1,numRows))
          gradients[[i]] <- matMult(t(d), ix)
        }
      }
    }else{
      d <- cbind(data,rep(1,numRows))
      endPos <- endPos + dims[[1]][1]*dims[[1]][2]
      weights <- matrix(par[startPos:endPos],dims[[1]][1],dims[[1]][2])
      
      ret <- getLayerFunction(darch,
        length(getLayers(darch)))(matMult(d, weights), darch=darch)

      output <- ret[[1]]
      
      f = -sum(target*log(output))
      
      ix <- output-target
      gradients[[1]] <- matMult(t(d), ix)
    }
    
    ret <- c(f)
    for(i in 1:length(gradients)){
      ret <- c(ret,c(gradients[[i]]))
    }
    return(ret)
  }
  # End function for gradients ###############################
  
  if (getDropoutInputLayer(darch) > 0)
  {
    trainData <- applyDropoutMask(trainData, getDropoutMask(darch, 0))
  }
  
  dropoutHidden <- darch@dropoutHidden
  
  numLayers <- length(getLayers(darch))
  par <- c()
  dims <- list()
  
  epochSwitch <- (getEpochs(darch) >= cg.switchLayers)
  
  if (epochSwitch)
  {  
    for(i in 1:numLayers)
    {
      if (dropoutHidden > 0 && (i < numLayers || darch@dropConnect))
      {
        weights <-
          applyDropoutMask(getLayerWeights(darch,i), getDropoutMask(darch, i))
      }
      else
      {
        weights <- getLayerWeights(darch,i)
      }
      
      dims[[i]] <- dim(weights)
      par <- c(par,c(weights))		
    }
  }
  else
  {  
    layers <- getLayers(darch)
    length <- length(layers)
    numRows <- dim(trainData)[1]
    
    # Fordward-propagate until last layer
    for(i in 1:(length-1))
    {
      if (dropoutHidden > 0)
      {
        weights <- applyDropoutMask(getLayerWeights(darch, i),
                                    getDropoutMask(darch, i))
      }
      else
      {
        weights <- getLayerWeights(darch,i)
      }
      
      if (darch@dropoutHidden > 0 && !darch@dropConnect)
      {
        trainData <- applyDropoutMask(getLayerFunction(darch, i)(
          matMult(cbind(trainData,rep(1, numRows)), weights), darch=darch)[[1]],
          getDropoutMask(darch, i))
      }
      else
      {
        trainData <- getLayerFunction(darch, i)(
          matMult(cbind(trainData,rep(1, numRows)), weights), darch=darch)[[1]]
      }
    }
    
    if (dropoutHidden > 0 && darch@dropConnect)
    {
      weights <- applyDropoutMask(getLayerWeights(darch,length), getDropoutMask(darch, length))
    }
    else
    {
      weights <- getLayerWeights(darch, length)
    }
    
    dims[[1]] <- dim(weights)
    par <- c(par,c(weights))
  }
  
  # optimize
  #flog.debug("Starting the minimize() function.")
  ret <- minimize(par, fr, cg.length, darch, dims, trainData, targetData,
    epochSwitch, matMult=matMult)
  
  par <- ret[[1]]
  
  # Add the optimized weights to the darch layers
  startPos <- 1
  endPos <- 0
  startLayer <- if (epochSwitch) 1 else lengtg(dims)
  
  for(i in startLayer:length(dims))
  {
    endPos <- endPos + dims[[i]][1]*dims[[i]][2]
    
    weightsNew <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
    
    if (dropoutHidden > 0)
    {
      if (darch@dropConnect)
      {
        weightsNew <- applyDropoutMask(weightsNew, getDropoutMask(darch, i))
      }
      
      maskDropped <- which(weightsNew == 0)
      weightsNew[maskDropped] <- getLayerWeights(darch, i)[maskDropped]
    }
    
    setLayerWeights(darch,i) <- weightsNew
    startPos <- endPos+1
  }
  
  return(darch)
}