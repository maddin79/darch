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
#' Conjugate gradient for a autoencoder network
#' 
#' This function trains a \code{\link{DArch}} autoencoder network with the 
#' conjugate gradient method. 
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
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param trainData The training data matrix
#' @param targetData The labels for the training data
#' @param epoch The actual epoch of the training
#' @param length Numbers of line search 
#' 
#' @return The trained \code{\link{DArch}} object.
#' @usage minimizeAutoencoder(darch,trainData,targetData,epoch,length)
#' @seealso \code{\link{DArch}}
#' \code{\link{fineTuneDArch}}
#' 
#' @docType methods
#' @rdname minimizeAutoencoder
#' @include darch.R
#' @export
minimizeAutoencoder <- function(darch,trainData,targetData,epoch,length){
    
  # Function for gradients ###############################
  fr <- function(par,darch,dims,data){
    
    startPos <- 1
    endPos <- 0
    numRows <- dim(data)[1]
    length <- length(dims)
    outputs <- list()
    gradients <- list()
    derivatives <- list()
    weights <- list()
    
    d <- data	
    # Calculating the outputs
    for(i in 1:length){
      d <- cbind(d,rep(1,numRows))
      endPos <- endPos + dims[[i]][1]*dims[[i]][2]
      weights[[i]] <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
      startPos <- endPos+1
      layer <- getLayer(darch,i)
      ret <- layer[[2]](d,weights[[i]]) # noch eine funktion getLayerFunction() einfügen
      outputs[[i]] <- ret[[1]]
      d <- ret[[1]]
      derivatives[[i]] <- ret[[2]]
    }
    
    output <- outputs[[length]]
    x = data*log(output) + (1-data)*log(1-output)
    f = -1/nrow(data)*sum(x)
    
    ix <- 1/nrow(data)*(output-data)
    out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
    gradients[[length]] <- gpuMatMult(t(out), ix)
    
    for(i in (length-1):1){
      derivatives[[i]] <- cbind(derivatives[[i]],rep(1,nrow(derivatives[[i]])))
      ix <- (gpuMatMult(ix, t(weights[[i+1]])))* derivatives[[i]] # outputs[[i]]*(1-outputs[[i]])
      ix <- ix[,1:(dim(ix)[2]-1)]
      if(i > 1){
        out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
        gradients[[i]] <- gpuMatMult(t(out), ix)
      }else{
        d <- cbind(data,rep(1,numRows))
        gradients[[i]] <- gpuMatMult(t(d), ix)
      }
    }
    
    ret <- c(f)
    for(i in 1:length(gradients)){
      ret <- c(ret,c(gradients[[i]]))
    }
    return(ret)
  }
  # End function for gradients ###############################
  
  if(is.null(dim(trainData))){
    trainData <- t(as.matrix(trainData))
  }
  
  numLayers <- length(getLayers(darch))
  par <- c()
  dims <- list()
  for(i in 1:numLayers){
    weights <- getLayerWeights(darch,i)
    dims[[i]] <- dim(weights)
    par <- c(par,c(weights))		
  }
  
  # optimize
  flog.debug("Starting the minimize() function.")
  ret <- minimize(par,fr,length,darch,dims,trainData)
  
  par <- ret[[1]]
  # Add the optimized weights to the darch layers
  startPos <- 1
  endPos <- 0
  for(i in 1:length(dims)){
    endPos <- endPos + dims[[i]][1]*dims[[i]][2]
    setLayerWeights(darch,i) <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
    startPos <- endPos+1
  }
  
  return(darch)
}