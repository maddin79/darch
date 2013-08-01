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
#' The parameter \code{switchLayers} is for the switch between two training 
#' type. Like in the original code, the top two layers can be trained alone 
#' until \code{epoch} is equal to \code{epochSwitch}. Afterwards the entire 
#' network will be trained.
#'
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param trainData The training data matrix
#' @param targetData The labels for the training data
#' @param epoch The actual epoch of the training
#' @param length Numbers of line search 
#' @param switchLayers Indicates when to train the full network instead of only 
#' the upper two layers
#' 
#' @return The trained \code{\link{DArch}} object.
#' @usage minimizeClassifier(darch,trainData,targetData,epoch,length,switchLayers)
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname minimizeClassifier
#' @include darch.R
#' @export
minimizeClassifier <- function(darch,trainData,targetData,epoch,length,switchLayers){
  
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
    
    if(epochSwitch){
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
      f = -sum(target*log(output))
      
      ix <- output-target
      out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
      gradients[[length]] <- t(out)%*%ix
      
      for(i in (length-1):1){
        derivatives[[i]] <- cbind(derivatives[[i]],rep(1,nrow(derivatives[[i]])))
        ix <- (ix%*%t(weights[[i+1]]))* derivatives[[i]] # outputs[[i]]*(1-outputs[[i]])
        ix <- ix[,1:(dim(ix)[2]-1)]
        if(i > 1){
          out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
          gradients[[i]] <- t(out)%*% ix
        }else{
          d <- cbind(data,rep(1,numRows))
          gradients[[i]] <- t(d)%*% ix
        }
      }
    }else{
      d <- cbind(data,rep(1,numRows))
      endPos <- endPos + dims[[1]][1]*dims[[1]][2]
      weights <- matrix(par[startPos:endPos],dims[[1]][1],dims[[1]][2])
      layer <- getLayer(darch,length(getLayers(darch)))
      ret <- layer[[2]](d,weights) # noch eine funktion getLayerFunction() einfügen
      output <- ret[[1]]
      
      f = -sum(target*log(output))
      
      ix <- output-target
      gradients[[1]] <- t(d)%*%ix
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
  
  epochSwitch <- TRUE
  if(epoch < switchLayers){
    epochSwitch <- FALSE
  }
  
  if(epochSwitch){
    
    for(i in 1:numLayers){
      weights <- getLayerWeights(darch,i)
      dims[[i]] <- dim(weights)
      par <- c(par,c(weights))		
    }
    
  }else{
    
    layers <- getLayers(darch)
    length <- length(layers)
    numRows <- dim(trainData)[1]
    
    for(i in 1:(length-1)){
      trainData <- cbind(trainData,rep(1,numRows))
      trainData <- 1/(1 + exp(-trainData %*% layers[[i]][[1]][]))
    }
    
    weights <- getLayerWeights(darch,length)
    dims[[1]] <- dim(weights)
    par <- c(par,c(weights))
    
  }
  
  # optimize
  log4r::debug(getLogger(darch),"Starting the minimize() function.")
  ret <- minimize(par,fr,length,darch,dims,trainData,targetData,epochSwitch)
  
  par <- ret[[1]]
  
  # Add the optimized weights to the darch layers
  startPos <- 1
  endPos <- 0
  
  if(epochSwitch){
    
    for(i in 1:length(dims)){
      endPos <- endPos + dims[[i]][1]*dims[[i]][2]
      setLayerWeights(darch,i) <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
      startPos <- endPos+1
    }			
    
  }else{
    
    endPos <- endPos + dims[[1]][1]*dims[[1]][2]
    setLayerWeights(darch,length(getLayers(darch))) <- matrix(par[startPos:endPos],dims[[1]][1],dims[[1]][2])
  }
  
  return(darch)
}