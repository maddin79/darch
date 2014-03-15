#' Backpropagation learning function
#' 
#' This function provides the backpropagtion algorithm for deep architectures. 
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
  
  # If the batch size is 1, the data must be converted to a matrix
  if(is.null(dim(trainData))){
    trainData <- t(as.matrix(trainData))
  }
  
  # 1. Forwardpropagate
  data <- trainData
  numRows <- dim(data)[1]
  for(i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    func <- layers[[i]][[2]]
    ret <- func(data,layers[[i]][[1]][])
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
  error <- (targetData - outputs[[numLayers]][])
  delta[[numLayers]] <- error * derivatives[[numLayers]]
  
  E <- getErrorFunction(darch)(targetData,outputs[[numLayers]][])
  flog.debug(paste("Error",E[[1]],E[[2]]))
  
  # Use only entrys bigger than index 3 in the stats-list
  if(length(stats) < 5){
    stats[[5]] <- c(E[[2]])
#     stats[[5]] <- list()
  }else{
    stats[[5]] <- c(stats[[5]],E[[2]])
  }
  
  # 4. Backpropagate the error
  for(i in (numLayers-1):1){
	  weights <- layers[[i+1]][[1]][]
	  weights <- weights[1:(nrow(weights)-1),]
	  
	  error <-  delta[[i+1]] %*% t(weights)
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
    
    # Set momentum
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
    
    weightsInc <- t(learnRateWeights * t(delta[[i]]) %*% output) 
    
#     absDW <- c(abs(weightsInc))
#     absDWstd <- stdabw(absDW)
#     stats[[5]][[i]][[epoch]] <- c(mean(absDW),absDWstd)
    
    weights <- weights + weightsInc + (getMomentum(darch) * layers[[i]][[3]][])
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
