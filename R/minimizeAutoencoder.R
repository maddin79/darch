#' BEARBEITEN
#' 
#' BEARBEITEN 
#' 
#' @param darch BEARBEITEN
#' @param trainData BEARBEITEN
#' @param targetData BEARBEITEN
#' @param epoch BEARBEITEN
#' @param ... BEARBEITEN
#' @return BEARBEITEN
#' 
#' @seealso \code{\link{DArch}}
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
  log4r::debug(getLogger(darch),"Starting the minimize() function.")
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