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
#' @rdname pseudoinverse
#' @include darch.R
#' @export
pseudoInverse <- function(darch,trainData,targetData,epoch,...){
  if(is.element("corpcor", installed.packages()[,1])){
    library(corpcor)
  }else{
    log4r::ERROR(getLogger(darch),"This function needs the package corpcor.")
    return(darch)
  }
	darch <- getExecuteFunction(darch)(darch,trainData)
	trainData <- getExecOutput(darch,(length(getExecOutputs(darch))-1))[]
	
	trainData <- cbind(trainData,rep(1,dim(trainData)[1]))
	targetData <- cbind(targetData,rep(1,dim(targetData)[1]))
	
	weights <- corpcor::pseudoinverse(t(trainData) %*% trainData) %*% t(trainData) %*% targetData
	#weights <- solve(t(trainData) %*% trainData) %*% t(trainData) %*% targetData
		
	numLayers <- length(getLayers(darch))
	setLayerWeights(darch,numLayers) <- weights[,1:(ncol(weights)-1)]
  
	return(darch)
}

