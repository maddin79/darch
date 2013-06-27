#' BEARBEITEN
#' 
#' BEARBEITEN 
#' 
#' @param rbm  BEARBEITEN 
#' @param dataList BEARBEITEN 
#' @param biases BEARBEITEN 
#' @param weights BEARBEITEN 
#' @param runParams BEARBEITEN 
#' @param ... BEARBEITEN 
#' @return The activations for the units
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname sigmUnitFuncSwitch
#' @include rbm.R
#' @export
sigmUnitFuncSwitch <- function(rbm, dataList, biases, weights, runParams){
  ret = list()
  
  if(runParams["actualCD"] == 1 | runParams["finishCD"] == 1){
    data <- dataList[[1]]
  }else{
    data <- dataList[[2]]
  }
  batchSize <- nrow(data)
  numUnits <- dim(biases)[2]
  
  randomNums <- matrix(runif(batchSize*numUnits),batchSize,numUnits) #matrix(0.2,batchSize,numUnits)
  ret[[1]] <- (1./(1 + exp(-data %*% weights - kronecker(matrix(1,batchSize,1),biases))))	
  ret[[2]] <- ret[[1]] > randomNums
  return(ret)
}

#' BEARBEITEN
#' 
#' BEARBEITEN 
#' 
#' @param rbm  BEARBEITEN 
#' @param dataList BEARBEITEN 
#' @param biases BEARBEITEN 
#' @param weights BEARBEITEN 
#' @param runParams BEARBEITEN 
#' @param ... BEARBEITEN 
#' @return The activations for the units
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname sigmUnitFunc
#' @include rbm.R
#' @export
sigmUnitFunc <- function(rbm, dataList, biases, weights, runParams){
  ret = list()
    
  data <- dataList[[2]]
  numUnits <- dim(biases)[2]
  batchSize <- nrow(data)
  randomNums <- matrix(runif(batchSize*numUnits),batchSize,numUnits)  
  ret[[1]] <- (1./(1 + exp(-data %*% weights - kronecker(matrix(1,batchSize,1),biases))))	
  ret[[2]] <- ret[[1]] > randomNums
  return(ret)
}

#' BEARBEITEN
#' 
#' BEARBEITEN 
#' 
#' @param rbm  BEARBEITEN 
#' @param dataList BEARBEITEN 
#' @param biases BEARBEITEN 
#' @param weights BEARBEITEN 
#' @param runParams BEARBEITEN 
#' @param ... BEARBEITEN 
#' @return The activations for the units
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname linearUnitFunc
#' @include rbm.R
#' @export
linearUnitFunc <- function(rbm, dataList, biases, weights, runParams){
  ret = list()
  
  data <- dataList[[1]]
  numUnits <- dim(biases)[2]
  batchSize <- nrow(data)
  randomNums <- matrix(rnorm(batchSize*numUnits),batchSize,numUnits) # matrix(0.2,batchSize,numUnits)
  ret[[1]] <- (data %*% weights) + kronecker(matrix(1,batchSize,1),biases)
  ret[[2]] <- ret[[1]] + randomNums
  
  return(ret)
}