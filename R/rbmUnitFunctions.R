#' Calculates the neuron output with the sigmoid function
#' 
#' Calculates the neuron output with the sigmoid function either from the real
#' value or binary input saved in the list \code{dataList}. 
#' 
#' @details The return value is a list with the output of the sigmoid function 
#' as first entry and binary representation calculated through a comparison of 
#' the output with random numbers. The random numbers a generated with the 
#' function \code{\link{runif}}.
#' If the parameter \code{runParams["actualCD"]} or \code{runParams["finishCD"]} 
#' is equal one, the calculation is made with the real value data 
#' (\code{dataList[[1]]}), otherwise with the binary representations 
#' (\code{dataList[[2]]}). 
#' 
#' @param rbm A instance of the class \code{\link{RBM}}. 
#' @param dataList A list with the data matrices for the calculations. 
#' @param biases The biases for the calculations 
#' @param weights The weight matrix for the calculations 
#' @param runParams Parameters which indicates the status of the training. 
#' \code{"actualCD"} and \code{"finishCD"} are needed 
#' (see \code{\link{trainRBM}})
#' 
#' @return The real value and binary activations for the units
#' @usage sigmUnitFuncSwitch(rbm, dataList, biases, weights, runParams)
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
  
  randomNums <- matrix(runif(batchSize*numUnits),batchSize,numUnits)
  ret[[1]] <- (1./(1 + exp(-data %*% weights - kronecker(matrix(1,batchSize,1),biases))))	
  ret[[2]] <- ret[[1]] > randomNums
  return(ret)
}

#' Calculates the neuron output with the sigmoid function
#' 
#' Calculates the neuron output with the sigmoid function from binary input 
#' saved in the second entry of the list \code{dataList}. 
#' 
#' @details The return value is a list with the output of the sigmoid function 
#' as first entry and binary representation calculated through a comparison of 
#' the output with random numbers. The random numbers a generated with the 
#' function \code{\link{runif}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}. 
#' @param dataList A list with the data matrices for the calculations. 
#' @param biases The biases for the calculations 
#' @param weights The weight matrix for the calculations 
#' @param runParams Parameters which indicates the status of the training. 
#' 
#' @return The real value and binary activations for the units
#' @usage sigmUnitFunc(rbm, dataList, biases, weights, runParams)
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

#' Calculates the linear neuron output no transfer function
#' 
#' Calculates the linear neuron output with no transfer function from real value
#' input saved in the first entry of the list \code{dataList}. 
#' 
#' @details The return value is a list with the output of the neurons as first 
#' entry and binary representation calculated through a comparison of the output 
#' with random numbers. The random numbers a generated with the 
#' function \code{\link{rnorm}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}. 
#' @param dataList A list with the data matrices for the calculations. 
#' @param biases The biases for the calculations 
#' @param weights The weight matrix for the calculations 
#' @param runParams Parameters which indicates the status of the training. 
#' 
#' @return The real value and binary activations for the units
#' @usage linearUnitFunc(rbm, dataList, biases, weights, runParams)
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
  randomNums <- matrix(rnorm(batchSize*numUnits),batchSize,numUnits)
  ret[[1]] <- (data %*% weights) + kronecker(matrix(1,batchSize,1),biases)
  ret[[2]] <- ret[[1]] + randomNums
  
  return(ret)
}