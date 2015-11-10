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

#' @include rbm.R
NULL

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
#' @return The real value and binary activations for the units
#' @family RBM unit functions
#' @export
sigmUnitFuncSwitch <- function(rbm, dataList, biases, weights, runParams){
  ret = list()
  
  if(runParams["currentCD"] == 1 | runParams["finishCD"] == 1){
    data <- dataList[[1]]
  }else{
    data <- dataList[[2]]
  }
  batchSize <- nrow(data)
  numUnits <- dim(biases)[2]
  
  randomNums <- matrix(runif(batchSize*numUnits),batchSize,numUnits)
  ret[[1]] <- (1./(1 + exp(get("matMult", darch.env)(-data, weights) - kronecker(matrix(1,batchSize,1),biases))))
  ret[[2]] <- (ret[[1]] > randomNums)*1.
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
#' @return The real value and binary activations for the units
#' @family RBM unit functions
#' @export
sigmUnitFunc <- function(rbm, dataList, biases, weights, runParams){
  ret = list()
    
  data <- dataList[[2]]
  numUnits <- dim(biases)[2]
  batchSize <- nrow(data)
  randomNums <- matrix(runif(batchSize*numUnits),batchSize,numUnits)  
  ret[[1]] <- (1./(1 + exp(get("matMult", darch.env)(-data, weights) - kronecker(matrix(1,batchSize,1),biases))))
  ret[[2]] <- (ret[[1]] > randomNums)*1.
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
#' @return The real value and binary activations for the units
#' @family RBM unit functions
#' @export
linearUnitFunc <- function(rbm, dataList, biases, weights, runParams){
  ret = list()
  
  data <- dataList[[1]]
  numUnits <- dim(biases)[2]
  batchSize <- nrow(data)
  randomNums <- matrix(rnorm(batchSize*numUnits),batchSize,numUnits)
  ret[[1]] <- get("matMult", darch.env)(data, weights) + kronecker(matrix(1,batchSize,1),biases)
  #ret[[2]] <- ret[[1]] + randomNums
  ret[[2]] <- (ret[[1]] > randomNums)*1.
  
  return(ret)
}