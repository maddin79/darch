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
#' Calculates the neuron output with the sigmoid function from input saved in
#' \code{data}. 
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
#' @param ... Additional parameters
#' @return The real value and binary activations for the units
#' @family RBM unit functions
#' @export
sigmUnitFunc <- function(rbm, data, biases, weights, runParams,
  matMult = getDarchParam("matMult", `%*%`, ...), ...)
{
  ret = list()

  numUnits <- ncol(biases)
  batchSize <- nrow(data)
  randomNums <- matrix(runif(batchSize*numUnits),batchSize,numUnits)  
  ret[[1]] <- (1/(1 + exp(matMult(-data, weights) - kronecker(matrix(1,batchSize,1),biases))))
  ret[[2]] <- (ret[[1]] > randomNums)*1
  ret
}

#' Calculates the neuron output with the hyperbolic tangent function
#' 
#' Calculates the neuron output with the hyperbolic tangent function from
#' input in \code{data}.
#' 
#' @details The return value is a list with the output of the hyperbolic tangent
#' function as first entry and binary (-1,1) representation calculated through a
#' comparison ofthe output with random numbers. The random numbers a generated
#' with the function \code{\link{runif}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}. 
#' @param dataList A list with the data matrices for the calculations. 
#' @param biases The biases for the calculations 
#' @param weights The weight matrix for the calculations 
#' @param runParams Parameters which indicates the status of the training. 
#' @param ... Additional parameters
#' @return The real value and binary (-1,1) activations for the units
#' @family RBM unit functions
#' @export
tanSigmUnitFunc <- function(rbm, data, biases, weights, runParams,
  matMult = getDarchParam("matMult", `%*%`, ...), ...)
{
  ret = list()
  
  numUnits <- ncol(biases)
  batchSize <- nrow(data)
  randomNums <- matrix(runif(batchSize * numUnits, min=-1, max=1),
                       batchSize, numUnits)
  ret[[1]] <- tanh(matMult(data, weights) + kronecker(matrix(1, batchSize, 1), biases))
  ret[[2]] <- (ret[[1]] > randomNums)*2-1
  ret
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
#' @param ... Additional parameters
#' @return The real value and binary activations for the units
#' @family RBM unit functions
#' @export
linearUnitFunc <- function(rbm, data, biases, weights, runParams,
  matMult = getDarchParam("matMult", `%*%`, ...), ...)
{
  ret = list()
  
  numUnits <- dim(biases)[2]
  batchSize <- nrow(data)
  randomNums <- matrix(rnorm(batchSize*numUnits),batchSize,numUnits)
  ret[[1]] <- matMult(data, weights) + kronecker(matrix(1,batchSize,1),biases)
  #ret[[2]] <- ret[[1]] + randomNums
  ret[[2]] <- (ret[[1]] > randomNums)*1.
  
  return(ret)
}