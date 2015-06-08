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
#' Sigmoid unit function.
#' 
#' The function calculates the activation and returns the result through the
#' sigmoid transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation of the unit in the first entry.
#' 
#' @usage sigmoidUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname sigmoidUnit
#' @include darch.R
#' @export
sigmoidUnit <- function(data,weights){
  ret <- list(1./(1 + exp(matMul(-data, weights))))
  return(ret)
}

#' Binary sigmoid unit function.
#' 
#' The function calculates the activation and the output from the sigmoid 
#' transfer function. It returns a binary matrix where a entry is 1 if the value 
#' is bigger than a random number generated with \code{\link{runif}}.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the binary activation of the unit in the first entry.
#' 
#' @usage binSigmoidUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#'
#' @docType methods
#' @rdname binSigmoidUnit
#' @include darch.R
#' @export
binSigmoidUnit <- function(data,weights){
  sig <- 1./(1 + exp(matMul(-data, weights)))
  rows <- nrow(data)
  cols <- ncol(weights)
  ret <- list(sig > matrix(runif(rows*cols),rows,cols))
  return(ret)
}

#' Sigmoid unit function with unit derivatives.
#' 
#' The function calculates the activation and returns a list which the first 
#' entry is the result through the sigmoid transfer function and the second 
#' entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation in the first entry and the derivative of
#' the transfer function in the second entry
#' 
#' @usage sigmoidUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}},
#'
#' @docType methods
#' @rdname sigmoidUnitDerivative
#' @include darch.R
#' @export
sigmoidUnitDerivative <- function(data,weights){
  ret <- list()
  ret[[1]] <- 1./(1 + exp(matMul(-data, weights)))
  ret[[2]] <- ret[[1]]*(1-ret[[1]])
  return(ret)
}

#' Linear unit function.
#'
#' The function calculates the activation of the units and returns it.
#'
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return  A list with the linear activation of the unit in the first entry.
#'
#' @usage linearUnit(data,weights)
#'
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname linearUnit
#' @include darch.R
#' @export
linearUnit <- function(data,weights){
  ret <- list(matMul(data, weights))
  return(ret)
}

#' Linear unit function with unit derivatives.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the linear activation of the units and the second 
#' entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the linear activation in the first entry and the 
#' derivative of the activation in the second entry
#' 
#' @usage linearUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname linearUnitDerivative
#' @include darch.R
#' @export
linearUnitDerivative <- function(data,weights){
  ret <- list()
  ret[[1]] <- matMul(data, weights)
  ret[[2]] <- matrix(1,nrow(ret[[1]]),ncol(ret[[1]]))
  return(ret)
}

#' Softmax unit function.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the softmax transfer function.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the softmax activation in the first entry
#' 
#' @usage softmaxUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname softmaxUnit
#' @include darch.R
#' @export
softmaxUnit <- function (data, weights) {
  ret <- list()
  x <- exp(matMul(data, weights))
  sums <- rep(rowSums(x),ncol(weights))
  ret[[1]] <- x/matrix(sums,nrow(x))
  return(ret)
}

#' Softmax unit function with unit derivatives.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the softmax transfer function 
#' and the second entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the softmax activation in the first entry and the 
#' derivative of the transfer function in the second entry
#' 
#' @usage softmaxUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}}
#'
#' @docType methods
#' @rdname softmaxUnitDerivative
#' @include darch.R
#' @export
softmaxUnitDerivative <- function (data, weights) {
  ret <- list()
  x <- exp(matMul(data, weights))
  sums <- rep(rowSums(x),ncol(weights))
  y <- matrix(sums,nrow(x))
  ret[[1]] <- x/y
  ret[[2]] <- ret[[1]] * (1 - ret[[1]])
  return(ret)
}

#' Maxout unit function with unit derivatives.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the maxout transfer function 
#' and the second entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the maxout activation in the first entry and the 
#' derivative of the transfer function in the second entry
#' 
#' @usage maxoutUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}}
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname maxoutUnitDerivative
#' @include darch.R
#' @export
maxoutUnitDerivative <- function (data, weights) {
  # convert vector to matrix if necessary
  if(is.null(dim(data))){
    data <- t(as.matrix(data))
  }
  
  ret <- list()
  # initialize matrixes to store activations and derivatives
  valuesMatrix <- matrix(nrow=nrow(data), ncol=ncol(weights))
  derivativesMatrix <- valuesMatrix
  
  # remove bias column in data matrix
  data <- data[,1:ncol(data)-1,drop=F]
  flog.debug("Initial data:")
  print(data)
  
  # extract biases
  biases <- weights[nrow(weights),,drop=F]
  flog.debug("Initial biases:")
  print(biases)
  
  # remove bias row in weights matrix
  weights <- weights[1:nrow(weights)-1,,drop=F]
  flog.debug("Initial weights:")
  print(weights)
  
  # iterate over input data
  for (i in 1:nrow(data))
  {
    # reset values and derivatives
    values <- c()
    derivatives <- c()
    flog.debug(paste("Data row:", i))
    # current row of data
    x <- data[i,]
    print(x)
    
    # iterate through neurons (each column corresponds to the incoming
    # connections for one neuron)
    for (j in 1:ncol(weights))
    {
      # initialize maximum values TODO start with 0?
      maxValue <- -Inf
      maxWeight <- -Inf
      
      flog.debug(paste("Weight column:", j))
      
      # iterate through individual weights
      for (k in 1:nrow(weights))
      {
        # We're looking for the maximum of these x*w+b terms
        flog.debug(paste(x[k],"*", weights[k,j], "+", biases[j]))
        v <- x[k] * weights[k,j] + biases[j]
        
        flog.debug(paste("Value:", v))
        
        # found a new maximum value?
        if (v > maxValue)
        {
          flog.debug(paste("New highest value:", v))
          maxValue <- v
          # store the weight for the current value, it is the derivative of the
          # activation function
          maxWeight <- weights[k,j]
        }
      }
      
      # collect maximum values for the neurons
      values <- c(values, maxValue)
      derivatives <- c(derivatives, maxWeight)
    }
    
    # collect activation and derivatives for the current input data
    valuesMatrix[i,] <- values
    derivativesMatrix[i,] <- derivatives
  }
  
  # collect activation and derivatives over all input data and return it
  ret[[1]] <- valuesMatrix
  ret[[2]] <- derivativesMatrix
  
  return(ret)
}

# #' Binary unit function.
# #' 
# #' Returns the binary activation of the units. 
# #' 
# #' @param data The data matrix for the calculation
# #' @param weights The weight and bias matrix for the calculation
# #' @return A list with the binary activation in the first entry
# #' 
# #' @usage binaryUnit(data,weights)
# #' 
# #' @seealso \code{\link{DArch}}
# #'          \code{\link{sigmoidUnit}}
# #'          \code{\link{binSigmoidUnit}}
# #'          \code{\link{sigmoidUnitDerivative}}
# #'          \code{\link{linearUnit}}
# #'          \code{\link{linearUnitDerivative}}
# #'          \code{\link{softmaxUnit}}
# #'          \code{\link{binaryUnit}}
# #' 
# #' @docType methods
# #' @rdname binaryUnit
# #' @include darch.R
# #' @export
# binaryUnit <- function(data,weights){
#   ret <- data%*%weights
#   ret <- ret >= 0
#   return(list(1*ret))
# }
