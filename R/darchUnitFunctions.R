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
  ret <- list(1./(1 + exp(gpuMatMult(-data, weights))))
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
  sig <- 1./(1 + exp(gpuMatMult(-data, weights)))
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
  ret[[1]] <- 1./(1 + exp(gpuMatMult(-data, weights)))
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
  ret <- list(gpuMatMult(data, weights))
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
  ret[[1]] <- gpuMatMult(data, weights)
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
  x <- exp(gpuMatMult(data, weights))
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
  x <- exp(gpuMatMult(data, weights))
  sums <- rep(rowSums(x),ncol(weights))
  y <- matrix(sums,nrow(x))
  ret[[1]] <- x/y
  ret[[2]] <- ret[[1]] * (1 - ret[[1]])
  return(ret)
}

#' Maxout unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the result through the maxout transfer function and
#' the second entry is the derivative of the transfer function.
#' 
#' Configuration of the poolSize possible via the global option
#' \code{darch.unitFunction.maxout.poolSize}.
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
  
  # TODO cleaner, local configuration
  poolSize <- getOption("darch.unitFunction.maxout.poolSize", 2)

  # TODO same outgoing weights for neurons of the same maxout unit?
  
  x <- gpuMatMult(data, weights)
  # TODO we need access to dropout masks to do this more cleanly
  # We don't want dropped out values to be considered by the max operator
  x[which(x==0)] <- -Inf
  xrows <- nrow(x)
  xcols <- ncol(x)

  ret <- list()
  # TODO sparse matrix?
  ret[[1]] <- matrix(0,nr=xrows,nc=xcols)
  ret[[2]] <- ret[[1]]
  mEmpty <- matrix(0, nr=xrows,nc=poolSize)

  # Abort if number of neurons in the current layer invalid
  if (xcols %% poolSize != 0)
  {
    flog.error(paste("Number of neurons in the current layer not divisible",
                     "by pool size (%d %% %d)"), xcols, poolSize)
    stop("Unrecoverable error, aborting.")
  }
  
  # Walk through the pools
  for (i in 1:(xcols/poolSize))
  {
    poolStart <- poolSize*(i-1)+1
    poolEnd <- poolStart + (poolSize-1)
    # Max indices in sigle index notation
    maxRowIndices <- max.col(x[,poolStart:poolEnd])
    # Convert to matrix index notation
    maxMatrixIndicesTemp <- 1:xrows + (maxRowIndices-1)*xrows
    # Convert the pool matrix indices back to indices in the original matrix
    maxMatrixIndicesX <- (poolStart-1)*xrows + maxMatrixIndicesTemp
    # Add the maximum values we found onto an empy matrix, effectively making
    # all other outputs 0
    # TODO cleaner solution?
    mTemp <- mEmpty
    mTemp[maxMatrixIndicesTemp] <- x[maxMatrixIndicesX]
    ret[[1]][,poolStart:poolEnd] <- mTemp
    # Derivatives of all non-0 values are 1, otherwise 0
    mTemp[which(mTemp!=0)] <- 1
    ret[[2]][,poolStart:poolEnd] <- mTemp
  }
  
  # Reset -Inf values to 0 (if all considered values were dropped out)
  ret[[1]][which(ret[[1]]==-Inf)] <- 0

  return(ret)
}