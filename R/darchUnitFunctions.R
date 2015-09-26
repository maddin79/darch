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

#' @include darch.R
NULL

#' Sigmoid unit function.
#'
#' The function calculates the activation and returns the result through the
#' sigmoid transfer function.
#'
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation of the unit in the first entry.
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
sigmoidUnit <- function(data,weights){
  ret <- list(1./(1 + exp(get("matMult", darch.env)(-data, weights))))
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
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
binSigmoidUnit <- function(data,weights){
  sig <- 1./(1 + exp(get("matMult", darch.env)(-data, weights)))
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
#'   the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
sigmoidUnitDerivative <- function(data,weights){
  ret <- list()
  ret[[1]] <- 1./(1 + exp(get("matMult", darch.env)(-data, weights)))
  ret[[2]] <- ret[[1]]*(1-ret[[1]])
  return(ret)
}

#' Continuous Tan-Sigmoid unit function.
#' 
#' Calculates the unit activations and returns them in a list.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation of the transfer function in the first
#'  entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
tanSigmoidUnit <- function(data, weights)
{
  ret <- list()
  ret[[1]] <- tanh(get("matMult", darch.env)(-data, weights))
  return (ret)
}

#' Continuous Tan-Sigmoid unit function.
#' 
#' Calculates the unit activations and returns them in a list.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
tanSigmoidUnitDerivative <- function(data, weights)
{
  ret <- list()
  ret[[1]] <- tanh(get("matMult", darch.env)(data, weights))
  ret[[2]] <- 1-ret[[1]]^2
  return (ret)
}

#' Linear unit function.
#'
#' The function calculates the activation of the units and returns it.
#'
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return  A list with the linear activation of the unit in the first entry.
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
linearUnit <- function(data, weights)
{
  ret <- list(get("matMult", darch.env)(data, weights))
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
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
linearUnitDerivative <- function(data, weights)
{
  ret <- list()
  ret[[1]] <- get("matMult", darch.env)(data, weights)
  ret[[2]] <- matrix(1, nrow(ret[[1]]), ncol(ret[[1]]))
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
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
softmaxUnit <- function (data, weights)
{
  ret <- list()
  x <- exp(get("matMult", darch.env)(data, weights))
  sums <- rep(rowSums(x), ncol(weights))
  ret[[1]] <- x / matrix(sums, nrow(x))
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
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
softmaxUnitDerivative <- function (data, weights)
{
  ret <- list()
  x <- exp(get("matMult", darch.env)(data, weights))
  sums <- rep(rowSums(x), ncol(weights))
  y <- matrix(sums, nrow(x))
  ret[[1]] <- x / y
  ret[[2]] <- ret[[1]] * (1 - ret[[1]])
  return(ret)
}

#' Maxout unit function with unit derivatives.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the maxout transfer function and 
#' the second entry is the derivative of the transfer function.
#' 
#' Configuration of the \code{poolSize} possible via the global option 
#' \code{darch.unitFunction.maxout.poolSize}.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the maxout activation in the first entry and the 
#'   derivative of the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
maxoutUnitDerivative <- function (data, weights)
{  
  # TODO cleaner, local configuration
  poolSize <- getOption("darch.unitFunction.maxout.poolSize", 2)

  # TODO same outgoing weights for neurons of the same maxout unit?

  # TODO make inner unit function configurable
  ret <- sigmoidUnitDerivative(data, weights)
  
  # TODO we need access to dropout masks to do this more cleanly
  # We don't want dropped out values to be considered by the max operator
  ret[[1]][which(ret[[1]] == 0)] <- -.Machine$integer.max
  nrows <- nrow(ret[[1]])
  ncols <- ncol(ret[[1]])
  
  # Abort if number of neurons in the current layer invalid
  if (ncols %% poolSize != 0)
  {
    flog.error(paste("Number of neurons in the current layer not divisible",
                     "by pool size (%d %% %d)"), ncols, poolSize)
    stop("Unrecoverable error, aborting.")
  }
  
  # Walk through the pools
  # TODO solve index problem simpler?
  for (i in 1:(ncols / poolSize))
  {
    poolStart <- poolSize * (i - 1) + 1
    poolEnd <- poolStart + (poolSize - 1)
    # Max indices in single index notation
    maxRowIndices <- max.col(ret[[1]][, poolStart:poolEnd])
    # Convert to matrix index notation
    maxMatrixIndicesTemp <- 1:nrows + (maxRowIndices - 1) * nrows
    # set values for maximum indices to 1 and multiply with original values
    mTemp <- matrix(0, nrow = nrows, ncol = poolSize)
    mTemp[maxMatrixIndicesTemp] <- 1
    ret[[1]][,poolStart:poolEnd] <- ret[[1]][, poolStart:poolEnd] * mTemp
    ret[[2]][,poolStart:poolEnd] <- ret[[2]][, poolStart:poolEnd] * mTemp
  }
  
  # Reset -Inf values to 0
  ret[[1]][which(ret[[1]] == -.Machine$integer.max)] <- 0

  return(ret)
}