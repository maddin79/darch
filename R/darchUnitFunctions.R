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

#' Sigmoid unit function with unit derivatives.
#' 
#' The function calculates the activation and returns a list which the first
#' entry is the result through the sigmoid transfer function and the second
#' entry is the derivative of the transfer function.
#' 
#' @param input Input for the activation function.
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
sigmoidUnitDerivative <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- 1./(1 + exp(-input))
  ret[[2]] <- ret[[1]] * (1 - ret[[1]])
  ret
}

#' Continuous Tan-Sigmoid unit function.
#' 
#' Calculates the unit activations and returns them in a list.
#' 
#' @param input Input for the activation function.
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
tanSigmoidUnitDerivative <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- tanh(input)
  ret[[2]] <- 1 - ret[[1]]^2
  ret
}

#' Linear unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the linear activation of the units and the second
#' entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @return A list with the linear activation in the first entry and the
#' derivative of the activation in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
linearUnitDerivative <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- input
  ret[[2]] <- matrix(1, nrow(ret[[1]]), ncol(ret[[1]]))
  ret
}

#' Softmax unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the result through the softmax transfer function
#' and the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @return A list with the softmax activation in the first entry and the
#' derivative of the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
softmaxUnitDerivative <- function (input, ...)
{
  ret <- list()
  x <- exp(input - max(input))
  #sums[which(sums == 0)] <- 1
  ret[[1]] <- x / rowSums(x)
  ret[[2]] <- ret[[1]] * (1 - ret[[1]])
  #ret[[2]] <- matrix(1, nrow=nrow(ret[[1]]), ncol=ncol(ret[[1]]))
  ret
}

#' Maxout / LWTA unit function
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the maxout transfer function and 
#' the second entry is the derivative of the transfer function.
#' 
#' Maxout sets the activations of all neurons but the one with the highest
#' activation within a pool to \code{0}. If this is used without
#' \link{maxoutWeightUpdate}, it becomes the local-winner-takes-all algorithm,
#' as the only difference between the two is that outgoing weights are shared
#' for maxout.
#' 
#' @param input Input for the activation function.
#' @param poolSize The size of each maxout pool.
#' @return A list with the maxout activation in the first entry and the 
#'   derivative of the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \linkS4class{DArch}
#' @export
maxoutUnitDerivative <- function (input, poolSize =
  getDarchParam("darch.layerFunction.maxout.poolSize", 2, ...), ...)
{  
  # TODO make inner unit function configurable
  ret <- linearUnitDerivative(input)
  
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
  for (i in 1:(ncols / poolSize))
  {
    poolStart <- poolSize * (i - 1) + 1
    poolEnd <- poolStart + (poolSize - 1)
    # Creates a mask with 1 for the highest activations, 0 everywhere else
    mMask <- diag(poolSize)[max.col(ret[[1]][, poolStart:poolEnd, drop = F]),]
    # Apply mask to activations and derivatives
    ret[[1]][,poolStart:poolEnd] <-
      ret[[1]][, poolStart:poolEnd, drop = F] * mMask
    ret[[2]][,poolStart:poolEnd] <-
      ret[[2]][, poolStart:poolEnd, drop = F] * mMask
  }
  
  # Reset -Inf values to 0 (relevant only if a pool contained only dropped-out
  # units)
  ret[[1]][which(ret[[1]] == -.Machine$integer.max)] <- 0

  ret
}

#' Rectified linear unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the rectified linear activation of the units and
#' the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @return A list with the rectified linear activation in the first entry and
#'  the derivative of the activation in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
rectifiedLinearUnitDerivative <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- input
  ret[[1]][which(ret[[1]]<0)] <- 0
  ret[[2]] <- matrix(1, nrow(ret[[1]]), ncol(ret[[1]]))
  ret
}