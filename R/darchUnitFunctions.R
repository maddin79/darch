# Copyright (C) 2013-2016 Martin Drees
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

#' @include darch.Class.R
NULL

#' Sigmoid unit function with unit derivatives.
#' 
#' The function calculates the activation and returns a list which the first
#' entry is the result through the sigmoid transfer function and the second
#' entry is the derivative of the transfer function.
#' 
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
sigmoidUnit <- function(input, ...)
{
  sigmoidUnitCpp(input)
}

#' Continuous Tan-Sigmoid unit function.
#' 
#' Calculates the unit activations and returns them in a list.
#' 
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
tanhUnit <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- tanh(input)
  ret[[2]] <- 1 - ret[[1]] ^ 2
  ret
}

#' Linear unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the linear activation of the units and the second
#' entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the linear activation in the first entry and the
#' derivative of the activation in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
linearUnit <- function(input, ...)
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
#' @param ... Additional parameters, not used.
#' @return A list with the softmax activation in the first entry and the
#' derivative of the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
softmaxUnit <- function(input, ...)
{
  softmaxUnitCpp(input)
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
#' @param ... Additional parameters, passed on to inner unit function.
#' @param poolSize The size of each maxout pool.
#' @param unitFunc Inner unit function for maxout.
#' @param dropoutMask Vector containing the dropout mask.
#' @return A list with the maxout activation in the first entry and the 
#'   derivative of the transfer function in the second entry
#' @family DArch unit functions
#' @seealso \linkS4class{DArch}
#' @export
maxoutUnit <- function(input, ..., poolSize =
  getDarchParam("darch.maxout.poolSize", 2, ...), unitFunc =
  getDarchParam(".darch.maxout.unitFunction", linearUnit, ...),
  dropoutMask = vector())
{  
  # TODO add unit func parameter to darch() function
  ret <- unitFunc(input, ...)
  
  ncols <- ncol(ret[[1]])
  
  # Abort if number of neurons in the current layer invalid
  if (ncols %% poolSize != 0)
  {
    stop(futile.logger::flog.error(paste("Number of neurons in the current",
      "layer not divisible by pool size (%d %% %d)"), ncols, poolSize))
  }
  
  maxoutUnitCpp(ret[[1]], ret[[2]], poolSize, dropoutMask)
  
  ret
}

#' Rectified linear unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the rectified linear activation of the units and
#' the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the rectified linear activation in the first entry and
#'  the derivative of the activation in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
rectifiedLinearUnit <- function(input, ...)
{
  rectifiedLinearUnitCpp(input)
}

#' Exponential linear unit (ELU) function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the exponential linear activation of the units and
#' the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param alpha ELU hyperparameter.
#' @param ... Additional parameters, passed to \code{\link{getDarchParam}}.
#' @return A list with the ELU activation in the first entry and
#'  the derivative of the activation in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
exponentialLinearUnit <- function(input, alpha =
  getDarchParam("darch.elu.alpha", 1, ...), ...)
{
  exponentialLinearUnitCpp(input, alpha)
}

#' Softplus unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the softmax activation of the units and the second
#' entry is the derivative of the transfer function. Softplus is a smoothed
#' version of the rectified linear activation function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the softplus activation in the first entry and
#'  the derivative of the activation in the second entry
#' @family DArch unit functions
#' @seealso \code{\linkS4class{DArch}}
#' @export
softplusUnit <- function(input, ...)
{
  softplusUnitCpp(input)
}
