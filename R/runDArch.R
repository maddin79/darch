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

#' Execute the darch
#' 
#' Runs the darch in a feed forward manner and saves the 
#' generated outputs for every layer in the list
#' \code{executeOutput} from the darch.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param data The input data to execute the darch on. 
#' @param outputLayer The output of which layer is to be returned, absolute
#'  number or offset from the last layer.
#' @param matMult Function to use for matrix multiplication.
#' @return The DArch object with the calculated outputs
#' 
#' @seealso \code{\link{DArch}}
#' @family darch execute functions
#' @keywords internal
runDArch <- function(darch, data, inputLayer = 1,
  outputLayer = length(darch@layers),
  matMult = getParameter(".matMult"))
{
  layers <- darch@layers
  numRows <- nrow(data)
  
  if (outputLayer == 0)
  {
    return(data)
  }
  
  for (i in inputLayer:outputLayer)
  {
    data <- cbind(data,rep(1,numRows))
    data <- layers[[i]][["unitFunction"]](matMult(data,
      layers[[i]][["weights"]]), net = darch)[[1]]
  }
  
  data
}

#' Execute the darch with dropout support
#' 
#' If dropout was disabled, \code{\link{runDArch}} will be called instead.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param data The input data to execute the darch on.
#' @param iterations Number of iterations for moment matching, if dropout is
#'  enabled.
#' @param outputLayer The output of which layer is to be returned, absolute
#'  number (0-based).
#' @param matMult Function to use for matrix multiplication.
#' @param dropout Dropout rates for the layers.
#' @return The DArch object with the calculated outputs
#' 
#' @seealso \code{\link{DArch}}
#' @family darch execute functions
#' @keywords internal
runDArchDropout <- function(darch, data, inputLayer = 1,
  outputLayer = length(darch@layers), matMult = getParameter(".matMult"),
  dropout = getParameter(".darch.dropout"),
  iterations = getParameter(".darch.dropout.momentMatching"))
{
  layers <- darch@layers
  numRows <- nrow(data)
  # In case DropConnect is disabled, we append 0 for the last layer
  dropout <- c(dropout, 0)
  
  if (outputLayer == 0)
  {
    return(data)
  }
  
  for (i in inputLayer:outputLayer)
  {
    data <- cbind(data, rep(1, numRows))
    input <- matMult(data, (1 - dropout[i + 1]) * layers[[i]][["weights"]])
    
    if (iterations > 0)
    {
      E <- as.vector(input)
      V <- as.vector(dropout[i + 1] * (1 - dropout[i + 1]) *
            (matMult(data ^ 2, layers[[i]][["weights"]] ^ 2)))
      n <- length(E)
      
      ret <- matrix(rep(0, n), nrow = numRows)
      
      for (j in 1:iterations)
      {
        ret <- ret + layers[[i]][["unitFunction"]](matrix(rnorm(n, E, V),
          nrow = numRows), darch = darch)[[1]]
      }
      
      data <- ret/iterations
    }
    else
    {
      data <- layers[[i]][["unitFunction"]](input, net = darch)[[1]]
    }
  }
  
  data
}