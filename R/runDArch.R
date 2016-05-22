# Copyright (C) 2013-2016 Martin Drees
# Copyright (C) 2015-2016 Johannes Rueckert
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

#' Forward-propagates data through the network
#' 
#' Runs the \code{DArch} in a feed-forward manner and returns the output.
#' 
#' Input and output layer can be chosen via the parameters \code{inputLayer}
#' and \code{outputLayer}.
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param data The input data to execute the darch on.
#' @param inputLayer Into which layer the given data is to be fed. Absolute
#'   number starting at 1 for the input layer.
#' @param outputLayer The output of which layer is to be returned, absolute
#'   number starting a 0 for the input layer (i.e. pre-processed data is
#'   returned).
#' @param matMult Function to use for matrix multiplication.
#' @return The network output
#' 
#' @seealso \code{\link{darch}}
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

#' Forward-propagate data through the network with dropout inference
#' 
#' 
#' If dropout was disabled, \code{\link{runDArch}} will be called instead.
#' 
#' @param dropout Dropout rates for the layers.
#' @param iterations If greater than 0, the numbr of iterations to use for
#'   moment matching dropout inference.
#' @return The network output.
#' 
#' @inheritParams runDArch
#' @seealso \code{\link{darch}}
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