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

#' Execute the darch
#' 
#' Runs the darch in a feed forward manner and saves the 
#' generated outputs for every layer in the list
#' \code{executeOutput} from the darch.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param data The input data to execute the darch on. 
#' @return The DArch object with the calculated outputs
#' 
#' @seealso \code{\link{DArch}}
#' @family darch execute functions
#' @export
runDArch <- function(darch, data,
  matMult=getDarchParam("matMult", `%*%`, darch=darch))
{
  darch <- resetExecOutput(darch)
  layers <- getLayers(darch)
  
  numRows <- nrow(data)
  
  for(i in 1:length(layers))
  {
    data <- cbind(data,rep(1,numRows))
    data <- layers[[i]][[2]](matMult(data, layers[[i]][[1]]), darch=darch)[[1]]
    darch <- addExecOutput(darch, data)
  }
  
  darch
}

#' Execute the darch with dropout support
#' 
#' If dropout was disabled, \code{\link{runDArch}} will be called instead.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param data The input data to execute the darch on. 
#' @return The DArch object with the calculated outputs
#' 
#' @seealso \code{\link{DArch}}
#' @family darch execute functions
#' @export
runDArchDropout <- function(darch, data,
  iterations=getDarchParam("darch.dropout.momentMatching", 0,
  darch=darch), matMult=getDarchParam("matMult", `%*%`, darch=darch))
{
  if (darch@dropoutHidden <= 0)
  {
    return(runDArch(darch, data, matMult))
  }
  
  darch <- resetExecOutput(darch)
  layers <- getLayers(darch)
  numRows <- nrow(data)
  
  for(i in 1:length(layers))
  {
    data <- cbind(data,rep(1,numRows))
    input <- matMult(data, (1 - darch@dropoutHidden) * layers[[i]][[1]])
    
    if (iterations > 0)
    {
      E <- as.vector(input)
      V <- as.vector(darch@dropoutHidden *
            (1 - darch@dropoutHidden) * (matMult(data^2, layers[[i]][[1]]^2)))
      n <- length(E)
      
      ret <- matrix(rep(0, n), nrow=numRows)
      
      for (j in 1:iterations)
      {
        ret <- ret + layers[[i]][[2]](matrix(rnorm(n, E, V), nrow=numRows),
                                        darch=darch)[[1]]
      }
      
      data <- ret/iterations
    }
    else
    {
      data <- layers[[i]][[2]](input, darch=darch)[[1]]
    }
    
    darch <- addExecOutput(darch, data)
  }
  
  return(darch)
}