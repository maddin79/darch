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
#' Execute the darch
#' 
#' Runs the darch in a feed forward manner and saves the 
#' generated outputs for every layer in the list
#' \code{executeOutput} from the darch.
#' To get the outputs call
#' 
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param data The input data to execute the darch on. 
#' @return The DArch object with the calculated outputs
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname runDArch
#' @include darch.R
#' @export
runDArch <- function(darch,data){
  darch <- resetExecOutput(darch)
  layers <- getLayers(darch)
  
  # TODO is this safe to do? Should we bail out instead?
  # we need a matrix to correctly identify the number of rows below
  if (is.null(dim(data)))
  {
    flog.warn("Converting data vector to matrix")
    data <- matrix(data, nrow=1)
  }
  
  numRows <- dim(data)[1]
  
  for(i in 1:length(layers)){
    dropoutWeightChange <- getLayerWeights(darch, i) * darch@dropoutHidden
    data <- cbind(data,rep(1,numRows))
    # temporarily change weights to account for dropout
    setLayerWeights(darch, i) <- getLayerWeights(darch, i) - dropoutWeightChange
    ret <- layers[[i]][[2]](data[],layers[[i]][[1]][])
    setLayerWeights(darch, i) <- getLayerWeights(darch, i) + dropoutWeightChange
    data <- ret[[1]]
    darch <- addExecOutput(darch,data)
  }
  
  return(darch)
}