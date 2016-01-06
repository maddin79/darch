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

#' Resets the output list of the \code{\link{DArch}} object
#' 
#' This function sets the attribute \code{executeOutput} of the 
#' \code{\link{DArch}} object to an empty list.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage resetExecOutput(darch)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname resetExecOutput-methods
setGeneric("resetExecOutput",function(darch){standardGeneric("resetExecOutput")})

#' @rdname resetExecOutput-methods
#' @aliases resetExecOutput,DArch-method
#' @export
setMethod(
  f="resetExecOutput",
  signature="DArch",
  definition=function(darch){
    darch@executeOutput <- list()
    return(darch)
  }
)

#' Resets the weights and biases of the \code{\link{DArch}} object
#' 
#' This function resets the weights and biases of the \code{\link{DArch}} object 
#' and all \code{\link{RBM}} objects if the parameter \code{resetRBMs} is 
#' \code{TRUE}.
#' 
#' @details
#' When the parameter \code{resetRBMs} is \code{FALSE} then the trained weights
#' and biases are copied from the \code{\link{RBM}} objects to the layers.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param resetRBMs If true the RBMs are also reseted.
#' 
#' @usage resetDArch(darch,resetRBMs=TRUE)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname resetDArch-methods
setGeneric("resetDArch",function(darch,resetRBMs=TRUE){standardGeneric("resetDArch")})

#' @rdname resetDArch-methods
#' @aliases resetDArch,DArch-method
setMethod(
  f="resetDArch",
  signature="DArch",
  definition=function(darch,resetRBMs=TRUE){
    rbmList <- getRBMList(darch)
    layers <- getLayers(darch)
    rbmListLength <- length(rbmList)
    if (resetRBMs){
      for(i in 1:rbmListLength){
        rbmList[[i]] <- resetRBM(rbmList[[i]])
      }
    }
    
    setLayers(darch) <- list()
    
    for(i in 1:rbmListLength){
      rbm <- rbmList[[i]]
      darch <- addLayer(darch, getWeights(rbm), getHiddenBiases(rbm),
                        sigmoidUnitDerivative)
    }
    
    if (rbmListLength < (length(layers))){
      for(i in (rbmListLength+1):(length(layers))){
        rows <- nrow(layers[[i]][[1]])-1
        cols <- ncol(layers[[i]][[1]])
        weights <- getGenWeightFunction(darch)(rows,cols)
        bias <-  getGenWeightFunction(darch)(1,cols)
        darch <- addLayer(darch,weights,bias,layers[[i]][[2]])
      }
    }
    
    return(darch)
  }
)