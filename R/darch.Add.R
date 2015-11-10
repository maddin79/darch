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

#' Adds a layer to the \code{\linkS4class{DArch}} object
#' 
#' Adds a layer to the given \code{\linkS4class{DArch}} object. The parameter
#' weights and biases will be put together in one matrix. 
#'
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @param weights The weights for the layer.
#' @param biases The biases for the layer.
#' @param unitFunction The functions of the units in the layer.
#' 
#' @seealso \code{\linkS4class{DArch}},
#'          \code{\link{sigmoidUnitDerivative}}
#' 
#' @keywords internal
#' @export
setGeneric("addLayer",function(darch, weights, biases, unitFunction){standardGeneric("addLayer")})

#' Adds a layer to the \code{\linkS4class{DArch}} object
#'
#' @inheritParams addLayer
#' @seealso \code{\link{addLayer}}
#' @keywords internal
#' @export
setMethod(
  f="addLayer",
  signature="DArch",
  definition=function(darch, weights, biases,unitFunction){
    numLayers <- length(getLayers(darch))
    w <- rbind(weights, biases)
    layer <- list()
    if (getFF(darch)){
      layer[[1]] <- ff(vmode = "double", dim = dim(w))
      layer[[1]][] <- w
    }else{
      layer[[1]] <- w
    }
    layer[[2]] <- unitFunction
    darch@layers[[numLayers + 1]] <- layer
    return(darch)
  }
)

#' Adds a field to a layer
#' 
#' Adds a field to the layer given by the index. 
#'
#' @param darch An instance of the class \code{\link{DArch}}.
#' @param index The position of the layer.
#' @param field The new field for the layer.
#' 
#' @seealso \code{\linkS4class{DArch}}
#' 
#' @keywords internal
#' @export
setGeneric("addLayerField",function(darch, index, field){standardGeneric("addLayerField")})

#' Adds a field to a layer
#'
#' @inheritParams addLayerField
#' @seealso \code{\link{addLayerField}}
#' @keywords internal
#' @export
setMethod(
  f="addLayerField",
  signature="DArch",
  definition=function(darch, index, field){
    num <- length(darch@layers[[index]])
    darch@layers[[index]][[num + 1]] <- field
    return(darch)
  }
)

#' Adds an execution output for a DArch object
#' 
#' This method can be used to save the execution outputs of every layer for the
#' DArch object. The outputs are saved in a list and every time this function is
#' called, the list is extended of one field with the new output.
#'
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @param output The output of the layer.
#' @seealso \code{\linkS4class{DArch}}
#' @keywords internal
#' @export
setGeneric("addExecOutput",function(darch, output){standardGeneric("addExecOutput")})

#' Adds an execution output for a DArch object
#'
#' @inheritParams addExecOutput
#' @seealso \link{addExecOutput}
#' @keywords internal
#' @export
setMethod(
  f="addExecOutput",
  signature="DArch",
  definition=function(darch, output){
    num <- length(darch@executeOutput)+1
    if (getFF(darch)){
      darch@executeOutput[[num]] <- ff(vmode = "double", dim = dim(output))
      darch@executeOutput[[num]][] <- output
    }else{
      darch@executeOutput[[num]] <- output
    }
    return(darch)
  }
)