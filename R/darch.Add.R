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
#' @seealso \code{\linkS4class{DArch}}
#' 
#' @keywords internal
#' @export
setGeneric("addLayer",function(darch, weights, biases, unitFunction, weightUpdateFunction){standardGeneric("addLayer")})

#' Adds a layer to the \code{\linkS4class{DArch}} object
#'
#' @inheritParams addLayer
#' @seealso \code{\link{addLayer}}
#' @keywords internal
#' @export
setMethod(
  f = "addLayer",
  signature = "DArch",
  definition = function(darch, weights, biases, unitFunction, weightUpdateFunction){
    numLayers <- length(darch@layers)
    w <- rbind(weights, biases)
    layer <- list()
    layer[["weights"]] <- w
    layer[["unitFunction"]] <- unitFunction
    layer[["weightUpdateFunction"]] <- weightUpdateFunction
    darch@layers[[numLayers + 1]] <- layer
    return(darch)
  }
)

#' Adds a field to a layer
#' 
#' Adds a field to the layer given by the index. 
#'
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
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
  f = "addLayerField",
  signature = "DArch",
  definition = function(darch, index, field){
    num <- length(darch@layers[[index]])
    darch@layers[[index]][[num + 1]] <- field
    return(darch)
  }
)
