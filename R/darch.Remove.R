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

#' Removes a layer from the \code{\link{DArch}} object
#' 
#' This function removes the layer with the given index from the 
#' \code{\link{DArch}} object.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.
#' 
#' @usage removeLayerField(darch, index)
#' @seealso \code{\link{DArch}}
#' @return The \code{\link{DArch}} object without the layer.
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname removeLayerField-methods
setGeneric("removeLayerField",function(darch, index){standardGeneric("removeLayerField")})

#' @rdname removeLayerField-methods
#' @aliases removeLayerField,DArch-method
setMethod(
  f="removeLayerField",
  signature="DArch",
  definition=function(darch, index){
    if (index < 3){
      flog.info("You can not remove an element with an index less than 3")
      return(darch)
    }
    darch@layers[index] <- NULL
    return(darch)
  }
)