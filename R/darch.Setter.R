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

#' Set the dropout mask for the given layer.
#' 
#' Sets the dropout mask to be applied to the weights between layer i and i+1,
#' for 0 < i < numLayers. For i = 0, sets the dropout mask for the input layer,
#' which will be applied to the initiali input data.
#'
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param i Layer index or 0 for input layer.
#' @param value Dropout mask for the given layer.
#' @usage setDropoutMask(darch, i) <- value
#' @return The darch with the updated dropout mask
#' @seealso \code{\linkS4class{DArch}}
#' @keywords internal
#' @include darch.Class.R
setGeneric("setDropoutMask<-",function(darch, i, value)
  {standardGeneric("setDropoutMask<-")})

setReplaceMethod(
  f = "setDropoutMask",
  signature = "DArch",
  definition = function(darch, i, value)
  {
    darch@dropoutMasks[[i + 1]] <- value
    darch
  }
)
