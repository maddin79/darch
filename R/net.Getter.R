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

#' Returns the current momentum of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @param numEpochsRemaining The number of epochs the net is going to be
#'  trained for.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getMomentum-methods
setGeneric("getMomentum",function(net){standardGeneric("getMomentum")})

#' @rdname getMomentum-methods
#' @aliases getMomentum,Net-method
setMethod(
  f="getMomentum",
  signature="Net",
  definition=function(net){
    momentum <- net@initialMomentum
    
    if (net@momentumRampLength == 0 || net@epochsScheduled <= 1)
    {
      momentum <- net@finalMomentum
    }
    else
    {
      momentum <- min(net@initialMomentum +
        (net@finalMomentum - net@initialMomentum) * (net@epochs - 1)
        / (net@epochsScheduled - 1) / net@momentumRampLength,
        net@finalMomentum)
    }
    
    return (momentum)
  }
)
