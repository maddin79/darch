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

#' Returns the batch size of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @include net.R
#' 
#' @export
#' @docType methods
#' @rdname getBatchSize-methods
setGeneric("getBatchSize",function(net){standardGeneric("getBatchSize")})

#' @rdname getBatchSize-methods
#' @aliases getBatchSize,Net-method
setMethod(
  f="getBatchSize",
  signature="Net",
  definition=function(net){
    return (net@batchSize)
  }
)

#' Returns the error function of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getErrorFunction-methods

setGeneric("getErrorFunction",function(net){standardGeneric("getErrorFunction")})

#' @rdname getErrorFunction-methods
#' @aliases getErrorFunction,Net-method
setMethod(
  f="getErrorFunction",
  signature="Net",
  definition=function(net){
    return (net@errorFunction)
  }
)

#' Returns the function for generating weight matrices.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getGenWeightFunction-methods
setGeneric("getGenWeightFunction",function(net){standardGeneric("getGenWeightFunction")})

#' @rdname getGenWeightFunction-methods
#' @aliases getGenWeightFunction,Net-method
setMethod(
  f="getGenWeightFunction",
  signature="Net",
  definition=function(net){
    return (net@genWeightFunction)
  }
)

#' Returns the final momentum of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getFinalMomentum-methods
setGeneric("getFinalMomentum",function(net){standardGeneric("getFinalMomentum")})

#' @rdname getFinalMomentum-methods
#' @aliases getFinalMomentum,Net-method
setMethod(
  f="getFinalMomentum",
  signature="Net",
  definition=function(net){
    return (net@finalMomentum )
  }
)

#' Returns the momentum of the \code{\link{Net}}
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
setGeneric("getInitialMomentum",function(net){standardGeneric("getInitialMomentum")})

#' Returns the momentum of the \code{\link{Net}}
#' 
#' @inheritParams getInitialMomentum
#' @seealso \link{getInitialMomentum}
#' @export
setMethod(
  f="getInitialMomentum",
  signature="Net",
  definition=function(net){
    return (net@initialMomentum )
  }
)

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

#' Returns the number of epochs the \code{\linkS4class{Net}} was trained for
#'
#' @param net An instance of the class \code{\linkS4class{Net}}.
#' 
#' @export
setGeneric("getEpochs",function(net){standardGeneric("getEpochs")})

#' Returns the number of epochs the \code{\linkS4class{Net}} was trained for
#' 
#' @inheritParams getEpochs
#' @seealso \link{getEpochs}
#' @export
setMethod(
  f="getEpochs",
  signature="Net",
  definition=function(net){
    return (net@epochs)
  }
)

#' Returns the list of statistics for the network
#' 
#' The list of statistics can contain values about errors, miss 
#' classifications and other useful things from the pre-training or fine-tuning
#'  of a deep architecture.
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @usage getStats(net)
#' 
#' @seealso \code{\link{Net}}
#' 
#' @include net.R
#' 
#' @export
#' @docType methods
#' @rdname getStats-methods
setGeneric("getStats",function(net){standardGeneric("getStats")})

#' @rdname getStats-methods
#' @aliases getStats,Net-method
setMethod(
  f="getStats",
  signature="Net",
  definition=function(net){
    return (net@stats)
  }
)

#' Returns whether weight normalization is active
#'
#' @param net An instance of the class \code{\linkS4class{Net}}.
#' 
#' @export
setGeneric("getNormalizeWeights",function(net){standardGeneric("getNormalizeWeights")})

#' Returns whether weight normalization is active
#' 
#' @inheritParams getNormalizeWeights
#' @seealso \link{getNormalizeWeights}
#' @export
setMethod(
  f="getNormalizeWeights",
  signature="Net",
  definition=function(net){
    return (net@normalizeWeights)
  }
)
