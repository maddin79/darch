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

#' Returns if the weights are saved as ff objects
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getFF-methods
setGeneric("getFF",function(net){standardGeneric("getFF")})

#' @rdname getFF-methods
#' @aliases getFF,Net-method
setMethod(
  f="getFF",
  signature="Net",
  definition=function(net){
    return (net@ff)
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

#' Returns the ff state of the \code{\link{Net}}.
#' 
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getFF-methods
setGeneric("getFF",function(net){standardGeneric("getFF")})

#' @rdname getFF-methods
#' @aliases getFF,Net-method
setMethod(
  f="getFF",
  signature="Net",
  definition=function(net){
    return (net@ff)
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
    
    if (net@epochs >= net@momentumSwitch)
    {
      momentum <- net@finalMomentum
    }
    
    return (momentum)
  }
)

#' Returns the momentum switch of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getMomentumSwitch-methods
setGeneric("getMomentumSwitch",function(net){standardGeneric("getMomentumSwitch")})

#' @rdname getMomentumSwitch-methods
#' @aliases getMomentumSwitch,Net-method
setMethod(
  f="getMomentumSwitch",
  signature="Net",
  definition=function(net){
    return (net@momentumSwitch )
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

#' Returns the learn rate of the weights.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname getLearnRateWeights-methods
setGeneric("getLearnRateWeights",function(net){standardGeneric("getLearnRateWeights")})

#' @rdname getLearnRateWeights-methods
#' @aliases getLearnRateWeights,Net-method
setMethod(
  f="getLearnRateWeights",
  signature="Net",
  definition=function(net){
    return (net@learnRateWeights)
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
