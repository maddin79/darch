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

#' Sets the batch size of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setBatchSize-methods
#' @include net.R
setGeneric("setBatchSize<-",function(net,value){standardGeneric("setBatchSize<-")})

#' @rdname setBatchSize-methods
#' @aliases setBatchSize<-,Net-method
#' @name setBatchSize
setReplaceMethod(
  f="setBatchSize",
  signature="Net",
  definition=function(net,value){
    net@batchSize <-value
    return (net)
  }
)

#' Sets the error function of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{function}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setErrorFunction-methods
setGeneric("setErrorFunction<-",function(net,value){standardGeneric("setErrorFunction<-")})

#' @rdname setErrorFunction-methods
#' @aliases setErrorFunction<-,Net-method
#' @name setErrorFunction
setReplaceMethod(
  f="setErrorFunction",
  signature="Net",
  definition=function(net,value){
    net@errorFunction <- value
    return (net)
  }
)

#' Sets the log level for the \code{\link{Net}}.
#'
#' The log levels a defined by the \code{\link{futile.logger}} package.
#' The following levels a available:
#' \tabular{ll}{
#' TRACE\cr 
#' DEBUG\cr
#' INFO\cr
#' WARN\cr
#' ERROR\cr
#' FATAL 
#' }
#' 
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLogLevel-methods
setGeneric("setLogLevel<-",function(net,value){standardGeneric("setLogLevel<-")})

#' @rdname setLogLevel-methods
#' @aliases setLogLevel<-,Net-method
#' @name setLogLevel
setReplaceMethod(
  f="setLogLevel",
  signature="Net",
  definition=function(net,value){
    if (value == TRACE | 
       value == DEBUG |
       value == WARN | 
       value == ERROR | 
       value == FATAL |
       value == INFO){
      flog.threshold(value)
    }else{
      flog.warn(net@logger,paste("It is not possible to set the log level to",value))
      flog.info(net@logger,paste("The current log level is:",flog.logger()$threshold))
    }
    return (net)
  }
)

#' Sets the function for generating weight matrices.
#' 
#' The function have to return a matrix with number of units in the lower layer
#' as number of rows and number of units in the upper layer as the number of 
#' columns.
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{function}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setGenWeightFunction-methods
setGeneric("setGenWeightFunction<-",function(net,value){standardGeneric("setGenWeightFunction<-")})

#' @rdname setGenWeightFunction-methods
#' @aliases setGenWeightFunction<-,Net-method
#' @name setGenWeightFunction
setReplaceMethod(
  f="setGenWeightFunction",
  signature="Net",
  definition=function(net,value){
    net@genWeightFunction <- value
    return (net)
  }
)

#' Sets the final momentum of the \code{\link{Net}}.
#' 
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setFinalMomentum-methods
setGeneric("setFinalMomentum<-",function(net,value){standardGeneric("setFinalMomentum<-")})

#' @rdname setFinalMomentum-methods
#' @aliases setFinalMomentum<-,Net-method
#' @name setFinalMomentum
setReplaceMethod(
  f="setFinalMomentum",
  signature="Net",
  definition=function(net,value){
    net@finalMomentum <-value
    return (net)
  }
)

#' Sets the initial momentum of the \code{\linkS4class{Net}}
#' 
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @export
setGeneric("setInitialMomentum<-",function(net,value){standardGeneric("setInitialMomentum<-")})

#' Sets the initial momentum of the \code{\linkS4class{Net}}
#' 
#' @inheritParams setInitialMomentum<-
#' @seealso \link{setInitialMomentum<-}
#' @export
setReplaceMethod(
  f="setInitialMomentum",
  signature="Net",
  definition=function(net,value){
    net@initialMomentum <-value
    return (net)
  }
)

#' Sets the learning rate.
#' 
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
setGeneric("setLearnRate<-",function(net,value){standardGeneric("setLearnRate<-")})

setReplaceMethod(
  f="setLearnRate",
  signature="Net",
  definition=function(net,value){
    net@learnRate <- value
    net@initialLearnRate <- value
    return (net)
  }
)

#' Adds a list of statistics to the network
#' 
#' The list of statistics can contain values about errors, miss 
#' classifications and other useful things from the pre-training or fine-tuning
#'  of a deep architecture.
#' 
#' @usage setStats(net) <- value
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Statistics for the \code{\link{Net}}.
#'  
#' @export
#' @docType methods
#' @rdname setStats-methods
#' @include net.R
setGeneric("setStats<-",function(net,value){standardGeneric("setStats<-")})

#' @rdname setStats-methods
#' @aliases setStats<-,Net-method
#' @name setStats
setReplaceMethod(
  f="setStats",
  signature="Net",
  definition=function(net,value){
    net@stats <- value
    return (net)
  }
)

#' Increment the number of epochs this \code{\linkS4class{Net}} has been trained
#' for
#'
#' @param net A instance of the class \code{\link{Net}}.
#'   
#' @export
#' @include net.R
setGeneric("incrementEpochs",function(net){standardGeneric("incrementEpochs")})

#' Increment the number of epochs this \code{\linkS4class{Net}} has been trained
#' for
#' 
#' @inheritParams incrementEpochs
#' @seealso \link{incrementEpochs}
#' @export
setMethod(
  f="incrementEpochs",
  signature="Net",
  definition=function(net){
    net@epochs <- net@epochs + 1
    return (net)
  }
)

#' Set whether weight normalization should be performed
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Boolean value indicating whether the weights should be
#' normalized
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @include net.R
setGeneric("setNormalizeWeights<-",function(net,value){standardGeneric("setNormalizeWeights<-")})

#' Set whether weight normalization should be performed
#' 
#' @inheritParams setNormalizeWeights<-
#' @seealso \link{setNormalizeWeights<-}
#' @export
setReplaceMethod(
  f="setNormalizeWeights",
  signature="Net",
  definition=function(net,value){
    net@normalizeWeights <- value
    return (net)
  }
)