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

#' Returns the logger of the \code{\link{Net}}.
#'
#' @param net A instance of the class \code{\link{Net}}.
#'
#' @seealso \code{\link{Net}} and \code{\link[log4r]{create.logger}}
#' 
#' @export
#' @docType methods
#' @rdname getLogger-methods
setGeneric("getLogger",function(net){standardGeneric("getLogger")})

#' @rdname getLogger-methods
#' @aliases getLogger,Net-method
setMethod(
  f="getLogger",
  signature="Net",
  definition=function(net){
    return (net@logger)
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

#' Returns the momentum of the \code{\link{Net}}.
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
    return (net@momentum )
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
#' classifications and other usefull things from the pre-training or fine-tuning
#'  of a deep architecture.
#'
#' @param net A instance of the class \code{\link{Net}}.
#' @usage list <- getStats(net)
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