#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getRBMList-methods
setGeneric("getRBMList",function(darch){standardGeneric("getRBMList")})

#' @rdname getRBMList-methods
#' @aliases getRBMList,DArch-method
setMethod(
  f="getRBMList",
  signature="DArch",
  definition=function(darch){
    return (darch@rbmList)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getLayers-methods
setGeneric("getLayers",function(darch){standardGeneric("getLayers")})

#' @rdname getLayers-methods
#' @aliases getLayers,DArch-method
setMethod(
  f="getLayers",
  signature="DArch",
  definition=function(darch){
    return (darch@layers)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getLayer-methods
setGeneric("getLayer",function(darch,index=1){standardGeneric("getLayer")})

#' @rdname getLayer-methods
#' @aliases getLayer,DArch-method
setMethod(
  f="getLayer",
  signature="DArch",
  definition=function(darch,index=1){
    return(darch@layers[[index]])
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getLayerWeights-methods
setGeneric("getLayerWeights",function(darch,index=1){standardGeneric("getLayerWeights")})

#' @rdname getLayerWeights-methods
#' @aliases getLayerWeights,DArch-method
setMethod(
  f="getLayerWeights",
  signature="DArch",
  definition=function(darch,index=1){
    if(getFF(darch)){
      return(darch@layers[[index]][[1]][])
    }
    return(darch@layers[[index]][[1]])
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getLayerFunction-methods
setGeneric("getLayerFunction",function(darch,index=1){standardGeneric("getLayerFunction")})

#' @rdname getLayerFunction-methods
#' @aliases getLayerFunction,DArch-method
setMethod(
  f="getLayerFunction",
  signature="DArch",
  definition=function(darch,index=1){
    return(darch@layers[[index]][[2]])
  }
)


#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getLayerField-methods
setGeneric("getLayerField",function(darch,layerIndex=1,fieldIndex=3){standardGeneric("getLayerField")})

#' @rdname getLayerField-methods
#' @aliases getLayerField,DArch-method
setMethod(
  f="getLayerField",
  signature="DArch",
  definition=function(darch,layerIndex=1,fieldIndex=3){
    if(getFF(darch)){
      return(darch@layers[[layerIndex]][[fieldIndex]][])
    }
    return(darch@layers[[layerIndex]][[fieldIndex]])
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getFineTuneFunction-methods
setGeneric("getFineTuneFunction",function(darch){standardGeneric("getFineTuneFunction")})

#' @rdname getFineTuneFunction-methods
#' @aliases getFineTuneFunction,DArch-method
setMethod(
  f="getFineTuneFunction",
  signature="DArch",
  definition=function(darch){
    return(darch@fineTuneFunction)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getExecuteFunction-methods
setGeneric("getExecuteFunction",function(darch){standardGeneric("getExecuteFunction")})

#' @rdname getExecuteFunction-methods
#' @aliases getExecuteFunction,DArch-method
setMethod(
  f="getExecuteFunction",
  signature="DArch",
  definition=function(darch){
    return(darch@executeFunction)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getExecOutput-methods
setGeneric("getExecOutput",function(darch,index=1){standardGeneric("getExecOutput")})

#' @rdname getExecOutput-methods
#' @aliases getExecOutput,DArch-method
setMethod(
  f="getExecOutput",
  signature="DArch",
  definition=function(darch,index=NULL){
    if(is.null(index)){
      index <- length(darch@executeOutput)
    }
    return(darch@executeOutput[[index]][])
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getExecOutputs-methods
setGeneric("getExecOutputs",function(darch){standardGeneric("getExecOutputs")})

#' @rdname getExecOutputs-methods
#' @aliases getExecOutputs,DArch-method
setMethod(
  f="getExecOutputs",
  signature="DArch",
  definition=function(darch){
    return(darch@executeOutput)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getLearnRateBiases-methods
setGeneric("getLearnRateBiases",function(darch){standardGeneric("getLearnRateBiases")})

#' @rdname getLearnRateBiases-methods
#' @aliases getLearnRateBiases,DArch-method
setMethod(
  f="getLearnRateBiases",
  signature="DArch",
  definition=function(darch){
    return (darch@learnRateBiases)
  }
)

#' Returns the cancel value.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getCancel-methods
setGeneric("getCancel",function(darch){standardGeneric("getCancel")})

#' @rdname getCancel-methods
#' @aliases getCancel,DArch-method
setMethod(
  f="getCancel",
  signature="DArch",
  definition=function(darch){
    return (darch@cancel)
  }
)

#' Returns the cancel message.
#' 
#' Returns the message, why the learing is canceled. If the message is not set,
#' the default value "no reason specified" will be returned.
#' 
#' @usage message <- getCancelMessage(darch)
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getCancelMessage-methods
setGeneric("getCancelMessage",function(darch){standardGeneric("getCancelMessage")})

#' @rdname getCancelMessage-methods
#' @aliases getCancelMessage,DArch-method
setMethod(
  f="getCancelMessage",
  signature="DArch",
  definition=function(darch){
    return (darch@cancelMessage)
  }
)