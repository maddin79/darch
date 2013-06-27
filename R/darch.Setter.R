#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setRBMList-methods
#' @include darch.R
setGeneric("setRBMList<-",function(darch,value){standardGeneric("setRBMList<-")})

#' @rdname setRBMList-methods
#' @aliases setRBMList<-,DArch-method
#' @name setRBMList
setReplaceMethod(
  f="setRBMList",
  signature="DArch",
  definition=function(darch,value){
    darch@rbmList <- value
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLayers-methods
#' @include darch.R
setGeneric("setLayers<-",function(darch,value){standardGeneric("setLayers<-")})

#' @rdname setLayers-methods
#' @aliases setLayers<-,DArch-method
#' @name setLayers
setReplaceMethod(
  f="setLayers",
  signature="DArch",
  definition=function(darch,value){
    darch@layers <- value
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLayer-methods
#' @include darch.R
setGeneric("setLayer<-",function(darch,index,value){standardGeneric("setLayer<-")})

#' @rdname setLayer-methods
#' @aliases setLayer<-,DArch-method
#' @name setLayer
setReplaceMethod(
  f="setLayer",
  signature="DArch",
  definition=function(darch,index,value){
    layer = list()
    if(darch@ff){
      if(!is.ff(darch@layers[[index]])){
        layer[[1]] <- ff(vmode="double",dim=dim(value))
      }
      layer[[1]][] <- value
    }else{
      layer[[1]] <- value
    }
    layer[[2]] <- sigmoidUnit
    darch@layers[[index]] <- layer
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLayerWeights-methods
#' @include darch.R
setGeneric("setLayerWeights<-",function(darch,index,value){standardGeneric("setLayerWeights<-")})

#' @rdname setLayerWeights-methods
#' @aliases setLayerWeights<-,DArch-method
#' @name setLayerWeights
setReplaceMethod(
  f="setLayerWeights",
  signature="DArch",
  definition=function(darch,index,value){
    if(darch@ff){
      if(!is.ff(darch@layers[[index]])){
        darch@layers[[index]][[1]] <- ff(vmode="double",dim=dim(value))
      }
      darch@layers[[index]][[1]][] <- value
    }else{
      darch@layers[[index]][[1]] <- value
    }
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLayerFunction-methods
#' @include darch.R
setGeneric("setLayerFunction<-",function(darch,index,value){standardGeneric("setLayerFunction<-")})

#' @rdname setLayerFunction-methods
#' @aliases setLayerFunction<-,DArch-method
#' @name setLayerFunction
setReplaceMethod(
  f="setLayerFunction",
  signature="DArch",
  definition=function(darch,index,value){
    darch@layers[[index]][[2]] <- value
    return (darch)
  }
)

#' Sets a field in a layer.
#' 
#' Sets the field on position \code{fieldIndex} of the layer given by the 
#' \code{layerIndex} to the \code{value}.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param layerIndex The index of the layer
#' @param fieldIndex The index of the field
#' @param value The value for the layer field
#' 
#' @return The darch with the updated layer
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setLayerField-methods
#' @include darch.R
setGeneric("setLayerField<-",function(darch,layerIndex,fieldIndex,value){standardGeneric("setLayerField<-")})

#' @rdname setLayerField-methods
#' @aliases setLayerField<-,DArch-method
#' @name setLayerField
setReplaceMethod(
  f="setLayerField",
  signature="DArch",
  definition=function(darch,layerIndex,fieldIndex,value){
    if(class(value) == "matrix"){
      if(darch@ff){
        if(length(darch@layers[[layerIndex]]) < fieldIndex || !is.ff(darch@layers[[layerIndex]][[fieldIndex]])){
          darch@layers[[layerIndex]][[fieldIndex]] <- ff(vmode="double",dim=dim(value))
        }
        darch@layers[[layerIndex]][[fieldIndex]][] <- value
      }else{
        darch@layers[[layerIndex]][[fieldIndex]] <- value
      }
      return (darch)
    }
    darch@layers[[layerIndex]][[fieldIndex]] <- value
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setFineTuneFunction-methods
#' @include darch.R
setGeneric("setFineTuneFunction<-",function(darch,value){standardGeneric("setFineTuneFunction<-")})

#' @rdname setFineTuneFunction-methods
#' @aliases setFineTuneFunction<-,DArch-method
#' @name setFineTuneFunction
setReplaceMethod(
  f="setFineTuneFunction",
  signature="DArch",
  definition=function(darch,value){
    darch@fineTuneFunction <- value
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setExecuteFunction-methods
#' @include darch.R
setGeneric("setExecuteFunction<-",function(darch,value){standardGeneric("setExecuteFunction<-")})

#' @rdname setExecuteFunction-methods
#' @aliases setExecuteFunction<-,DArch-method
#' @name setExecuteFunction
setReplaceMethod(
  f="setExecuteFunction",
  signature="DArch",
  definition=function(darch,value){
    darch@executeFunction <- value
    return (darch)
  }
)

#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value BEARBEITEN
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLearnRateBiases-methods
#' @include darch.R
setGeneric("setLearnRateBiases<-",function(darch,value){standardGeneric("setLearnRateBiases<-")})

#' @rdname setLearnRateBiases-methods
#' @aliases setLearnRateBiases<-,DArch-method
#' @name setLearnRateBiases
setReplaceMethod(
  f="setLearnRateBiases",
  signature="DArch",
  definition=function(darch,value){
    darch@learnRateBiases <-value
    return (darch)
  }
)

#' Set whether the learning shall be canceled.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value Boolean value if the learning shall canceled
#' 
#' @usage setCancel(darch) <- TRUE
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setCancel-methods
#' @include darch.R
setGeneric("setCancel<-",function(darch,value){standardGeneric("setCancel<-")})

#' @rdname setCancel-methods
#' @aliases setCancel<-,DArch-method
#' @name setCancel
setReplaceMethod(
  f="setCancel",
  signature="DArch",
  definition=function(darch,value){
    darch@cancel <-value
    return (darch)
  }
)

#' Sets the cancel message. 
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value The message for the termination
#' 
#' @usage setCancelMessage(darch) <- "Therefore!!!"
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setCancelMessage-methods
#' @include darch.R
setGeneric("setCancelMessage<-",function(darch,value){standardGeneric("setCancelMessage<-")})

#' @rdname setCancelMessage-methods
#' @aliases setCancelMessage<-,DArch-method
#' @name setCancelMessage
setReplaceMethod(
  f="setCancelMessage",
  signature="DArch",
  definition=function(darch,value){
    darch@cancelMessage <-value
    return (darch)
  }
)