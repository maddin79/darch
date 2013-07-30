#' Returns a list of \code{\link{RBM}}s of the 
#' \code{\link{DArch}} object.
#' 
#' This function returns a list of \code{\link{RBM}}s 
#' of the \code{\link{DArch}} which are needed for 
#' the pre training of the network. 
#' 
#' @value A list of all code{\link{RBM}}s of the 
#' \code{\link{DArch}} object.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getRBMList(darch)
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

#' Returns the a list of layers from the \code{\link{DArch}} 
#' object.
#' 
#' The function returns the layers list which contains all
#'  weights, functions for the neurons, and possible
#'   additional parameters for the training.
#' 
#' @value A list of layers from the \code{\link{DArch}} 
#' object.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getLayers(darch)
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

#' Returns a layer from the \code{\link{DArch}} object.
#' 
#' The function returns the layer with the given 
#' \code{index} which contains all weights, 
#' functions for the neurons, and possible additional
#'  parameters for the training.
#' 
#' @value A layers from the \code{\link{DArch}} object.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.Default is 1.
#' @usage getLayer(darch,index=1)
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

#' Returns the weights of a layer from the 
#' \code{\link{DArch}} object.
#' 
#' The function returns the weights of the layer with 
#' the given \code{index}. 
#' 
#' @value A weigth matrix.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.Default is 1.
#' @usage getLayerWeights(darch,index=1)
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

#' Returns the neuron function of a layer from the 
#' \code{\link{DArch}} object.
#' 
#' The function returns the neuron function of the layer 
#' with the given \code{index}. 
#' 
#' @value A neuron function.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.Default is 1.
#' @usage getLayerFunction(darch,index=1)
#' @seealso \code{\link{DArch}}
#'  \code{\link{sigmoidUnit}},
#' \code{\link{binSigmoidUnit}},
#' \code{\link{sigmoidUnitDerivative}},
#' \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
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


#' Returns the field of a layer from the 
#' \code{\link{DArch}} object.
#' 
#' The function returns the field given by the
#' \code{fieldIndex} of the layer given by the 
#' \code{layerIndex}. 
#' 
#' @value A content of the layer field.
#' 
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param layerIndex The index of the layer.
#' @param fieldIndex The index of the field in the layer.
#' 
#' @usage getLayerField(darch,layerIndex=1,fieldIndex=3)
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

#' Returns the fine tune function for the \code{\link{DArch}}
#' object.
#' 
#' Returns the fine tune function which is executed by the
#' function \code{\link{fineTuneDArch}}.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getFineTuneFunction(darch)
#' @seealso \code{\link{DArch}},
#' \code{\link{backpropagation}},
#' \code{\link{rpropagation}},
#' \code{\link{minimizeAutoencoder}},
#' \code{\link{minimizeClassifier}}
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

#' Returns the function for the execution of the \code{\link{DArch}} network.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getExecuteFunction(darch)
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

#' Returns the execution output of the layer from the \code{\link{DArch}} object
#' 
#' Returns the execution output of the layer by the given \code{index}. If the 
#' index is not set, the output of the last layer will be returned.
#' 
#' @value The output array of the layer. 
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.
#' @usage getExecOutput(darch,index=1)
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

#' Returns the execution output list of the 
#' \code{\link{DArch}} object
#' 
#' Returns the execution output of the 
#' \code{\link{DArch}} object. The list 
#' contains all outputs of every layer 
#' in the network.
#' 
#' @value A list with all output of every layer.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getExecOutput(darch,index=1)
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

#' Returns the learning rate for the bias weigths of the \code{\link{DArch}} 
#' object.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getLearnRateBiases(darch)
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
#' @usage getCancel(darch)
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
#' @usage getCancelMessage(darch)
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getCancelMessage(darch)
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