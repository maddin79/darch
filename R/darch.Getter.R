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

#' Returns a list of \code{\link{RBM}}s of the \code{\linkS4class{DArch}} 
#' object
#' 
#' This function returns a list of \code{\link{RBM}}s of the 
#' \code{\linkS4class{DArch}} which are needed for the pre training of the 
#' network.
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return A list of all code{\link{RBM}}s of the \code{\linkS4class{DArch}}
#'   object.
#' @seealso \code{\linkS4class{DArch}}
#' @include darch.R
#' @export
setGeneric("getRBMList",function(darch){standardGeneric("getRBMList")})

#' Returns a list of \code{\link{RBM}}s of the \code{\linkS4class{DArch}} 
#' object
#' 
#' @inheritParams getRBMList
#' @seealso \link{getRBMList}
#' @export
setMethod(
  f="getRBMList",
  signature="DArch",
  definition=function(darch){
    return (darch@rbmList)
  }
)

#' Returns the a list of layers from the \code{\linkS4class{DArch}} object
#' 
#' The function returns the layers list which contains all weights, functions
#' for the neurons, and possible additional parameters for the training.
#'
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return A list of layers from the \code{\linkS4class{DArch}} object.
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getLayers",function(darch){standardGeneric("getLayers")})

#' Returns the a list of layers from the \code{\linkS4class{DArch}} object.
#' 
#' @inheritParams getLayers
#' @seealso \link{getLayers}
#' @export
setMethod(
  f="getLayers",
  signature="DArch",
  definition=function(darch){
    return (darch@layers)
  }
)

#' Returns a layer from the \code{\linkS4class{DArch}} object
#' 
#' The function returns the layer with the given \code{index} which contains all
#' weights, functions for the neurons, and possible additional parameters for
#' the training.
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param index The index of the layer. Default is 1.
#' @return A layers from the \code{\linkS4class{DArch}} object.
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getLayer",function(darch,index=1){standardGeneric("getLayer")})

#' Returns a layer from the \code{\linkS4class{DArch}} object
#' 
#' @inheritParams getLayer
#' @seealso \link{getLayer}
#' @export
setMethod(
  f="getLayer",
  signature="DArch",
  definition=function(darch,index=1){
    return(darch@layers[[index]])
  }
)

#' Returns the weights of a layer from the \code{\linkS4class{DArch}} object
#' 
#' The function returns the weights of the layer with the given \code{index}.
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param index The index of the layer.Default is 1.
#' @return A weight matrix.
#' @usage getLayerWeights(darch,index=1)
#' @seealso \code{\linkS4class{DArch}}
#'
#' @include darch.R
#'
#' @export
setGeneric("getLayerWeights",function(darch,index=1){standardGeneric("getLayerWeights")})

#' Returns the weights of a layer from the \code{\linkS4class{DArch}} object
#' 
#' @inheritParams getLayerWeights
#' @seealso \link{getLayerWeights}
#' @export
setMethod(
  f="getLayerWeights",
  signature="DArch",
  definition=function(darch,index=1){
    if (getFF(darch)){
      return(darch@layers[[index]][[1]][])
    }
    return(darch@layers[[index]][[1]])
  }
)

#' Returns the neuron function of a layer from the \code{\linkS4class{DArch}}
#' object
#' 
#' The function returns the neuron function of the layer with the given
#' \code{index}.
#' 
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param index The index of the layer.Default is 1.
#' @return A neuron function.
#' @seealso \code{\linkS4class{DArch}} \code{\link{sigmoidUnit}}, 
#'   \code{\link{binSigmoidUnit}}, \code{\link{sigmoidUnitDerivative}}, 
#'   \code{\link{linearUnitDerivative}}, \code{\link{softmaxUnit}}, 
#'   \code{\link{softmaxUnitDerivative}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getLayerFunction",function(darch,index=1){standardGeneric("getLayerFunction")})

#' Returns the neuron function of a layer from the \code{\linkS4class{DArch}}
#' object
#' 
#' @inheritParams getLayerFunction
#' @seealso \link{getLayerFunction}
#' @export
setMethod(
  f="getLayerFunction",
  signature="DArch",
  definition=function(darch,index=1){
    return(darch@layers[[index]][[2]])
  }
)


#' Returns the field of a layer from the \code{\linkS4class{DArch}} object
#' 
#' The function returns the field given by the \code{fieldIndex} of the layer
#' given by the \code{layerIndex}.
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param layerIndex The index of the layer.
#' @param fieldIndex The index of the field in the layer.
#' @return A content of the layer field.
#'   
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getLayerField",function(darch,layerIndex=1,fieldIndex=3){standardGeneric("getLayerField")})

#' Returns the field of a layer from the \code{\linkS4class{DArch}} object
#' 
#' @inheritParams getLayerField
#' @seealso \link{getLayerField}
#' @export
setMethod(
  f="getLayerField",
  signature="DArch",
  definition=function(darch,layerIndex=1,fieldIndex=3){
    if (getFF(darch)){
      return(darch@layers[[layerIndex]][[fieldIndex]][])
    }
    return(darch@layers[[layerIndex]][[fieldIndex]])
  }
)

#' Returns the fine tune function for the \code{\linkS4class{DArch}} object
#' 
#' Returns the fine tune function which is executed by the function
#' \code{\link{fineTuneDArch}}.
#' 
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @return Fine-tune function of the \code{\linkS4class{DArch}} object.
#' 
#' @seealso \code{\linkS4class{DArch}}, \code{\link{backpropagation}}, 
#'   \code{\link{rpropagation}}, \code{\link{minimizeAutoencoder}}, 
#'   \code{\link{minimizeClassifier}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getFineTuneFunction",function(darch){standardGeneric("getFineTuneFunction")})

#' Returns the fine tune function for the \code{\linkS4class{DArch}} object
#' 
#' @inheritParams getFineTuneFunction
#' @seealso \link{getFineTuneFunction}
#' @export
setMethod(
  f="getFineTuneFunction",
  signature="DArch",
  definition=function(darch){
    return(darch@fineTuneFunction)
  }
)

#' Returns the function for the execution of the \code{\linkS4class{DArch}}
#' network
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return Returns the execution, aka forward propagation, function.
#' 
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getExecuteFunction",function(darch){standardGeneric("getExecuteFunction")})

#' Returns the function for the execution of the \code{\linkS4class{DArch}}
#' network
#' 
#' @inheritParams getExecuteFunction
#' @seealso \link{getExecuteFunction}
#' @export
setMethod(
  f="getExecuteFunction",
  signature="DArch",
  definition=function(darch){
    return(darch@executeFunction)
  }
)

#' Returns the execution output of the layer from the \code{\linkS4class{DArch}}
#' object
#' 
#' Returns the execution output of the layer by the given \code{index}. If the 
#' index is not set, the output of the last layer will be returned.
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param index The index of the layer.
#' @return The output array of the layer.
#' 
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getExecOutput",function(darch,index=1){standardGeneric("getExecOutput")})

#' Returns the execution output of the layer from the \code{\linkS4class{DArch}}
#' object
#' 
#' @inheritParams getExecOutput
#' @seealso \link{getExecOutput}
#' @export
setMethod(
  f="getExecOutput",
  signature="DArch",
  definition=function(darch,index=NULL){
    if (is.null(index)){
      index <- length(darch@executeOutput)
    }
    return(darch@executeOutput[[index]][])
  }
)

#' Returns the execution output list of the \code{\linkS4class{DArch}} object
#' 
#' Returns the execution output of the \code{\linkS4class{DArch}} object. The
#' list contains all outputs of every layer in the network.
#' 
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}
#' @return A list with all output of every layer.
#'
#' @seealso \code{\linkS4class{DArch}}
#'
#' @include darch.R
#'
#' @export
setGeneric("getExecOutputs",function(darch){standardGeneric("getExecOutputs")})

#' Returns the execution output list of the \code{\linkS4class{DArch}} object
#' 
#' @inheritParams getExecOutputs
#' @seealso \link{getExecOutputs}
#' @export
setMethod(
  f="getExecOutputs",
  signature="DArch",
  definition=function(darch){
    return(darch@executeOutput)
  }
)

#' Returns the learning rate for the bias weights of the
#' \code{\linkS4class{DArch}} object
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return Learning rate of the biases.
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getLearnRateBiases",function(darch){standardGeneric("getLearnRateBiases")})

#' Returns the learning rate for the bias weights of the
#' \code{\linkS4class{DArch}} object
#' 
#' @inheritParams getLearnRateBiases
#' @seealso \link{getLearnRateBiases}
#' @export
setMethod(
  f="getLearnRateBiases",
  signature="DArch",
  definition=function(darch){
    return (darch@learnRateBiases)
  }
)

#' Returns the cancel value
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return Cancel value.
#' 
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getCancel",function(darch){standardGeneric("getCancel")})

#' Returns the cancel value
#' 
#' @inheritParams getCancel
#' @seealso \link{getCancel}
#' @export
setMethod(
  f="getCancel",
  signature="DArch",
  definition=function(darch){
    return (darch@cancel)
  }
)

#' Returns the cancel message
#' 
#' Returns the message, why the learning is canceled. If the message is not set,
#' the default value "no reason specified" will be returned.
#' 
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return Cancel message.
#' @seealso \code{\linkS4class{DArch}}
#'   
#' @include darch.R
#'   
#' @export
setGeneric("getCancelMessage",function(darch){standardGeneric("getCancelMessage")})

#' Returns the cancel message
#' 
#' @inheritParams getCancelMessage
#' @seealso \link{getCancelMessage}
#' @export
setMethod(
  f="getCancelMessage",
  signature="DArch",
  definition=function(darch){
    return (darch@cancelMessage)
  }
)

#' Returns the dropout rate for the input layer
#' 
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @return Dropout rate for the input layer.
#' @seealso \code{\linkS4class{DArch}}
#' @include darch.R
#' @export
setGeneric("getDropoutInputLayer",function(darch){standardGeneric("getDropoutInputLayer")})

#' Returns the dropout rate for the input layer
#' 
#' @inheritParams getDropoutInputLayer
#' @seealso \link{getDropoutInputLayer}
#' @export
setMethod(
  f="getDropoutInputLayer",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutInput)
  }
)

#' Returns the dropout rate for the hidden layers
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return Dropout rate for the hidden layers.
#' @seealso \code{\linkS4class{DArch}}
#' @include darch.R
#' @export
setGeneric("getDropoutHiddenLayers",function(darch){standardGeneric("getDropoutHiddenLayers")})

#' Returns the dropout rate for the hidden layers
#' 
#' @inheritParams getDropoutHiddenLayers
#' @seealso \link{getDropoutHiddenLayers}
#' @export
setMethod(
  f="getDropoutHiddenLayers",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutHidden)
  }
)

#' Return the dropout usage
#' 
#' Return whether the same dropout mask should be used for all batches of an
#' epoch, or a new set of masks should be generated for each batch (default).
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return Dropout usage.
#' @seealso \code{\linkS4class{DArch}}
#' @include darch.R
#' @export
setGeneric("getDropoutOneMaskPerEpoch",function(darch){standardGeneric("getDropoutOneMaskPerEpoch")})

#' Return the dropout usage
#' 
#' @inheritParams getDropoutOneMaskPerEpoch
#' @seealso \link{getDropoutOneMaskPerEpoch}
#' @export
setMethod(
  f="getDropoutOneMaskPerEpoch",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutOneMaskPerEpoch)
  }
)

#' Returns the dropout masks
#' 
#' Only available during fine-tuning, returns an empty list otherwise. Unlike 
#' \link{getDropoutMask}, this list is 1-based, starting with the 
#' dropout mask for the input layer at index 1. So \code{getDropoutMask(darch,
#' 0)} returns the same as \code{getDropoutMasks(darch)[[1]]}.
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @return List of dropout masks
#' @seealso \code{\linkS4class{DArch}}
#' @include darch.R
#' @export
setGeneric("getDropoutMasks",function(darch){standardGeneric("getDropoutMasks")})

#' Returns the dropout masks
#' 
#' @inheritParams getDropoutMasks
#' @seealso \link{getDropoutMasks}
#' @export
setMethod(
  f="getDropoutMasks",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutMasks)
  }
)

#' Returns the dropout mask for the given layer
#' 
#' The dropout mask is applied to the weights between layer i and i+1, for 0 < i
#' < numLayers. For i = 0, the dropout mask for the input layer is returned,
#' which will be applied to the initial input data.
#' 
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param i Layer index or 0 for input dropout mask.
#' @return Dropout mask for the given layer.
#' @seealso \code{\linkS4class{DArch}}
#' @include darch.R
#' @export
setGeneric("getDropoutMask",function(darch, i){standardGeneric("getDropoutMask")})

#' Returns the dropout mask for the given layer
#' 
#' @inheritParams getDropoutMask
#' @seealso \link{getDropoutMask}
#' @export
setMethod(
  f="getDropoutMask",
  signature="DArch",
  definition=function(darch, i){
    return (darch@dropoutMasks[[i+1]])
  }
)