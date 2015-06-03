# Copyright (C) 2015 darch2
# based on code by Martin Drees, copyright (C) 2013 Darch
#
# This file is part of darch2.
#
# Darch2 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch2.  If not, see <http://www.gnu.org/licenses/>.
#
#
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
#' @value A weight matrix.
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
#' @param darch A instance of the class \code{\link{DArch}}
#' 
#' @usage getExecOutputs(darch)
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

#' Returns the learning rate for the bias weights of the \code{\link{DArch}} 
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
#' Returns the message, why the learning is canceled. If the message is not set,
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

#' Returns the dropout rate for the input layer.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getDropoutInputLayer(darch)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getDropoutInputLayer-methods
setGeneric("getDropoutInputLayer",function(darch){standardGeneric("getDropoutInputLayer")})

#' @rdname getDropoutInputLayer-methods
#' @aliases getDropoutInputLayer,DArch-method
setMethod(
  f="getDropoutInputLayer",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutInput)
  }
)

#' Returns the dropout rate for the hidden layers.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getDropoutHiddenLayers(darch)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getDropoutHiddenLayers-methods
setGeneric("getDropoutHiddenLayers",function(darch){standardGeneric("getDropoutHiddenLayers")})

#' @rdname getDropoutHiddenLayers-methods
#' @aliases getDropoutHiddenLayers,DArch-method
setMethod(
  f="getDropoutHiddenLayers",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutHidden)
  }
)

#' Returns the dropout masks.
#' 
#' Only available during fine-tuning, returns an empty list otherwise. Unlike
#' \link{getDropoutMask(darch, i)}, this list is 1-based, starting with the
#' dropout mask for the input layer at index 1. So
#' \code{getDropoutMask(darch, 0)} returns the same as
#' \code{getDropoutMasks(darch)[[1]]}.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage getDropoutMasks(darch)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getDropoutMasks-methods
setGeneric("getDropoutMasks",function(darch){standardGeneric("getDropoutMasks")})

#' @rdname getDropoutMasks-methods
#' @aliases getDropoutMasks,DArch-method
setMethod(
  f="getDropoutMasks",
  signature="DArch",
  definition=function(darch){
    return (darch@dropoutMasks)
  }
)

#' Returns the dropout mask for the given layer.
#' 
#' The dropout mask is applied to the weights between layer i and i+1, for
#' 0 < i < numLayers. For i = 0, the dropout mask for the input layer is
#' returned, which will be applied to the initial input data.
#' 
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param i Layer index or 0 for input dropout mask.
#' @usage getDropoutMask(darch, i)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname getDropoutMask-methods
setGeneric("getDropoutMask",function(darch, i){standardGeneric("getDropoutMask")})

#' @rdname getDropoutMask-methods
#' @aliases getDropoutMask,DArch-method
setMethod(
  f="getDropoutMask",
  signature="DArch",
  definition=function(darch, i){
    return (darch@dropoutMask[[i+1]])
  }
)