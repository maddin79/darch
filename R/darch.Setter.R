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

#' Sets the list of RBMs
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value The list of RBMs.
#' @usage setRBMList(darch) <- value
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @include darch.R
#' @rdname setRBMList-methods
setGeneric("setRBMList<-",function(darch,value){standardGeneric("setRBMList<-")})

#' @rdname setRBMList-methods
#' @aliases setRBMList<-,DArch-method
#' @name setRBMList<-
setReplaceMethod(
  f="setRBMList",
  signature="DArch",
  definition=function(darch,value){
    darch@rbmList <- value
    return (darch)
  }
)

#' Sets the layers for the network
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value The layers for the network.
#' @usage setLayers(darch) <- value
#' @seealso \code{\link{DArch}}
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

#' Sets a layer with the given index for the network
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer
#' @param value The layer for the network.
#' @usage setLayer(darch,index) <- value
#' @seealso \code{\link{DArch}}
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
    if (darch@ff){
      if (!is.ff(darch@layers[[index]])){
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

#' Sets the weights of a layer with the given index
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.
#' @param value The weights for the layer.
#' @usage setLayerWeights(darch,index) <- value
#' @seealso \code{\link{DArch}}
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
    # normalize weights
    if (darch@normalizeWeights)
    {
      value[1:(nrow(value) - 1),] <-
        apply(value[1:(nrow(value) - 1),, drop = F], 2,
              function (e) { e/norm(e, "2") })
    }
    
    if (darch@ff){
      if (!is.ff(darch@layers[[index]])){
        darch@layers[[index]][[1]] <- ff(vmode="double",dim=dim(value))
      }
      darch@layers[[index]][[1]][] <- value
    }else{
      darch@layers[[index]][[1]] <- value
    }
    return (darch)
  }
)

#' Sets the function for a layer with the given index
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param index The index of the layer.
#' @param value The function for the layer.
#' @usage setLayerFunction(darch,index) <- value
#' @seealso \code{\link{DArch}}
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
#' @usage setLayerField(darch,layerIndex,fieldIndex) <- value
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
    if (class(value) == "matrix"){
      if (darch@ff){
        if (length(darch@layers[[layerIndex]]) < fieldIndex || !is.ff(darch@layers[[layerIndex]][[fieldIndex]])){
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

#' Sets the fine tuning function for the network
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value The fine tuning function for the network.
#' @usage setFineTuneFunction(darch)  <- value
#' @seealso \code{\link{DArch}}
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

#' Sets the execution function for the network
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value The execution function for the network.
#' @usage setExecuteFunction(darch)  <- value
#' @seealso \code{\link{DArch}}
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

#' Sets the learning rate for the biases
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value The learning rate for the biases.
#' @usage setLearnRateBiases(darch)  <- value
#' @seealso \code{\link{DArch}}
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
#' @usage setCancel(darch) <- value
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
#' @usage setCancelMessage(darch) <- value
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

#' Sets the dropout rate for the input layer.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value Dropout rate for the input layer
#' @usage setDropoutInputLayer(darch) <- value
#' @return The darch with the updated dropout rate
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setDropoutInputLayer-methods
#' @include darch.R
setGeneric("setDropoutInputLayer<-",function(darch,value){standardGeneric("setDropoutInputLayer<-")})

#' @rdname setDropoutInputLayer-methods
#' @aliases setDropoutInputLayer<-,DArch-method
#' @name setDropoutInputLayer
setReplaceMethod(
  f="setDropoutInputLayer",
  signature="DArch",
  definition=function(darch,value){
    darch@dropoutInput <- value
    return (darch)
  }
)

#' Sets the dropout rate for the hidden layers.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value Dropout rate for the hidden layers
#' @usage setDropoutHiddenLayers(darch) <- value
#' @return The darch with the updated dropout rate
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setDropoutHiddenLayers-methods
#' @include darch.R
setGeneric("setDropoutHiddenLayers<-",function(darch,value){standardGeneric("setDropoutHiddenLayers<-")})

#' @rdname setDropoutHiddenLayers-methods
#' @aliases setDropoutHiddenLayers<-,DArch-method
#' @name setDropoutHiddenLayers
setReplaceMethod(
  f="setDropoutHiddenLayers",
  signature="DArch",
  definition=function(darch,value){
    darch@dropoutHidden <- value
    return (darch)
  }
)

#' Set dropout mask usage
#' 
#' Set whether the same dropout mask should be used for all batches of an epoch,
#' or a new set of masks should be generated for each batch (default).
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param value Boolean value indicating whether to use a dropout mask for each
#'   epoch.
#'   
#' @export
#' @include darch.R
setGeneric("setDropoutOneMaskPerEpoch<-",function(darch,value){standardGeneric("setDropoutOneMaskPerEpoch<-")})

#' Set dropout mask usage
#' 
#' @inheritParams setDropoutOneMaskPerEpoch<-
#' @seealso \link{setDropoutOneMaskPerEpoch<-}
#' @export
setReplaceMethod(
  f="setDropoutOneMaskPerEpoch",
  signature="DArch",
  definition=function(darch,value){
    darch@dropoutOneMaskPerEpoch <- value
    return (darch)
  }
)

#' Set the dropout masks.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param value List of dropout masks
#' @usage setDropoutMasks(darch) <- value
#' @return The darch with the updated dropout masks
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setDropoutMasks-methods
#' @include darch.R
setGeneric("setDropoutMasks<-",function(darch,value){standardGeneric("setDropoutMasks<-")})

#' @rdname setDropoutMasks-methods
#' @aliases setDropoutMasks<-,DArch-method
#' @name setDropoutMasks
setReplaceMethod(
  f="setDropoutMasks",
  signature="DArch",
  definition=function(darch,value){
    darch@dropoutMasks <- value
    return (darch)
  }
)

#' Set the dropout mask for the given layer.
#' 
#' Sets the dropout mask to be applied to the weights between layer i and i+1,
#' for 0 < i < numLayers. For i = 0, sets the dropout mask for the input layer,
#' which will be applied to the initiali input data.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param i Layer index or 0 for input layer.
#' @param value Dropout mask for the given layer.
#' @usage setDropoutMask(darch, i) <- value
#' @return The darch with the updated dropout mask
#' @seealso \code{\link{DArch}}
#' 
#' @export
#' @docType methods
#' @rdname setDropoutMask-methods
#' @include darch.R
setGeneric("setDropoutMask<-",function(darch,i,value){standardGeneric("setDropoutMask<-")})

#' @rdname setDropoutMask-methods
#' @aliases setDropoutMask<-,DArch-method
#' @name setDropoutMask
setReplaceMethod(
  f="setDropoutMask",
  signature="DArch",
  definition=function(darch,i,value){
    darch@dropoutMasks[[i+1]] <- value
    return (darch)
  }
)