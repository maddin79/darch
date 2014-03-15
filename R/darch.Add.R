#' Adds a layer to the DArch object
#' 
#' Adds a layer to the given DArch object. The parameter weights and biases will
#' be put together in one matrix. 
#'
#' @param darch An instance of the class \code{\link{DArch}}.
#' @param weights The weights for the layer.
#' @param biases The biases for the layer.
#' @param unitFunction The functions of the units in the layer.
#' 
#' @usage addLayer(darch, weights, biases, unitFunction)
#' 
#' @seealso \code{\link{DArch}}, 
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}},
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname addLayer-methods
setGeneric("addLayer",function(darch, weights, biases,unitFunction){standardGeneric("addLayer")})

#' @rdname addLayer-methods
#' @aliases addLayer,DArch-method
setMethod(
  f="addLayer",
  signature="DArch",
  definition=function(darch, weights, biases,unitFunction){
    numLayers <- length(getLayers(darch))
    w <- rbind(weights,biases)
    layer <- list()
    if(getFF(darch)){
      layer[[1]] <- ff(vmode="double",dim=dim(w))
      layer[[1]][] <- w
    }else{
      layer[[1]] <- w
    }
    layer[[2]] <- unitFunction
    darch@layers[[numLayers+1]] <- layer
    return(darch)
  }
)

#' Adds a field to a layer
#' 
#' Adds a field to the layer given by the index. 
#'
#' @param darch An instance of the class \code{\link{DArch}}.
#' @param index The position of the layer.
#' @param field The new field for the layer.
#' 
#' @usage addLayerField(darch, index, field)
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname addLayerField-methods
setGeneric("addLayerField",function(darch, index, field){standardGeneric("addLayerField")})

#' @rdname addLayerField-methods
#' @aliases addLayerField,DArch-method
setMethod(
  f="addLayerField",
  signature="DArch",
  definition=function(darch, index, field){
    num <- length(darch@layers[[index]])
    darch@layers[[index]][[num+1]] <- field
    return(darch)
  }
)

#' Adds an execution output for a DArch object
#' 
#' This method can be used to save the execution outputs of every layer for the
#' DArch object. The outputs are saved in a list and every time this function is
#' called, the list is extended of one field with the new output.
#'
#' @param darch An instance of the class \code{\link{DArch}}.
#' @param output The output of the layer.
#' 
#' @usage addExecOutput(darch, output)
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname addExecOutput-methods
setGeneric("addExecOutput",function(darch, output){standardGeneric("addExecOutput")})

#' @rdname addExecOutput-methods
#' @aliases addExecOutput,DArch-method
setMethod(
  f="addExecOutput",
  signature="DArch",
  definition=function(darch,output){
    num <- length(darch@executeOutput)+1
    if(getFF(darch)){
      darch@executeOutput[[num]] <- ff(vmode="double",dim=dim(output))
      darch@executeOutput[[num]][] <- output
    }else{
      darch@executeOutput[[num]] <- output
    }
    return(darch)
  }
)