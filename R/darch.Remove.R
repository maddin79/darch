#' Removes a layer from the \code{\link{DArch}} object
#' 
#' This function removes the layer with the given index from the 
#' \code{\link{DArch}} object.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage removeLayerField(darch, index)
#' @seealso \code{\link{DArch}}
#' @return The \code{\link{DArch}} object without the layer.
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname removeLayerField-methods
setGeneric("removeLayerField",function(darch, index){standardGeneric("removeLayerField")})

#' @rdname removeLayerField-methods
#' @aliases removeLayerField,DArch-method
setMethod(
  f="removeLayerField",
  signature="DArch",
  definition=function(darch, index){
    if(index < 3){
      log4r::info(getLogger(darch),"You can not remove an element with an index less than 3")
      return(darch)
    }
    darch@layers[index] <- NULL
    return(darch)
  }
)