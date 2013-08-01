#' Resets the output list of the \code{\link{DArch}} object
#' 
#' This function sets the attribute \code{executeOutput} of the 
#' \code{\link{DArch}} object to an empty list.
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage resetExecOutput(darch)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname resetExecOutput-methods
setGeneric("resetExecOutput",function(darch){standardGeneric("resetExecOutput")})

#' @rdname resetExecOutput-methods
#' @aliases resetExecOutput,DArch-method
setMethod(
  f="resetExecOutput",
  signature="DArch",
  definition=function(darch){
    darch@executeOutput <- list()
    return(darch)
  }
)

#' Resets the weights and biases of the \code{\link{DArch}} object
#' 
#' This function resets the weights and biases of the \code{\link{DArch}} object 
#' and all \code{\link{RBM}} objects if the parameter \code{resetRBMs} is 
#' \code{TRUE}.
#' 
#' @details
#' When the parameter \code{resetRBMs} is \code{FALSE} then the trained weights
#' and biases are copied from the \code{\link{RBM}} objects to the layers.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @usage resetDArch(darch,resetRBMs=TRUE)
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname resetDArch-methods
setGeneric("resetDArch",function(darch,resetRBMs=TRUE){standardGeneric("resetDArch")})

#' @rdname resetDArch-methods
#' @aliases resetDArch,DArch-method
setMethod(
  f="resetDArch",
  signature="DArch",
  definition=function(darch,resetRBMs=TRUE){
    rbmList <- getRBMList(darch)
    layers <- getLayers(darch)
    rbmListLength <- length(rbmList)
    if(resetRBMs){
      for(i in 1:rbmListLength){
        rbmList[[i]] <- resetRBM(rbmList[[i]])
      }
    }
    
    setLayers(darch) <- list()
    
    for(i in 1:rbmListLength){
      rbm <- rbmList[[i]]
      darch <- addLayer(darch,getWeights(rbm),getHiddenBiases(rbm),sigmoidUnit)  			
    }
    
    if(rbmListLength < (length(layers))){
      for(i in (rbmListLength+1):(length(layers))){
        rows <- nrow(layers[[i]][[1]])-1
        cols <- ncol(layers[[i]][[1]])
        weights <- generateWeights(rows,cols)
        bias <-  matrix(0,1,cols)
        darch <- addLayer(darch,weights,bias,layers[[i]][[2]])
      }
    }
    
    return(darch)
  }
)