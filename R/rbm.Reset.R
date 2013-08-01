#' Resets the weights and biases of the \code{\link{RBM}} object
#' 
#' This function resets the weights and biases of the \code{\link{RBM}} object.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage resetRBM(darch)
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' @include rbm.Setter.R
#' 
#' @export
#' @docType methods
#' @rdname resetRBM-methods
setGeneric(
  name="resetRBM",
  def=function(rbm){standardGeneric("resetRBM")}
)

#' @rdname resetRBM-methods
#' @aliases resetRBM,RBM-method
setMethod(
  f="resetRBM",
  signature="RBM",
  definition=function(rbm){
    numVisible <- getNumVisible(rbm)
    numHidden <- getNumHidden(rbm)
    
    setWeights(rbm) <- getGenWeightFunction(rbm)(numVisible,numHidden) 
    setHiddenBiases(rbm) <- matrix(0,1,numHidden)
    setVisibleBiases(rbm) <- matrix(0,1,numVisible)
    
    setWeightInc(rbm) <- matrix(0,numVisible,numHidden)
    setHiddenBiasesInc(rbm) <- matrix(0,1,numHidden)
    setVisibleBiasesInc(rbm) <- matrix(0,1,numVisible)
    rbm@stats <- list()
    
    return(rbm)
  }
)