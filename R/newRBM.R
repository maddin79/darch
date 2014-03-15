#' Constructor function for RBM object.
#' 
#' TODO: Doc ...
#' 
#' @param numVisible Number of visible units.
#' @param numHidden Number of hidden units.
#' @param batchSize Size of the batches
#' @param ff Indicates whether the \code{\link[ff]{ff}} package is used for the
#'        weights, biases and outputs
#' @param logLevel The logging level. 
#'        See \code{\link{setLogLevel}} for details.
#' @param genWeightFunc The function for generating the weight matrices
#' 
#' @return The new RBM object
#' @include rbm.R
#' @include rbm.Setter.R
#' @include rbm.Reset.R
#' @export
newRBM <- function(numVisible,numHidden,batchSize,ff=FALSE, logLevel=INFO,genWeightFunc=generateWeights){
  rbm <- new("RBM")
  setFF(rbm) <- ff
  setBatchSize(rbm) <- batchSize
  setNumHidden(rbm) <- numHidden
  setNumVisible(rbm) <- numVisible
  setGenWeightFunction(rbm) <- genWeightFunc
  flog.threshold(logLevel)
  flog.info(paste("Construct new RBM instance with ",numVisible, " visible and ", numHidden," hidden units.",sep=""))
  rbm <- resetRBM(rbm)
  
  return(rbm)
}